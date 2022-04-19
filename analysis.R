# load packages ----------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(tidylo)
library(tidymodels)
library(textrecipes)
library(vip)
library(colorblindr)

# load data --------------------------------------------------------------------

sc_raw <- read_csv(here::here("processed-data/schitts-creek.csv"))
dc_raw <- read_csv(here::here("processed-data/dawsons-creek.csv"))

show_lines_raw <- bind_rows(sc_raw, dc_raw)

# remove stage directions ------------------------------------------------------

show_lines <- show_lines_raw %>%
  filter(
    character != "Stage directions",
    !is.na(character)
    )

# characters -------------------------------------------------------------------

sc_characters <- sc_raw %>%
  count(character, sort = TRUE) %>%
  filter(n > 100) %>%
  pull(character) %>%
  str_to_lower()

dc_characters <- dc_raw %>%
  count(character, sort = TRUE) %>%
  filter(n > 30) %>%
  pull(character) %>%
  str_to_lower()

# eda --------------------------------------------------------------------------

show_lines %>%
  unnest_tokens(word, line) %>%
  anti_join(stop_words) %>%
  count(show, word, sort = TRUE) %>%
  group_by(show) %>%
  slice_head(n = 10) %>%
  ggplot(aes(y = fct_reorder(word, n), x = n)) +
  geom_col() +
  facet_wrap(~show, scales = "free")

show_lines %>%
  unnest_tokens(bigram, line, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) %>%
  count(show, bigram, sort = TRUE) %>%
  group_by(show) %>%
  slice_head(n = 10) %>%
  ggplot(aes(y = fct_reorder(bigram, n), x = n)) +
  geom_col() +
  facet_wrap(~show, scales = "free")

show_lines %>%
  unnest_tokens(word, line) %>%
  anti_join(stop_words) %>%
  count(show, word) %>%
  bind_log_odds(show, word, n) %>%
  group_by(show) %>%
  arrange(desc(log_odds_weighted)) %>%
  slice_head(n = 10)

show_lines %>%
  unnest_tokens(word, line) %>%
  anti_join(stop_words) %>%
  count(show, word) %>%
  bind_log_odds(show, word, n) %>%
  group_by(show) %>%
  arrange(desc(log_odds_weighted)) %>%
  filter(
      !(word %in% sc_characters),
      !(word %in% dc_characters),
      word != "dawson's",
      word != "jen's",
    ) %>%
  slice_head(n = 10) %>%
  mutate(
    log_odds_weighted = if_else(show == "Dawson's Creek", -1*log_odds_weighted, log_odds_weighted)
  ) %>%
  ggplot(aes(
    y = fct_reorder(word, log_odds_weighted),
    x = log_odds_weighted,
    fill = show
    )) +
  geom_col() +
  theme_minimal() +
  scale_fill_OkabeIto() +
  theme(legend.position = c(0.85, 0.2))

# initial split ----------------------------------------------------------------

set.seed(123)
show_lines_split <- initial_split(data = show_lines, strata = show)

show_lines_train <- training(show_lines_split)
show_lines_test <- testing(show_lines_split)

show_lines_folds <- vfold_cv(data = show_lines_train, strata = show)

# recipe -----------------------------------------------------------------------

show_lines_rec_1 <- recipe(show ~ line, data = show_lines_train) %>%
  step_tokenize(line) %>%
  step_stopwords(line) %>%
  step_tokenfilter(line, max_tokens = 100) %>%
  step_tfidf(line)

show_lines_rec_2 <- recipe(show ~ line, data = show_lines_train) %>%
  step_tokenize(line) %>%
  step_stopwords(line) %>%
  step_ngram(line, num_tokens = 3L, min_num_tokens = 1L) %>%
  step_tokenfilter(line, max_tokens = 100) %>%
  step_tfidf(line)

# model specification ----------------------------------------------------------

glm_spec <- logistic_reg() %>%
  set_engine("glm")

# workflow ---------------------------------------------------------------------

glm_workflow_1 <- workflow() %>%
  add_recipe(show_lines_rec_1) %>%
  add_model(glm_spec)

glm_workflow_2 <- workflow() %>%
  add_recipe(show_lines_rec_2) %>%
  add_model(glm_spec)

# cv ---------------------------------------------------------------------------

glm_cv_1 <- glm_workflow_1 %>%
  fit_resamples(
    resamples = show_lines_folds,
    control = control_resamples(save_pred = TRUE)
  )

glm_cv_2 <- glm_workflow_2 %>%
  fit_resamples(
    resamples = show_lines_folds,
    control = control_resamples(save_pred = TRUE)
  )

write_rds(glm_cv_1, file = "intermediary-output/glm_cv_1.rds")
write_rds(glm_cv_2, file = "intermediary-output/glm_cv_2.rds")

# collect metrics --------------------------------------------------------------

glm_cv_metrics_1 <- collect_metrics(glm_cv_1)
glm_cv_predictions_1 <- collect_predictions(glm_cv_1)

glm_cv_metrics_2 <- collect_metrics(glm_cv_2)
glm_cv_predictions_2 <- collect_predictions(glm_cv_2)

glm_cv_metrics_1
glm_cv_metrics_2

glm_cv_predictions_1 %>%
  group_by(id) %>%
  roc_curve(truth = show, `.pred_Dawson's Creek`) %>%
  autoplot()

glm_cv_predictions_2 %>%
  group_by(id) %>%
  roc_curve(truth = show, `.pred_Dawson's Creek`) %>%
  autoplot()

conf_mat_resampled(x = glm_cv_1, tidy = FALSE) %>%
  autoplot(type = "heatmap")

conf_mat_resampled(x = glm_cv_2, tidy = FALSE) %>%
  autoplot(type = "heatmap")

# final fit --------------------------------------------------------------------

glm_final_1 <- glm_workflow_1 %>%
  last_fit(split = show_lines_split)

glm_final_2 <- glm_workflow_2 %>%
  last_fit(split = show_lines_split)

write_rds(glm_final_1, file = "intermediary-output/glm_final_1.rds")
write_rds(glm_final_2, file = "intermediary-output/glm_final_2.rds")

collect_metrics(glm_final_1)
collect_metrics(glm_final_2)

# variable importance ----------------------------------------------------------

glm_imp_1 <- extract_fit_parsnip(glm_final_1) %>% vi()
glm_imp_2 <- extract_fit_parsnip(glm_final_2) %>% vi()

glm_imp_1 %>%
  mutate(
    Variable = str_remove(Variable, "tfidf_line_"),
    Sign = case_when(
      Sign == "POS" ~ "More likely from Schitt's Creek",
      Sign == "NEG" ~ "More likely from Dawson's Creen"
    ),
    Importance = abs(Importance)
  ) %>%
  group_by(Sign) %>%
  # extract 20 most important n-grams for each artist
  slice_max(order_by = Importance, n = 20) %>%
  ggplot(mapping = aes(
    x = Importance,
    y = fct_reorder(Variable, Importance),
    fill = Sign
  )) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_OkabeIto() +
  facet_wrap(facets = vars(Sign), scales = "free") +
  labs(
    y = NULL,
    title = "Variable importance for predicting the show",
    subtitle = "These features are the most important in predicting\nwhether a line is from Dawson's or Schitt's Creek"
  )

glm_imp_2 %>%
  mutate(
    Variable = str_remove(Variable, "tfidf_line_"),
    Sign = case_when(
      Sign == "POS" ~ "More likely from Schitt's Creek",
      Sign == "NEG" ~ "More likely from Dawson's Creen"
    ),
    Importance = abs(Importance)
  ) %>%
  group_by(Sign) %>%
  # extract 20 most important n-grams for each artist
  slice_max(order_by = Importance, n = 20) %>%
  ggplot(mapping = aes(
    x = Importance,
    y = fct_reorder(Variable, Importance),
    fill = Sign
  )) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_OkabeIto() +
  facet_wrap(facets = vars(Sign), scales = "free") +
  labs(
    y = NULL,
    title = "Variable importance for predicting the show",
    subtitle = "These features are the most important in predicting\nwhether a line is from Dawson's or Schitt's Creek"
  )

glm_imp_1 %>%
  mutate(
    Variable = str_remove(Variable, "tfidf_line_"),
    Sign = case_when(
      Sign == "POS" ~ "More likely from Schitt's Creek",
      Sign == "NEG" ~ "More likely from Dawson's Creen"
    ),
    Importance = abs(Importance)
  ) %>%
  group_by(Sign) %>%
  # extract 20 most important n-grams for each artist
  slice_max(order_by = Importance, n = 20) %>%
  ggplot(mapping = aes(
    x = Importance,
    y = fct_reorder(Variable, Importance),
    fill = Sign
  )) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_OkabeIto() +
  facet_wrap(facets = vars(Sign), scales = "free") +
  labs(
    y = NULL,
    title = "Variable importance for predicting the show",
    subtitle = "These features are the most important in predicting\nwhether a line is from Dawson's or Schitt's Creek"
  )

glm_imp_2 %>%
  mutate(
    Variable = str_remove(Variable, "tfidf_line_"),
    Sign = case_when(
      Sign == "POS" ~ "More likely from Schitt's Creek",
      Sign == "NEG" ~ "More likely from Dawson's Creen"
    ),
    Importance = abs(Importance)
  ) %>%
  group_by(Sign) %>%
  # extract 20 most important n-grams for each artist
  slice_max(order_by = Importance, n = 20) %>%
  ggplot(mapping = aes(
    x = Importance,
    y = fct_reorder(Variable, Importance),
    fill = Sign
  )) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_OkabeIto() +
  facet_wrap(facets = vars(Sign), scales = "free") +
  labs(
    y = NULL,
    title = "Variable importance for predicting the show",
    subtitle = "These features are the most important in predicting\nwhether a line is from Dawson's or Schitt's Creek"
  )
