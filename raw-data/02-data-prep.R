# load packages ----------------------------------------------------------------

library(tidyverse)
library(fs)

# load data --------------------------------------------------------------------

sc_paths <- dir_ls(here::here("raw-data/schitts-creek/"))
dc_paths <- dir_ls(here::here("raw-data/dawsons-creek/"))

sc_raw <- read_delim(sc_paths, delim = "\n", col_names = "line", id = "file_path")
dc_raw <- read_delim(dc_paths, delim = "\n", col_names = "line", id = "file_path")

# schitts creek ----------------------------------------------------------------

sc_temp <- sc_raw %>%
  mutate(
    file_path = str_remove(file_path, "\\.txt"),
    file_path = str_remove(file_path, "data/schitts-creek/sc-")
  ) %>%
  separate(file_path, sep = "-", into = c("season", "episode")) %>%
  rowid_to_column(var = "line_no") %>%
  relocate(season, episode)

sc_stage_directions <- sc_temp %>%
  filter(str_detect(line, "^\\(")) %>%
  mutate(character = "Stage directions")

sc_lines <- sc_temp %>%
  filter(!str_detect(line, "^\\(")) %>%
  separate(line, sep = ":", into = c("temp", "line"), extra = "merge") %>%
  mutate(
    character = if_else(!is.na(line), temp, NA_character_),
    line = if_else(is.na(line), temp, line),
    line = str_trim(line)
    ) %>%
  fill(character) %>%
  select(-temp)

sc <- sc_stage_directions %>%
  bind_rows(sc_lines) %>%
  arrange(line_no) %>%
  mutate(show = "Schitt's Creek") %>%
  relocate(show, season, episode, character, line)

# dawsons creek ----------------------------------------------------------------

dc_temp <- dc_raw %>%
  mutate(
    file_path = str_remove(file_path, "\\.txt"),
    file_path = str_remove(file_path, "data/dawsons-creek/dc-")
  ) %>%
  separate(file_path, sep = "-", into = c("season", "episode")) %>%
  rowid_to_column(var = "line_no") %>%
  relocate(season, episode)

dc_stage_directions <- dc_temp %>%
  filter(str_detect(line, "^\\(")) %>%
  mutate(character = "Stage directions")

dc_lines <- dc_temp %>%
  filter(!str_detect(line, "^\\(")) %>%
  separate(line, sep = ":", into = c("temp", "line"), extra = "merge") %>%
  mutate(
    character = if_else(!is.na(line), temp, NA_character_),
    line = if_else(is.na(line), temp, line),
    line = str_trim(line)
  ) %>%
  fill(character) %>%
  select(-temp)

dc <- dc_stage_directions %>%
  bind_rows(dc_lines) %>%
  arrange(line_no) %>%
  mutate(show = "Dawson's Creek") %>%
  relocate(show, season, episode, character, line)

# save datasets ----------------------------------------------------------------

write_csv(sc, file = here::here("processed-data/schitts-creek.csv"))
write_csv(dc, file = here::here("processed-data/dawsons-creek.csv"))
