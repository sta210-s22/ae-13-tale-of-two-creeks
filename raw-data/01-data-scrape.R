# load packages ----------------------------------------------------------------

library(tidyverse)

# sample code for data scraping ------------------------------------------------

# load packages ----------------------------------------------------------------

library(tidyverse)
library(rvest)

# dawson's creek (sample code) -------------------------------------------------

# source: https://transcripts.foreverdreaming.org/viewforum.php?f=20&sid=c19393ed4cea74bdb7b028a2756da94c

dc_index_page <- read_html("https://transcripts.foreverdreaming.org/viewtopic.php?f=20&t=6197")

dc_urls <- dc_index_page %>%
  html_nodes(".postlink") %>%
  html_attr("href") %>%
  str_subset("copyright/", negate = TRUE)

script_page <- read_html(dc_urls[1])

script_page %>%
  html_node(".postbody") %>%
  html_text() %>%
  str_remove_all("\\[.*\\]") %>%
  str_remove_all("\\(.*\\)") %>%
  str_remove_all("\n\n\n\t\t\t\t\t\t") %>%
  str_remove(";\n\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t") %>%
  str_replace_all("\n \n", "\n") %>%
  str_trim()
