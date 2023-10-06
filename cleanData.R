library(tidyverse)
library(readxl)
library(janitor)

## read and clean data ----
sheet.names <- excel_sheets("data/20230419_Beispielfälle_alle_Empfehlungen.xlsx")
cols_to_fill <- c("galactica_v1", "galactica_v2", "bio_med_lm_v1",
                  "bio_med_lm_v2", "bio_med_lm_v3", "bio_med_lm_v4",
                  "perplexity_ai_v1", "perplexity_v2")
lapply(sheet.names, function(x){
  read_excel("20230419_Beispielfälle_alle_Empfehlungen.xlsx", sheet = x)
}) %>% 
  bind_rows() %>% 
  clean_names() %>% 
  fill(model, .direction = "down") %>% 
  mutate(across(all_of(cols_to_fill), ~ifelse(is.na(.), 0, .)))  %>% 
  fill(model, .direction = "down") %>% 
  mutate(recommendation = tolower(recommendation)) %>% 
  mutate(recommendation = str_replace(recommendation, "\\(and", "and")) %>% 
  mutate(recommendation = str_remove(recommendation, "\\(.*\\)$")) %>% 
  mutate(recommendation = str_remove(recommendation, "\\)$")) %>% 
  remove_empty() %>% 
  select(-x19, -x20) %>% 
  mutate_all(.funs = str_squish) %>% 
  write_csv("data/RecommendationsClean.csv")

