library(tidyverse)
library(jsonlite)

csv <- read_csv("data/jdsa_membership_best_parition_uli.csv")

load("data/uhreg.rda")

csv <- csv %>%
  inner_join(uhreg %>% mutate(.META_patient_id = as.double(.META_patient_id)), by = c(".META_patient_id")) %>%
  filter(.META_visit_type == "S") %>%
  group_by(.META_patient_id) %>%
  arrange(.META_visit_day) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(TSCHQ_q02_sex = factor(TSCHQ_q02_sex, labels = c("male", "female")))

write_rds(csv, "data/jdsa.rds")