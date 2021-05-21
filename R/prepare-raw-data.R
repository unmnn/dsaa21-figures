library(tidyverse)
library(jsonlite)

community_data <- jsonlite::read_json("data/community_data.json", simplifyVector = TRUE)

community_data <- lapply(community_data, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

community_data <- bind_cols(community_data)

write_rds(community_data, "data/community_data.rds")


# dir("data", pattern = "score\\.csv", full.names = TRUE) %>%
#   walk(~ vroom::vroom(.x) %>%
#          select(significance, weight = 3) %>%
#          mutate(q = str_replace(.x, ".*significance_(.+)_score.*", "\\1")) %>%
#          write_rds(str_replace(.x, "csv", "rds"), compress = "gz")
#   )

dir("data", pattern = "score\\.rds", full.names = TRUE) %>%
  walk(
    ~ read_rds(.x) %>%
      mutate(weight_disc = ggplot2::cut_number(weight, 5)) %>%
      mutate(sig_disc = significance < 0.05) %>%
      count(q, weight_disc, sig_disc) %>%
      write_rds(str_replace(.x, "\\.rds", "-count\\.rds"))
  )