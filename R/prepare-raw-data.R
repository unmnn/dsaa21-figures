library(tidyverse)

community_data <- jsonlite::read_json("data/community_data.json", simplifyVector = TRUE)

community_data <- lapply(community_data, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

community_data <- bind_cols(community_data)

write_rds(community_data, "data/community_data.rds")



# 
# weight_significance <- read_csv("data/weight_significance.csv")
# 
# weight_significance %>%
#   mutate(e1 = str_replace(e, "\\(([:digit:]+), .*", "\\1")) %>%
#   mutate(e2 = str_replace(e, ".*, ([:digit:]+)\\).*", "\\1"))
