library(tidyverse)
library(quantreg)

cr <- read_csv("~/research/CR_Project_Tom/data_processed/logit_s11_data.csv")

# winsorize 6000+ = 6000
# cr <- 
#   cr %>%
#     mutate(startingtrophies = if_else(startingtrophies > 6000, 6000, startingtrophies))



# filter 4300-6000
# and convert battletime -> day of the month
cr <-
  cr %>%
  filter(startingtrophies >= 4300) %>%
  mutate(battletime = str_remove(battletime, "\\s.+")) %>%
  mutate(battletime = str_remove(battletime, "[:digit:]{4}-[:digit:]{2}-")) %>%
  mutate(battletime = as.numeric(battletime))



# build formula
quad_level_terms = cr %>%
  names() %>%
  str_detect(., pattern = "Level") %>%
  which() %>%
  names(cr)[.] %>%
  paste("I(",., "^2)", sep = "")

f <- # trophies ~ battletime (day) + levels (first degree)
  as.formula(
    paste(
      paste("trophychange ~ "), # response
      paste(names(cr)[-3], collapse =  " + ")#, " + ", #  battletime and  first level vars
      #paste(names(cr)[-(1:2)], collapse =  " + "), " + ", # no battletime and yes first level vars
#      paste(quad_level_terms, collapse =  " + "), " + ", # quadratic level vars
      #paste(interactions[-1:-98], collapse = " + "), " + ",
#      paste("battletime:", c(names(cr)[101:198], quad_level_terms), sep = "", collapse = " + ") # battle interact with levels and levels^2
    )
  )



# fit model
logit_s11_noWinsor <- glm(formula = f, data = cr, family = "binomial")

saveRDS(logit_s11_noWinsor, file = "logit_s11_noWinsor.rds")

rm(logit_s11_noWinsor)





