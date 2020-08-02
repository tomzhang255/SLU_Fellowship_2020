library(tidyverse)
library(quantreg)

cr <- read_csv("~/research/CR_Project_Tom/data_processed/quantmod_s11_data.csv")



# filter 2000-4000
# and convert battletime -> day of the month
cr1 <-
  cr %>%
  filter(startingtrophies >= 2000 & startingtrophies < 4000) %>%
  mutate(battletime = str_remove(battletime, "\\s.+")) %>%
  mutate(battletime = str_remove(battletime, "[:digit:]{4}-[:digit:]{2}-")) %>%
  mutate(battletime = as.numeric(battletime))



# filter 4000-4500
# and convert battletime -> day of the month
cr2 <-
  cr %>%
  filter(startingtrophies >= 4000 & startingtrophies < 4500) %>%
  mutate(battletime = str_remove(battletime, "\\s.+")) %>%
  mutate(battletime = str_remove(battletime, "[:digit:]{4}-[:digit:]{2}-")) %>%
  mutate(battletime = as.numeric(battletime))



# filter 4500-5500
# and convert battletime -> day of the month
cr3 <-
  cr %>%
  filter(startingtrophies >= 4500 & startingtrophies < 5500) %>%
  mutate(battletime = str_remove(battletime, "\\s.+")) %>%
  mutate(battletime = str_remove(battletime, "[:digit:]{4}-[:digit:]{2}-")) %>%
  mutate(battletime = as.numeric(battletime))



# filter 5500+ no winsorizing
# and convert battletime -> day of the month
cr4 <-
  cr %>%
  filter(startingtrophies >= 5500) %>%
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
      paste("startingtrophies ~ "), # response
      paste(names(cr)[-c(2:100)], collapse =  " + ")#, " + ", #  battletime and  first level vars
      #paste(names(cr)[-(1:2)], collapse =  " + "), " + ", # no battletime and yes first level vars
#      paste(quad_level_terms, collapse =  " + "), " + ", # quadratic level vars
      #paste(interactions[-1:-98], collapse = " + "), " + ",
#      paste("battletime:", c(names(cr)[101:198], quad_level_terms), sep = "", collapse = " + ") # battle interact with levels and levels^2
    )
  )



# fit model
rq_s11_slice_1 <- rq(
  formula = f, data = cr1, tau = c(0.25, 0.50, 0.75, 0.95)
  #, method = "sfn",
  #control = list(tmpmax= dim(cr)[2]*600)
)

saveRDS(rq_s11_slice_1, file = "rq_s11_slice_1.rds")

rm(rq_s11_slice_1)



rq_s11_slice_2 <- rq(formula = f, data = cr2, tau = c(0.25, 0.50, 0.75, 0.95))

saveRDS(rq_s11_slice_2, file = "rq_s11_slice_2.rds")

rm(rq_s11_slice_2)



rq_s11_slice_3 <- rq(formula = f, data = cr3, tau = c(0.25, 0.50, 0.75, 0.95))

saveRDS(rq_s11_slice_3, file = "rq_s11_slice_3.rds")

rm(rq_s11_slice_3)



rq_s11_slice_4 <- rq(formula = f, data = cr4, tau = c(0.25, 0.50, 0.75, 0.95))

saveRDS(rq_s11_slice_4, file = "rq_s11_slice_4.rds")

rm(rq_s11_slice_4)









