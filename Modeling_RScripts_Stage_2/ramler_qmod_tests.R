library(tidyverse)
library(quantreg)

#qmod.75 = readRDS("~/research/CR_Project_Tom/data_processed/quantmod_s11_75.rds")

cr <- read_csv("~/research/CR_Project_Tom/data_processed/quantmod_s11_data.csv")



# filter 4000+
# and convert battletime -> day of the month
cr <-
  cr %>%
  filter(startingtrophies >= 4001 & startingtrophies < 6600) %>%
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
quantmod_s11_rq_25 <- rq(
  formula = f, data = cr, tau = 0.25#, method = "sfn",
  #control = list(tmpmax= dim(cr)[2]*600)
)

saveRDS(quantmod_s11_rq_25, file = "quantmod_s11_rq_25.RDS")

rm(quantmod_s11_rq_25)



quantmod_s11_rq_50 <- rq(formula = f, data = cr, tau = 0.50)

saveRDS(quantmod_s11_rq_50, file = "quantmod_s11_rq_50.RDS")

rm(quantmod_s11_rq_50)



quantmod_s11_rq_75 <- rq(formula = f, data = cr, tau = 0.75)

saveRDS(quantmod_s11_rq_75, file = "quantmod_s11_rq_75.RDS")

rm(quantmod_s11_rq_75)



quantmod_s11_rq_95 <- rq(formula = f, data = cr, tau = 0.95)

saveRDS(quantmod_s11_rq_95, file = "quantmod_s11_rq_95.RDS")

rm(quantmod_s11_rq_95)







quantmod_s11_rqss_25 <- rqss(formula = f, data = cr, tau = 0.25)

saveRDS(quantmod_s11_rqss_25, file = "quantmod_s11_rqss_25.RDS")

rm(quantmod_s11_rqss_25)



quantmod_s11_rqss_50 <- rqss(formula = f, data = cr, tau = 0.50)

saveRDS(quantmod_s11_rqss_50, file = "quantmod_s11_rqss_50.RDS")

rm(quantmod_s11_rqss_50)



quantmod_s11_rqss_75 <- rqss(formula = f, data = cr, tau = 0.75)

saveRDS(quantmod_s11_rqss_75, file = "quantmod_s11_rqss_75.RDS")

rm(quantmod_s11_rqss_75)



quantmod_s11_rqss_95 <- rqss(formula = f, data = cr, tau = 0.95)

saveRDS(quantmod_s11_rqss_95, file = "quantmod_s11_rqss_95.RDS")

rm(quantmod_s11_rqss_95)







#View(quantmod_s11_all$coefficients)

#plot(quantmod_s11_all)

#saveRDS(quantmod_s11_all, "quantmod_s11_all.rds")