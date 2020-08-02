library(tidyverse)
library(quantreg)

cr <- read_csv("~/research/CR_Project_Tom/data_processed/quantmod_s11_data.csv")

# winsorize 6300+
cr <- 
  cr %>%
    mutate(startingtrophies = if_else(startingtrophies > 6300, 6300, startingtrophies))



# filter 4000+
# and convert battletime -> day of the month
cr <-
  cr %>%
  filter(startingtrophies >= 4200 & startingtrophies < 6300) %>%
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
rq_s11_winsor_42_63 <- rq(
  formula = f, data = cr, tau = c(0.25, 0.50, 0.75, 0.95)
  #, method = "sfn",
  #control = list(tmpmax= dim(cr)[2]*600)
)

saveRDS(rq_s11_winsor_42_63, file = "rq_s11_winsor_42_63.rds")

rm(rq_s11_winsor_42_63)










# 
# quantmod_s11_rq_50 <- rq(formula = f, data = cr, tau = 0.50)
# 
# saveRDS(quantmod_s11_rq_50, file = "quantmod_s11_rq_50.RDS")
# 
# rm(quantmod_s11_rq_50)
# 
# 
# 
# quantmod_s11_rq_75 <- rq(formula = f, data = cr, tau = 0.75)
# 
# saveRDS(quantmod_s11_rq_75, file = "quantmod_s11_rq_75.RDS")
# 
# rm(quantmod_s11_rq_75)
# 
# 
# 
# quantmod_s11_rq_95 <- rq(formula = f, data = cr, tau = 0.95)
# 
# saveRDS(quantmod_s11_rq_95, file = "quantmod_s11_rq_95.RDS")
# 
# rm(quantmod_s11_rq_95)












#View(quantmod_s11_all$coefficients)

#plot(quantmod_s11_all)

#saveRDS(quantmod_s11_all, "quantmod_s11_all.rds")