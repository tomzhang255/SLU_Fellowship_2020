
## mod2v2_4000_biglm
## trophies ~ day + card indicators + card numeric level + (card numeric level)^2 + card interactions

# import data
library(tidyverse)
library(broom)
library(biglm)
cr_s3_df_ind_left <- read_csv("data/cr_s3_df_ind_left.csv")

# cleaning up data
df_mod2_4000 <-
  cr_s3_df_ind_left %>%
  filter(leftstartingtrophies >= 4000) %>%
  mutate(battletime = as.character(as.POSIXct(battletime, origin="2019-09-02", tz="GMT"))) %>%
  mutate(battletime = str_remove(battletime, "\\s.*")) %>%
  separate(battletime, into = c("yearMonth", "dayOfTheMonth"), sep = -2) %>%
  mutate(dayOfTheMonth = as.numeric(dayOfTheMonth)) %>%
  select(leftstartingtrophies, dayOfTheMonth, 27:214)

# mod_cardInt: trophies ~ cards + all 2-way card interactions
df_cardInt_4000 <-
  df_mod2_4000 %>%
  select(1, 3:96) %>%
  slice(1:100) # since we only care about interaction names

fInt_4000 <- as.formula(
  paste(
    "leftstartingtrophies ~ (",
    paste(
      paste("`", names(df_cardInt_4000)[-1], "`", sep = ""),
      collapse = " + "
    ),
    ")^2"
  )
)

mod_cardInt_4000_biglm <- biglm::biglm(formula = fInt_4000, data = df_cardInt_4000)

# mod2v2
f2_4000 <- as.formula(
  paste(
    "leftstartingtrophies ~",
    paste(
      c(
        paste('`', names(df_mod2_4000)[2:190], '`', sep = ''), # day + cards + levels
        paste('I(`', names(df_mod2_4000)[97:190], '`^2)', sep = ''), # quadratic levels
        mod_cardInt_4000_biglm$names[-1:-95] # all 2-way card interactions
      ),
      collapse = " + "
    )
  )
)

t2_start <- Sys.time()
mod2v2_4000_biglm <- biglm(formula = f2_4000, data = df_mod2_4000)
t2_end <- Sys.time()
t2_end - t2_start

# save model
saveRDS(mod2v2_4000_biglm, "mod/mod2v2_4000_biglm.rds")






## mod2v2_0: excluding day of the month variable

# cleaning up data
df_mod2_0 <-
  cr_s3_df_ind_left %>%
  filter(leftstartingtrophies < 4000) %>%
  mutate(battletime = as.character(as.POSIXct(battletime, origin="2019-09-02", tz="GMT"))) %>%
  mutate(battletime = str_remove(battletime, "\\s.*")) %>%
  separate(battletime, into = c("yearMonth", "dayOfTheMonth"), sep = -2) %>%
  mutate(dayOfTheMonth = as.numeric(dayOfTheMonth)) %>%
  select(leftstartingtrophies, 27:214)

# mod_cardInt: trophies ~ cards + all 2-way card interactions
df_cardInt_0 <-
  df_mod2_0 %>%
  select(1, 2:95) %>%
  slice(1:100) # since we only care about interaction names

fInt_0 <- as.formula(
  paste(
    "leftstartingtrophies ~ (",
    paste(
      paste("`", names(df_cardInt_0)[-1], "`", sep = ""),
      collapse = " + "
    ),
    ")^2"
  )
)

mod_cardInt_0_biglm <- biglm::biglm(formula = fInt_0, data = df_cardInt_0)

# mod2v2
f2_0 <- as.formula(
  paste(
    "leftstartingtrophies ~",
    paste(
      c(
        paste('`', names(df_mod2_0)[2:189], '`', sep = ''), # day + cards + levels
        paste('I(`', names(df_mod2_0)[96:189], '`^2)', sep = ''), # quadratic levels
        mod_cardInt_0_biglm$names[-1:-95] # all 2-way card interactions
      ),
      collapse = " + "
    )
  )
)

t2_start <- Sys.time()
mod2v2_0_biglm <- biglm(formula = f2_0, data = df_mod2_0)
t2_end <- Sys.time()
t2_end - t2_start

# save model
saveRDS(mod2v2_0_biglm, "mod/mod2v2_0_biglm.rds")

