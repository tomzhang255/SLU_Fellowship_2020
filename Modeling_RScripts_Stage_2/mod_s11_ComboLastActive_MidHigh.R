library(tidyverse)
library(automl)

rarity <- read_csv("./research/CR_Project_Tom/cr_rarity_level_adjustments.csv")

cr_cards <-
  jsonlite::fromJSON("./research/CR_Project_Tom/clash_royale_card_info.json")$cards %>%
  select(name, rarity) %>%
  # season 1 added in Fisherman - nothing more up to season 3
  bind_rows(., tibble(name = "Fisherman", rarity = "Legendary")) %>%
  # season 4: elixir golem
  bind_rows(., tibble(name = "Elixir Golem", rarity = "Rare")) %>%
  # season 6: battle healer
  bind_rows(., tibble(name = "Battle Healer", rarity = "Rare")) %>%
  # season 7: firecracker
  bind_rows(., tibble(name = "Firecracker", rarity = "Common")) %>%
  # season 9: royal delivery
  bind_rows(., tibble(name = "Royal Delivery", rarity = "Common")) %>%
  # season 10: replaces heal with heal spirit
  mutate(name = str_replace(name, "^Heal$", "Heal Spirit"))

# reading in all lines
cr_full <- read_csv("./research/CR_Project_Tom/data_by_season/clash_royale_matches_season_11_2020-05-04_to_2020-06-01.csv")



# combine left and right tags
df_left <- cr_full %>% select(1:7) %>% mutate()
names(df_left) <- c("id", "battletime", "arenaid",
                    "playertag", "startingtrophies", "trophychange", "deck")

df_right <- cr_full %>% select(1:3, 8:11)
names(df_right) <- c("id", "battletime", "arenaid",
                    "playertag", "startingtrophies", "trophychange", "deck")

df_combined <- bind_rows(df_left, df_right)



# filter only the active players based on playertag
cr_sub <-
  df_combined %>%
    group_by(playertag) %>%
    filter(n() >= 25) %>%
    arrange(desc(battletime)) %>%
    slice(1) %>% # only take the last match for a player
    ungroup() #187,234




# reduce vars
cr_reduced <-
  cr_sub %>%
  select(playertag, battletime, startingtrophies, trophychange, deck)


set.seed(3007)
cr <- cr_reduced %>%
  #filter(battletime > '2020-05-29') %>% # only look at the last few days in the season
  #sample_n(tbl = ., size = 20000) %>% # random sample of 20k
  filter(str_count(deck, pattern = ",") == 15) # ensures 8 card decks



# fix card/level part
xcards <-
cr %>%  
  separate_rows(deck, sep = "\\],") %>%
  separate(deck, into = c("Cards","Level"), sep = ", ") %>%
  mutate(Level = parse_number(Level),
         Cards = str_remove_all(Cards, "\\[+"),
         Cards = str_remove_all(Cards, '\\"'),
         Cards = str_trim(Cards, side = c("both"))
         ) %>%
  left_join(., cr_cards, by = c("Cards" = "name")) %>% # add rarity info
  left_join(rarity, by = "rarity") %>% # add level adjustment
  mutate(Level = Level  + level_adjustment) %>% # adjust levels
  select(-rarity, -level_adjustment, -trophychange) # reduce size of dataframe

x = model.matrix(~0 + Cards + Level, model.frame(~ ., xcards, na.action=na.pass)) # make indictor variables

#x = xcards %>%  model.matrix(~0 + Cards + Level, data = .) 

x = sweep(x[,-ncol(x)], MARGIN = 1, STATS = x[,ncol(x)], FUN = "*")

colnames(x) = str_remove_all(colnames(x), "Cards")
colnames(x) = str_replace_all(colnames(x), " ", "_")
colnames(x) = str_replace_all(colnames(x), "-", "_")

# create names for all card and card interaction terms
interactions = labels(terms(~.^2, data = x[1:10,]))

colnames(x) = paste(colnames(x),"_Level",sep = "")

deck <- 
  x %>%
  as_tibble() %>%
  mutate(tmpid = xcards$playertag) %>%
  group_by(tmpid) %>%
  summarize_if(is.numeric, sum)

rm(x) # free up memory
rm(xcards)

# put back together
cr <- cr %>%
  select(battletime, startingtrophies) %>%
  bind_cols(sign(deck[,-1])) %>%
  bind_cols(deck[,-1])

rm(deck) # free memory from deck

names(cr) = str_remove_all(names(cr), "_Level") # fix anmes
names(cr) = str_replace_all(names(cr), "1","_Level") # one more fix



# filter 4000 <= trophies < 6600
# and convert battletime -> day of the month
cr <-
  cr %>%
  filter(startingtrophies >= 4000 & startingtrophies < 6600) %>%
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

f <- 
  as.formula(
    paste(
      paste("startingtrophies ~ "), # response
      paste(names(cr)[-2], collapse =  " + "), " + ", #  battletime and  first level vars
      #paste(names(cr)[-(1:2)], collapse =  " + "), " + ", # no battletime and yes first level vars
      paste(quad_level_terms, collapse =  " + "), " + ", # quadratic level vars
      paste(interactions[-1:-98], collapse = " + "), " + ",
      paste("battletime:", c(names(cr)[101:198], quad_level_terms), sep = "", collapse = " + ") # battle interact with levels and levels^2
    )
  )


# fit model
mod_s11_ComboLastActive_MidHigh <- lm(formula = f, data = cr)

saveRDS(mod_s11_ComboLastActive_MidHigh, "mod_s11_ComboLastActive_MidHigh.rds")









# 
# # build X matrix
# # dayOfTheMonth (named as battletime), cards, levels, levels^2, card interactions,
# # day interact with levels & levels^2
# 
# # level squared
# lvl <-
#   cr %>%
#   select(101:198)
# 
# lvl_Sq <-
#   lvl^2
# 
# names(lvl_Sq) <- paste(names(lvl_Sq), "_Sq", sep = "")
# 
# # card interactions
# cards <-
#   cr %>%
#   select(3:100)
# 
# cardsIntMat <- model.matrix(~(.)^2, cards)
# cardsIntMat <- cardsIntMat[,100:4852]
# 
# # battletime interact with levels and levels^2
# 
# lvls <- bind_cols(cr[1], lvl, lvl_Sq)
# 
# dayIntMat <- model.matrix(~ battletime * ., lvls)
# dayIntMat <- dayIntMat[,199:394]
# 
# # combine to create matrix X
# xMat <- cbind(data.matrix(cr[,-2]), data.matrix(lvl_Sq), cardsIntMat, dayIntMat)
# 
# # extract Y the response
# yMat <- data.matrix(cr[,2])
# 
# 
# 
# # fit model with automl
# amlmodel <- automl_train(Xref = xMat, Yref = yMat,
#                          autopar = list(psopartpopsize = 15,
#                                         numiterations = 5,
#                                         auto_layers_max = 1,
#                                         nbcores = 4))
# 
# saveRDS(amlmodel, "mod_4000_s11_active_full_automl.rds")

