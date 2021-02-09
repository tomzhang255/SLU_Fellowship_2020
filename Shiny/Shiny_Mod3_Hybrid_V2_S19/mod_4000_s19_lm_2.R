library(tidyverse)

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
  mutate(name = str_replace(name, "^Heal$", "Heal Spirit")) %>%
  # season 12: skeleton dragons
  bind_rows(., tibble(name = "Skeleton Dragons", rarity = "Common")) %>%
  # season 16: Electro Giant
  bind_rows(., tibble(name = "Electro Giant", rarity = "Epic")) %>%
  # season 16: Electro Spirit
  bind_rows(., tibble(name = "Electro Spirit", rarity = "Common")) %>%
  # season 18: Mother Witch
  bind_rows(., tibble(name = "Mother Witch", rarity = "Legendary"))
  
# reading in all lines
cr_full <- read_csv("./research/clash_royale/data_by_season/clash_royale_matches_season_19_2021-01-05_to_2021-01-31.csv")

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
    ungroup() #210,092




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

names(cr) = str_remove_all(names(cr), "_Level.+") # fix anmes
names(cr)[-1:-(nrow(cr_cards)+2)] = paste(names(cr)[-1:-(nrow(cr_cards)+2)], "_Level", sep = "") # one more fix



# filter 4000+ and convert battletime -> day of the month
cr <-
  cr %>%
  filter(startingtrophies >= 4000) %>%
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
      paste(interactions[-1:-nrow(cr_cards)], collapse = " + "), " + ",
      paste("battletime:", c(names(cr)[-1:-(nrow(cr_cards)+2)], quad_level_terms), sep = "", collapse = " + ") # battle interact with levels and levels^2
    )
  )


# fit model
mod_4000_s19_lm_2 <- lm(formula = f, data = cr)

saveRDS(mod_4000_s19_lm_2, "mod_4000_s19_lm_2.rds")



# strip model - reduce size
library(strip)
lmc <- strip(mod_4000_s19_lm_2, keep = "predict")
saveRDS(lmc, "mod_4000_s19_lm_2_strip.rds")



# save df as well
write_csv(cr, "mod_4000_s19_lm_2_data.csv")



# residual adjustment

# Calculate the quantiles on the residuals for each “trophy group”
dfQuant <- tibble(
  predRounded = round(mod_4000_s19_lm_2$fitted.values / 100) * 100,
  resid = mod_4000_s19_lm_2$residuals
) %>%
  group_by(predRounded) %>%
  summarise(
    resid05 = quantile(resid, probs = 0.05),
    resid25 = quantile(resid, probs = 0.25),
    resid50 = quantile(resid, probs = 0.50),
    resid75 = quantile(resid, probs = 0.75),
    resid95 = quantile(resid, probs = 0.95)
  )

write_csv(dfQuant, "residualAdj.csv")



# df template - saved as df_mod3.csv - to make newxEmpty for predictions
df_temp <-
  cr %>%
    slice(1:10) %>%
    mutate(battletime = 30)
write_csv(df_temp, "df_mod3.csv")




