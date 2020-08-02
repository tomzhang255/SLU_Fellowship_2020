library(tidyverse)
# library(automl)
#library(quantmod)
library(quantreg)

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



# filter active left player
cr_sub <-
  cr_full %>%
    group_by(lefttag) %>%
    filter(n() >= 25) %>% # define active
    filter(lefttrophychange > 0) %>% # keep winned matches
    arrange(desc(battletime)) %>%
    slice(1) %>% # only take the last match for a player
    ungroup() %>%
    mutate(nettrophychange = lefttrophychange + righttrophychange)



write_csv(cr_sub, "netTrophyChange_data.csv")





# keep all wins
cr_sub2 <-
  cr_full %>%
  group_by(lefttag) %>%
  filter(n() >= 25) %>% # define active
  filter(lefttrophychange > 0) %>% # keep winned matches
  #arrange(desc(battletime)) %>%
  #slice(1) %>% # only take the last match for a player
  ungroup() %>%
  mutate(nettrophychange = lefttrophychange + righttrophychange)



write_csv(cr_sub2, "netTrophyChange_data_2.csv")








# keep all matches, including losses
cr_sub3 <-
  cr_full %>%
  group_by(lefttag) %>%
  filter(n() >= 25) %>% # define active
  #filter(lefttrophychange > 0) %>% # keep winned matches
  #arrange(desc(battletime)) %>%
  #slice(1) %>% # only take the last match for a player
  ungroup() %>%
  mutate(nettrophychange = lefttrophychange + righttrophychange)



write_csv(cr_sub3, "netTrophyChange_data_3.csv")








