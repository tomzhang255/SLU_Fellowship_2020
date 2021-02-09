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
  select(startingtrophies, trophychange, deck)



set.seed(3007)
cr <- cr_reduced %>%
  #filter(battletime > '2020-05-29') %>% # only look at the last few days in the season
  #sample_n(tbl = ., size = 20000) %>% # random sample of 20k
  filter(str_count(deck, pattern = ",") == 15) # ensures 8 card decks



cr <-
  cr %>%
  drop_na(trophychange) %>%
  mutate(
    trophies = round(startingtrophies / 100) * 100,
    win = if_else(trophychange > 0, 1, 0),
    draw = if_else(trophychange == 0, 1, 0),
    lose = if_else(trophychange < 0, 1, 0)
  ) %>%
  select(-startingtrophies, -trophychange) %>%
  
  # separate cards
  separate_rows(deck, sep = "\\],") %>%
  separate(deck, into = c("card","level"), sep = ", ") %>%
  mutate(level = parse_number(level),
         card = str_remove_all(card, "\\[+"),
         card = str_remove_all(card, '\\"'),
         card = str_trim(card, side = c("both"))
  ) %>%
  left_join(., cr_cards, by = c("card" = "name")) %>% # add rarity info
  left_join(rarity, by = "rarity") %>% # add level adjustment
  mutate(level = level + level_adjustment) %>% # adjust levels
  select(-rarity, -level_adjustment) %>%
  
  # group_by
  group_by(card, trophies, level) %>%
  summarise(
    win = sum(win),
    draw = sum(draw),
    lose = sum(lose)
  ) %>%
  select(card, level, trophies, win, draw, lose)



# save df
write_csv(cr, "cr_allcard_profiles_s19.csv")













