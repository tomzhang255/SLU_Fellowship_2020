library(tidyverse)

rarity <- read_csv("./research/CR_Project_Tom/cr_rarity_level_adjustments.csv")

cr_cards <-
  jsonlite::fromJSON("./research/CR_Project_Tom/clash_royale_card_info.json")$cards %>%
  select(name, rarity) %>%
  # season 1 added in Fisherman
  bind_rows(., tibble(name = "Fisherman", rarity = "Legendary"))

# reading in 1,000,000 lines
cr <- read_csv("./research/CR_Project_Tom/data_by_season/clash_royale_matches_season_1_2019-07-01_to_2019-08-05_left.csv", n_max = 1000000)



# fix card/level part
x <-
cr %>%  
  mutate(leftdeck = str_remove_all(leftdeck, "[\\[\"]")) %>%
  separate_rows(leftdeck, sep = "\\], ") %>%
  separate(leftdeck, into = c("Cards","Level"), sep = ", ") %>%
  mutate(Level = parse_number(Level)) %>%
  left_join(., cr_cards, by = c("Cards" = "name")) %>% # add rarity info
  left_join(rarity, by = "rarity") %>% # add level adjustment
  mutate(Level = Level  + level_adjustment) %>% # adjust levels
  select(-rarity, -level_adjustment, -lefttrophychange) %>% # reduce size of dataframe
  model.matrix(~0 + Cards + Level, data = .) # make indictor variables


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
  mutate(tmpid = 1 + 0:(nrow(x)-1) %/% 8) %>%
  group_by(tmpid) %>%
  summarize_if(is.numeric, sum) %>%
  select(-tmpid)

rm(x) # free up memory from x

# put back together
cr <- cr %>%
  select(battletime, leftstartingtrophies) %>%
  bind_cols(sign(deck)) %>%
  bind_cols(deck)

rm(deck) # free memory from deck

names(cr) = str_remove_all(names(cr), "_Level") # fix anmes
names(cr) = str_replace_all(names(cr), "1","_Level") # one more fix



# save df
write_csv(cr, "cr_s1_left.csv")


# process battle time -> day of the month
cr <-
  cr %>%
    mutate(battletime = str_remove(battletime, "\\s.+")) %>%
    mutate(battletime = str_remove(battletime, "[:digit:]{4}-[:digit:]{2}-")) %>%
    mutate(battletime = as.numeric(battletime))


# build formula

f <- 
  as.formula(
    paste(
      paste("leftstartingtrophies ~ "), 
      paste(names(cr)[-2], collapse =  " + "), " + ",
      paste(interactions[-1:-93], collapse = " + ")
    )
)


# fit model...


# cr_model <- lm(formula = f, data = cr)

# saveRDS(cr_model, "cr_model.rds")

