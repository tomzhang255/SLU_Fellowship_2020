# Routine for updating the app

## (1) Run mod_4000_s15_lm_2.R on HPCC

** if there are any new cards, just add another row in the RScript **

It should save 5 objects after you run it:
  1. mod_4000_s15_lm_2.rds - the original model (very large ~ 6 GB)
  2. mod_4000_s15_lm_2_strip.rds - same model but with reduced size for predictions
  3. mod_4000_s15_lm_2_data.csv - the data used for regression (don't need it but just in case)
  4. residualAdj.csv (residual adjustments specific for that model; need it in the app)
  5. df_mod3.csv (a small template of the model dataframe; used for creating an empty df for predictions)
  
  
## (2) update cr_allcard_profiles_s15.R
Run this file to get cr_allcard_profiles_s15.csv (for win rate plot)

  
## (3) modify app.R

** if there are any new cards, just add another row in the RScript (in the section called "# for upgrade cost calculation" - it's somewhere at the top) **


## (4) the files needed to publish the app
 - app.R
 - clashroyale_card_info.json (for card rarity info)
 - cr_allcard_profiles_s15.csv (for win rate plot)
 - cr_cards_s15.csv (for card icon URLs, got it from CR API)
 - cr_upgrade_cost.csv (no need to modify it unless there's an update from CR about costs)
 - df_mod3.csv (got it by running the model-fitting RScript)
 - mod_4000_s15_lm_2_strip.rds (got it from running the model-fitting RScript)
 - residualAdj.csv (from model RScript)
 
 
 
 
 
 
 
 