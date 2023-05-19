library(dplyr)
library(lubridate)
library(baseballr)

#### Loading static data ####
teams = readRDS(".\\data\\work\\teams.rds")
eval(parse(".\\scripts\\obtaining-data.R", encoding="UTF-8"))

#### Past results ####
results = past_results()
saveRDS(results,".\\data\\work\\results.rds")
results2 = results %>%
  left_join(teams %>% select(abb,div_opp=division_name),by=c("Opp"="abb")) %>%
  mutate(div_opp = gsub("American League","AL",div_opp),
         div_opp = gsub("National League","NL",div_opp))
saveRDS(results2,".\\data\\work\\results2.rds")

#### Standings ####
standings = readRDS(".\\data\\work\\standings.rds")
standings_upd = update_standings(standings)
saveRDS(standings_upd,".\\data\\work\\standings.rds")

#### Yesterday's games for evaluation ####
#games_before = mlb_game_pks(Sys.Date()-2)
yesterdays_games = prepare_games(Sys.Date()-1)
saveRDS(yesterdays_games,".\\data\\work\\yesterday.rds")

#### Today's games for prediction ####
todays_games = prepare_games(Sys.Date())
saveRDS(todays_games,".\\data\\work\\today.rds")
