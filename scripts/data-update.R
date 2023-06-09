library(dplyr)
library(lubridate)
library(baseballr)

#### Loading static data ####
standings = readRDS("data\\work\\standings.rds")
teams = readRDS("data\\work\\teams.rds")
eval(parse("scripts\\obtaining-data.R", encoding="UTF-8"))

#### Past results ####
results = past_results()
saveRDS(results,"data\\work\\results.rds")
results2 = results %>%
  left_join(teams %>% select(abb,div_opp=division_name),by=c("Opp"="abb")) %>%
  mutate(div_opp = gsub("American League","AL",div_opp),
         div_opp = gsub("National League","NL",div_opp))
saveRDS(results2,"data\\work\\results2.rds")

#### Standings ####

standings_upd = update_standings(standings)

saveRDS(standings_upd,"data\\work\\standings.rds")

#### Yesterday's games for evaluation ####
#games_before = mlb_game_pks(Sys.Date()-2)

setdiff(colnames(mlb_game_pks(Sys.Date()-1)),colnames(mlb_game_pks(Sys.Date()) ))
yesterdays_games = prepare_games(Sys.Date()-1)
yesterdays_games %>% select(winHome,teams.away.isWinner)
saveRDS(yesterdays_games,"data\\work\\yesterday.rds")

#### Today's games for prediction ####
todays_games = prepare_games(Sys.Date())
saveRDS(todays_games,"data\\work\\today.rds")

#### COmbines yesterday's frame with today's data to train new model ####
findlastfile = function(){
  prev_dir = getwd()
  setwd(".\\data\\games")
  files = file.info(list.files()) %>% tibble::rownames_to_column("filename") %>%
    mutate(date =as.Date(substring(filename,1,8), format = "%Y%m%d")) %>%
    slice_max(n=1,order_by = date,with_ties = F)
  setwd(prev_dir)
  return(files$filename)
}
filename = findlastfile()
df_games = read.csv(paste0("data\\games\\",filename))
max(df_games$officialDate)

#Caso tiver algum jogo com winHome nulo
# df_games %>% group_by(officialDate) %>% count(winHome) %>% tail()
# yesterdays_games %>% group_by(officialDate) %>% count(winHome) %>% tail()
# df_games = df_games %>% mutate(winHome = case_when(teams.away.isWinner&is.na(winHome)~0,
#                                      !teams.away.isWinner&is.na(winHome)~1,
#                                      TRUE~winHome))%>% group_by(officialDate)

df_games = df_games %>% #filter(officialDate < Sys.Date()-1) %>%
  #select(-c(season,seasonDisplay,status.codedGameState,status.statusCode)) %>%
  bind_rows(yesterdays_games%>% #filter(officialDate < Sys.Date()-1) %>%
              select(-c(season,seasonDisplay,status.codedGameState,status.statusCode,
                        status.abstractGameCode))) %>%
  select(-starts_with("X"))



write.csv(df_games,paste0("data\\games\\",format(Sys.Date(), "%Y%m%d"),"_games.csv"))
#executar o model

# pitchers_allseason = pitchers_all_season()
# pitchers_last30 = pitchers_last_30()
# for (i in dates){
#   print(paste0("Date: ",i))
#   games = mlb_game_pks(i)
#   games_before = mlb_game_pks(as.Date(i)-1)
#   print(paste0("Games: ",nrow(games)))
#   games_aux = games %>% as_tibble() %>%
#     left_join(teams %>% select(team_full_name,abb,league_name,division_name) %>% rename(home_team_abb = 2,
#                                                                                         league_home= 3,
#                                                                                         division_home= 4),by=c("teams.home.team.name"="team_full_name")) %>%
#     left_join(teams %>% select(team_full_name,abb,league_name,division_name) %>% rename(away_team_abb = 2,
#                                                                                         league_away= 3,
#                                                                                         division_away= 4),by=c("teams.away.team.name"="team_full_name")) %>%
#     mutate(interplay=if_else(league_home != league_away,1,0),
#            intradivision=if_else(division_home == division_away,1,0))
#   print("Obtaining pitchers data")
#   pitchers_allseason = pitchers_all_season(i)#baseballr::bref_daily_pitcher("2023-03-30",as.Date(i)-1)
#   pitchers_last30 = pitchers_last_30(i)#baseballr::bref_daily_pitcher("2023-03-30",as.Date(i)-1)
#
#   for(j in 1:nrow(games)){
#     #combina com lista de times para obter a abreviação
#     starters = baseballr::mlb_probables(games[j,"game_pk"])
#     home_team_name = games_aux[j,]$teams.home.team.name
#     away_team_name = games_aux[j,]$teams.away.team.name
#     home_team = games_aux[j,]$home_team_abb
#     away_team = games_aux[j,]$away_team_abb
#
#     results_yday_home = all_standings %>%
#       mutate(date_standing = date_standing+1) %>%
#       filter(Tm==home_team & date_standing==i)
#     results_yday_away = all_standings %>%
#       mutate(date_standing = date_standing+1) %>%
#       filter(Tm==away_team & date_standing==i)
#
#     home_starter = filter(starters,team  == home_team_name)$fullName
#     away_starter = filter(starters,team  == away_team_name)$fullName
#     pitchers_all_stats = pitchers_allseason %>%
#       filter(Name %in% c(home_starter,away_starter)) %>%
#       mutate(h_a = if_else(Name == home_starter,"home_pitcher","away_pitcher")) %>%
#       select(h_a,GS,W,L,IP,ER,ERA,WHIP,SO,BF,SO9,SO.W) %>%
#       mutate(winpct=coalesce(W/GS,0),mean_ip=IP/GS) %>%
#       select(h_a,winpct,mean_ip,ERA,WHIP) %>%
#       pivot_wider(values_from=winpct:WHIP,names_from=h_a,names_glue = "{h_a}_{.value}")
#
#     pitchers_last_stats = pitchers_last30 %>%
#       filter(Name %in% c(home_starter,away_starter)) %>%
#       mutate(h_a = if_else(Name == home_starter,"home_pitcher","away_pitcher")) %>%
#       select(h_a,GS,W,L,IP,ER,ERA,WHIP,SO,BF,SO9,SO.W) %>%
#       mutate(winpct=coalesce(W/GS,0),mean_ip=IP/GS) %>%
#       select(h_a,winpct,mean_ip,ERA,WHIP) %>%
#       pivot_wider(values_from=winpct:WHIP,names_from=h_a,names_glue = "last30_{h_a}_{.value}")
#
#
#     print(paste0(j," - Home: ",home_team," x Away: ",away_team))
#     past_games_home = results %>% filter(Tm %in% c(games_aux$home_team_abb[j])
#                                          & Date2 < i)
#     home_games_10days = results %>% filter(Tm %in% c(games_aux$home_team_abb[j])
#                                            & between(Date2,(as.Date(i)-10),as.Date(i)-1)) %>%
#       nrow()
#     sos_home = results2 %>% filter(Tm==home_team) %>%
#       filter(Date2 < i) %>%
#       left_join(all_standings %>%
#                   mutate(date_standing = date_standing+1),by=c("Opp" = "Tm","Date2"="date_standing"))
#     sos_grouped_home = sos_home %>%
#       group_by(result=str_detect(Result,"W")) %>%
#       summarize(wlpct=mean(winloss))
#     sos_total_home = sos_home %>%  summarize(wlpct=mean(winloss))
#     home_sos_losses = as.numeric(sos_grouped_home[sos_grouped_home$result==F,]$wlpct)
#     if (identical(home_sos_losses, numeric(0))) {
#       home_sos_losses <- NA_complex_
#     }
#     home_sos_wins = sos_grouped_home[sos_grouped_home$result==T,]$wlpct
#     home_sos_total = sos_total_home$wlpct
#
#     sos_away = results2 %>% filter(Tm==home_team) %>%
#       filter(Date2 < i) %>%
#       left_join(all_standings %>%
#                   mutate(date_standing = date_standing+1),by=c("Opp" = "Tm","Date2"="date_standing"))
#     sos_grouped_away = sos_away %>%
#       group_by(result=str_detect(Result,"W")) %>%
#       summarize(wlpct=mean(winloss))
#     sos_total_away = sos_away %>%  summarize(wlpct=mean(winloss))
#     away_sos_losses = as.numeric(sos_grouped_away[sos_grouped_away$result==F,]$wlpct)
#     if (identical(away_sos_losses, numeric(0))) {
#       away_sos_losses <- NA_complex_
#     }
#     away_sos_wins = sos_grouped_away[sos_grouped_away$result==T,]$wlpct
#     away_sos_total = sos_total_away$wlpct
#
#     past_games_away = results %>% filter(Tm %in% c(games_aux$away_team_abb[j])
#                                          & Date2 < i)
#     away_games_10days = results %>% filter(Tm %in% c(games_aux$away_team_abb[j])
#                                            & between(Date2,(as.Date(i)-10),as.Date(i)-1)) %>%
#       nrow()
#
#     game = games_aux[j,] %>%
#       mutate(winHome = if_else(teams.away.isWinner,0,1)) %>%
#       select(-c(link:gameDate,isTie:publicFacing,gamedayType:seasonDisplay,scheduledInnings:teams.away.seriesNumber,
#                 teams.away.team.id:teams.home.seriesNumber,teams.home.team.id:content.link)) %>%
#       mutate(teams.away.leagueRecord.wins = results_yday_away$W,
#              teams.home.leagueRecord.wins = results_yday_home$W,
#              teams.away.leagueRecord.losses = results_yday_away$L,
#              teams.home.leagueRecord.losses = results_yday_home$L,
#              teams.home.leagueRecord.pct = results_yday_home$winloss,
#              teams.away.leagueRecord.pct = results_yday_away$winloss,
#              home_games_10days,
#              away_games_10days,
#              home_sos_losses,
#              home_sos_wins,
#              home_sos_total,
#              away_sos_losses,
#              away_sos_wins,
#              away_sos_total
#       ) %>%
#       bind_cols(
#         past_games_home %>% summarize(R=sum(R,na.rm = T),RA=sum(RA,na.rm = T),
#                                       home_exp_pct = R^2/((R^2)+(RA^2))) %>%
#           select(home_exp_pct),
#         past_games_home %>% as_tibble()%>% slice_max(n=5,order_by = Date2) %>%
#           summarize(wins_last5 = sum(str_detect(Result,"W"))),
#
#         past_games_home %>% as_tibble() %>% slice_max(n=10,order_by = Date2) %>%
#           summarize(home_wins_last10 = sum(str_detect(Result,"W")),
#                     home_losses_last10 = sum(str_detect(Result,"L")),
#                     home_wins_last10_athome = sum(str_detect(Result,"W")& H_A == "H"),
#                     home_losses_last10_athome = sum(str_detect(Result,"L")& H_A == "H"),
#                     home_runs_scored_last10 = sum(R,na.rm = T),
#                     home_runs_against_last10 = sum(RA,na.rm = T),
#                     home_extra_innings_games = sum(Inn!=""),
#                     home_extra_innings_games_last = sum(Inn!=""&row_number()==10)) ,
#
#
#         past_games_home %>% filter(Opp==away_team ) %>%
#           summarize(htoh_wins_home = sum(str_detect(Result,"W")),
#                     htoh_wins_away = sum(str_detect(Result,"L"))))
#
#     game = game %>% bind_cols(
#       past_games_away %>% summarize(R=sum(R,na.rm = T),RA=sum(RA,na.rm = T),
#                                     away_exp_pct = R^2/((R^2)+(RA^2))) %>%
#         select(away_exp_pct),
#       past_games_away %>% as_tibble()%>% slice_max(n=5,order_by = Date2) %>%
#         summarize(away_wins_last5 = sum(str_detect(Result,"W"))),
#
#       past_games_away %>% as_tibble() %>% tail(10) %>%
#         summarize(away_wins_last10 = sum(str_detect(Result,"W")),
#                   away_losses_last10 = sum(str_detect(Result,"L")),
#                   away_wins_last10_away = sum(str_detect(Result,"W")& H_A == "A"),
#                   away_losses_last10_away = sum(str_detect(Result,"L")& H_A == "A"),
#                   away_runs_scored_last10 = sum(R,na.rm = T),
#                   away_runs_against_last10 = sum(RA,na.rm = T),
#                   away_extra_innings_games = sum(Inn!=""),
#                   away_extra_innings_games_last = sum(Inn!=""&row_number()==10)
#         ))
#     game = game %>% bind_cols(pitchers_all_stats) %>%bind_cols(pitchers_last_stats) %>%
#       # mutate(winHome = if_else(teams.away.isWinner,0,1)) %>%
#       # select(-c(link:gameDate,isTie:publicFacing,gamedayType:seasonDisplay,scheduledInnings:teams.away.seriesNumber,
#       # teams.away.team.id:teams.home.seriesNumber,teams.home.team.id:content.link)) %>%
#       mutate(teams.home.leagueRecord.pct = as.numeric(teams.home.leagueRecord.pct),
#              teams.away.leagueRecord.pct = as.numeric(teams.away.leagueRecord.pct)) %>%
#       mutate(home_dif_win_pct = home_exp_pct-teams.home.leagueRecord.pct,
#              away_dif_win_pct = away_exp_pct-teams.away.leagueRecord.pct)
#     df = bind_rows(df,game)
#   }
#
# }
