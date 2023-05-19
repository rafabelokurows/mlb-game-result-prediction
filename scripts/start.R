#install.packages("baseballr")
library(baseballr)
library(tidyverse)
library(lubridate)

#### Loading static data ####
teams = readRDS("data\\work\\teams.rds")
eval(parse("scripts\\obtaining-data.R", encoding="UTF-8"))
#TODO:
#criar script que executa todo dia e coloca dados atualizados na pasta
#


#### Starting data ####
results = past_results()
standings = readRDS("data\\work\\standings.rds")
standings_upd = update_standings(standings)
standings_upd= standings_upd%>% filter(date_standing <= Sys.Date())
saveRDS(results,"data\\work\\results.rds")
saveRDS(standings_upd,"data\\work\\standings.rds")
#results = readRDS("data\\results.rds")
all_standings = standings_upd
#all_standings = all_standings %>% mutate(date_standing = date_standing+1)


#### Regenerating the entire dataframe ####
dates = unique(results$Date2)[unique(results$Date2)>"2023-04-10"] %>% as.character() %>% sort()

df=data.frame()
for (i in dates){
  print(paste0("Date: ",i))
  games = prepare_games(i)
  df = bind_rows(df,games)
}

df= df%>%
  select(-c(description,rescheduledFrom,rescheduledFromDate))
#Saving updated data
write.csv(df,paste0("data\\",format(Sys.Date(), "%Y%m%d"),"_games.csv"))

#investigar dados no excel
#correlação entre features
#bar plots




#### Obtendo jogos de hoje para gerar previsão ####
todays_games = mlb_game_pks(Sys.Date())
games_aux = todays_games %>% as_tibble() %>%
  left_join(teams %>% select(team_full_name,abb,league_name,division_name) %>% rename(home_team_abb = 2,
                                                                                      league_home= 3,
                                                                                      division_home= 4),by=c("teams.home.team.name"="team_full_name")) %>%
  left_join(teams %>% select(team_full_name,abb,league_name,division_name) %>% rename(away_team_abb = 2,
                                                                                      league_away= 3,
                                                                                      division_away= 4),by=c("teams.away.team.name"="team_full_name")) %>%
  mutate(interplay=if_else(league_home != league_away,1,0),
         intradivision=if_else(division_home == division_away,1,0))
print("Obtaining pitchers data")
pitchers_all_season = baseballr::bref_daily_pitcher("2023-03-30",Sys.Date()-1)
dfToday = data.frame()
for(j in 1:nrow(todays_games)){
  #combina com lista de times para obter a abreviação
  starters = baseballr::mlb_probables(todays_games[j,"game_pk"])
  home_team_name = games_aux[j,]$teams.home.team.name
  away_team_name = games_aux[j,]$teams.away.team.name
  home_starter = filter(starters,team  == home_team_name)$fullName
  away_starter = filter(starters,team  == away_team_name)$fullName
  pitchers_stats = pitchers_all_season %>%
    filter(Name %in% c(home_starter,away_starter)) %>%
    mutate(h_a = if_else(Name == home_starter,"home_pitcher","away_pitcher")) %>%
    select(h_a,GS,W,L,IP,ER,ERA,WHIP,SO,BF,SO9,SO.W) %>%
    mutate(winpct=coalesce(W/GS,0),mean_ip=IP/GS) %>%
    select(h_a,winpct,mean_ip,ERA,WHIP) %>%
    pivot_wider(values_from=winpct:WHIP,names_from=h_a,names_glue = "{h_a}_{.value}")


  home_team = games_aux[j,]$home_team_abb
  away_team = games_aux[j,]$away_team_abb
  print(paste0(j," - Home: ",home_team," x Away: ",away_team))
  past_games_home = results %>% filter(Tm %in% c(games_aux$home_team_abb[j])
                                       & Date2 < i)

  past_games_away = results %>% filter(Tm %in% c(games_aux$away_team_abb[j])
                                       & Date2 < i)
  game = games_aux[j,] %>%
    bind_cols(
      past_games_home %>% summarize(R=sum(R,na.rm = T),RA=sum(RA,na.rm = T),
                                    home_exp_pct = R^2/((R^2)+(RA^2))) %>%
        select(home_exp_pct),
      past_games_home %>% as_tibble()%>% slice_max(n=5,order_by = Date2) %>%
        summarize(wins_last5 = sum(str_detect(Result,"W"))),

      past_games_home %>% as_tibble() %>% slice_max(n=10,order_by = Date2) %>%
        summarize(home_wins_last10 = sum(str_detect(Result,"W")),
                  home_losses_last10 = sum(str_detect(Result,"L")),
                  home_wins_last10_athome = sum(str_detect(Result,"W")& H_A == "H"),
                  home_losses_last10_athome = sum(str_detect(Result,"L")& H_A == "H"),
                  home_runs_scored_last10 = sum(R,na.rm = T),
                  home_runs_against_last10 = sum(RA,na.rm = T),
                  home_extra_innings_games = sum(Inn!=""),
                  home_extra_innings_games_last = sum(Inn!=""&row_number()==10)) ,


      past_games_home %>% filter(Opp==away_team ) %>%
        summarize(htoh_wins_home = sum(str_detect(Result,"W")),
                  htoh_wins_away = sum(str_detect(Result,"L"))))



  game = game %>% bind_cols(
    past_games_away %>% summarize(R=sum(R,na.rm = T),RA=sum(RA,na.rm = T),
                                  away_exp_pct = R^2/((R^2)+(RA^2))) %>%
      select(away_exp_pct),
    past_games_away %>% as_tibble()%>% slice_max(n=5,order_by = Date2) %>%
      summarize(away_wins_last5 = sum(str_detect(Result,"W"))),

    past_games_away %>% as_tibble() %>% tail(10) %>%
      summarize(away_wins_last10 = sum(str_detect(Result,"W")),
                away_losses_last10 = sum(str_detect(Result,"L")),
                away_wins_last10_away = sum(str_detect(Result,"W")& H_A == "A"),
                away_losses_last10_away = sum(str_detect(Result,"L")& H_A == "A"),
                away_runs_scored_last10 = sum(R,na.rm = T),
                away_runs_against_last10 = sum(RA,na.rm = T),
                away_extra_innings_games = sum(Inn!=""),
                away_extra_innings_games_last = sum(Inn!=""&row_number()==10)
      ))
  game = game %>% bind_cols(pitchers_stats) %>%
    #mutate(winHome = NA) %>%
    select(-c(link:gameDate,gameNumber:publicFacing,gamedayType:seasonDisplay,scheduledInnings:teams.away.seriesNumber,
              teams.away.team.id:teams.home.seriesNumber,teams.home.team.id:content.link)) %>%
    mutate(teams.home.leagueRecord.pct = as.numeric(teams.home.leagueRecord.pct),
           teams.away.leagueRecord.pct = as.numeric(teams.away.leagueRecord.pct)) %>%
    mutate(home_dif_win_pct = home_exp_pct-teams.home.leagueRecord.pct,
           away_dif_win_pct = away_exp_pct-teams.away.leagueRecord.pct)
  dfToday = bind_rows(dfToday,game)
}


pred <- h2o.predict(m, as.h2o(dfToday))
test2 = dfToday%>% as.data.frame() %>%
  bind_cols(pred %>% as.data.frame())
write.csv(test2,paste0(format(Sys.Date(),"%Y%m%d"),"predictions.csv"))

#### Avaliando previsões de ontem ####
pred = test2 %>% select(game_pk,home_team_abb,away_team_abb,predict,p0,p1)
yesterdays_games = mlb_game_pks(Sys.Date()-1)
actual = yesterdays_games%>% as_tibble() %>%
  left_join(teams %>% select(team_full_name,abb,league_name,division_name) %>% rename(home_team_abb = 2,
                                                                                      league_home= 3,
                                                                                      division_home= 4),by=c("teams.home.team.name"="team_full_name")) %>%
  left_join(teams %>% select(team_full_name,abb,league_name,division_name) %>% rename(away_team_abb = 2,
                                                                                      league_away= 3,
                                                                                      division_away= 4),by=c("teams.away.team.name"="team_full_name"))  %>%
  mutate(winHome = if_else(teams.away.isWinner,0,1))%>%
  select(game_pk,officialDate,away_team_abb,teams.away.score,
         home_team_abb,teams.home.score,winHome)

pred_result = pred %>% left_join(actual) %>% relocate(c(p0,p1,predict),.before=winHome)

table(pred_result$winHome,pred_result$predict)
sum(diag(table(pred_result$winHome,pred_result$predict)))/nrow(pred_result)




correlations <- cor(df[, !colnames(df) %in% "winHome"], df$winHome)
teste = cor(df2 %>% purrr::keep(is.numeric),method="pearson",use="complete.obs" )
options(scipen = 999)

res <- cor(iris[,-5])
teste[lower.tri(teste)] <- NA #assuming there are no actual NAs already
# which seems likely with complete.obs
#use lower.tri(res, diag = TRUE) to remove the diagonal too
na.omit(reshape2::melt(teste))

x <- reshape2::melt(teste)
#subset first the variable 3 when it is equal to 1
x <- subset(x, value != 1)
#remove duplicate entries in that same variable
x[duplicated(x$value),] %>%
  filter(Var2 =="winHome") %>% arrange(desc(value))

#Fetch today's games
todays_games = mlb_game_pks(Sys.Date())
todays_games = todays_games %>% select(game_pk,teams.home.team.name,teams.away.team.name,gameDate,officialDate,
                        dayNight,gamesInSeries,seriesGameNumber,
                        teams.away.leagueRecord.wins,teams.away.leagueRecord.losses,
                        teams.away.leagueRecord.pct,
                        teams.home.leagueRecord.wins,teams.home.leagueRecord.losses,
                        teams.away.leagueRecord.pct)


bref_standings_on_date(Sys.Date())

teams = 103:104 %>%
  map_df(\(x) mlb_teams(season = 2023,league_ids = x))
teams %>% count(league_name,division_name)
standings = unique(teams$division_name)  %>%
  gsub("American League","AL",.) %>%
  gsub("National League","NL",.) %>%
  map_df(\(x) bref_standings_on_date(Sys.Date(),division = x))
standings


df=data.frame()

for(i in 1:nrow(todays_games)){
  print(paste0("Game :",i))
  game = todays_games[i,] %>%
    left_join(teams %>% select(team_full_name,team_abbreviation,league_name,division_name) %>% rename(home_team_abb = 2,
                                                                                                      league_home= 3,
                                                                                                      division_home= 4),by=c("teams.home.team.name"="team_full_name")) %>%
    left_join(teams %>% select(team_full_name,team_abbreviation,league_name,division_name) %>% rename(away_team_abb = 2,
                                                                                                      league_away= 3,
                                                                                                      division_away= 4),by=c("teams.away.team.name"="team_full_name")) %>%
    mutate(interplay=if_else(league_home != league_away,1,0),
           intradivision=if_else(division_home == division_away,1,0))

  home_team_games = baseballr::bref_team_results(game$home_team_abb, 2023)
  away_team_games = baseballr::bref_team_results(game$away_team_abb, 2023)

  #### ATRIBUTOS HOME TEAM ####
  game = game %>% bind_cols(
    home_team_games %>% as_tibble() %>% tail(5)%>%
      summarize(home_wins_last5 = sum(str_detect(Result,"W"))),

    home_team_games %>% as_tibble() %>% tail(10) %>%
      summarize(home_wins_last10 = sum(str_detect(Result,"W")),
                home_losses_last10 = sum(str_detect(Result,"L")),
                home_wins_last10_athome = sum(str_detect(Result,"W")& H_A == "H"),
                home_losses_last10_athome = sum(str_detect(Result,"L")& H_A == "H"),
                home_runs_scored_last10 = sum(R,na.rm = T),
                home_runs_against_last10 = sum(RA,na.rm = T),
                home_extra_innings_games = sum(Inn!=""),
                home_extra_innings_games_last = sum(Inn!=""&row_number()==10)
                ) ,
    home_team_games %>% filter(Opp==jogo1$away_team_abb ) %>%
      summarize(htoh_wins_home = sum(str_detect(Result,"W")),
                htoh_wins_away = sum(str_detect(Result,"L"))))

  game = game %>% bind_cols(
    away_team_games %>% as_tibble() %>% tail(5)%>%
      summarize(away_wins_last5 = sum(str_detect(Result,"W"))),

    away_team_games %>% as_tibble() %>% tail(10) %>%
      summarize(away_wins_last10 = sum(str_detect(Result,"W")),
                away_losses_last10 = sum(str_detect(Result,"L")),
                away_wins_last10_away = sum(str_detect(Result,"W")& H_A == "A"),
                away_losses_last10_away = sum(str_detect(Result,"L")& H_A == "A"),
                away_runs_scored_last10 = sum(R,na.rm = T),
                away_runs_against_last10 = sum(RA,na.rm = T),
                away_extra_innings_games = sum(Inn!=""),
                away_extra_innings_games_last = sum(Inn!=""&row_number()==10)
      ))
  df = bind_rows(df,game)


}

df %>% View()