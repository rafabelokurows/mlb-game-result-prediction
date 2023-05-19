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
