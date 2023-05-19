library(baseballr)
library(tidyverse)
library(lubridate)

past_results = function(){

  teams <- c("ATL", "MIA", "NYM", "PHI", "WSN",
             "CHC", "CIN", "MIL", "PIT", "STL",
             "ARI", "COL", "LAD", "SDP", "SFG",
             "BAL", "BOS", "NYY", "TBR", "TOR",
             "CHW", "CLE", "DET", "KCR", "MIN",
             "HOU", "LAA", "OAK", "SEA", "TEX")

  # Empty dataframe to store results
  results <- data.frame()

  # Loop through each team
  for (team in teams) {
    print(team)
    # Try to execute function and store results in temp variable
    temp <- try(bref_team_results(team, 2023), silent = TRUE)

    # If no error occurred, add team column to results dataframe
    if (!inherits(temp, "try-error")) {
      temp$team <- team
      results <- rbind(results, temp)
    }
  }
  results = results %>% as_tibble() %>%
    mutate(Date2 = as.Date(parse_date_time(gsub("\\(1\\)|\\(2\\)", "",Date), "A, b d",locale="English")),
           doubleheader = if_else( str_detect(Date,"\\(") ,1,0))
  return(results)
#saveRDS(results,"data\results.rds")
}

update_standings = function(standings){
  last_date_standing = max(standings$date_standing)
  if(last_date_standing==Sys.Date()){
    return(null)
  }
  dates_standing = seq.Date(last_date_standing+1,Sys.Date()-1,by = "day")
  print(dates_standing)
  for (i in 1:length(dates_standing)){
    date_check=as.Date(dates_standing[i])
    print(date_check)
    AL = baseballr::standings_on_date_bref(date = as.Date(date_check),division = "AL Overall") %>%
      as_tibble() %>% mutate(winloss=coalesce(`W-L%`,0))
    NL = baseballr::standings_on_date_bref(date = as.Date(date_check),division = "NL Overall") %>%
      as_tibble() %>% mutate(winloss=coalesce(`W-L%`,0))
    aux = bind_rows(AL,NL) %>% mutate(date_standing =as.Date(date_check)) %>%
      mutate(date_standing = date_standing + 1)

    standings = bind_rows(standings,aux)
  }

  return(standings)
}





# View results dataframe
# View(results)
# results %>% count(Tm)


#### Teams ####
# teams = 103:104 %>%
#   map_df(\(x) mlb_teams(season = 2023,league_ids = x))
# teams = teams %>% mutate(abb = c("OAK", "SEA", "TBR","TEX","LAA",
#                          "TOR","MIN","BAL","BOS","CHW",
#                          "CLE","NYY", "DET","HOU","KCR",
#                          "PIT","SDP","SFG", "STL",
#                          "ARI","PHI","ATL","CHC","CIN",
#                          "MIA","COL","LAD","WSN", "NYM",  "MIL"))
# saveRDS(teams,"data\\teams.rds")

#### Pitchers ####

pitchers_all_season = function(date=Sys.Date()-1){
  pitchers_all_season = baseballr::bref_daily_pitcher("2023-03-30",date)
  #saveRDS(pitchers_all_season,"data\\pitchers.rds")
  return(pitchers_all_season)
}

pitchers_last_30 = function(date=Sys.Date()-1){
  if ((as.Date(date)-30) < "2023-03-31"){date_aux = "2023-03-30"}
  else {date_aux = as.Date(date)-30}
  pitcher_last_30 = baseballr::bref_daily_pitcher(date_aux,date)
  #saveRDS(pitchers_all_season,"data\\pitchers.rds")
  return(pitcher_last_30)
}

df=data.frame()
prepare_games = function(date =NULL){
  #date=Sys.Date()
  print(paste0("Preparing and obtaining data for: ",date))
  games = mlb_game_pks(date) %>% as_tibble() %>%
    left_join(teams %>% select(team_full_name,abb,league_name,division_name) %>% rename(home_team_abb = 2,
                                                                                        league_home= 3,
                                                                                        division_home= 4),by=c("teams.home.team.name"="team_full_name")) %>%
    left_join(teams %>% select(team_full_name,abb,league_name,division_name) %>% rename(away_team_abb = 2,
                                                                                        league_away= 3,
                                                                                        division_away= 4),by=c("teams.away.team.name"="team_full_name")) %>%
    mutate(interplay=if_else(league_home != league_away,1,0),
           intradivision=if_else(division_home == division_away,1,0))
  pitchers_allseason = pitchers_all_season(date)#baseballr::bref_daily_pitcher("2023-03-30",as.Date(i)-1)
  pitchers_last30 = pitchers_last_30(date)#baseballr::bref_daily_pitcher("2023-03-30",as.Date(i)-1)
  print(paste0("Games: ",nrow(games)))
  for(j in 1:nrow(games)){
    #combina com lista de times para obter a abreviação
    starters = baseballr::mlb_probables(games[j,"game_pk"])
    home_team_name = games[j,]$teams.home.team.name
    away_team_name = games[j,]$teams.away.team.name
    home_team = games[j,]$home_team_abb
    away_team = games[j,]$away_team_abb

    results_yday_home = standings_upd  %>%
      filter(Tm==home_team & date_standing==date)
    results_yday_away = standings_upd  %>%
      filter(Tm==away_team & date_standing==date)

    home_starter = filter(starters,team  == home_team_name)$fullName
    away_starter = filter(starters,team  == away_team_name)$fullName
    pitchers_all_stats = pitchers_allseason %>%
      filter(Name %in% c(home_starter,away_starter)) %>%
      mutate(h_a = case_when(Name == home_starter~"home_pitcher",
                             Name == away_starter~"away_pitcher")) %>%
      select(h_a,GS,W,L,IP,ER,ERA,WHIP,SO,BF,SO9,SO.W) %>%
      mutate(winpct=coalesce(W/GS,0),mean_ip=IP/GS) %>%
      select(h_a,winpct,mean_ip,ERA,WHIP) %>%
      pivot_wider(values_from=winpct:WHIP,names_from=h_a,names_glue = "{h_a}_{.value}")

    pitchers_last_stats = pitchers_last30 %>%
      filter(Name %in% c(home_starter,away_starter)) %>%
      mutate(h_a = case_when(Name == home_starter~"home_pitcher",
                             Name == away_starter~"away_pitcher")) %>%
      select(h_a,GS,W,L,IP,ER,ERA,WHIP,SO,BF,SO9,SO.W) %>%
      mutate(winpct=coalesce(W/GS,0),mean_ip=IP/GS) %>%
      select(h_a,winpct,mean_ip,ERA,WHIP) %>%
      pivot_wider(values_from=winpct:WHIP,names_from=h_a,names_glue = "last30_{h_a}_{.value}")


    print(paste0(j," - Home: ",home_team," x Away: ",away_team))
    past_games_home = results %>% filter(Tm %in% c(games$home_team_abb[j])
                                         & Date2 < date)
    home_games_10days = results %>% filter(Tm %in% c(games$home_team_abb[j])
                                           & between(Date2,(as.Date(date)-10),as.Date(date)-1)) %>%
      nrow()
    sos_home = results2 %>% filter(Tm==home_team) %>%
      filter(Date2 < i) %>%
      left_join(all_standings,by=c("Opp" = "Tm","Date2"="date_standing"))
    sos_grouped_home = sos_home %>%
      group_by(result=str_detect(Result,"W")) %>%
      summarize(wlpct=mean(winloss))
    sos_total_home = sos_home %>%  summarize(wlpct=mean(winloss))
    home_sos_losses = as.numeric(sos_grouped_home[sos_grouped_home$result==F,]$wlpct)
    if (identical(home_sos_losses, numeric(0))) {
      home_sos_losses <- NA_complex_
    }
    home_sos_wins = sos_grouped_home[sos_grouped_home$result==T,]$wlpct
    home_sos_total = sos_total_home$wlpct

    sos_away = results2 %>% filter(Tm==home_team) %>%
      filter(Date2 < i) %>%
      left_join(all_standings ,by=c("Opp" = "Tm","Date2"="date_standing"))
    sos_grouped_away = sos_away %>%
      group_by(result=str_detect(Result,"W")) %>%
      summarize(wlpct=mean(winloss))
    sos_total_away = sos_away %>%  summarize(wlpct=mean(winloss))
    away_sos_losses = as.numeric(sos_grouped_away[sos_grouped_away$result==F,]$wlpct)
    if (identical(away_sos_losses, numeric(0))) {
      away_sos_losses <- NA_complex_
    }
    away_sos_wins = sos_grouped_away[sos_grouped_away$result==T,]$wlpct
    away_sos_total = sos_total_away$wlpct

    past_games_away = results %>% filter(Tm %in% c(games$away_team_abb[j])
                                         & Date2 < date)
    away_games_10days = results %>% filter(Tm %in% c(games$away_team_abb[j])
                                           & between(Date2,(as.Date(date)-10),as.Date(date)-1)) %>%
      nrow()

    game = games[j,] %>%
      mutate({if("winHome" %in% names(.)) winHome = if_else(teams.away.isWinner,0,1) else NULL} )%>%
      #mutate(winHome = if_else(teams.away.isWinner,0,1)) %>%
      # select(-any_of(c(link:gameDate,isTie:publicFacing,gamedayType:seasonDisplay,scheduledInnings:teams.away.seriesNumber,
      #           teams.away.team.id:teams.home.seriesNumber,teams.home.team.id:content.link))) %>%
      mutate(teams.away.leagueRecord.wins = results_yday_away$W,
             teams.home.leagueRecord.wins = results_yday_home$W,
             teams.away.leagueRecord.losses = results_yday_away$L,
             teams.home.leagueRecord.losses = results_yday_home$L,
             teams.home.leagueRecord.pct = results_yday_home$winloss,
             teams.away.leagueRecord.pct = results_yday_away$winloss,
             home_games_10days,
             away_games_10days,
             home_sos_losses,
             home_sos_wins,
             home_sos_total,
             away_sos_losses,
             away_sos_wins,
             away_sos_total
      ) %>%
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
    game = game %>%
      { if(nrow(pitchers_all_stats) > 0) bind_cols(., pitchers_all_stats) else . }%>%
      { if(nrow(pitchers_last_stats) > 0) bind_cols(., pitchers_last_stats) else . } %>%
      # mutate(winHome = if_else(teams.away.isWinner,0,1)) %>%
      # select(-c(link:gameDate,isTie:publicFacing,gamedayType:seasonDisplay,scheduledInnings:teams.away.seriesNumber,
      # teams.away.team.id:teams.home.seriesNumber,teams.home.team.id:content.link)) %>%
      mutate(teams.home.leagueRecord.pct = as.numeric(teams.home.leagueRecord.pct),
             teams.away.leagueRecord.pct = as.numeric(teams.away.leagueRecord.pct)) %>%
      mutate(home_dif_win_pct = home_exp_pct-teams.home.leagueRecord.pct,
             away_dif_win_pct = away_exp_pct-teams.away.leagueRecord.pct)
    df = bind_rows(df,game)
  }
  return(df)
}
