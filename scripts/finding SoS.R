sched = mlb_schedule(season = 2023, level_ids = "1")
sched %>% filter(game_type == "R") %>% View()
i="2023-03-31"
games_day = mlb_game_pks(i)
games_day_before = mlb_game_pks(as.Date(i)-1)
b4 = games_day_before %>% select(game_pk,officialDate,teams.home.leagueRecord.pct,teams.away.leagueRecord.pct,
                                 teams.away.team.name,teams.home.team.name) %>% as_tibble() %>%
  left_join(teams %>% select(team_full_name,abb) %>% rename(home = 2),by=c("teams.home.team.name"="team_full_name")) %>%
  left_join(teams %>% select(team_full_name,abb) %>% rename(away = 2),by=c("teams.away.team.name"="team_full_name")) %>%
  select(-c(teams.away.team.name,teams.home.team.name)) %>%
  rename(home_winpct = teams.home.leagueRecord.pct,
         away_winpct = teams.away.leagueRecord.pct) %>%
  pivot_longer(c(home_winpct, away_winpct)) %>%
  mutate(team = if_else(name == "home_winpct",home,away)) %>%
  select(game_pk,officialDate,team,value) %>%
  rename(last_game=game_pk,
         last_date=officialDate,
         sos=value)
opp_win = games_day %>% select(game_pk,officialDate,teams.away.team.name,teams.home.team.name) %>% as_tibble() %>%
  left_join(teams %>% select(team_full_name,abb) %>% rename(home = 2),by=c("teams.home.team.name"="team_full_name")) %>%
  left_join(teams %>% select(team_full_name,abb) %>% rename(away = 2),by=c("teams.away.team.name"="team_full_name")) %>%
  select(-c(teams.away.team.name,teams.home.team.name)) %>%
  pivot_longer(cols=c(home,away)) %>%
  left_join(b4,by=c("value"="team"))

i="2023-04-01"
games_day = mlb_game_pks(i)
games_day_before = mlb_game_pks(as.Date(i)-1)
b4 = games_day_before %>% select(game_pk,officialDate,teams.home.leagueRecord.pct,teams.away.leagueRecord.pct,
                                 teams.away.team.name,teams.home.team.name) %>% as_tibble() %>%
  left_join(teams %>% select(team_full_name,abb) %>% rename(home = 2),by=c("teams.home.team.name"="team_full_name")) %>%
  left_join(teams %>% select(team_full_name,abb) %>% rename(away = 2),by=c("teams.away.team.name"="team_full_name")) %>%
  select(-c(teams.away.team.name,teams.home.team.name)) %>%
  rename(home_winpct = teams.home.leagueRecord.pct,
         away_winpct = teams.away.leagueRecord.pct) %>%
  pivot_longer(c(home_winpct, away_winpct)) %>%
  mutate(team = if_else(name == "home_winpct",home,away)) %>%
  select(game_pk,officialDate,team,value) %>%
  rename(last_game=game_pk,
         last_date=officialDate,
         sos=value)


games_day %>% select(game_pk,officialDate,teams.away.team.name,teams.home.team.name) %>% as_tibble() %>%
  left_join(teams %>% select(team_full_name,abb) %>% rename(home = 2),by=c("teams.home.team.name"="team_full_name")) %>%
  left_join(teams %>% select(team_full_name,abb) %>% rename(away = 2),by=c("teams.away.team.name"="team_full_name")) %>%
  select(-c(teams.away.team.name,teams.home.team.name)) %>%
  pivot_longer(cols=c(home,away)) %>%
  left_join(b4,by=c("value"="team"))

team_check = "LAA"
date_check = "2023-04-01"


results2 = results %>%
  left_join(teams %>% select(abb,div_opp=division_name),by=c("Opp"="abb")) %>%
  mutate(div_opp = gsub("American League","AL",div_opp),
         div_opp = gsub("National League","NL",div_opp))

# winloss_opp <- function(date,opp,division){
#   #print(paste0(date," - ",opp," - ",division))
#   winloss = baseballr::standings_on_date_bref(date = (as.Date(date)-1),division = division)  %>%
#     as.data.frame() %>% filter(Tm==opp) %>% mutate(winloss=coalesce(`W-L%`,0)) %>%
#     pull(winloss)
#   return (winloss)
# }
baseballr::standings_on_date_bref(date = "2023-03-30",division = "AL West")
#winloss_opp("2023-04-01","OAK","AL West")
# opp = "OAK"
# division= "AL West"

#divisions = unique(results2$div_opp)
dates = as.Date(c(as.Date("2023-03-29"),sort(unique(results2$Date2))))
#date_divisions = data.frame(dates) %>% group_by(dates) %>% expand(divisions)
all_standings = data.frame()
for (i in 16:length(dates)){
  #datevar = enquo(date)
  date_check=as.Date(dates[i])
  print(date_check)
  #print(as.Date(date_check))
  AL = baseballr::standings_on_date_bref(date = as.Date(date_check),division = "AL Overall") %>%
    as_tibble() %>% mutate(winloss=coalesce(`W-L%`,0))
  NL = baseballr::standings_on_date_bref(date = as.Date(date_check),division = "NL Overall") %>%
    as_tibble() %>% mutate(winloss=coalesce(`W-L%`,0))
  aux = bind_rows(AL,NL) %>% mutate(date_standing =as.Date(date_check))
  all_standings = bind_rows(all_standings,aux)
}

all_standings %>% count(date_standing)
saveRDS(all_standings,file="standings.rds")







dia30 = results %>% arrange(Date2) %>%  head(30)

games_day %>% clipr::write_clip()

games_aux = games %>% as_tibble() %>%
  left_join(teams %>% select(team_full_name,abb,league_name,division_name) %>% rename(home_team_abb = 2,
                                                                                      league_home= 3,
                                                                                      division_home= 4),by=c("teams.home.team.name"="team_full_name")) %>%
  left_join(teams %>% select(team_full_name,abb,league_name,division_name) %>% rename(away_team_abb = 2,
                                                                                      league_away= 3,
                                                                                      division_away= 4),by=c("teams.away.team.name"="team_full_name")) %>%
  mutate(interplay=if_else(league_home != league_away,1,0),
         intradivision=if_else(division_home == division_away,1,0))

games %>% View()
718626

df=data.frame()
for (i in dates){
  print(paste0("Date: ",i))
  games = mlb_game_pks(i)
  print(paste0("Games: ",nrow(games)))
  games_aux = games %>% as_tibble() %>%
    left_join(teams %>% select(team_full_name,abb,league_name,division_name) %>% rename(home_team_abb = 2,
                                                                                        league_home= 3,
                                                                                        division_home= 4),by=c("teams.home.team.name"="team_full_name")) %>%
    left_join(teams %>% select(team_full_name,abb,league_name,division_name) %>% rename(away_team_abb = 2,
                                                                                        league_away= 3,
                                                                                        division_away= 4),by=c("teams.away.team.name"="team_full_name")) %>%
    mutate(interplay=if_else(league_home != league_away,1,0),
           intradivision=if_else(division_home == division_away,1,0))
  print("Obtaining pitchers data")
  pitchers_allseason = pitchers_all_season()#baseballr::bref_daily_pitcher("2023-03-30",as.Date(i)-1)
  pitchers_last30 = pitchers_last_30()#baseballr::bref_daily_pitcher("2023-03-30",as.Date(i)-1)
  #pitchers_all_season = baseballr::bref_daily_pitcher("2023-04-01",Sys.Date())

  for(j in 1:nrow(games)){
    #combina com lista de times para obter a abreviação
    starters = baseballr::mlb_probables(games[j,"game_pk"])
    home_team_name = games_aux[j,]$teams.home.team.name
    away_team_name = games_aux[j,]$teams.away.team.name
    home_starter = filter(starters,team  == home_team_name)$fullName
    away_starter = filter(starters,team  == away_team_name)$fullName
    pitchers_all_stats = pitchers_allseason %>%
      filter(Name %in% c(home_starter,away_starter)) %>%
      mutate(h_a = if_else(Name == home_starter,"home_pitcher","away_pitcher")) %>%
      select(h_a,GS,W,L,IP,ER,ERA,WHIP,SO,BF,SO9,SO.W) %>%
      mutate(winpct=coalesce(W/GS,0),mean_ip=IP/GS) %>%
      select(h_a,winpct,mean_ip,ERA,WHIP) %>%
      pivot_wider(values_from=winpct:WHIP,names_from=h_a,names_glue = "{h_a}_{.value}")

    pitchers_last_stats = pitchers_last30 %>%
      filter(Name %in% c(home_starter,away_starter)) %>%
      mutate(h_a = if_else(Name == home_starter,"home_pitcher","away_pitcher")) %>%
      select(h_a,GS,W,L,IP,ER,ERA,WHIP,SO,BF,SO9,SO.W) %>%
      mutate(winpct=coalesce(W/GS,0),mean_ip=IP/GS) %>%
      select(h_a,winpct,mean_ip,ERA,WHIP) %>%
      pivot_wider(values_from=winpct:WHIP,names_from=h_a,names_glue = "last30_{h_a}_{.value}")

    home_team = games_aux[j,]$home_team_abb
    away_team = games_aux[j,]$away_team_abb
    print(paste0(j," - Home: ",home_team," x Away: ",away_team))
    past_games_home = results %>% filter(Tm %in% c(games_aux$home_team_abb[j])
                                         & Date2 < i)
    home_games_10days = results %>% filter(Tm %in% c(games_aux$home_team_abb[j])
                                           & between(Date2,(as.Date(i)-10),as.Date(i)-1)) %>%
      nrow()

    past_games_away = results %>% filter(Tm %in% c(games_aux$away_team_abb[j])
                                         & Date2 < i)
    away_games_10days = results %>% filter(Tm %in% c(games_aux$away_team_abb[j])
                                           & between(Date2,(as.Date(i)-10),as.Date(i)-1)) %>%
      nrow()

    game = games_aux[j,] %>%
      mutate(home_games_10days = home_games_10days,
             away_games_10days = away_games_10days) %>%
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
    game = game %>% bind_cols(pitchers_all_stats) %>%bind_cols(pitchers_last_stats) %>%
      mutate(winHome = if_else(teams.away.isWinner,0,1)) %>%
      select(-c(link:gameDate,isTie:publicFacing,gamedayType:seasonDisplay,scheduledInnings:teams.away.seriesNumber,
                teams.away.team.id:teams.home.seriesNumber,teams.home.team.id:content.link)) %>%
      mutate(teams.home.leagueRecord.pct = as.numeric(teams.home.leagueRecord.pct),
             teams.away.leagueRecord.pct = as.numeric(teams.away.leagueRecord.pct)) %>%
      mutate(home_dif_win_pct = home_exp_pct-teams.home.leagueRecord.pct,
             away_dif_win_pct = away_exp_pct-teams.away.leagueRecord.pct)
    df = bind_rows(df,game)
  }

}
