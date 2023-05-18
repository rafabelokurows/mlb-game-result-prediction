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
#baseballr::standings_on_date_bref(date = "2023-03-30",division = "AL West")
#winloss_opp("2023-04-01","OAK","AL West")
# opp = "OAK"
# division= "AL West"

#divisions = unique(results2$div_opp)
dates = as.Date(c(as.Date("2023-03-29"),sort(unique(results2$Date2))))
dput(dates)
#date_divisions = data.frame(dates) %>% group_by(dates) %>% expand(divisions)
all_standings = data.frame()
for (i in 36:length(dates)){
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
saveRDS(all_standings,file="data\\standings.rds")
#all_standings = readRDS("data\\standings.rds")
#standings2 = readRDS("data\\standings2.rds")
#all_standings = bind_rows(all_standings,standings2)
all_standings %>% filter(Tm=="LAA") %>% arrange(date_standing)
all_standings %>% filter(Tm=="OAK") %>% View()

results2 %>% filter(Tm=="NYY") %>%
  left_join(all_standings %>%
              mutate(date_standing = date_standing+1),by=c("Opp" = "Tm","Date2"="date_standing")) %>%
  group_by(str_detect(Result,"W")) %>% summarize(mean(winloss))

