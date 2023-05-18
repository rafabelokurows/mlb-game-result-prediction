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
    break
    return(null)
  }
  dates_standing = seq.Date(last_date_standing+1,Sys.Date(),by = "day")
  print(dates_standing)
  for (i in 1:length(dates_standing)){
    date_check=as.Date(dates_standing[i])
    print(date_check)
    AL = baseballr::standings_on_date_bref(date = as.Date(date_check),division = "AL Overall") %>%
      as_tibble() %>% mutate(winloss=coalesce(`W-L%`,0))
    NL = baseballr::standings_on_date_bref(date = as.Date(date_check),division = "NL Overall") %>%
      as_tibble() %>% mutate(winloss=coalesce(`W-L%`,0))
    aux = bind_rows(AL,NL) %>% mutate(date_standing =as.Date(date_check))
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

pitchers_all_season = function(){
  pitchers_all_season = baseballr::bref_daily_pitcher("2023-03-30",Sys.Date()-1)
  #saveRDS(pitchers_all_season,"data\\pitchers.rds")
  return(pitchers_all_season)
}

pitchers_last_30 = function(){
  pitcher_last_30 = baseballr::bref_daily_pitcher(Sys.Date()-30,Sys.Date()-1)
  #saveRDS(pitchers_all_season,"data\\pitchers.rds")
  return(pitcher_last_30)
}
