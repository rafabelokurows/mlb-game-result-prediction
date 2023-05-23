#Pontos a avaliar:
#Tamanho do split
#Incluir Ensemble ou não
#Testar com xgboost/GBM
#Testar com mais tempo de treinamento
#Normalizar predictors
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
df = read.csv(paste0("data\\games\\",filename))
predictors = c("teams.away.leagueRecord.pct",
               "teams.home.leagueRecord.pct",
               "wins_last5","away_wins_last5"  ,
               "home_wins_last10","home_runs_scored_last10",
               "htoh_wins_home","htoh_wins_away",
               "away_wins_last10","away_runs_scored_last10",
               "home_extra_innings_games_last",
               "away_extra_innings_games_last",
               "home_pitcher_ERA","away_pitcher_ERA",
               "home_pitcher_mean_ip","away_pitcher_mean_ip",
               "away_pitcher_winpct","home_pitcher_winpct",
               "home_dif_win_pct",
               "away_dif_win_pct"
               # "last30_away_pitcher_winpct",
               # "last30_home_pitcher_winpct", "last30_away_pitcher_mean_ip",
               # "last30_home_pitcher_mean_ip","last30_away_pitcher_ERA",
               # "last30_home_pitcher_ERA","last30_away_pitcher_WHIP",
               # "last30_home_pitcher_WHIP",
               #  "home_games_10days",
               #  "away_games_10days",
               #  #"home_sos_losses",
               #  "home_sos_wins",
               #  "home_sos_total",
               #  #"away_sos_losses",
               #  "away_sos_wins",
               # "away_sos_total"
               )
response = "winHome"


#Split aleatorio
#
# train_idx <- sample(nrow(df), 0.85 * nrow(df), replace = FALSE)
# train_data <- df[train_idx,]
# test_data <- df[-train_idx,]
library(caTools)
# df=df %>% mutate(winHome = case_when(teams.away.isWinner&is.na(winHome)~0,
#                                      !teams.away.isWinner&is.na(winHome)~1,
#                                      TRUE~winHome))
set.seed(123)
df = df %>% filter(!is.na(winHome)) %>% mutate(winHome = as.factor(winHome))
sample <- sample.split(df$officialDate, SplitRatio = 0.9)
train_data  <- subset(df, sample == TRUE)
test_data   <- subset(df, sample == FALSE)


#Split por data
# set.seed(123) # for reproducibility
# train_data = df %>% filter(officialDate < Sys.Date()-5) %>% mutate(winHome = as.factor(winHome))%>%
#   filter(!is.na(winHome))
# test_data = df %>%
#   mutate(winHome = as.factor(winHome)) %>% anti_join(train_data) %>% mutate(winHome = as.factor(winHome)) %>%
#   filter(!is.na(winHome))

#Representatividade train/test split
nrow(train_data)/(nrow(train_data)+nrow(test_data))
train_data %>% count(officialDate) %>% bind_cols(df %>% count(officialDate) %>% select(total=n)) %>%
  mutate(n/total)
# train_data = train_data %>%
#   mutate_if(is.numeric, ~(scale(.) %>% as.vector)) %>% as.data.frame()

library(h2o)
h2o.init(nthreads = 6)

automl_model <- h2o.automl(
  x = predictors,
  y = response,
  training_frame = as.h2o(train_data),
  distribution = "bernoulli",
  #leaderboard_frame = as.h2o(test_data),
  max_runtime_secs = 240, # maximum time for AutoML to run (in seconds)
  stopping_metric = "mean_per_class_error", # metric to use for early stopping
  sort_metric = "mean_per_class_error", # metric to sort models by in leaderboard
  nfolds = 5, # number of cross-validation folds
  include_algos = c("GBM", "DRF", "XGBoost", "DeepLearning","GLM","StackedEnsemble"), # list of algorithms to include
  seed = 123 # for reproducibility
)

#Avaliação modelo treinado
#h2o.get_leaderboard(automl_model, "mean_per_class_error")
#h2o.get_leaderboard(automl_model, "AUC")
m <- h2o.get_best_model(automl_model,criterion ="mean_per_class_error" )
h2o.confusionMatrix(m)


metrics_train = m@model$cross_validation_metrics_summary %>% as.data.frame() %>% select(mean)
varimp = h2o.varimp(m) %>% as.data.frame()


#exa <- h2o.explain(automl_model, as.h2o(test_data))

# test_data = test_data %>%
#   mutate_if(is.numeric, ~(scale(.) %>% as.vector)) %>% as.data.frame()

#Gerando previsões dataframe teste
pred <- h2o.predict(m, as.h2o(test_data))
test2 = test_data%>% as.data.frame() %>%
  bind_cols(pred %>% as.data.frame()) %>% #count(winHome,predict) %>%
  filter(!is.na(winHome))
table(test2$winHome,test2$predict)
sum(diag(table(test2$winHome,test2$predict)))/nrow(test2)
library(caret)
cm =confusionMatrix(test2$predict, reference = test2$winHome)
metrics_test = cm$overall %>% as.data.frame() %>% rename(metrics=1) %>% bind_rows(
cm$byClass %>% as.data.frame() %>% rename(metrics=1))


saveRDS(metrics_train,paste0("data/results/",format(Sys.Date(), "%Y%m%d"),"_metrics_train.rds"))
saveRDS(metrics_test,paste0("data/results/",format(Sys.Date(), "%Y%m%d"),"_metrics_test.rds"))
saveRDS(test2,paste0("data/results/",format(Sys.Date(), "%Y%m%d"),"_data_test.rds"))
# Stop the H2O cluster
saveRDS(m,paste0("data/models/",format(Sys.Date(), "%Y%m%d"),"_model.rds"))
if(nrow(varimp)>0){saveRDS(varimp,paste0("data/models/",format(Sys.Date(), "%Y%m%d"),"_varimp.rds"))}



#### Avaliando previsões de ontem ####

findlastpredictions = function(){
  prev_dir = getwd()
  setwd(".\\data\\predictions")
  files = file.info(list.files()) %>% tibble::rownames_to_column("filename") %>%
    mutate(date =as.Date(substring(filename,1,8), format = "%Y%m%d")) %>%
    slice_max(n=1,order_by = date,with_ties = F)
  setwd(prev_dir)
  return(files$filename)
}
filename = findlastpredictions()
preds= read.csv(paste0("data\\predictions\\",filename))

pred = preds %>% select(game_pk,home_team_abb,away_team_abb,predict,p0,p1)
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
#adicionar medidas principais aqui
table(factor(pred_result$winHome),factor(pred_result$predict))
sum(diag(table(pred_result$winHome,pred_result$predict)))/nrow(pred_result)

#combinar resultado e salvar métricas
cm_yesterday =caret::confusionMatrix(factor(pred_result$winHome),factor(pred_result$predict))
metrics_eval = cm_yesterday$overall %>% as.data.frame() %>% rename(metrics=1) %>% bind_rows(
  cm_yesterday$byClass %>% as.data.frame() %>% rename(metrics=1))
saveRDS(pred_result,paste0("data/eval/",format(Sys.Date(), "%Y%m%d"),"_data_eval.rds"))
saveRDS(metrics_eval,paste0("data/eval/",format(Sys.Date(), "%Y%m%d"),"_metrics_eval.rds"))


#### Obtendo jogos de hoje para gerar previsão ####
dfToday = readRDS("data\\work\\today.rds")
pred <- h2o.predict(m, as.h2o(dfToday))
pred_frame = dfToday%>% as.data.frame() %>%
  bind_cols(pred %>% as.data.frame())
write.csv(pred_frame,paste0("data/predictions/",format(Sys.Date(),"%Y%m%d"),"_predictions.csv"))

h2o.shutdown()
