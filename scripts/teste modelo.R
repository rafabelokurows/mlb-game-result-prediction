library(tidymodels)
library(xgboost)
set.seed(123)  # For reproducibility
train = train_data %>% select(any_of(predictors),winHome)
recipe_obj <- recipe(winHome ~ ., data = train) %>%
  #step_select(winHome,all_of(predictors)) %>%
  step_dummy(all_nominal_predictors())
as.matrix(prep(recipe_obj) %>% bake(new_data = train_data) )

xgb_spec <-
  boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_wflow <-
  workflow() %>%
  add_recipe(recipe_obj) %>%
  add_model(xgb_spec)


xgb_res <-
  xgb_wflow %>%
  fit_resamples(
    resamples = cv_split,
    metrics = metric_set(
      recall, precision, f_meas,
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
  )

xgb_res %>% collect_metrics(summarize = TRUE)


options(na.action='na.pass')
train = train_data %>% select(any_of(predictors),winHome)
test = test_data %>% select(any_of(predictors),winHome)
trainm <- Matrix::sparse.model.matrix(winHome ~ ., data = train)
train_label <- train$winHome
train_matrix <- xgb.DMatrix(data = trainm, label=train$winHome)



as.matrix(train_data$winHome)
model_spec <- boost_tree(
  trees = tune(),  # Number of trees in the ensemble
  tree_depth = tune(),  # Maximum depth of each tree
  mtry = tune(),  # Number of randomly selected predictors to consider at each split
  loss_reduction = 0.01,  # Minimum loss reduction required for a split
  learn_rate = 0.01,  # Learning rate,
  mode="classification"
) %>%
  set_engine("xgboost")
workflow_obj <- workflow() %>%
  add_recipe(recipe_obj) %>%
  add_model(model_spec)
param_grid <- expand.grid(
  trees = c(200,500, 1000),  # Number of trees
  tree_depth = c(4, 6),  # Maximum depth of each tree
  mtry = c(2, 3,5,10)  # Number of randomly selected predictors to consider at each split
)
cv_split = vfold_cv(train,5)
tune_results <- tune_grid(
  workflow_obj,
  resamples = cv_split,
  grid = param_grid,
  metrics = metric_set(accuracy, roc_auc),
  control = control_grid(save_pred = TRUE)
)

autoplot(tune_results)



best_model = rank_results(tune_results,
                          rank_metric = 'huber_loss',
                          select_best = T)

best_model <- select_best(tune_results, metric = "accuracy")


gbm_params1 <- list(learn_rate = c(0.01, 0.1),
                    max_depth = c(3, 5, 9),
                    sample_rate = c(0.5,0.8, 1.0),
                    col_sample_rate = c(0.2, 0.5, 1.0),
                    ntrees=c(100,200,500,1000))

# Train and validate a cartesian grid of GBMs
gbm_grid1 <- h2o.grid("gbm", x = predictors,
                      y = response,
                      training_frame = as.h2o(train_data),
                      #ntrees = 500,
                      seed = 1,
                      hyper_params = gbm_params1)

# Get the grid results, sorted by validation AUC
gbm_gridperf1 <- h2o.getGrid(grid_id = "gbm_grid1",
                             sort_by = "auc",
                             decreasing = TRUE)
print(gbm_gridperf1)

best_gbm1 <- h2o.getModel(gbm_grid1@model_ids[[1]])

# Now let's evaluate the model performance on a test set
# so we get an honest estimate of top model performance
best_gbm_perf1 <- h2o.performance(model = best_gbm1,
                                  newdata = as.h2o(test_data))
h2o.auc(best_gbm_perf1)
h2o.confusionMatrix(best_gbm_perf1)
# 0.7781779

# Look at the hyperparameters for the best model
print(best_gbm1@model[["model_summary"]])


#https://tmv.netlify.app/site/slides/rmed03-tune.html#57
#https://rpubs.com/Joaquin_AR/406480
