# Machine Learning Models for mortality prediction using Vital DB
#H2O package
#install.packages("h2o")
#install.packages("DALEX")
library(h2o)
library(DALEX)

# 0. load the data
df <- read.csv("~/temp/VitalDB_20200508_3daysLab_average_death (1).csv")
df$death_inhosp <- as.factor(df$death_inhosp)

# 1. preparation for modeling

n_seed <- 12345
library(h2o) # for H2O Machine Learning
library(mlbench) # for Datasets

target <- "death_inhosp" # death_inhosp
features <- setdiff(colnames(df), target)
features <- setdiff(features, "caseid")
print(features)

h2o.init()

df <- as.h2o(df)

h_split <- h2o.splitFrame(df, ratios = 0.75, seed = n_seed)
h_train <- h_split[[1]] # 75% for modelling
h_test <- h_split[[2]] # 25% for evaluation

model_glm <- h2o.glm(x = features,               # All 13 features
                     y = target,                 # medv (median value of owner-occupied homes in $1000s)
                     training_frame = h_train,   # H2O dataframe with training data
                     #validation_frame = h_test,   # H2O dataframe with test data
                     model_id = "baseline_glm",  # Give the model a name
                     nfolds = 5,                 # Using 5-fold CV
                     seed = n_seed)              # Your lucky seed


model_drf <- h2o.randomForest(x = features,
                              y = target,
                              training_frame = h_train,
                              #validation_frame = h_test,
                              model_id = "baseline_drf",
                              nfolds = 5,
                              seed = n_seed)

model_gbm <- h2o.gbm(x = features,
                     y = target,
                     training_frame = h_train,
                     #validation_frame = h_test,
                     model_id = "baseline_gbm",
                     nfolds = 5,
                     seed = n_seed)

model_dnn <- h2o.deeplearning(x = features,
                              y = target,
                              training_frame = h_train,
                              #validation_frame = h_test,
                              model_id = "baseline_dnn",
                              nfolds = 5,
                              seed = n_seed)

model_xgb <- h2o.xgboost(x = features,
                         y = target,
                         training_frame = h_train,
                         #validation_frame = h_test,
                         model_id = "baseline_xgb",
                         nfolds = 5,
                         seed = n_seed)

d_eval <- data.frame(model = c("H2O GLM: Generalized Linear Model (Baseline)",
                               "H2O DRF: Distributed Random Forest (Baseline)",
                               "H2O GBM: Gradient Boosting Model (Baseline)",
                               "H2O DNN: Deep Neural Network (Baseline)",
                               "XGBoost: eXtreme Gradient Boosting Model (Baseline)"),
                     stringsAsFactors = FALSE)
d_eval$RMSE_cv <- NA
d_eval$RMSE_test <- NA

# Store RMSE values
d_eval[1, ]$RMSE_cv <- model_glm@model$cross_validation_metrics@metrics$RMSE
d_eval[2, ]$RMSE_cv <- model_drf@model$cross_validation_metrics@metrics$RMSE
d_eval[3, ]$RMSE_cv <- model_gbm@model$cross_validation_metrics@metrics$RMSE
d_eval[4, ]$RMSE_cv <- model_dnn@model$cross_validation_metrics@metrics$RMSE
d_eval[5, ]$RMSE_cv <- model_xgb@model$cross_validation_metrics@metrics$RMSE

d_eval[1, ]$RMSE_test <- h2o.rmse(h2o.performance(model_glm, newdata = h_test))
d_eval[2, ]$RMSE_test <- h2o.rmse(h2o.performance(model_drf, newdata = h_test))
d_eval[3, ]$RMSE_test <- h2o.rmse(h2o.performance(model_gbm, newdata = h_test))
d_eval[4, ]$RMSE_test <- h2o.rmse(h2o.performance(model_dnn, newdata = h_test))
d_eval[5, ]$RMSE_test <- h2o.rmse(h2o.performance(model_xgb, newdata = h_test))

datatable(d_eval, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE, round)) %>%
  formatRound(columns = -1, digits = 4)

automl = h2o.automl(x = features,
                    y = target,
                    training_frame = h_train,
                    validation_frame = h_test,
                    nfolds = 5,                     # 5-fold Cross-Validation
                    max_models = 30,                # Max number of models
                    stopping_metric = "AUC",       # Metric to optimize
                    seed = n_seed)

datatable(as.data.frame(automl@leaderboard),
          rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE, round)) %>%
  formatRound(columns = -1, digits = 4)

automl@leader

d_eval_tmp <- data.frame(model = "Best Model from H2O AutoML",
                         RMSE_cv = automl@leader@model$cross_validation_metrics@metrics$RMSE,
                         RMSE_test = h2o.rmse(h2o.performance(automl@leader, newdata = h_test)))
d_eval <- rbind(d_eval, d_eval_tmp)

datatable(d_eval, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE, round)) %>%
  formatRound(columns = -1, digits = 4)

datatable(as.data.frame(h_test[1, ]),
          rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))

custom_predict <- function(model, newdata) {
  newdata_h2o <- as.h2o(newdata)
  res <- as.data.frame(h2o.predict(model, newdata_h2o))
  return(as.numeric(res$predict))
}

explainer_glm <- DALEX::explain(model = model_glm,
                                data = as.data.frame(h_test)[, features],
                                y = as.data.frame(h_test)[, target],
                                predict_function = custom_predict,
                                label = "Generalized Linear Model")

explainer_drf <- DALEX::explain(model = model_drf,
                                data = as.data.frame(h_test)[, features],
                                y = as.data.frame(h_test)[, target],
                                predict_function = custom_predict,
                                label = "Random Forest")

explainer_gbm <- DALEX::explain(model = model_gbm,
                                data = as.data.frame(h_test)[, features],
                                y = as.data.frame(h_test)[, target],
                                predict_function = custom_predict,
                                label = "Gradient Boosting Machine")

explainer_dnn <- DALEX::explain(model = model_dnn,
                                data = as.data.frame(h_test)[, features],
                                y = as.data.frame(h_test)[, target],
                                predict_function = custom_predict,
                                label = "Deep Neural Networks")

explainer_xgb <- DALEX::explain(model = model_xgb,
                                data = as.data.frame(h_test)[, features],
                                y = as.data.frame(h_test)[, target],
                                predict_function = custom_predict,
                                label = "XGBoost")

explainer_automl <- DALEX::explain(model = automl@leader,
                                   data = as.data.frame(h_test)[, features],
                                   y = as.data.frame(h_test)[, target],
                                   predict_function = custom_predict,
                                   label = "H2O AutoML")

# eva_glm <- DALEX::model_performance(explainer_glm)
# eva_dfr <- DALEX::model_performance(explainer_drf)
# eva_gbm <- DALEX::model_performance(explainer_gbm)
# eva_dnn <- DALEX::model_performance(explainer_dnn)
# eva_xgb <- DALEX::model_performance(explainer_xgb)
# eva_automl <- DALEX::model_performance(explainer_automl)

eva_glm <- h2o::h2o.performance(model_glm)
eva_xgb <- h2o::h2o.performance(model_xgb)

plot(eva_glm, eva_xgb, geom = "roc") +
  ggtitle("ROC Curves - All Models") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

vi_glm <- variable_importance(explainer_glm, type="difference")
vi_drf <- variable_importance(explainer_drf, type="difference")
vi_gbm <- variable_importance(explainer_gbm, type="difference")
vi_dnn <- variable_importance(explainer_dnn, type="difference")
vi_xgb <- variable_importance(explainer_xgb, type="difference")
vi_automl <- variable_importance(explainer_automl, type="difference")

plot(vi_glm, vi_drf, vi_gbm, vi_dnn, vi_xgb, vi_automl)

h2o.auc(eva_automl)

library(cvAUC)
