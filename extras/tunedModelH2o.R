# @file tunedModelH2o.R
#
# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of FEARLESS
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Machine Learning Models for mortality prediction using Vital DB
#H2O package
#install.packages("h2o")
library(h2o)
library(dplyr)

# 0. load the data
df <- read.csv("~/temp/VitalDB_20200508_3daysLab_average_death (1).csv")
df$death_inhosp <- as.factor(df$death_inhosp)

# 1. preparation for modeling

n_seed <- 12345

target <- "death_inhosp" # death_inhosp
features <- setdiff(colnames(df), target)
features <- setdiff(features, "caseid")
print(target)
print(features)

h2o.init()

df <- as.h2o(df)

h_split <- h2o.splitFrame(df, ratios = 0.75, seed = n_seed)
h_train <- h_split[[1]] # 75% for modelling
h_test <- h_split[[2]] # 25% for evaluation



model_glm <- h2o.glm(x = features,               # All 19 features
                     y = target,                 # In-hospital mortality
                     training_frame = h_train,   # H2O dataframe with training data
                     validation_frame = h_test, # H2O dataframe with test data
                     model_id = "tuned_glm",  # Give the model a name
                     nfolds = 5,                 # Using 5-fold CV
                     seed = n_seed)

#########################Tuning for RF ########################################
hyper_grid.h2o <- list(ntrees = seq(100, 300, 500),
                       mtries = seq(3, 5, by = 1),
                       max_depth = seq(10, 30, by = 10), #
                       min_rows = seq(1, 3, by = 1), #
                       nbins = seq(20, 30, by = 10), #
                       sample_rate = c(0.55, 0.75))

# The number of models is 90:
sapply(hyper_grid.h2o, length) %>% prod()

system.time(grid_cartesian <- h2o.grid(algorithm = "randomForest",
                                       grid_id = "rf_grid1",
                                       x = features,
                                       y = target,
                                       seed = 29,
                                       nfolds = 10,
                                       training_frame = h_train,
                                       validation_frame = h_test,
                                       stopping_metric = "AUC",
                                       hyper_params = hyper_grid.h2o,
                                       search_criteria = list(strategy = "Cartesian")))

# Collect the results and sort by our model performance metric of choice:
grid_perf <- h2o.getGrid(grid_id = "rf_grid1",
                         sort_by = "auc",
                         decreasing = FALSE)

# Best model chosen by validation error:
model_drf <- h2o.getModel(grid_perf@model_ids[[1]])
#################################################################

#########################Tuning for DNN ########################################
hyper_params <- list(
  activation=c("Rectifier","Tanh","Maxout","RectifierWithDropout","TanhWithDropout","MaxoutWithDropout"),
  hidden=list(c(20,20),c(50,50),c(30,30,30),c(25,25,25,25)),
  input_dropout_ratio=c(0,0.05),
  l1=seq(0,1e-4,1e-6),
  l2=seq(0,1e-4,1e-6)
)
hyper_params

## Stop once the top 5 models are within 1% of each other (i.e., the windowed average varies less than 1%)
search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 360, max_models = 100, seed=1234567, stopping_rounds=5, stopping_tolerance=1e-2)
dl_random_grid <- h2o.grid(
  algorithm="deeplearning",
  grid_id = "dl_grid_random",
  training_frame=h_train,
  validation_frame=h_test,
  x=features,
  y=target,
  epochs=1,
  stopping_metric="logloss",
  stopping_tolerance=1e-2,        ## stop when logloss does not improve by >=1% for 2 scoring events
  stopping_rounds=2,
  score_validation_samples=10000, ## downsample validation set for faster scoring
  score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
  max_w2=10,                      ## can help improve stability for Rectifier
  hyper_params = hyper_params,
  search_criteria = search_criteria
)
grid <- h2o.getGrid("dl_grid_random",sort_by="logloss",decreasing=FALSE)
grid

grid@summary_table[1,]
model_dnn <- h2o.getModel(grid@model_ids[[1]]) ## model with lowest logloss

#################################################################


#########################Tuning for XGB ########################################
hyper_params = list( max_depth = seq(1,29,2) )

grid <- h2o.grid(

  hyper_params = hyper_params,
  search_criteria = list(strategy = "Cartesian"),

  algorithm="xgboost",
  grid_id="depth_grid",

  x = features,
  y = target,
  training_frame = h_train,
  validation_frame = h_test,

  ntrees = 10000,
  learn_rate = 0.05,

  sample_rate = 0.8,
  col_sample_rate = 0.8,

  seed = 12345,

  stopping_rounds = 5,
  stopping_tolerance = 1e-4,
  stopping_metric = "AUC",

  score_tree_interval = 10
)

grid

sortedGrid <- h2o.getGrid("depth_grid", sort_by="auc", decreasing = TRUE)
sortedGrid

topDepths = sortedGrid@summary_table$max_depth[1:5]
minDepth = min(as.numeric(topDepths))
maxDepth = max(as.numeric(topDepths))

xgboost <- h2o.getModel(sortedGrid@model_ids[[1]])
print(h2o.auc(h2o.performance(xgboost, newdata = h_test)))

xgboost@parameters

model <- do.call(h2o.xgboost,
                 ## update parameters in place
                 {
                   p <- xgboost@parameters
                   p$model_id = NULL          ## do not overwrite the original grid model
                   p$training_frame = df      ## use the full dataset
                   p$validation_frame = NULL  ## no validation frame
                   p$nfolds = 5               ## cross-validation
                   p
                 }
)
model@model$cross_validation_metrics_summary

for (i in 1:5) {
  xgboost <- h2o.getModel(sortedGrid@model_ids[[i]])
  cvgbm <- do.call(h2o.xgboost,
                   ## update parameters in place
                   {
                     p <- xgboost@parameters
                     p$model_id = NULL          ## do not overwrite the original grid model
                     p$training_frame = h_train      ## use the full dataset
                     p$validation_frame = h_test  ## no validation frame
                     p$nfolds = 5               ## cross-validation
                     p
                   }
  )
  print(xgboost@model_id)
  print(cvgbm@model$cross_validation_metrics_summary[5,]) ## Pick out the "AUC" row
}

model_xgb <- h2o.getModel(sortedGrid@model_ids[[1]])

#################################################################

automl = h2o.automl(x = features,
                    y = target,
                    training_frame = h_train,
                    validation_frame = h_test,
                    nfolds = 5,                     # 5-fold Cross-Validation
                    max_models = 30,                # Max number of models
                    stopping_metric = "AUC",       # Metric to optimize
                    seed = n_seed)

#################################

#plotting



#shap_explain_row_plot <- h2o.shap_explain_row_plot(model_drf, h_test, row_index = 3)
shap_xgb <- h2o.shap_summary_plot(model_xgb, newdata = h_test)
shap_drf <- h2o.shap_summary_plot(model_drf, newdata = h_test)
shap_automl <- h2o.shap_summary_plot(automl, newdata = h_test)

shap_drf$labels$title <- "Summary Plot\nfor \"Random Forest Model\""

### ROC Curve for train
list(model_dnn, model_gbm, model_xgb, model_glm, model_drf, automl@leader) %>%
  # map a function to each element in the list
  map(function(x) x %>% h2o.performance(train=T) %>%
        # from all these 'paths' in the object
        .@metrics %>% .$thresholds_and_metric_scores %>%
        # extracting true positive rate and false positive rate
        .[c('tpr','fpr')] %>%
        # add (0,0) and (1,1) for the start and end point of ROC curve
        add_row(tpr=0,fpr=0,.before=T) %>%
        add_row(tpr=0,fpr=0,.before=F)) %>%
  # add a column of model name for future grouping in ggplot2
  map2(c('dnn','gbm','xgb','glm', 'rf', 'automl'),
       function(x,y) x %>% add_column(model=y)) %>%
  # reduce four data.frame to one
  reduce(rbind) %>%
  # plot fpr and tpr, map model to color as grouping
  ggplot(aes(fpr,tpr,col=model))+
  geom_line()+
  geom_segment(aes(x=0,y=0,xend = 1, yend = 1),linetype = 2,col='grey')+
  xlab('False Positive Rate')+
  ylab('True Positive Rate')+
  ggtitle('ROC Curve for Six Models (Train)')


### ROC Curve for test
list(model_dnn, model_gbm, model_xgb, model_glm, model_drf, automl@leader) %>%
  # map a function to each element in the list
  map(function(x) x %>% h2o.performance(valid=T) %>%
        # from all these 'paths' in the object
        .@metrics %>% .$thresholds_and_metric_scores %>%
        # extracting true positive rate and false positive rate
        .[c('tpr','fpr')] %>%
        # add (0,0) and (1,1) for the start and end point of ROC curve
        add_row(tpr=0,fpr=0,.before=T) %>%
        add_row(tpr=0,fpr=0,.before=F)) %>%
  # add a column of model name for future grouping in ggplot2
  map2(c('dnn','gbm','xgb','glm', 'rf', 'automl'),
       function(x,y) x %>% add_column(model=y)) %>%
  # reduce four data.frame to one
  reduce(rbind) %>%
  # plot fpr and tpr, map model to color as grouping
  ggplot(aes(fpr,tpr,col=model))+
  geom_line()+
  geom_segment(aes(x=0,y=0,xend = 1, yend = 1),linetype = 2,col='grey')+
  xlab('False Positive Rate')+
  ylab('True Positive Rate')+
  ggtitle('ROC Curve for Six Models (Test)')

DNN_AUC <- round(h2o.auc(model_dnn, train = T, valid = T, xval = FALSE), 2)
XGB_AUC <- round(h2o.auc(model_xgb, train = T, valid = T, xval = FALSE), 2)
GLM_AUC <- round(h2o.auc(model_glm, train = T, valid = T, xval = FALSE), 2)
RF_AUC <- round(h2o.auc(model_drf, train = T, valid = T, xval = FALSE), 2)
XGB_AUTO_AUC <- round(h2o.auc(automl@leader, train = T, valid = T, xval = FALSE), 2)

rbind(DNN_AUC, XGB_AUC, GLM_AUC, RF_AUC, XGB_AUTO_AUC)

DNN_AUCPR <- round(h2o.aucpr(model_dnn, train = T, valid = T, xval = FALSE), 2)
XGB_AUCPR <- round(h2o.aucpr(model_xgb, train = T, valid = T, xval = FALSE), 2)
GLM_AUCPR <- round(h2o.aucpr(model_glm, train = T, valid = T, xval = FALSE), 2)
RF_AUCPR <- round(h2o.aucpr(model_drf, train = T, valid = T, xval = FALSE), 2)
XGB_AUTO_AUCPR <- round(h2o.aucpr(automl@leader, train = T, valid = T, xval = FALSE), 2)

rbind(DNN_AUCPR, XGB_AUCPR, GLM_AUCPR, RF_AUCPR, XGB_AUTO_AUCPR)

#Save five models
model_path <- h2o.saveModel(object = model_dnn, path = getwd(), force = TRUE) #dl_grid_random_model_81
model_path <- h2o.saveModel(object = model_xgb, path = getwd(), force = TRUE) #depth_grid_model_108
model_path <- h2o.saveModel(object = model_glm, path = getwd(), force = TRUE) #tuned_glm
model_path <- h2o.saveModel(object = model_drf, path = getwd(), force = TRUE) #rf_grid1_model_691
model_path <- h2o.saveModel(object = automl@leader, path = getwd(), force = TRUE) #GBM_grid__1_AutoML_20201209_204236_model_1



## SHAP contribution values
contributions_automl <- h2o::predict_contributions.H2OModel(automl, newdata = h_test)

shap_df <- contributions_automl %>%
    as.data.frame() %>%
    select(-BiasTerm) %>%
  gather(feature, shap_value) %>%
  group_by(feature) %>%
  mutate(shap_importance = mean(abs(shap_value)),
         shap_force = mean(shap_value)) %>%
  ungroup()

p1 <- ggplot(shap_df, aes (x = shap_value, y = reorder(feature, shap_importance))) +
    ggbeeswarm::geom_quasirandom(groupOnX = FALSE, varwidth = T, size = 0.9, alpha = 0.5, width = 0.15) +
    xlab("SHAP value") +
    ylab(NULL) +
    theme_minimal(base_size = 15)

p2 <- shap_df %>%
  select(feature, shap_importance) %>%
  distinct() %>%
  ggplot(aes(x = reorder(feature, shap_importance),
             y = shap_importance)) +
  geom_col(fill = 'black') +
  coord_flip() +
  xlab(NULL) +
  ylab("mean(|SHAP value|)") +
  theme_minimal(base_size = 15) +
  ggtitle("mean absolute SHAP values")

# Combine p1 and p2 plots
gridExtra::grid.arrange(p1, p2, nrow = 1)
gridExtra::grid.arrange(shap_automl, p2, nrow = 1)


# shapley-base dependence plots for a numerical features: Albumin
contributions_automl %>%
  as.data.frame() %>%
  select(-BiasTerm) %>%
  mutate(albumin = as.vector(h_test[,3])) %>%
  ggplot(aes(x = albumin, y = Albumin)) +
  geom_point(aes(color = Albumin), width = 0.1) +
         scale_colour_gradient(low = "red", high = "blue", name = 'SHAP values') +
  ylab('Shapley\'s values for Albumin feature') +
  xlab('Albumin values') +
  theme_minimal(base_size = 15) +
  geom_smooth


# shapley-base dependence plots for a numerical features: protein
contributions_automl %>%
  as.data.frame() %>%
  select(-BiasTerm) %>%
  mutate(protein = as.vector(h_test[,4])) %>%
  ggplot(aes(x = protein, y = Protein..total)) +
  geom_point(aes(color = Protein..total), width = 0.1) +
  scale_colour_gradient(low = "red", high = "blue", name = 'SHAP values') +
  ylab('Shapley\'s values for Albumin feature') +
  xlab('Albumin values') +
  theme_minimal(base_size = 15) +
  geom_smooth()

# shapley-base dependence plots for a numerical features: Alkaline
contributions_automl %>%
  as.data.frame() %>%
  select(-BiasTerm) %>%
  mutate(alkaline = as.vector(h_test[,5])) %>%
  ggplot(aes(x = alkaline, y = Alkaline.phosphatase)) +
  geom_point(aes(color = Alkaline.phosphatase), width = 0.1) +
  scale_colour_gradient(low = "red", high = "blue", name = 'SHAP values') +
  ylab('Shapley\'s values for alkaline feature') +
  xlab('alkaline values') +w
  theme_minimal(base_size = 15) +
  geom_smooth()


# # Shapley-based dependence plots for a categorical feature
# SHAP_values %>%
#   as.data.frame() %>%
#   select(-BiasTerm) %>%
#   mutate(x4_feature_values = as.vector(df_frame_split[[2]]$x4)) %>%
#   ggplot(aes(x = x4_feature_values, y = x4)) +
#   geom_jitter(aes(color = x4), width = 0.1) +
#   scale_colour_gradient(low = "red", high = "blue", name = 'SHAP values') +
#   ylab('Shapley\'s values for x4 feature') +
#   xlab('x4 feature classes') +
#   theme_minimal(base_size = 15)
