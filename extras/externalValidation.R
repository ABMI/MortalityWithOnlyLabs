# @file externalValidation.R
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

#########External validation

library(PatientLevelPrediction)
library(MortalityWithOnlyLabs)
library(dplyr)

#connection server
# add details of your database setting:
databaseName <- 'AUSOM'
# add the cdm database schema with the data
cdmDatabaseSchema <- 'CDMPv532.dbo'
# add the work database schema this requires read/write privileges
cohortDatabaseSchema <- 'cohortdb.dbo' # 'cohortdb.dbo'
# if using oracle please set the location of your temp schema
oracleTempSchema <- NULL
# the name of the table that will be created in cohortDatabaseSchema to hold the cohorts
cohortTable <- 'MortalityWithOnlyLabs'
# the location to save the prediction models results to:
outputFolder <- '~/output/MortalityWithOnlyLabs'
# add connection details:
options(fftempdir = '~/fftemp')
# dbms <- ""
# user <- ''
# pw <- ''
# server <- ''

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw)
#######learning code
ParallelLogger::logInfo("Creating Cohorts")
createCohorts(connectionDetails,
              cdmDatabaseSchema=cdmDatabaseSchema,
              cohortDatabaseSchema=cohortDatabaseSchema,
              cohortTable=cohortTable,
              outputFolder = outputFolder)
##Create population
# 776 JHCho_any_death
# 1448 JHCho_surgery_V6
covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T,
                                                                useDemographicsAge = T,
                                                                useDemographicsAgeGroup = T)
plpData <- PatientLevelPrediction::getPlpData(connectionDetails,
                                              cdmDatabaseSchema = cdmDatabaseSchema,
                                              cohortId = 1448, outcomeIds = 776, # cohortId = 1373,
                                              cohortDatabaseSchema = cohortDatabaseSchema,
                                              outcomeDatabaseSchema = cohortDatabaseSchema,
                                              cohortTable = cohortTable,
                                              outcomeTable = cohortTable,
                                              covariateSettings=covariateSettings)
# outcome data
population <- PatientLevelPrediction::createStudyPopulation(plpData = plpData,
                                                            outcomeId = 776,
                                                            binary = T,
                                                            includeAllOutcomes = T,
                                                            requireTimeAtRisk = T,
                                                            minTimeAtRisk = 1,
                                                            riskWindowStart = 4,
                                                            riskWindowEnd = 30,
                                                            removeSubjectsWithPriorOutcome = T)
# View(population)
connection <- DatabaseConnector::connect(connectionDetails)
########################
covariates <- c('3024561','3035995','3013682','3024128','3006906','3014576','3016723','3013721','3006923','3027484','3023314','3007461','3011904','3023103','3020630','3019550','3037556','3010813','3020460')
sql <- "select * from @cdmDatabaseSchema.measurement a, (select * from @cohortDatabaseSchema.@cohortTable where cohort_definition_id=1448) b
where a.person_id=b.subject_id
and (measurement_date >= cohort_start_date and measurement_date <= DATEADD(day, 3, cohort_end_date))
and measurement_concept_id in (@covariates)"
sql <- SqlRender::renderSql(sql=sql, cdmDatabaseSchema=cdmDatabaseSchema, cohortDatabaseSchema=cohortDatabaseSchema, cohortTable=cohortTable, covariates=covariates)$sql
features <- DatabaseConnector::querySql(connection = connection, sql = sql)
external_df <- features

colnames(external_df) <- tolower(colnames(external_df))
external_df <- external_df %>% select(person_id, measurement_date, measurement_concept_id, value_as_number)
population2 <- population

test2 <- left_join(population2, external_df, by=c("subjectId"="person_id", "cohortStartDate"="measurement_date"))
outcomeData <- unique(test2 %>% select(subjectId, outcomeCount))

external_df <- reshape2::dcast(external_df, person_id+measurement_date ~ measurement_concept_id, value.var = 'value_as_number', fun.aggregate = mean, na.rm=T)
temp <- external_df %>% select(person_id, measurement_date)
temp1 <- temp
for(i in 1:30){
  temp2 <- temp1 %>% group_by(person_id)
  temp3 <- temp2 %>% summarise(minDate=min(measurement_date))
  temp2 <- left_join(temp2, temp3, by=c("person_id"="person_id"))
  temp2$dateDiff <- temp2$measurement_date-temp2$minDate

  assign(paste0("data", i), temp2 %>% filter(dateDiff<4))
  temp1 <- temp2 %>% filter(dateDiff>=4) %>% select(-c(dateDiff, minDate))
}
rm(data27, data28, data29, data30)
seqLabelData <- rbind(data1, data2,data3,data4,data5,data6,data7,data8,data9,data10,data11,data12,data13,data14,data15,data16,data17,data18,data19,data20,data21,data22,data23,data24,data25,data26)
rm(data1, data2,data3,data4,data5,data6,data7,data8,data9,data10,data11,data12,data13,data14,data15,data16,data17,data18,data19,data20,data21,data22,data23,data24,data25,data26)

test <- left_join(seqLabelData, external_df, by=c("person_id", "person_id"))



table(outcomeData$outcomeCount)
#       0      1
# 198914    369

# subset(external_df, (death_inhosp %in% NA)) %>% select(person_id)

#colnames(external_df) <- c("person_id", "Albumin", "Alkaline.phosphatase", "BUN", "Bilirubin..total", "Calcium", "Chloride", "Creatinine", "GOT..AST.", "GPT..ALT.", "Hb", "Hct", "PLT", "Phosphorus", "Potassium", "Protein..total", "Sodium", "Uric.Acid", "WBC", "hs.CRP.quantitation", "death_inhosp")
#colnames(external_df) <- c("person_id", "Calcium", "GPT..ALT.", "PLT", "hs.CRP.quantitation", "WBC", "Phosphorus", "BUN", "GOT..AST.", "Chloride", "Creatinine", "Sodium", "Protein..total", "Potassium", "Hct", "Bilirubin..total", "Albumin", "Hb", "Alkaline.phosphatase", "Uric.Acid", "death_inhosp")
colnames(external_df) <- c("person_id", "measurement_date", "Calcium", "GPT..ALT.", "PLT", "WBC", "Phosphorus", "BUN", "GOT..AST.", "Chloride", "Creatinine", "Sodium", "hs.CRP.quantitation", "Protein..total", "Potassium", "Hct", "Bilirubin..total", "Albumin", "Hb", "Alkaline.phosphatase", "Uric.Acid", "death_inhosp")

external_df_h2o <- external_df

external_df$countNA <- apply(external_df, 1, function(x) sum(is.na(x)))
external_df_h2o <- external_df[external_df[,"countNA"]<2,]

########################

h2o.init()

modelPath <- getwd()

#Load h2o models
model_dnn <- h2o.loadModel(file.path(modelPath, "dl_grid_random_model_81"))
model_xgb <- h2o.loadModel(file.path(modelPath, "depth_grid_model_108"))
model_glm <- h2o.loadModel(file.path(modelPath, "tuned_glm"))
model_drf <- h2o.loadModel(file.path(modelPath, "rf_grid1_model_691"))
automl <- h2o.loadModel(file.path(modelPath, "GBM_grid__1_AutoML_20201209_204236_model_1"))

#H2O Form
external_df_h2o$death_inhosp <- as.factor(external_df_h2o$death_inhosp)
external_df_h2o <- as.h2o(external_df_h2o)

#External validation
external_perf_dnn <- h2o.performance(model_dnn, external_df_h2o)
external_perf_xgb <- h2o.performance(model_xgb, external_df_h2o)
external_perf_glm <- h2o.performance(model_glm, external_df_h2o)
external_perf_drf <- h2o.performance(model_drf, external_df_h2o)
external_perf_automl <- h2o.performance(automl, external_df_h2o)

external_perf_dnn@metrics$AUC
external_perf_xgb@metrics$AUC
external_perf_glm@metrics$AUC
external_perf_drf@metrics$AUC
external_perf_automl@metrics$AUC

DNN_AUC_ext <- round(external_perf_dnn@metrics$AUC, 2)
XGB_AUC_ext <- round(external_perf_xgb@metrics$AUC, 2)
GLM_AUC_ext <- round(external_perf_glm@metrics$AUC, 2)
RF_AUC_ext <- round(external_perf_drf@metrics$AUC, 2)
XGB_AUTO_AUC_ext <- round(external_perf_automl@metrics$AUC, 2)

rbind(DNN_AUC_ext, XGB_AUC_ext, GLM_AUC_ext, RF_AUC_ext, XGB_AUTO_AUC_ext)

DNN_AUCPR_ext <- round(external_perf_dnn@metrics$pr_auc, 2)
XGB_AUCPR_ext <- round(external_perf_xgb@metrics$pr_auc, 2)
GLM_AUCPR_ext <- round(external_perf_glm@metrics$pr_auc, 2)
RF_AUCPR_ext <- round(external_perf_drf@metrics$pr_auc, 2)
XGB_AUTO_AUCPR_ext <- round(external_perf_automl@metrics$pr_auc, 2)

rbind(DNN_AUCPR_ext, XGB_AUCPR_ext, GLM_AUCPR_ext, RF_AUCPR_ext, XGB_AUTO_AUCPR_ext)

#Plotting
list(model_glm, model_dnn, model_drf, automl) %>%
  # map a function to each element in the list
  map(function(x) x %>% h2o.performance(external_df_h2o) %>%
        # from all these 'paths' in the object
        .@metrics %>% .$thresholds_and_metric_scores %>%
        # extracting true positive rate and false positive rate
        .[c('tpr','fpr')] %>%
        # add (0,0) and (1,1) for the start and end point of ROC curve
        add_row(tpr=0,fpr=0,.before=T) %>%
        add_row(tpr=0,fpr=0,.before=F)) %>%
  # add a column of model name for future grouping in ggplot2
  map2(c('GLM','DNN','RF', 'XGB'),
       function(x,y) x %>% add_column(model=y)) %>%
  # reduce six data.frame to one
  reduce(rbind) %>%
  # plot fpr and tpr, map model to color as grouping
  ggplot(aes(fpr,tpr,col=model))+
  geom_line()+
  geom_segment(aes(x=0,y=0,xend = 1, yend = 1),linetype = 2,col='grey')+
  xlab('False Positive Rate')+
  ylab('True Positive Rate')+
  ggtitle('ROC Curve for Four Models (External valid)')

# Get table1
external_df_h2o <- external_df

exp_ext_df <- external_df_h2o
exp_int_df <- exp_df

exp_ext_df <- exp_ext_df[,setdiff(colnames(exp_ext_df), c("person_id", "countNA"))]
exp_int_df <- exp_int_df[,setdiff(colnames(exp_int_df), c("caseid"))]

exp_ext_df$db <- "AUSOM"
exp_int_df$db <- "VITALDB"

comparativeDB <- rbind(exp_ext_df, exp_int_df)
mytableNoAtt <- mytable(db~.,data=comparativeDB)


external_df$countNA <- apply(external_df, 1, function(x) sum(is.na(x)))
external_df_h2o <- external_df[external_df[,"countNA"]<2,]
exp_ext_df <- external_df_h2o
exp_ext_df <- exp_ext_df[,setdiff(colnames(exp_ext_df), c("person_id", "countNA"))]
exp_ext_df$db <- "AUSOM"
comparativeDB <- rbind(exp_ext_df, exp_int_df)
mytableAtt <- mytable(db~.,data=comparativeDB)


##AUPRC

load_model_performance_metrics <- function(path, h_test) {

  model_h2o <- h2o.loadModel(path)
  perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(h_test))

  perf_h2o %>%
    h2o.metric() %>%
    as_tibble() %>%
    mutate(auc = h2o.auc(perf_h2o)) %>%
    select(tpr, fpr, auc, precision, recall)

}

model_metrics_tbl <- fs::dir_info(path = "~/study/MortalityWithOnlyLabs/models") %>%
  select(path) %>%
  mutate(metrics = map(path, load_model_performance_metrics, h_test)) %>%
  unnest(cols = metrics)

model_metrics_tbl %>%
  mutate(
    #path = str_split(path, pattern = "/", simplify = T)[,3] %>% as_factor(),
    auc  = auc %>% round(3) %>% as.character() %>% as_factor()
  ) %>%
  ggplot(aes(recall, precision, color = auc)) +
  geom_line(size = 1) +
  #theme_new +
  theme(
    legend.direction = "vertical"
  ) +
  labs(
    title = "Precision vs Recall Plot",
    subtitle = "Performance of 4 Models (test)"
  )




colnames(features) <- tolower(colnames(features))
a <- data.frame(features %>% group_by(person_id) %>%  mutate(min_date = min(measurement_date, na.rm = T)))
a$datediff <- a$measurement_date - a$min_date
