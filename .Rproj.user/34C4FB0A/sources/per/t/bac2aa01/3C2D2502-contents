library(PatientLevelPrediction)
library(MortalityWithOnlyLabs)
library(dplyr)
library(glmnet)
#install.packages("mice")
# library(tidyr)

#connection server
# add details of your database setting:
databaseName <- 'add a shareable name for the database you are currently validating on'

# add the cdm database schema with the data
cdmDatabaseSchema <- 'your cdm database schema for the validation'

# add the work database schema this requires read/write privileges
cohortDatabaseSchema <- 'your work database schema'

# if using oracle please set the location of your temp schema
oracleTempSchema <- NULL

# the name of the table that will be created in cohortDatabaseSchema to hold the cohorts
cohortTable <- 'MortalityWithOnlyLabs'

# the location to save the prediction models results to:
outputFolder <- '~/MortalityWithOnlyLabs'

# add connection details:
options(fftempdir = 'T:/fftemp')
dbms <- "pdw"
user <- NULL
pw <- NULL
server <- Sys.getenv('server')
port <- Sys.getenv('port')
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)


#######learning code
ParallelLogger::logInfo("Creating Cohorts")
createCohorts(connectionDetails,
              cdmDatabaseSchema=cdmDatabaseSchema,
              cohortDatabaseSchema=cohortDatabaseSchema,
              cohortTable=cohortTable,
              outputFolder = outputFolder)

##Create population
covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T,
                                                                useDemographicsAge = T,
                                                                useDemographicsAgeGroup = T)

plpData <- PatientLevelPrediction::getPlpData(connectionDetails,
                                              cdmDatabaseSchema = cdmDatabaseSchema,
                                              cohortId = 356, outcomeIds = 355,
                                              cohortDatabaseSchema = cohortDatabaseSchema,
                                              outcomeDatabaseSchema = cohortDatabaseSchema,
                                              cohortTable = cohortTable,
                                              outcomeTable = cohortTable,
                                              covariateSettings=covariateSettings)
# outcome data
population <- PatientLevelPrediction::createStudyPopulation(plpData = plpData,
                                                            outcomeId = 355,
                                                            binary = T,
                                                            includeAllOutcomes = T,
                                                            requireTimeAtRisk = T,
                                                            minTimeAtRisk = 0,
                                                            riskWindowStart = 0,
                                                            riskWindowEnd = 27,
                                                            removeSubjectsWithPriorOutcome = T)

connection <- DatabaseConnector::connect(connectionDetails)

# feature/covariate extraction
covariateSettings <- list()
covariateSettings <- FeatureExtraction::createCovariateSettings(
  shortTermStartDays = -7,
  endDays = 0,
  useMeasurementValueShortTerm = T
)

data <- FeatureExtraction::getDbCovariateData(
  connectionDetails = connectionDetails,
  #connection = connection,
  oracleTempSchema = NULL,
  cdmDatabaseSchema=cdmDatabaseSchema,
  cdmVersion = "5",
  cohortTable = cohortTable,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTableIsTemp = FALSE,
  cohortId = -1,
  rowIdField = "subject_id",
  covariateSettings=covariateSettings,
  aggregated = FALSE
)

## connect to db
con <- dbConnect(drv=RSQLite::SQLite(), dbname=data@dbname)
ref <- dbGetQuery(conn=con, "select * from covariateRef")
# cov <- dbGetQuery(conn=con, "select * from covariates")

covs <- c(
 "Albumin serum/plasma (gram per deciliter)",
 "Alkaline phosphatase serum/plasma (unit per liter)",
 "Urea nitrogen [Mass/volume] in Blood (milligram per deciliter)",
 "Basophils % (percent)",
 "Bilirubin.total [Mass/volume] in Blood (milligram per deciliter)",
 "Calcium serum/plasma serum/plasma (milligram per deciliter)",
 "Chloride [Moles/volume] in Serum, Plasma or Blood (millimole per liter)",
 "Creatine [Mass/volume] in Serum or Plasma (milligram per deciliter)",
 "Eosinophils % (percent)",
 "Aspartate aminotransferase serum/plasma (unit per liter)",
 "Alanine aminotransferase serum/plasma (unit per liter)",
 "Hemoglobin [Mass/volume] in Blood by calculation (gram per deciliter)",
 "Hematocrit [Volume Fraction] of Blood by Automated count (percent)",
 "Lymphocytes % (percent)",
 "MCH (pg)",
 "MCHC (gram per deciliter)",
 "MCV (fL)",
 "Platelet mean volume [Entitic volume] in Blood (fL)",
 "Monocytes % (percent)",
 "Platelet count (thousand per microliter)",
 "Inorganic phosphorus measurement (milligram per deciliter)",
 "Potassium [Moles/volume] in Serum, Plasma or Blood (millimole per liter)",
 "Protein serum/plasma (gram per deciliter)",
 "Red blood cell (RBC) count (million per microliter)",
 "Erythrocyte distribution width [Ratio] by Automated count (percent)",
 "Neutrophils % (percent)",
 "Sodium [Moles/volume] in Serum, Plasma or Blood (millimole per liter)",
 "Serum uric acid measurement (milligram per deciliter)",
 "White Blood cell (WBC) count (leukocyte) (thousand per microliter)"
 )

## list all tables
tables <- dbListTables(con)

# Labvalue covariates
# covariateIds <- dbGetQuery(conn=con, "select * from covariateRef where conceptId in (
#                             3001123, 3002030, 3003338, 3013682, 3006504, 3006906, 3006923, 3007461, 3010813, 3013721, 3016723, 3018010,
#                             3019069, 3019897, 3020630, 3022096, 3023314, 3024561, 3024731, 3026361, 3027484, 3024128, 3035941, 3035995,
#                             3011904, 3037556, 3023103, 3014576, 3019550
# )")

#cov_ids <- paste0(covs, collapse = ",")


covariateIds <- dbGetQuery(conn=con, paste0("select * from covariateName where conceptId like (", covs, ")")[1])
covariateIds <- dbGetQuery(conn=con, "select * from covariateRef where covariateName in ('Albumin serum/plasma (gram per deciliter)')")

covariateIds <- paste0(covariateIds$covariateId, collapse = ",")
# continuousCovariateIds <- paste0(continuousCovariateIds$covariateId, collapse = ",")

df <- dbGetQuery(conn=con, paste0("select * from covariates where covariateId IN (", covariateIds, ")"))
df <- df %>% left_join(dbGetQuery(conn=con, "select * from covariateRef"), by=c("covariateId"="covariateId"))
df <- df %>% filter()

population2 <- population

### preprocessing
# test$covariateValue <- as.double(df$covariateValue)
value <- reshape2::dcast(df, rowId ~ conceptId, value.var = 'covariateValue', fun.aggregate = mean, na.rm=T)
# value <- value %>% select(c("3004249", "3012888", "3027018", "3024171", "40762499", "43008898", "3007461", "3024128", "3030477", "3013682", "3017250", "3016723", "3010813"))
meas <- list("3004249", "3012888", "3027018", "3024171", "40762499", "43008898", "3007461", "3024128", "3030477", "3013682", "3017250", "3016723", "3010813")



for(i in 2:ncol(value)){
  if(colnames(value)[i] %in% meas){
    value[,i] <- imputeTS::na_mean(value[,i])
  }
}
value <- replace(value, is.na(value), 0)
temp <- left_join(population2, value, by=c("subjectId"="rowId"))
temp2 <- tidyr::drop_na(temp[,c(1,14:length(temp))])
temp3 <- inner_join(temp[,1:13], temp2, by=c("rowId"="rowId"))
df <- temp3
###
df <- df[,c(8:10,14:length(df))]
df$gender <- ifelse(df$gender=='8532', 0, 1) # female 0, male 1

label = as.integer(df$outcomeCount)
# table(label)
n <- nrow(df)

train.index = sample(n,floor(0.75*n))
train.data = df[train.index,]
train.label = label[train.index]
# table(train.label)
test.data = df[-train.index,]
test.label = label[-train.index]
# table(test.label)

model <- glm(outcomeCount ~., data = train.data, family = 'binomial')
summary(model)
model$coefficients

prob <- model %>% predict(test.data, type='response')
