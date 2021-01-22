# @file server.R
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

riskCalculate <- function(){
  if(inputModelType=="GLM"){
    return(GLM_assess)
  }else if(inputModelType=="DNN"){
    return(DNN_assess)
  }else if(inputModelType=="XGB"){
    return(XGB_assess)
  }else if(inputModelType=="RF"){
    return(RF_assess)
  }
}

patientLevelAssessment <- function(){
  if(inputModelType=="GLM"){
    return(GLM_VALUE)
  }else if(inputModelType=="DNN"){
    return(DNN_VALUE)
  }else{
    return(SHAP_VALUE)
  }
}

summaryModel1 <- function(){

}

summaryModel2 <- function(){

}

summaryModel3 <- function(){

}

summaryModel4 <- function(){

}
