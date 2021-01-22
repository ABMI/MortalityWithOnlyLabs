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

library(shiny)
library(plotly)
library(shinycssloaders)
library(shinydashboard)

model_dnn <- h2o.loadModel("./models/dl_grid_random_model_81")
#model_xgb <- h2o.loadModel("./models/depth_grid_model_108")
model_glm <- h2o.loadModel("./models/tuned_glm")
model_drf <- h2o.loadModel("./models/rf_grid1_model_691")
model_xgb <- h2o.loadModel("./models/GBM_grid__1_AutoML_20201209_204236_model_1")

ui <- shinydashboard::dashboardPage(skin = 'black',

                                    shinydashboard::dashboardHeader(title = "FEARLES"
                                    ),

                                    shinydashboard::dashboardSidebar(
                                      shinydashboard::sidebarMenu(
                                        shinydashboard::menuItem("FEARLESS description", tabName = "Description", icon = shiny::icon("table")),
                                        shinydashboard::menuItem("Model", tabName = "Model", icon = shiny::icon("table")),
                                        shinydashboard::menuItem("Risk calculator", tabName = "Risk", icon = shiny::icon("list"))
                                      )
                                    ),

                                    shinydashboard::dashboardBody(
                                      shinydashboard::tabItems(

                                        # First tab content

                                        # Second tab
                                        shinydashboard::tabItem(tabName = "Model",

                                                                shiny::fluidRow(
                                                                  shiny::column(10, style = "background-color:#F3FAFC;",

                                                                                # do this inside tabs:
                                                                                shiny::tabsetPanel(

                                                                                  shiny::tabPanel("Model 1",
                                                                                                  shiny::div(DT::dataTableOutput('summaryModel1'),
                                                                                                             style = "font-size:70%")),

                                                                                  shiny::tabPanel("Model 2",
                                                                                                  shiny::div(DT::dataTableOutput('summaryModel2'),
                                                                                                             style = "font-size:70%")),

                                                                                  shiny::tabPanel("Model 3",
                                                                                                  shiny::div(DT::dataTableOutput('summaryModel3'),
                                                                                                             style = "font-size:70%")),

                                                                                  shiny::tabPanel("Model 4",
                                                                                                  shiny::div(DT::dataTableOutput('summaryModel4'),
                                                                                                             style = "font-size:70%"))
                                                                                )

                                                                  )

                                                                )),
                                        # Third tab
                                        shinydashboard::tabItem(tabName = "Performance",

                                                                shiny::fluidRow(
                                                                  shiny::column(2,
                                                                                shiny::h4('Filters'),
                                                                                shiny::numericInput("Albumin", "Observations:", 10, min = 1, max = 10000),
                                                                                shiny::numericInput("Alkaline phosphatase", "Observations:", 10, min = 1, max = 10000),
                                                                                shiny::numericInput("Urea nitrogen", "Observations:", 10, min = 1, max = 10000),
                                                                                shiny::numericInput("Bilirubin", "Observations:", 10, min = 1, max = 10000),
                                                                                shiny::numericInput("Calcium", "Observations:", 10, min = 1, max = 10000),
                                                                                shiny::numericInput("Chloride", "Observations:", 10, min = 1, max = 10000),
                                                                                shiny::numericInput("Creatinine", "Observations:", 10, min = 1, max = 1000),
                                                                                shiny::numericInput("Aspartate", "Observations:", 10, min = 1, max = 10000),
                                                                                shiny::numericInput("Alanine", "Observations:", 10, min = 1, max = 10000),
                                                                                shiny::numericInput("Hemoglobin", "Observations:", 10, min = 1, max = 10000),
                                                                                shiny::numericInput("Hematocrit", "Observations:", 10, min = 1, max = 10000),
                                                                                shiny::numericInput("Palatelet", "Observations:", 10, min = 1, max = 10000),
                                                                                shiny::numericInput("Phosphorus", "Observations:", 10, min = 1, max = 10000),
                                                                                shiny::numericInput("Potassium", "Observations:", 10, min = 1, max = 10000),
                                                                                shiny::numericInput("Protein", "Observations:", 10, min = 1, max = 10000),
                                                                                shiny::numericInput("Sodium", "Observations:", 10, min = 1, max = 10000),
                                                                                shiny::numericInput("Urate", "Observations:", 10, min = 1, max = 10000),
                                                                                shiny::numericInput("WBC", "Observations:", 10, min = 1, max = 10000),
                                                                                shiny::numericInput("CRP", "Observations:", 10, min = 1, max = 10000)
                                                                  ),

                                                                  tabBox(
                                                                    title = "Performance",
                                                                    # The id lets us use input$tabset1 on the server to find the current tab
                                                                    id = "tabset1", height = "100%", width='100%',
                                                                    tabPanel("Summary",

                                                                             shiny::fluidRow(
                                                                               shiny::column(width = 4,
                                                                                             shinydashboard::box(width = 12,
                                                                                                                 title = tagList(shiny::icon("question"),"Prediction Question"), status = "info", solidHeader = TRUE,
                                                                                                                 shiny::textOutput('info')
                                                                                             ),
                                                                                             shinydashboard::box(width = 12,
                                                                                                                 title = tagList(shiny::icon("gear"), "Input"),
                                                                                                                 status = "info", solidHeader = TRUE,
                                                                                                                 shiny::splitLayout(
                                                                                                                   cellWidths = c('5%', '90%', '5%'),
                                                                                                                   shiny::h5(' '),
                                                                                                                   shiny::sliderInput("slider1",
                                                                                                                                      shiny::h4("Threshold value slider: ", strong(shiny::textOutput('threshold'))),
                                                                                                                                      min = 1, max = 100, value = 50, ticks = F),
                                                                                                                   shiny::h5(' ')
                                                                                                                 ),
                                                                                                                 shiny::splitLayout(
                                                                                                                   cellWidths = c('5%', '90%', '5%'),
                                                                                                                   shiny::h5(strong('0')),
                                                                                                                   shiny::h5(' '),
                                                                                                                   shiny::h5(strong('1'))
                                                                                                                 ),
                                                                                                                 shiny::tags$script(shiny::HTML("
                                                                                                                                                $(document).ready(function() {setTimeout(function() {
                                                                                                                                                supElement = document.getElementById('slider1').parentElement;
                                                                                                                                                $(supElement).find('span.irs-max, span.irs-min, span.irs-single, span.irs-from, span.irs-to').remove();
                                                                                                                                                }, 50);})
                                                                                                                                                "))
                                                                                                                 )

                                                                                             ),


                                                                               shiny::column(width = 8,
                                                                                             shinydashboard::box(width = 12,
                                                                                                                 title = "Dashboard",
                                                                                                                 status = "warning", solidHeader = TRUE,
                                                                                                                 shinydashboard::infoBoxOutput("performanceBoxThreshold"),
                                                                                                                 shinydashboard::infoBoxOutput("performanceBoxIncidence"),
                                                                                                                 shinydashboard::infoBoxOutput("performanceBoxPPV"),
                                                                                                                 shinydashboard::infoBoxOutput("performanceBoxSpecificity"),
                                                                                                                 shinydashboard::infoBoxOutput("performanceBoxSensitivity"),
                                                                                                                 shinydashboard::infoBoxOutput("performanceBoxNPV")

                                                                                             ),
                                                                                             shinydashboard::box(width = 12,
                                                                                                                 title = "Cutoff Performance",
                                                                                                                 status = "warning", solidHeader = TRUE,
                                                                                                                 shiny::tableOutput('twobytwo')
                                                                                                                 #infoBoxOutput("performanceBox"),
                                                                                             )
                                                                               )
                                                                               )


                                                                               ),
                                                                    tabPanel("Discrimination",

                                                                             shiny::fluidRow(
                                                                               shinydashboard::box( status = 'info',
                                                                                                    title = "ROC Plot", solidHeader = TRUE,
                                                                                                    shinycssloaders::withSpinner(plotly::plotlyOutput('roc'))),
                                                                               shinydashboard::box(status = 'info',
                                                                                                   title = "Precision recall plot", solidHeader = TRUE,
                                                                                                   side = "right",
                                                                                                   shinycssloaders::withSpinner(plotly::plotlyOutput('pr')))),

                                                                             shiny::fluidRow(
                                                                               shinydashboard::box(status = 'info',
                                                                                                   title = "F1 Score Plot", solidHeader = TRUE,
                                                                                                   shinycssloaders::withSpinner(plotly::plotlyOutput('f1'))),
                                                                               shinydashboard::box(status = 'info',
                                                                                                   title = "Box Plot", solidHeader = TRUE,
                                                                                                   side = "right",
                                                                                                   shinycssloaders::withSpinner(shiny::plotOutput('box')))),

                                                                             shiny::fluidRow(
                                                                               shinydashboard::box(status = 'info',
                                                                                                   title = "Prediction Score Distribution", solidHeader = TRUE,
                                                                                                   shinycssloaders::withSpinner(shiny::plotOutput('preddist'))),
                                                                               shinydashboard::box(status = 'info',
                                                                                                   title = "Preference Score Distribution", solidHeader = TRUE,
                                                                                                   side = "right",
                                                                                                   shinycssloaders::withSpinner(shiny::plotOutput('prefdist'))))


                                                                    ),
                                                                    tabPanel("Calibration",
                                                                             shiny::fluidRow(
                                                                               shinydashboard::box(status = 'info',
                                                                                                   title = "Calibration Plot", solidHeader = TRUE,
                                                                                                   shinycssloaders::withSpinner(shiny::plotOutput('cal'))),
                                                                               shinydashboard::box(status = 'info',
                                                                                                   title = "Demographic Plot", solidHeader = TRUE,
                                                                                                   side = "right",
                                                                                                   shinycssloaders::withSpinner(shiny::plotOutput('demo')))
                                                                             )
                                                                    )
                                                                                             )))


                                                                )
                                    )
                                    )
