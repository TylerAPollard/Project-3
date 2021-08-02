## ST558 Project 3 UI
# Author: Tyler Pollard
# Date Created: 26 July 2021
# Date Last Modfied: 26 July 2021
# Version: 1.0

library(shiny)
library(shinydashboard)
library(bs4Dash)
library(shinyWidgets)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)
library(fresh)
library(lubridate)
library(forcats)
library(knitr)
library(DT)
library(readr)
library(plotly)

## The Different sections of code may be examined individually by condensing the chunk with the heart button on the left next to 
## the code line number

shinyUI(
    bs4DashPage(dark = NULL,
        # =========== Dashboard Navbar =============
        bs4DashNavbar(title = div(h1("NFL Games"), align = "center", color = "white"), status = "indigo", skin = "light",
                      rightUi = tags$li(class = "dropdown",actionBttn(inputId = "about", label = "", style = "minimal", icon = icon("info-circle")))), # close bs4DashNavbar
        # =========== Dashboard Sidebar ============
        bs4DashSidebar(status = "indigo", skin = "light",
            bs4SidebarMenu(
                bs4SidebarMenuItem(text = "Dashboard Home", tabName = "home", icon = icon("home")),
                bs4SidebarMenuItem(text = "Data", tabName = "data", icon = icon("table")),
                bs4SidebarMenuItem(text = "Data Exploration", tabName = "data_exploration", icon = icon("chart-area")),
                bs4SidebarMenuItem(text = "Modeling", icon = icon("chart-line"),
                                   bs4SidebarMenuSubItem(text = "Modeling Info", tabName = "model_info"),
                                   bs4SidebarMenuSubItem(text = "Model Fitting", tabName = "model_fit"),
                                   bs4SidebarMenuSubItem(text = "Prediction", tabName = "prediction")
                                   )
            ) # close bs4sidebarMenu
        ), # close bs4Dashsidebar
        # =========== Dashboard Body ===============
        bs4DashBody(
            bs4TabItems(
                # ========= About Tab ==============
                bs4TabItem(tabName = "about"),
                # ========= Data Tab ===============
                bs4TabItem(tabName = "data",
                           fluidPage(
                               h1("Data"),
                               hr(),
                               # Data Filter box
                               box(title = strong("Data Filters"), collapsible = TRUE, collapsed = FALSE, closable = FALSE, width =12, color = "warning", status ="warning", solidHeader = TRUE, elevation = 3,
                                   # Column Filter
                                   fluidRow(
                                       column(width = 4,
                                              h4("Filter by column"),
                                              switchInput(inputId = "switch_column_filter",
                                                          onStatus = "success",
                                                          offStatus = "danger",
                                                          value = FALSE),
                                              conditionalPanel(condition = "input.switch_column_filter == 1",
                                                               uiOutput("column.filter")
                                              )
                                       )
                                   ), # end Column Filter
                                   # Row Filters
                                   fluidRow(
                                       # Season Row Filter
                                       column(width = 3,
                                              h4("Filter by Season"),
                                              switchInput(inputId = "switch_season_filter", onStatus = "success", offStatus = "danger"),
                                              conditionalPanel(condition = "input.switch_season_filter",
                                                               uiOutput("season.filter")
                                              )
                                       ),
                                       # Game Type Filter
                                       column(width = 3,
                                              h4("Filter by Game Type"),
                                              switchInput(inputId = "switch_game_filter", onStatus = "success", offStatus = "danger"),
                                              conditionalPanel(condition = "input.switch_game_filter",
                                                               uiOutput("game.filter")
                                              )
                                       ),
                                       # Team Filter
                                       column(width = 3,
                                              h4("Filter by Team"),
                                              switchInput(inputId = "switch_team_filter", onStatus = "success", offStatus = "danger"),
                                              conditionalPanel(condition = "input.switch_team_filter",
                                                               uiOutput("team.filter")
                                              )
                                       )
                                   )
                               ), # end Data Filter box
                               # Data table box
                               box(title = strong("Data"), collapsible = FALSE, closable = FALSE, maximizable = TRUE, status = "warning", solidHeader = TRUE, width = 12, 
                                   label = strong("Save Data"),
                                   dropdownMenu = boxDropdown(icon = icon("save"),
                                                              boxDropdownItem(textInput(inputId = "data.title", label = "Name of data file")),
                                                              boxDropdownItem(actionBttn(inputId = "save.data", label = "Save Data", icon = icon("save")))
                                   ),
                                   # actionBttn(inputId = "save.data", label = "Save Data", icon = icon("save")), align = "right",
                                   fluidRow(
                                       column(width =12,
                                              DT::dataTableOutput(outputId = "data.table")
                                       )
                                   )
                               ) # end data table box
                           ) # end fluidPage
                ), # data tab
                # =========== Data Exploration Tab ==============
                bs4TabItem(tabName = "data_exploration",
                           fixedPage(
                               h1("Data Exploration"),
                               hr(),
                               fluidRow(
                                   column(width = 4,
                                          box(title = strong("Summary Inputs"), width = 12, status = "warning", solidHeader = TRUE, collapsible = FALSE, closable = FALSE, elevation = 3,
                                              h3("Contingency Table"),
                                              pickerInput(inputId = "contingency_variable1",
                                                          label = "Select variable to create contingency table for",
                                                          choices = list(
                                                              "Season" = "season",
                                                              "Game Type" = "game_type",
                                                              "Weekday" = "weekday",
                                                              "Away Team" = "away_team",
                                                              "Home Team" = "home_team",
                                                              "Overtime" = "overtime",
                                                              "Division Game" = "div_game",
                                                              "Roof" = "roof",
                                                              "Surface" = "surface"
                                                          ),
                                                          selected = NULL,
                                                          options = pickerOptions(actionsBox = TRUE)
                                              ),
                                              h6("Add variable"),
                                              switchInput(inputId = "switch_contingency_filter",
                                                          onStatus = "success",
                                                          offStatus = "danger",
                                                          value = FALSE
                                              ),
                                              conditionalPanel(condition = "input.switch_contingency_filter",
                                                               pickerInput(inputId = "contingency_variable2",
                                                                           label = "Select second variable for contigency table",
                                                                           choices = list(
                                                                               "Season" = "season",
                                                                               "Game Type" = "game_type",
                                                                               "Weekday" = "weekday",
                                                                               "Away Team" = "away_team",
                                                                               "Home Team" = "away_team",
                                                                               "Overtime" = "overtime",
                                                                               "Division Game" = "div_game",
                                                                               "Roof" = "roof",
                                                                               "Surface" = "surface"
                                                                           ),
                                                                           selected = "game_type",
                                                                           options = pickerOptions(actionsBox = TRUE)
                                                               )
                                              ),
                                              hr(),
                                              h3("Summary Table"),
                                              pickerInput(inputId = "summary.variable",
                                                          label = "Select variables to summarize",
                                                          choices = list(
                                                              "Point Total" = "total",
                                                              "Total Line" = "total_line",
                                                              "Point Differential" = "result",
                                                              "Spread Line" = "spread_line",
                                                              "Temperature" = "temp",
                                                              "Wind Speed" = "wind"),
                                                          multiple = TRUE,
                                                          options = pickerOptions(actionsBox = TRUE)
                                              ),
                                              h6("Filter by row"),
                                              switchInput(inputId = "switch_summary_filter",
                                                          onStatus = "success",
                                                          offStatus = "danger",
                                                          value = FALSE
                                              ),
                                              conditionalPanel(condition = "input.switch_summary_filter",
                                                               pickerInput(inputId = "summary_variable_filter",
                                                                           label = "Select variable to filter by",
                                                                           choices = list(
                                                                               "Season" = "season",
                                                                               "Game Type" = "game_type",
                                                                               "Weekday" = "weekday",
                                                                               "Team" = "away_team",
                                                                               "Overtime" = "overtime",
                                                                               "Division Game" = "div_game",
                                                                               "Roof" = "roof",
                                                                               "Surface" = "surface"
                                                                           )
                                                               )
                                              ),
                                              conditionalPanel(condition = "input.switch_summary_filter",
                                                               uiOutput("summary.data")
                                              )
                                          )
                                   ),
                                   column(width = 8,
                                          box(title = strong("Summaries"), width = 12, status = "warning", solidHeader = TRUE, collapsible = FALSE, closable = FALSE, elevation = 3,
                                              h3("Contingency Table"),
                                              tableOutput("contingency.table"),
                                              hr(),
                                              h3("Summary Table"),
                                              tableOutput("summary.table")
                                          )
                                   )
                               ),
                               hr(),
                               # Visual Inputs
                               fluidRow(
                                   column(width = 4,
                                          box(title = strong("Visual Inputs"), width = 12, status = "gray-dark", solidHeader = TRUE, collapsible = FALSE, closable = FALSE, elevation = 3,
                                              radioGroupButtons(inputId = "plot_type",
                                                                label = "Please select graph type",
                                                                choices = c(
                                                                    "Bar Chart" = "bar",
                                                                    "Histogram" = "histogram",
                                                                    "Boxplot" = "boxplot",
                                                                    "Scatter Plot" = "scatter"
                                                                ),
                                                                checkIcon = list(
                                                                    yes = icon("ok", lib = "glyphicon")
                                                                ),
                                                                direction = "vertical"
                                              ),
                                              # Boxplot
                                              conditionalPanel(condition = "input.plot_type == 'bar'",
                                                               pickerInput(inputId = "bar_variable",
                                                                           label = "Please select variables for bar chart",
                                                                           choices = list(
                                                                               "Season" = "season",
                                                                               "Game Type" = "game_type",
                                                                               "Weekday" = "weekday",
                                                                               "Away Team" = "away_team",
                                                                               "Home Team" = "home_team",
                                                                               "Overtime" = "overtime",
                                                                               "Division Game" = "div_game",
                                                                               "Roof" = "roof",
                                                                               "Surface" = "surface"
                                                                           ),
                                                                           options = pickerOptions(actionsBox = TRUE, dropupAuto = TRUE)
                                                               )
                                              ), # end boxplot
                                              # Histogram
                                              conditionalPanel(condition = "input.plot_type == 'histogram'",
                                                               pickerInput(inputId = "histogram_variables",
                                                                           label = "Please select variables for histogram",
                                                                           choices = list(
                                                                               "Point Total" = "total",
                                                                               "Total Line" = "total_line",
                                                                               "Point Differential" = "result",
                                                                               "Spread Line" = "spread_line",
                                                                               "Temperature" = "temp",
                                                                               "Wind Speed" = "wind"
                                                                           ),
                                                                           options = pickerOptions(actionsBox = TRUE, dropupAuto = TRUE)
                                                               )
                                              ),
                                              # Boxplot
                                              conditionalPanel(condition = "input.plot_type == 'boxplot'",
                                                               # Discrete Variable
                                                               pickerInput(inputId = "boxplot_discrete_variables",
                                                                           label = "Please select discrete variable for boxplot",
                                                                           choices = list(
                                                                               "Season" = "season",
                                                                               "Game Type" = "game_type",
                                                                               "Weekday" = "weekday",
                                                                               "Away Team" = "away_team",
                                                                               "Home Team" = "home_team",
                                                                               "Overtime" = "overtime",
                                                                               "Division Game" = "div_game",
                                                                               "Roof" = "roof",
                                                                               "Surface" = "surface"
                                                                           ),
                                                               ),
                                                               # Continuous Variable
                                                               pickerInput(inputId = "boxplot_continuous_variable",
                                                                           label = "Please select continuous variable for boxplot",
                                                                           choices = list(
                                                                               "Point Total" = "total",
                                                                               "Total Line" = "total_line",
                                                                               "Point Differential" = "result",
                                                                               "Spread Line" = "spread_line",
                                                                               "Temperature" = "temp",
                                                                               "Wind Speed" = "wind"
                                                                           ),
                                                               )
                                              ), # end boxplot
                                              # Scatter plot
                                              conditionalPanel(condition = "input.plot_type == 'scatter'",
                                                               pickerInput(inputId = "scatter_variable1",
                                                                           label = "Please select x variable",
                                                                           choices = list(
                                                                               "Point Total" = "total",
                                                                               "Total Line" = "total_line",
                                                                               "Point Differential" = "result",
                                                                               "Spread Line" = "spread_line",
                                                                               "Temperature" = "temp",
                                                                               "Wind Speed" = "wind"
                                                                           ),
                                                                           selected = "total"
                                                               )
                                              ),
                                              conditionalPanel(condition = "input.plot_type == 'scatter'",
                                                               pickerInput(inputId = "scatter_variable2",
                                                                           label = "Please select y variable",
                                                                           choices = list(
                                                                               "Point Total" = "total",
                                                                               "Total Line" = "total_line",
                                                                               "Point Differential" = "result",
                                                                               "Spread Line" = "spread_line",
                                                                               "Temperature" = "temp",
                                                                               "Wind Speed" = "wind"
                                                                           ),
                                                                           selected = "total_line"
                                                               )
                                              ),
                                              # Visual filter
                                              h6("Filter by row"),
                                              switchInput(inputId = "switch_visual_filter",
                                                          onStatus = "success",
                                                          offStatus = "danger",
                                                          value = FALSE
                                              ),
                                              # Visual filter variable
                                              conditionalPanel(condition = "input.switch_visual_filter == 1",
                                                               uiOutput("visual_filter_variable"),
                                              ),
                                              conditionalPanel(condition = "input.switch_visual_filter == 1",
                                                               uiOutput("visual_filter_rows")
                                              )
                                          ) # end visual input box
                                   ),
                                   column(width = 8,
                                          box(title = strong("Visuals"), width = 12, status = "gray-dark", solidHeader = TRUE, collapsible = FALSE, closable = FALSE, elevation = 3,
                                              plotlyOutput("visual_output"),
                                              div("Scroll over plot and click ", icon("camera"),  "to download graph")
                                          )
                                   )
                               )
                           )
                ),
                # ========== Modeling Tab ===============
                bs4TabItem(tabName = "model_info",
                           fluidPage(
                               h1("Modeling Info"),
                               hr()
                           )
                ),
                bs4TabItem(tabName = "model_fit",
                           fluidPage(
                               h1("Model Fit"),
                               hr(),
                               fluidRow(
                                   column(width = 4,
                                          box(title = strong("Model Settings"), width = 12, status = "warning", solidHeader = TRUE, collapsible = FALSE, closable = FALSE, elevation = 3,
                                              sliderTextInput(inputId = "data_split",
                                                              label = "Select proportion of data for training data set",
                                                              choices = seq(0, 1, by = 0.05),
                                                              selected = .8,
                                                              from_min = .5,
                                                              from_max = .95
                                              ),
                                              br(),
                                              pickerInput(inputId = "model_variables",
                                                          label = "Please select predictor variables",
                                                          choices = list(
                                                              "Season" = "season",
                                                              "Game Type" = "game_type",
                                                              "Weekday" = "weekday",
                                                              "Team" = "away_team",
                                                              "Overtime" = "overtime",
                                                              "Team Rest" = "away_rest",
                                                              "Spread Line" = "spread_line",
                                                              "Total Line" = "total_line",
                                                              "Total Odds" = "under_odds",
                                                              "Divsion Game" = "div_game",
                                                              "Stadium Type" = "roof",
                                                              "Field Surface" = "surface",
                                                              "Temperature" = "temp",
                                                              "Wind" = "wind"
                                                          ),
                                                          multiple = TRUE,
                                                          options = pickerOptions(actionsBox = TRUE, dropupAuto = TRUE)
                                              ),
                                              br(),
                                              sliderInput(inputId = "cross_validation_folds",
                                                          label = "Number of folds for cross validation to select model",
                                                          min = 3,
                                                          max = 10,
                                                          value = 5
                                              ),
                                              hr(),
                                              actionBttn(inputId = "fit_models",
                                                         label = "Fit models"
                                              ),
                                              br(),
                                              h6("Model fit may take a few minutes")
                                          )
                                   ),
                                   column(width = 8,
                                          box(title = strong("Model Statistics"), width = 12, status = "warning", solidHeader = TRUE, collapsible = FALSE, closable = FALSE, elevation = 3,
                                              h3("Training Data Fit Statistics"),
                                              DT::dataTableOutput("model_statistics"),
                                              br(),
                                              h3("Test Data Fit Statistics"),
                                              DT::dataTableOutput("test_statistics")
                                          )
                                   )
                               )
                           )
                ),
                # ========= Prediction Tab ===============
                bs4TabItem(tabName = "prediction",
                           fluidPage(
                               h1("Prediction"),
                               hr(),
                               fluidRow(
                                   column(width = 4,
                                          box(title = strong("Prediction Inputs"), width = 12, status = "warning", solidHeader = TRUE, collapsible = FALSE, closable = FALSE, elevation = 3,
                                              radioGroupButtons(inputId = "prediction_model",
                                                                label = "Please select model for prediction",
                                                                choices = list(
                                                                    "Linear Regression" = "linear_regression",
                                                                    "Regression Tree" = "regression_tree",
                                                                    "Random Forest" = "random_forest"
                                                                ),
                                                                checkIcon = list(
                                                                    yes = icon("ok", lib = "glyphicon")
                                                                ),
                                                                direction = "vertical"
                                              ),
                                              br(),
                                              uiOutput("season_predictor"),
                                              uiOutput("game_type_predictor"),
                                              uiOutput("weekday_predictor"),
                                              uiOutput("team_predictor"),
                                              uiOutput("overtime_predictor"),
                                              uiOutput("rest_predictor"),
                                              uiOutput("spread_line_predictor"),
                                              uiOutput("total_line_predictor"),
                                              uiOutput("odds_predictor"),
                                              uiOutput("div_game_predictor"),
                                              uiOutput("roof_predictor"),
                                              uiOutput("surface_predictor"),
                                              uiOutput("temp_predictor"),
                                              uiOutput("wind_predictor"),
                                              hr(),
                                              actionBttn(inputId = "run_prediction",
                                                         label = "Generate prediction"
                                              )
                                          )
                                   ),
                                   column(width = 8,
                                          box(title = strong("Prediction Output"), width = 12, status = "warning", solidHeader = TRUE, collapsible = FALSE, closable = FALSE, elevation = 3,
                                              textOutput("prediction_value")
                                          )
                                   )
                               )
                           )
                )
            ) # end bs4Tabitems
        ) # end bs4DashBody
    ) # bs4dashPage
) # end ShinyUI
