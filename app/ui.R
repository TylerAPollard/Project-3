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
                           fluidPage(
                               h1("Data Exploration"),
                               hr(),
                               fluidRow(
                                   column(width = 4,
                                          box(title = strong("Summary Inputs"), width = 12, status = "warning", solidHeader = TRUE, collapsible = FALSE, closable = FALSE, elevation = 3,
                                              pickerInput(inputId = "summary.variable",
                                                          label = "Select variable to summarize",
                                                          choices = list(
                                                              "Point Total" = "total",
                                                              "Total Line" = "total_line",
                                                              "Point Differential" = "result",
                                                              "Spread Line" = "spread_line",
                                                              "Temperature" = "temp",
                                                              "Wind Speed" = "wind"),
                                                          multiple = TRUE
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
                                              tableOutput("summary.table")
                                          )
                                   )
                               ),
                               hr(),
                               fluidRow(
                                   column(width = 4,
                                          box(title = strong("Visual Inputs"), width = 12, status = "gray-dark", solidHeader = TRUE, collapsible = FALSE, closable = FALSE, elevation = 3
                                              
                                          )
                                   ),
                                   column(width = 8,
                                          box(title = strong("Visuals"), width = 12, status = "gray-dark", solidHeader = TRUE, collapsible = FALSE, closable = FALSE, elevation = 3
                                          )
                                   )
                               )
                           )
                ),
                bs4TabItem(tabName = "model_info",
                           fluidPage(
                               h1("Modeling Info")
                           )
                ),
                bs4TabItem(tabName = "model_fit",
                           fluidPage(
                               h1("Model Fit")
                           )
                ),
                bs4TabItem(tabName = "prediction",
                           fluidPage(
                               h1("Prediction")
                           )
                )
            ) # end bs4Tabitems
        ) # end bs4DashBody
    ) # bs4dashPage
) # end ShinyUI
