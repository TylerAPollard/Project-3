## ST558 Project 3 Server
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

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    # ========= Homepage / Navbar ==========
    observeEvent(input$about, {
        showModal(
            modalDialog(title = "About",
                        div(strong("Created by: "), "Tyler Pollard"),
                        div(strong("Version: "), "1.0"),
                        div(strong("Release Date: "), "27 July 2021"))
        )
    })
    
    # ========= Data Tab ===================
    game.df <- read.csv("../games.csv")
    game.df <- as.data.frame(game.df)
    game.df <- game.df %>% filter(season != 2021)
    game.df$game_id <- as.character(game.df$game_id)
    game.df$gameday <- as.character(game.df$gameday)
    game.df$season <- as_factor(game.df$season)
    levels(game.df$season) <- list("1999" = 1, "2000" = 2, "2001" = 3, "2002" = 4, "2003" = 5, "2004" = 6, "2005" = 7, "2006" = 8,
                                   "2007" = 9, "2008" = 10, "2009" = 11, "2010" = 12, "2011" = 13, "2012" = 14, "2013" = 15, "2014" = 16,
                                   "2015" = 17, "2016" = 18, "2017" = 19, "2018" = 20, "2019" = 21, "2020" = 22)
    game.df$game_type <- relevel(game.df$game_type, "REG", "WC", "DIV", "CON", "SB")
    game.df$gametime <- as.character(game.df$gametime)
    game.df$away_qb_id <- as.character(game.df$away_qb_id)
    game.df$home_qb_id <- as.character(game.df$home_qb_id)
    game.df$away_qb_name <- as.character(game.df$away_qb_name)
    game.df$home_qb_name <- as.character(game.df$home_qb_name)
    game.df$away_coach <- as.character(game.df$away_coach)
    game.df$home_coach <- as.character(game.df$home_coach)
    game.df$referee <- as.character(game.df$referee)
    game.df$stadium_id <- as.character(game.df$stadium_id)
    game.df$stadium <- as.character(game.df$stadium)
    game.df$div_game <- as_factor(game.df$div_game)
    game.df$overtime <- as_factor(game.df$overtime)
    levels(game.df$overtime) <- list("Yes" = 1, "No" = 0)
    game.df$div_game <- as_factor(game.df$div_game)
    levels(game.df$div_game) <- list("Yes" = 1, "No" = 0)
    game.df$temp[game.df$roof == "dome" | game.df$roof == "closed" | game.df$roof == "open"] <- 72
    game.df$wind[game.df$roof == "dome" | game.df$roof == "closed" | game.df$roof == "open"] <- 0
    game.df$roof <- droplevels(game.df$roof)
    
    # Column Filter UI
    output$column.filter <- renderUI({
        pickerInput(inputId = "data.column.filter",
                    label = "Select columns to filter by",
                    choices = attr(game.df, "names"),
                    multiple = TRUE,
                    options = pickerOptions(actionsBox = TRUE, title = "Make a selection")
        )
    })
    
    # Update pickerInput if column switchInput is changed
    observeEvent(input$switch_column_filter,{
        updatePickerInput(session, inputId = "data.column.filter", selected = character(0))
    })
    
    # Filter dataframe by column
    column.filter.df <- reactive({
        column.filters <- input$data.column.filter
        # No filters then return full dataframe
        if(input$switch_column_filter == 0 & is.null(input$data.column.filter)){
            return(game.df)
        }else{
            column.filter.df <- game.df %>% select(column.filters)
            return(column.filter.df)
        }
    })
    
    filter.df <- reactive({
        season.selected <- input$data.season.filter
        game.selected <- input$data.game.filter
        team.selected <- input$data.team.filter
        if(input$switch_season_filter == 0 & input$switch_game_filter == 0 & input$switch_team_filter == 0){
            filter.df <- column.filter.df()
        }else if(input$switch_season_filter == 0 & input$switch_game_filter == 0 & input$switch_team_filter == 1){
            filter.df <- column.filter.df() %>% filter(away_team %in% team.selected | home_team %in% team.selected)
        }else if(input$switch_season_filter == 0 & input$switch_game_filter == 1 & input$switch_team_filter == 0){
            filter.df <- column.filter.df() %>% filter(game_type %in% game.selected)
        }else if(input$switch_season_filter == 0 & input$switch_game_filter == 1 & input$switch_team_filter == 1){
            filter.df <- column.filter.df() %>% filter(game_type %in% game.selected, away_team %in% team.selected | home_team %in% team.selected)
        }else if(input$switch_season_filter == 1 & input$switch_game_filter == 0 & input$switch_team_filter == 0){
            filter.df <- column.filter.df() %>% filter(season %in% season.selected)
        }else if(input$switch_season_filter == 1 & input$switch_game_filter == 0 & input$switch_team_filter == 1){
            filter.df <- column.filter.df() %>% filter(season %in% season.selected, away_team %in% team.selected | home_team %in% team.selected)
        }else if(input$switch_season_filter == 1 & input$switch_game_filter == 1 & input$switch_team_filter == 0){
            filter.df <- column.filter.df() %>% filter(season %in% season.selected, game_type %in% game.selected)
        }else if(input$switch_season_filter == 1 & input$switch_game_filter == 1 & input$switch_team_filter == 1){
            filter.df <- column.filter.df() %>% filter(season %in% season.selected, game_type %in% game.selected, away_team %in% team.selected | home_team %in% team.selected)
        }
        return(filter.df)
    })
    
    # Season Filter Ui
    output$season.filter <- renderUI({
        if(input$switch_column_filter == 0){
            pickerInput(inputId = "data.season.filter",
                        label = "Select seasons to filter by",
                        choices = levels(column.filter.df()$season),
                        multiple = TRUE,
                        options = pickerOptions(actionsBox = TRUE, title = "Make a selection")
            )
        }else{
            validate(
                need(input$data.column.filter %in% "season", "Season is not selected")
            )
            pickerInput(inputId = "data.season.filter",
                        label = "Select seasons to filter by",
                        choices = levels(column.filter.df()$season),
                        multiple = TRUE,
                        options = pickerOptions(actionsBox = TRUE, title = "Make a selection")
            )
        }
    })
    
    # Update pickerInput if season switchInput is changed
    # observeEvent(input$switch_season_filter,{
    #     updatePickerInput(session, inputId = "data.season.filter", selected = character(0))
    # })
    
    # Game Filter Ui
    output$game.filter <- renderUI({
        if(input$switch_column_filter == 0){
            pickerInput(inputId = "data.game.filter",
                        label = "Select game type to filter by",
                        choices = levels(column.filter.df()$game_type),
                        multiple = TRUE,
                        options = pickerOptions(actionsBox = TRUE, title = "Make a selection")
            )
        }else{
            validate(
                need(input$data.column.filter %in% "game_type", "Game Type is not selected")
            )
            pickerInput(inputId = "data.game.filter",
                        label = "Select game type to filter by",
                        choices = levels(column.filter.df()$game_type),
                        multiple = TRUE,
                        options = pickerOptions(actionsBox = TRUE, title = "Make a selection")
            )
        }
    })
    
    # Update pickerInput if season switchInput is changed
    # observeEvent(input$switch_game_filter,{
    #     updatePickerInput(session, inputId = "data.game.filter", selected = character(0))
    # })
    
    # Team Filter Ui
    output$team.filter <- renderUI({
        if(input$switch_column_filter == 0){
            pickerInput(inputId = "data.team.filter",
                        label = "Select teams to filter by",
                        choices = levels(column.filter.df()$away_team),
                        multiple = TRUE,
                        options = pickerOptions(actionsBox = TRUE, title = "Make a selection")
            )
        }else{
            validate(
                need("away_team" %in% input$data.column.filter | "home_team" %in% input$data.column.filter, "Neither team column is not selected")
            )
            if("away_team" %in% input$data.column.filter){
                pickerInput(inputId = "data.team.filter",
                            label = "Select teams to filter by",
                            choices = levels(column.filter.df()$away_team),
                            multiple = TRUE,
                            options = pickerOptions(actionsBox = TRUE, title = "Make a selection")
                )
            }else{
                pickerInput(inputId = "data.team.filter",
                            label = "Select teams to filter by",
                            choices = levels(column.filter.df()$home_team),
                            multiple = TRUE,
                            options = pickerOptions(actionsBox = TRUE, title = "Make a selection")
                )
            }
        }
    })
    
    # Update pickerInput if season switchInput is changed
    # observeEvent(input$switch_team_filter,{
    #     updatePickerInput(session, inputId = "data.team.filter", selected = character(0))
    # })
    
    # Output the data table to scroll through
    output$data.table <- DT::renderDataTable(options = list(scrollX = TRUE),{
        # Check if column filter is selected with no column filters selected 
        if(input$switch_column_filter == 1){
            validate(
                need(input$data.column.filter, "Please select columns to filter by")
            )
            return(filter.df())
        }else{
            return(filter.df())
        }
    })
    
    observeEvent(input$save.data, {
        # Check for data file title and if blank save as NFL Games Data.csv
        if(input$data.title == ""){
            file.title <- "NFL Games Data"
        }else{
            file.title <- input$data.title
        }
        write_csv(column.filter.df(), file = paste0(file.title, ".csv"))
        withProgress(message = "Saving data...", value = 0,{
            for(n in 1:10){
                incProgress(1/10)
                Sys.sleep(0.1)
            }
        })
    })
    
    # ============ Data Exploration Tab ======================
    ## Contigency Tables
    output$contingency.table <- renderTable(options = list(scrollX = TRUE),{
        validate(
            need(input$contingency_variable1, "Please select variable for contingency table")
        )
        if(input$switch_contingency_filter){
            contingency.table <- table(game.df[[input$contingency_variable1]], game.df[[input$contingency_variable2]])
            contingency.table <- as.data.frame(contingency.table)
            colnames(contingency.table) <- c(input$contingency_variable1, input$contingency_variable2, "Frequency")
            contingency.table <- contingency.table %>% spread(key = input$contingency_variable2, value = Frequency)
        }else{
            # if(input$contingency_variable1 == "away_team"){
            #     contingency.table <- table(game.df[["away_team"]], game.df[["home_team"]])
            #     contingency.table <- as.data.frame(contingency.table)
            #     colnames(contingency.table) <- c(input$contingency_variable1, "Frequency")
            # }else{
            #     contingency.table <- table(game.df[[input$contingency_variable1]])
            #     contingency.table <- as.data.frame(contingency.table)
            #     colnames(contingency.table) <- c(input$contingency_variable1, "Frequency")
            # }
            contingency.table <- table(game.df[[input$contingency_variable1]])
            contingency.table <- as.data.frame(contingency.table)
            colnames(contingency.table) <- c(input$contingency_variable1, "Frequency")
        }
        contingency.table
    })
    
    ## Summary Tables
    # Filter summary data
    output$summary.data <- renderUI({
        summary.df <- game.df[[input$summary_variable_filter]]
        pickerInput(inputId = "summary_variable_rows",
                    label = "Select rows to filter by",
                    choices = levels(summary.df),
                    multiple = TRUE,
                    options = pickerOptions(actionsBox = TRUE, dropupAuto = TRUE)
                    )
    })
    
    # Summary table
    output$summary.table <- renderTable(rownames = TRUE,{
        validate(
            need(input$summary.variable, "Please select variable to summarize")
        )
        if(input$switch_summary_filter){
            validate(
                need(input$summary_variable_rows, "Please select row to filter by")
            )
            if(input$summary_variable_filter == "away_team"){
                summary_variable_filter <- c("away_team", "home_team")
                summary.variable <- input$summary.variable
                summary_variable_rows <- input$summary_variable_rows
                summary.df <- game.df[c(summary_variable_filter, summary.variable)]
                summary.df <- summary.df[summary.df[[summary_variable_filter[1]]] %in% summary_variable_rows | summary.df[[summary_variable_filter[2]]] %in% summary_variable_rows, ]
                summary.df <- summary.df[-c(1,2)]
            }else{
                summary.df <- game.df[c(input$summary_variable_filter, input$summary.variable)]
                summary.df <- summary.df[summary.df[[input$summary_variable_filter]] %in% input$summary_variable_rows, ]
                summary.df <- summary.df[-1]
            }
        }else{
            summary.df <- game.df[c(input$summary.variable)]
        }
        summary.table <- data.frame()
        for(i in colnames(summary.df)){
            sum.table <- summarise(summary.df[i],
                Minimum = min(summary.df[[i]], na.rm = TRUE),
                `1st Qu.` = quantile(summary.df[[i]], 0.25, na.rm = TRUE),
                Median = median(summary.df[[i]], na.rm = TRUE),
                Mean = mean(summary.df[[i]], na.rm = TRUE),
                `3rd Qu.` = quantile(summary.df[[i]], 0.75, na.rm = TRUE),
                Maximum = max(summary.df[[i]],na.rm = TRUE),
                `St. Dev.` = sd(summary.df[[i]],na.rm = TRUE)
            )
            rownames(sum.table) <- i
            summary.table <- rbind(summary.table, sum.table)
        }
        
        summary.table
    }) # end summary table
    
    # Visual Outputs
    # Visual Filter
    output$visual_filter_variable <- renderUI({
        if(input$plot_type == "bar"){
            pickerInput(inputId = "bar_filter_variable",
                        label = "Please select rows to filter by",
                        choices = levels(game.df[[input$bar_variable]]),
                        multiple = TRUE,
                        options = pickerOptions(actionsBox = TRUE, dropupAuto = TRUE)
            )
        }
    }) 
    # Visual plot
    output$visual_output <- renderPlotly({
        if(input$plot_type == "bar"){
            bar.df <- game.df
            if(input$switch_visual_filter){
                validate(
                    need(input$bar_filter_variable, "Please select rows to filter by")
                )
                bar_nrow <- c()
                bar.df <- bar.df[bar.df[[input$bar_variable]] %in% input$bar_filter_variable, ]
                # bar_unique <- as.character(unique(bar.df[[input$bar_variable]]))
                # bar.df[[input$bar_variable]] <- unclass(bar.df[[input$bar_variable]])
                # bar.df[[input$bar_variable]] <- as_factor(bar.df[[input$bar_variable]])
                # levels(bar.df[[input$bar_variable]]) <- bar_unique
                for(j in levels(bar.df[[input$bar_variable]])){
                    nrow_var <- nrow(bar.df[bar.df[[input$bar_variable]] == j, ])
                    bar_nrow <- c(bar_nrow, nrow_var)
                }
                plot_ly(x = levels(bar.df[[input$bar_variable]]), y = bar_nrow, type = "bar") %>%
                    layout(xaxis = list(title = input$bar_variable), yaxis = list(title = "Number of Games"))
            }else{
                bar_nrow <- c()
                for(j in levels(bar.df[[input$bar_variable]])){
                    nrow_var <- nrow(bar.df[bar.df[[input$bar_variable]] == j, ])
                    bar_nrow <- c(bar_nrow, nrow_var)
                }
                plot_ly(x = levels(bar.df[[input$bar_variable]]), y = bar_nrow, type = "bar") %>%
                    layout(xaxis = list(title = input$bar_variable), yaxis = list(title = "Number of Games"))
            }
        }else if(input$plot_type == "histogram"){
            hist.df <- game.df
            if(input$visual_filter){
                
            }else{
                plot_ly(data = hist.df, x = hist.df[input$histogram_variables], type = "histogram")
            }
        }else if(input$plot_type == "boxplot"){
            if(input$visual_filter){
                
            }else{
                
            }
        }else{
            if(input$visual_filter){
                
            }else{
                
            }
        }
    }) # end visual filter
})
