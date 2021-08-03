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

## The Different sections of code may be examined individually by condensing the chunk with the heart button on the left next to 
## the code line number

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
    
    output$image <- renderImage({
        filename <- normalizePath(file.path("nfl_logo.jpeg"))
        list(src = filename,
             width = 800,
             height = 400,
             align = "center")
    })
    
    # ========= Data Tab ===================
    game.df <- read.csv("games.csv")
    game.df <- as.data.frame(game.df)
    game.df$temp[game.df$roof == "dome" | game.df$roof == "closed" | game.df$roof == "open"] <- 72
    game.df$wind[game.df$roof == "dome" | game.df$roof == "closed" | game.df$roof == "open"] <- 0
    game.df <- game.df[complete.cases(game.df), ]
    game.df$roof <- droplevels(game.df$roof)
    game.df$game_id <- as.character(game.df$game_id)
    game.df$gameday <- as.character(game.df$gameday)
    game.df$season <- as_factor(game.df$season)
    # levels(game.df$season) <- list("1999" = 1, "2000" = 2, "2001" = 3, "2002" = 4, "2003" = 5, "2004" = 6, "2005" = 7, "2006" = 8,
    #                                "2007" = 9, "2008" = 10, "2009" = 11, "2010" = 12, "2011" = 13, "2012" = 14, "2013" = 15, "2014" = 16,
    #                                "2015" = 17, "2016" = 18, "2017" = 19, "2018" = 20, "2019" = 21, "2020" = 22)
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
    
    output$saved_data <- downloadHandler(
        # Check for data file title and if blank save as NFL Games Data.csv
        filename = function(){
            paste0("NFL Game Data", ".csv")
        },
        content = function(file){
            write_csv(filter.df(), file)
        }
    )
    
    
    # ============ Data Exploration Tab ======================
    ## Contigency Tables
    output$contingency.table <- renderTable({
        #extensions = "FixedColumns",
        #options = list(paging = FALSE, searching = FALSE, rownames = FALSE, scrollX = TRUE, autoWidth = TRUE, fixedColumns = list(leftColumns = 2), columnDefs = list(list(width = '100px', targets = "_all"))),{
        validate(
            need(input$contingency_variable1, "Please select variable for contingency table")
        )
        if(input$switch_contingency_filter){
            validate(
                need(input$contingency_variable1 != input$contingency_variable2, "Please select different variables for table")
            )
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
        }else if(input$plot_type == "histogram"){
            pickerInput(inputId = "histogram_filter_variable",
                        label = "Please select variable to filter by",
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
        }else if(input$plot_type == "boxplot"){
            pickerInput(inputId = "boxplot_filter_variable",
                        label = "Please select variable to filter by",
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
        }else if(input$plot_type == "scatter"){
            pickerInput(inputId = "scatter_filter_variable",
                        label = "Please select variable to filter by",
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
        }
    })
    
    output$visual_filter_rows <- renderUI({
        if(input$plot_type == "histogram"){
            pickerInput(inputId = "histogram_filter_rows",
                        label = "Please select rows to filter by",
                        choices = levels(game.df[[input$histogram_filter_variable]]),
                        multiple = TRUE,
                        options = pickerOptions(actionsBox = TRUE, dropupAuto = TRUE)
            )
        }else if(input$plot_type == "boxplot"){
            pickerInput(inputId = "boxplot_filter_rows",
                        label = "Please select rows to filter by",
                        choices = levels(game.df[[input$boxplot_filter_variable]]),
                        multiple = TRUE,
                        options = pickerOptions(actionsBox = TRUE, dropupAuto = TRUE)
            )
        }else if(input$plot_type == "scatter"){
            pickerInput(inputId = "scatter_filter_rows",
                        label = "Please select rows to filter by",
                        choices = levels(game.df[[input$scatter_filter_variable]]),
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
                #ggplot(data = bar.df, aes(x = bar.df[[input$bar_variable]])) + geom_bar()
                plot_ly(x = levels(bar.df[[input$bar_variable]]), y = bar_nrow, type = "bar") %>%
                    layout(xaxis = list(title = input$bar_variable), yaxis = list(title = "Number of Games"))
            }else{
                bar_nrow <- c()
                for(j in levels(bar.df[[input$bar_variable]])){
                    nrow_var <- nrow(bar.df[bar.df[[input$bar_variable]] == j, ])
                    bar_nrow <- c(bar_nrow, nrow_var)
                }
                #ggplot(data = bar.df, aes(x = bar.df[[input$bar_variable]])) + geom_bar()
                plot_ly(x = levels(bar.df[[input$bar_variable]]), y = bar_nrow, type = "bar") %>%
                    layout(xaxis = list(title = input$bar_variable), yaxis = list(title = "Number of Games")) 
            }
        }else if(input$plot_type == "histogram"){
            hist.df <- game.df
            if(input$switch_visual_filter){
                validate(
                    need(input$histogram_filter_rows, "Please select rows to filter by")
                )
                hist.df <- hist.df[hist.df[[input$histogram_filter_variable]] %in% input$histogram_filter_rows, ]
                plot_ly(data = hist.df, x = hist.df[[input$histogram_variables]], type = "histogram", nbinsx = 30) %>%
                    layout(xaxis = list(title = input$histogram_variables), yaxis = list(title = "Number of Games"))
            }else{
                #ggplot(data = hist.df, aes(x = hist.df[[input$histogram_variables]])) + geom_histogram()
                plot_ly(data = hist.df, x = hist.df[[input$histogram_variables]], type = "histogram", nbinsx = 30) %>%
                    layout(xaxis = list(title = input$histogram_variables), yaxis = list(title = "Number of Games"))
            }
        }else if(input$plot_type == "boxplot"){
            box.df <- game.df
            if(input$switch_visual_filter){
                validate(
                    need(input$boxplot_filter_rows, "Please select rows to filter by")
                )
                box.df <- box.df[box.df[[input$boxplot_filter_variable]] %in% input$boxplot_filter_rows, ]
                plot_ly(data = box.df, x = box.df[[input$boxplot_discrete_variables]], y = box.df[[input$boxplot_continuous_variable]], type = "box") %>%
                    layout(xaxis = list(title = input$boxplot_discrete_variables), yaxis = list(title = input$boxplot_continuous_variable))
            }else{
                plot_ly(data = box.df, x = box.df[[input$boxplot_discrete_variables]], y = box.df[[input$boxplot_continuous_variable]], type = "box") %>%
                    layout(xaxis = list(title = input$boxplot_discrete_variables), yaxis = list(title = input$boxplot_continuous_variable))
            }
        }else{
            scatter.df <- game.df
            if(input$switch_visual_filter){
                validate(
                    need(input$scatter_filter_rows, "Please select rows to filter by")
                )
                scatter.df <- scatter.df[scatter.df[[input$scatter_filter_variable]] %in% input$scatter_filter_rows, ]
                plot_ly(data = scatter.df, x = scatter.df[[input$scatter_variable1]], y = scatter.df[[input$scatter_variable2]], type = "scatter", mode = "markers",
                        text = paste("Home Team:", scatter.df[["home_team"]], '<br>', "Away Team:", scatter.df[["away_team"]], '<br>', input$scatter_variable1, "=", scatter.df[[input$scatter_variable1]], '<br>', input$scatter_variable2, "=", scatter.df[[input$scatter_variable2]])) %>%
                    layout(xaxis = list(title = input$scatter_variable1), yaxis = list(title = input$scatter_variable2))
            }else{
                # plot_ly(data = scatter.df, x = scatter.df[[input$scatter_variable1]], y = scatter.df[[input$scatter_variable2]], type = "scatter", mode = "markers",
                #         text = paste("Home Team:", scatter.df[["home_team"]], '<br>', "Away Team:", scatter.df[["away_team"]], '<br>', input$scatter_variable1, "=", scatter.df[[input$scatter_variable1]], '<br>', input$scatter_variable2, "=", scatter.df[[input$scatter_variable2]])) %>%
                #     layout(xaxis = list(title = input$scatter_variable1), yaxis = list(title = input$scatter_variable2))
                plot_ly(data = scatter.df, x = reformulate(input$scatter_variable1), y = reformulate(input$scatter_variable2), type = "scatter", mode = "markers",
                        text = paste("Home Team:", scatter.df[["home_team"]], '<br>', "Away Team:", scatter.df[["away_team"]], '<br>', input$scatter_variable1, "=", scatter.df[[input$scatter_variable1]], '<br>', input$scatter_variable2, "=", scatter.df[[input$scatter_variable2]])) %>%
                    layout(xaxis = list(title = input$scatter_variable1), yaxis = list(title = input$scatter_variable2))
            }
        }
    }) # end visual filter
    # ============= Model Info ===============
    
    # ============= Model Fitting ============
    data_split <- eventReactive(input$fit_models, {
        input$data_split
    })
    
    cross_validation_folds <- eventReactive(input$fit_models, {
        input$cross_validation_folds
    })
    
    model_variables <- eventReactive(input$fit_models, {
        input$model_variables
    })
    
    train_vec <- reactive({
        set.seed(52)
        sample(1:nrow(game.df), size = nrow(game.df)*data_split())
    })
    test_vec <- reactive({
        set.seed(52)
        dplyr::setdiff(1:nrow(game.df), train_vec())
    })
    game.df.train <- reactive({
        game.df[train_vec(), ]
    })
    game.df.test <- reactive({
        game.df[test_vec(), ]
    })
    
    lm_fit <- reactive({
        set.seed(52)
        train_control <- trainControl(method = "cv", number = cross_validation_folds())
        model_formula <- reformulate(termlabels = model_variables(), response = "total")
        lm_fit <- train(model_formula, data = game.df.train(),
                        method = "lm",
                        trControl = train_control)
        lm_fit
    })
    
    rt_fit <- reactive({
        set.seed(52)
        train_control <- trainControl(method = "cv", number = cross_validation_folds())
        model_formula <- reformulate(termlabels = model_variables(), response = "total")
        rt_fit <- train(model_formula, data = game.df.train(),
                        method = "rpart",
                        trControl = train_control)
        rt_fit
    })
    
    rf_fit <- reactive({
        set.seed(52)
        train_control <- trainControl(method = "cv", number = cross_validation_folds())
        model_formula <- reformulate(termlabels = model_variables(), response = "total")
        rf_fit <- train(model_formula, data = game.df.train(),
                        method = "rf",
                        trControl = train_control)
        rf_fit
    })
    
    output$model_statistics <- DT::renderDataTable(
        options = list(paging = FALSE, searching = FALSE),{
        validate(
            need(model_variables(), "Please select predictor variables")
        )
        
        # Fit linear model
        lm_results <- lm_fit()$results
        lm_RMSE <- round(lm_results[["RMSE"]], 4)
        lm_Rsquared <- round(lm_results[["Rsquared"]], 4)
        
        # Fit regression tree
        rt_results <- rt_fit()$results
        rt_RMSE <- round(rt_fit()$results[rt_fit()$results["cp"] == rt_fit()$finalModel$tuneValue[["cp"]], ][["RMSE"]] ,4)
        rt_Rsquared <- round(rt_fit()$results[rt_fit()$results["cp"] == rt_fit()$finalModel$tuneValue[["cp"]], ][["Rsquared"]], 4)
        
        # Fit random forest model
        rf_results <- rf_fit()$results
        rf_RMSE <- round(rf_fit()$results[rf_fit()$results["mtry"] == rf_fit()$finalModel$tuneValue[["mtry"]], ][["RMSE"]], 4)
        rf_Rsquared <- round(rf_fit()$results[rf_fit()$results["mtry"] == rf_fit()$finalModel$tuneValue[["mtry"]], ][["Rsquared"]], 4)
        
        model_statistics <- data.frame(
            RMSE = c(lm_RMSE, rt_RMSE, rf_RMSE), Rsquared = c(lm_Rsquared, rt_Rsquared, rf_Rsquared),
            row.names = c("Linear Regression", "Regression Tree", "Random Forest")
        )
        model_statistics
    })
    
    output$test_statistics <- DT::renderDataTable(
        options = list(paging = FALSE, searching = FALSE),{
        set.seed(52)
        lm_pred <- predict(lm_fit(), newdata = game.df.test())
        lm_test_stat <- postResample(lm_pred, game.df.test()$total)
        rt_pred <- predict(rt_fit(), newdata = game.df.test())
        rt_test_stat <- postResample(rt_pred, game.df.test()$total)
        rf_pred <- predict(rf_fit(), newdata = game.df.test())
        rf_test_stat <- postResample(rf_pred, game.df.test()$total)
        test_statistics <- data.frame(rbind(lm_test_stat, rt_test_stat, rf_test_stat), row.names = c("Linear Regression", "Regression Tree", "Random Forest"))
        round(test_statistics, 4)
    })
    
    # ============= Prediction ===============
    # Render UI outputs for predictor variables
    output$season_predictor <- renderUI({
        if("season" %in% model_variables()){
            pickerInput(inputId = "season_predictor",
                        label = "Please select season predictor",
                        choices = levels(game.df$season)
            )
        }
    })
    output$game_type_predictor <- renderUI({
        if("game_type" %in% model_variables()){
            pickerInput(inputId = "game_type_predictor",
                        label = "Please select game type predictor",
                        choices = levels(game.df$game_type)
            )
        }
    })
    output$weekday_predictor <- renderUI({
        if("weekday" %in% model_variables()){
            pickerInput(inputId = "weekday_predictor",
                        label = "Please select weekday predictor",
                        choices = levels(game.df$weekday)
            )
        }
    })
    output$team_predictor <- renderUI({
        if("away_team" %in% model_variables()){
            pickerInput(inputId = "team_predictor",
                        label = "Please select team predictor",
                        choices = levels(game.df$away_team)
            )
        }
    })
    output$overtime_predictor <- renderUI({
        if("overtime" %in% model_variables()){
            pickerInput(inputId = "overtime_predictor",
                        label = "Please select overtime predictor",
                        choices = levels(game.df$overtime)
            )
        }
    })
    output$rest_predictor <- renderUI({
        if("away_rest" %in% model_variables()){
            sliderInput(inputId = "rest_predictor",
                        label = "Please select rest predictor",
                        min = 4,
                        max = 17,
                        step = 1,
                        value = 7
            )
        }
    })
    output$spread_line_predictor <- renderUI({
        if("spread_line" %in% model_variables()){
            sliderInput(inputId = "spread_line_predictor",
                        label = "Please select spread line predictor",
                        min = -30,
                        max = 30,
                        step = 0.5,
                        value = 0
            )
        }
    })
    output$total_line_predictor <- renderUI({
        if("total_line" %in% model_variables()){
            numericInput(inputId = "total_line_predictor",
                        label = "Please select total line predictor",
                        min = 20,
                        max = 80,
                        step = 0.5,
                        value = 45
            )
        }
    })
    output$odds_predictor <- renderUI({
        if("under_odds" %in% model_variables()){
            sliderTextInput(inputId = "odds_predictor",
                        label = "Please select odds predictor",
                        choices = c(seq(-130, -100, by = 1), seq(100, 130, by = 1)),
                        selected = -100
            )
        }
    })
    output$div_game_predictor <- renderUI({
        if("div_game" %in% model_variables()){
            pickerInput(inputId = "div_game_predictor",
                        label = "Please select division predictor",
                        choices = levels(game.df$div_game)
            )
        }
    })
    output$roof_predictor <- renderUI({
        if("roof" %in% model_variables()){
            pickerInput(inputId = "roof_predictor",
                        label = "Please select roof predictor",
                        choices = levels(game.df$roof)
            )
        }
    })
    output$surface_predictor <- renderUI({
        if("surface" %in% model_variables()){
            pickerInput(inputId = "surface_predictor",
                        label = "Please select surface predictor",
                        choices = levels(game.df$surface)
            )
        }
    })
    output$temp_predictor <- renderUI({
        if("temp" %in% model_variables()){
            numericInput(inputId = "temp_predictor",
                         label = "Please select temperature predictor",
                         min = -20,
                         max = 110,
                         step = 1,
                         value = 60
            )
        }
    })
    output$wind_predictor <- renderUI({
        if("wind" %in% model_variables()){
            numericInput(inputId = "wind_predictor",
                         label = "Please select wind speed predictor",
                         min = 0,
                         max = 80,
                         step = 1,
                         value = 6
            )
        }
    })
    
    # Prediction
    prediction.df <- eventReactive(input$run_prediction, {
        pred.df <- game.df[-(2:nrow(game.df)), ]
        pred.df$season <- input$season_predictor
        pred.df$game_type <- input$game_type_predictor
        pred.df$weekday <- input$weekday_predictor
        pred.df$away_team <- input$team_predictor
        pred.df$overtime <- input$overtime_predictor
        pred.df$away_rest <- input$rest_predictor
        pred.df$spread_line <- input$spread_line_predictor
        pred.df$total_line <- input$total_line_predictor
        pred.df$under_odds <- input$odds_predictor
        pred.df$div_game <- input$div_game_predictor
        pred.df$roof <- input$roof_predictor
        pred.df$surface <- input$surface_predictor
        pred.df$temp <- input$temp_predictor
        pred.df$wind <- input$wind_predictor
        pred.df
    })
    
    prediction_model <- eventReactive(input$run_prediction, {
        input$prediction_model
    })
    
    output$prediction_value <- renderText({
        if(prediction_model() == "linear_regression"){
            prediction <- predict(lm_fit(), newdata = prediction.df())
        }else if(prediction_model() == "regression_tree"){
            prediction <- predict(rt_fit(), newdata = prediction.df())
        }else if(prediction_model() == "random_forest"){
            prediction <- predict(rf_fit(), newdata = prediction.df())
        }
        paste0("The predicted total points is ", prediction)
    })
})
