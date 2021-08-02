# REad in Data
game.df <- read.csv("games.csv")
game.df$temp[game.df$roof == "dome" | game.df$roof == "closed" | game.df$roof == "open"] <- 72
game.df$wind[game.df$roof == "dome" | game.df$roof == "closed" | game.df$roof == "open"] <- 0
game.df <- game.df[complete.cases(game.df), ]
game.df <- as.data.frame(game.df)
game.df$game_id <- as.character(game.df$game_id)
game.df$season <- as_factor(game.df$season)
# levels(game.df$season) <- list("1999" = 1, "2000" = 2, "2001" = 3, "2002" = 4, "2003" = 5, "2004" = 6, "2005" = 7, "2006" = 8,
#                                "2007" = 9, "2008" = 10, "2009" = 11, "2010" = 12, "2011" = 13, "2012" = 14, "2013" = 15, "2014" = 16,
#                                "2015" = 17, "2016" = 18, "2017" = 19, "2018" = 20, "2019" = 21, "2020" = 22)
game.df$game_type <- relevel(game.df$game_type, "REG", "WC", "DIV", "CON", "SB")
game.df$gameday <- as.character(game.df$gameday)
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
# for(i in 1:nrow(game.df)){
#   if(game.df$total[i] > game.df$total_line[i]){
#     game.df$total_outcome[i] <- "Over"
#   }else{
#     game.df$total_outcome[i] <- "Under"
#   }
# }
# game.df$total_outcome <- as_factor(game.df$total_outcome)
# game.df.totals <- game.df %>% select(total, total_line, total_outcome)

# Contigency Table
switch_contingency_filter <- TRUE
contingency_variable2 <- "weekday"
contingency_variable1 <- "game_type"
if(switch_contingency_filter){
  contingency.table <- table(game.df[[contingency_variable1]], game.df[[contingency_variable2]])
}else{
  contingency.table <- table(game.df[[contingency_variable1]])
}
contingency.table
contingency.table <- as.data.frame(contingency.table)
colnames(contingency.table) <- c("Game Type", "Weekday", "Frequency")
contingency.table %>% spread(key = c(`Game Type`), value = `Frequency`)

# Summary Table
summary_variable_filter <- c("away_team")
summary.variable <- c("total", "total_line")
summary_variable_rows <- "BAL" #unique(game.df[[summary_variable_filter]])
if(summary_variable_filter == "away_team"){
  summary_variable_filter <- c(summary_variable_filter, "home_team")
  summary.df <- game.df[c(summary_variable_filter, summary.variable)]
  summary.df <- summary.df[summary.df[[summary_variable_filter[1]]] %in% summary_variable_rows |
                             summary.df[[summary_variable_filter[2]]] %in% summary_variable_rows, ]
  summary.df <- summary.df[-c(1,2)]
}else{
  summary.df <- game.df[c(summary_variable_filter, summary.variable)]
  summary.df <- summary.df[summary.df[[summary_variable_filter]] %in% summary_variable_rows, ]
  summary.df <- summary.df[-1]
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

# Visuals
library(plotly)
library(dplyr)
visual_filter <- FALSE
bar_variable <- "weekday"
if(visual_filter){
  
}else{
  bar_nrow <- c()
  for(j in levels(game.df[[bar_variable]])){
    nrow_var <- nrow(game.df[game.df[[bar_variable]] == j, ])
    bar_nrow <- c(bar_nrow, nrow_var)
  }
}

test <- game.df[game.df[[bar_variable]] %in% c("Saturday", "Friday", "Tuesday", "Sunday"), ]
levels(test[[bar_variable]])
test_un <- as.character(unique(test[[bar_variable]]))
test[[bar_variable]] <- unclass(test[[bar_variable]])
test[[bar_variable]] <- as_factor(test[[bar_variable]])
levels(test[[bar_variable]]) <- test_un
levels(test[[bar_variable]])

library(ggplot2)
library(plotly)
ggplot(data = game.df, aes(x = total)) + geom_histogram()
plot_ly(data = game.df, x = game.df[["total"]], type = "histogram")

plot_ly(data = game.df, x = game.df[["weekday"]], y = game.df[["total"]], type = "box")

# Modeling
library(caret)
set.seed(52)
train <- sample(1:nrow(game.df), size = nrow(game.df)*0.8)
test <- dplyr::setdiff(1:nrow(game.df), train)
game.df.train <- game.df[train, ]
game.df.test <- game.df[test, ]
train_control <- trainControl(method = "cv", number = 5)
lm_variables1 <- c("season", "game_type","weekday", "away_team", "home_team", "overtime", "away_rest", "spread_line", "total_line", "under_odds", "div_game", "roof", "surface", "temp", "wind")
lm_variables <- c("overtime", "total_line", "div_game", "temp", "wind")
model <- reformulate(termlabels = lm_variables1, response = "total")
step(lm(model, game.df.train), direction = "backward")
set.seed(52)
lm_fit <- train(model, data = game.df.train, 
            method = "lm",
            preProcess = c("center", "scale"),
            trControl = train_control
            )
lm_results <- lm_fit$results
lm_pred <- predict(lm_fit, newdata = game.df.test)
lm_test_stat <- postResample(lm_pred, game.df.test$total)

set.seed(52)
rt_fit <- train(model, data = game.df.train, 
                method = "rpart",
                preProcess = c("center", "scale"),
                trControl = train_control
)
rt_results <- rt_fit$results
rt_pred <- predict(rt_fit, newdata = game.df.test)
rt_test_stat <- postResample(rt_pred, game.df.test$total)

set.seed(52)
rf_fit <- train(model, data = game.df.train, 
                method = "rf",
                preProcess = c("center", "scale"),
                trControl = train_control
)
rf_results <- rf_fit$results
rf_pred <- predict(rf_fit, game.df.test)
rf_test_stat <- postResample(rf_pred, game.df.test$total)

model_statistics <- data.frame(
  RMSE = c(
    round(lm_fit$results[["RMSE"]],4),
    round(rt_fit$results[rt_fit$results["cp"] == rt_fit$finalModel$tuneValue[["cp"]], ][["RMSE"]], 4),
    round(rf_fit$results[rf_fit$results["mtry"] == rf_fit$finalModel$tuneValue[["mtry"]], ][["RMSE"]], 4)
  ),
  Rsquared = c(
    round(lm_fit$results[["Rsquared"]], 4),
    round(rt_fit$results[rt_fit$results["cp"] == rt_fit$finalModel$tuneValue[["cp"]], ][["Rsquared"]], 4),
    round(rf_fit$results[rf_fit$results["mtry"] == rf_fit$finalModel$tuneValue[["mtry"]], ][["Rsquared"]], 4)
  )
)
rownames(model_statistics) <- c("Linear Regression", "Regression Tree", "Random Forest")

test_statistics <- rbind(lm_test_stat, rt_test_stat, rf_test_stat)

# Prediction
pred.df <- game.df[-(1:nrow(game.df)), ]
pred.df$season[1] <- "2000"
pred.df$game_type[1] <- "REG"
pred.df$weekday[1] <- "Sunday"
pred.df$away_team[1] <- "BAL"
pred.df$overtime[1] <- "Yes"
pred.df$away_rest[1] <- 7
pred.df$spread_line[1] <- -4.5
pred.df$under_odds[1] <- -110
pred.df$div_game[1] <- "Yes"
pred.df$roof[1] <- "outdoors"
pred.df$surface[1] <- "grass"
pred.df$temp[1] <- 72
pred.df$wind[1] <- 5
