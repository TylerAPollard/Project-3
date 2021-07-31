# REad in Data
game.df <- read.csv("games.csv")
game.df <- as.data.frame(game.df)
game.df <- game.df %>% filter(season != 2021)
game.df$game_id <- as.character(game.df$game_id)
#game.df$weekday <- relevel(game.df$weekday, "Thursday", "Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday")
game.df$season <- as_factor(game.df$season)
levels(game.df$season) <- list("1999" = 1, "2000" = 2, "2001" = 3, "2002" = 4, "2003" = 5, "2004" = 6, "2005" = 7, "2006" = 8,
                               "2007" = 9, "2008" = 10, "2009" = 11, "2010" = 12, "2011" = 13, "2012" = 14, "2013" = 15, "2014" = 16,
                               "2015" = 17, "2016" = 18, "2017" = 19, "2018" = 20, "2019" = 21, "2020" = 22)
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
game.df$temp[game.df$roof == "dome" | game.df$roof == "closed" | game.df$roof == "open"] <- 72
game.df$wind[game.df$roof == "dome" | game.df$roof == "closed" | game.df$roof == "open"] <- 0
game.df$roof <- droplevels(game.df$roof)

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
ggplot(data = game.df, aes(x = total)) + geom_histogram()
