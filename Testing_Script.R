# summary.df <- game.df[c(input$summary_variable_filter,input$summary.variable)]
# summary.df <- summary.df[input$summary_variable_rows, ]
# summary.df <- summary.df[ , -1]
summary.df <- game.df[c(input$summary.variable)]
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