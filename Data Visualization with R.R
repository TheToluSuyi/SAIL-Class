library(wordcloud2)
install.packages("wordcloud2")
covid_data
oruko <- unique(covid_data$states)
oruko
covid_data2 <- covid_data %>%
  subset(states %in% oruko[11:20])
covid_data2
qplot(total_death, total_confirmed, data=covid_data2, facets=.~states,
      geom = c("point", "smooth"))
?qplot
qplot(total_death, total_confirmed, data=covid_data2, facets=.~states,
      geom = c("point", "smooth"), method = "lm")
g <- ggplot(covid_data2, aes(total_death, total_confirmed))
summary(g)
#layer(point)
g + geom_point()
g + geom_point() + geom_smooth()
g + geom_point() + geom_smooth(method = "lm")
g + geom_point()  + facet_grid(. ~ states) + geom_smooth(method = "lm") 
g + geom_point(color = "steelblue", size = 4, alpha = 1/2)
g + geom_point(aes(color = states), size = 4, alpha = 1/2)
###########
covid_data2 <- covid_data %>% 
  subset(states %in% oruko[11:20])

g <- ggplot(covid_data2, aes(total_death, total_confirmed))

g + geom_point(aes(color = states), size = 4, alpha = 1/2) + 
  labs(title = "Selected States Covid Death & Cases") + 
  labs(x = expression("Number of Death"), y = "Total Confirmed Cases")
g + geom_point(aes(color = states), size = 4, alpha = 1/2) +
  geom_smooth(size = 4, linetype = 3, method = "lm", se = FALSE) +
  labs(title = "Selected Stated Covid Death & Cases") + 
  labs(x = expression("Number of Death"), y = "Total Confirmed Cases")
g + geom_point(aes(color = states), size = 4, alpha = 1/2) +
  theme_bw(base_family = "Times") +
  geom_smooth(size = 4, linetype = 3, method = "lm", se = FALSE) +
  labs(title = "Selected Stated Covid Death & Cases") + 
  labs(x = "Number of Death", y = "Total Confirmed Cases")
covid_data2 <- covid_data %>% 
  subset(states %in% oruko[11:20])

foma <- ggplot(covid_data2, aes(total_death, total_confirmed)) +
  geom_point(aes(color = states), size = 4, alpha = 1/2) +
  theme_bw(base_family = "Times") +
  geom_smooth(size = 4, linetype = 3, method = "lm", se = FALSE) +
  labs(title = "Selected Stated Covid Death & Cases") + 
  labs(x = "Number of Death", y = "Total Confirmed Cases")
foma_plotly <- ggplotly(foma)
foma_plotly
ggplotly(foma)
update.packages(ask = FALSE, checkBuilt = TRUE)
