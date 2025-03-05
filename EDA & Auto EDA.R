iris
library(ggplot2)
set.seed(234)
ggplot(iris, aes(sample = Sepal.Length))+
  stat_qq()
install.packages("explore")
install.packages("wordcloud")
install.packages("SmartEDA")
install.packages("learnr")
install.packages("qdapRegex")
covid_data <- read.csv("C:/Users/Open User/Downloads/covid19_nigeria_states.csv")
covid_data %>% plot_histogram
covid_data %>% select(-states) %>% explore()
covid_data %>% describe_all()
covid_data %>%
     explore(
       target = total_death,
       var    = total_death,
       var2   = states
aipapers %>% explore()
covid_data %>%
     report(
       target      = total_death,
       output_dir  = "reports/",
       output_file = "covid_death_nigerian_states_plots.html"
     )
covid_data <- read.csv("C:/Users/Open User/Downloads/covid19_nigeria_states.csv"),
covid_data %>%
  report(
    target
  )
covid_data %>%
     create_report(
       output_file  = "covid_data_report",
       output_dir   = "reports/",
       y            = "total_death",
       report_title = "EDA Report - Covid Death in Nigerian States"
     )
##data distribution
covid_data <- read.csv("C:/Users/Open User/Downloads/covid19_nigeria_states.csv")
covid_data %>% introduce()
covid_data %>% plot_intro() 
covid_data %>% plot_missing()
covid_data %>% profile_missing()
covid_data %>% plot_density()
covid_data %>% explore()
covid_data %>% describe_all()
covid_data %>% describe_cat(states)        
covid_data %>%
  explore_all(
    target = total_death,
    ncol = 3
  )
covid_data %>%
  explore_all(
    target = total_death,
    var = total_death,
    var2 = states
  )
selectfoods <- read.
##############################
aipapers <- read.csv("C:/Users/Open User/Downloads/ai_sample_papers.csv")
aipap <- aipapers$abstract %>% 
  str_replace_all("[^A-Za-z]"," ") %>% 
  VectorSource() %>% 
  Corpus() %>% 
  tm_map(tolower) %>% 
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(stripWhitespace) 
set.seed(234)
w_cloud <- wordcloud(aipap, max.words = 250, 
           colors = brewer.pal(8, "Dark2"), 
          scale=c(5,1), random.order = FALSE)
dtm <- DocumentTermMatrix(aipap)
freq = colSums(as.matrix(dtm))
ord = order(freq, decreasing = TRUE)
plot = data.frame(words = names(freq), count = freq)
plot = subset(plot, plot$count > 150) 
plot <- plot[with(plot, order(-count)), ]
plot
aipapers %>% introduce()

aipapers %>% describe()

aipaperssample %>% plot_missing()