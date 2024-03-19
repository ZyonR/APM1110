library(tidyverse)

user_probability <- as.numeric(readline(prompt = "Input Probability: "))
geom_dist <- rgeom(10000,user_probability)
geom_dist_df <- data.frame("n_failures_before_success" = geom_dist) %>%
  group_by(n_failures_before_success) %>% 
    summarize("count" = n()/10000)
geom_dist_df_2 <- data.frame("n_failures_before_success" = geom_dist)
View(geom_dist)
weighted_mean <- mean(geom_dist)
weighted_variance <- var(geom_dist)

ggplot(geom_dist_df,mapping = aes(x=n_failures_before_success,y=count))+
  geom_line(col='red', size=0.5)+
  geom_point()+
  labs(x="# of Trials Before Keyword was found",
       y="Probability Density Function",
       title = "Generated 10,000 searches until Key Phrase is Found",
       subtitle = paste("Mean: ",round(weighted_mean,3)," Variance: ",round(weighted_variance,3)))

       