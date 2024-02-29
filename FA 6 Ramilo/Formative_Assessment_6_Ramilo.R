library(tidyverse)

geom_dist <- rgeom(1000,0.2)
geom_dist_df <- data.frame("n_failures_before_success" = geom_dist)
geom_dist_df_summary <- geom_dist_df %>% 
  summarize("Mean" = round(mean(n_failures_before_success),2),
            "Variance" = round(sd(n_failures_before_success),2),
            "Standard Diviation" = round(var(n_failures_before_success),2))
for(n_fail in geom_dist){
  if(n_fail == 0){
    match_ind <- which(geom_dist == 0)[1]
    break
  }
}
print(match_ind)
print(geom_dist_df_summary)
ggplot(data = geom_dist_df,mapping = aes(x=n_failures_before_success))+
  geom_histogram()+
    labs(x="Number of Failures Before Successes",
         y="Frequency",
         title = "Histogram of a Geometric Distribution",
         subtitle = "1000 Trials")

hypergeom_prob <- function(success,n_success,n,N){
  M <- success
  L <- N-M
  x <- n_success
  x_ <- n-x
  numerator <- choose(M,x) * choose(L,x_)
  dinominator <- choose(N,n)
  probability <- numerator / dinominator
  return(probability)
}
estimated_success_40 <- 40*0.1
estimated_success_5000 <- 5000*0.1
popu_40 <- 1-hypergeom_prob(estimated_success_40,1,10,40)
popu_5000 <- 1-hypergeom_prob(estimated_success_5000,1,10,5000)

sum_cumulitative <- c()
for (i in 2:10) {
  sum_cumulitative <- c(sum_cumulitative,hypergeom_prob(estimated_success_5000,i,10,5000))
}
print(sum(sum_cumulitative))
