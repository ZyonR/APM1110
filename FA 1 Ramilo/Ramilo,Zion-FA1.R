gender <- c('m', 'm', 'm', 'm', 'm', 'm', 'm', 'f', 'm', 'm', 'm', 'm', 'f', 'm', 'm', 'm', 'm', 'm', 'm', 'm', 'm', 'm', 'm', 'm', 'f', 'f', 'm', 'm', 'm', 'm', 'm', 'm', 'm', 'm', 'f', 'm', 'm', 'm', 'm', 'f', 'm', 'm', 'm', 'm', 'm', 'm', 'm', 'm', 'f', 'm', 'm', 'm', 'm', 'm', 'm', 'f', 'm', 'm', 'm', 'm', 'f', 'm', 'm', 'f', 'm', 'm', 'm', 'm', 'm', 'm', 'm', 'm', 'm', 'm', 'm', 'm', 'm', 'm', 'm', 'm', 'f', 'm', 'm', 'm', 'f', 'm', 'm', 'm', 'm', 'f', 'm', 'm', 'm', 'f', 'm', 'm', 'f', 'm', 'm', 'm', 'f', 'm', 'm', 'f', 'm', 'm', 'm', 'f', 'm', 'm', 'm', 'f', 'm', 'm', 'm', 'm', 'm', 'm', 'm')
arch1 <- c(99, NA, 97, 99, 89, 91, 100, 86, 89, 85, 50, 96, 98, 96, 73, 67, 80, 91, 89, 77, 71, 84, 95, 3, 95, NA, 59, 95, 80, 97, 81, 77, 69, 82, 85, 87, 88, 83, 51, 76, 88, 61, 83, 90, 40, 92, 76, 72, 77, 58, 63, 48, 40, 40, 75, 49, 54, 56, 75, 64, 88, 82, 73, 59, 74, 45, 70, 74, 43, 49, 45, 74, 46, 56, 16, 21, 47, 77, 27, 74, 16, 14, 23, 83, NA, 45, 40, 48, 91, 50, 77, 49, 96, 21, 61, 50, 68, 50, 69, 60, 43, 43, 47, 60, 40, 45, 45, 31, 49, 87, 40, 8, 62, 14, 7, 16, 73, 56, 46)
prog1 <- c(98, NA, 97, 97, 92, 97, 88, 82, 88, 90, 91, 71, 80, 76, 72, 82, 85, 76, 81, 81, 82, 81, 83, 87, 65, NA, 79, 83, 80, 92, 89, 70, 74, 79, 66, 68, 76, 76, 67, 63, 64, 53, 60, 78, 67, 61, 69, 61, 53, 52, 62, 73, 75, 40, 67, 61, 47, 55, 40, 86, 40, 66, 64, 28, 57, 69, 52, 29, 25, 69, 29, 71, 56, 52, 33, 25, 56, 60, 40, 13, 14, 31, 54, 76, 15, 40, 28, 27, 89, 27, 82, 49, 84, 29, 40, 19, 74, 40, 59, 36, 14, 30, 68, 47, 68, 26, 31, 21, 12, 40, 76, 29, 46, 21, 25, 27, 51, 54, 64)
arch2 <- c(83, 86, 92, 95, 86, 91, 96, 89, 65, 83, 84, 56, 81, 59, 91, 80, 94, 85, 77, 88, 59, 88, 92, 56, 63, 91, 73, 49, 87, 98, 41, 51, 83, 57, 56, 56, 47, 41, 49, 57, 48, 54, 56, 81, 53, 47, 44, 62, 48, 50, 40, 74, 43, 48, 40, 49, 43, 44, 40, 50, 43, 51, 28, 60, 45, 35, 40, 44, 31, 40, 32, 40, 50, 42, 16, 26, 43, 47, 37, 40, NA, 14, 48, 58, 16, 40, 26, 23, 6, 22, 45, 36, 48, 25, 34, 41, 30, 51, 25, 40, NA, 40, 43, 40, 57, 38, NA, 32, 24, 40, 49, 15, 50, NA, 27, 25, 48, 49, 13)
prog2 <- c(94, 77, 93, 96, 94, 97, 85, 87, 84, 85, 93, 83, 94, 84, 87, 77, 72, 84, 81, 91, 79, 77, 63, 76, 82, 65, 82, 69, 72, 96, 57, 71, 68, 45, 67, 78, 61, 65, 79, 76, 53, 61, 49, 50, 68, 64, 59, 56, 60, 73, 48, 53, 52, 62, 45, 44, 52, 55, 51, 81, 83, 63, 54, 51, 61, 40, 43, 52, 14, 24, 25, 46, 28, 57, 9, 12, 16, 62, 6, 18, NA, 20, NA, 75, NA, 61, 9, 16, 73, 11, 65, 31, 29, 5, 11, NA, 48, 56, 40, 28, NA, 14, 34, NA, 75, 6, NA, 8, 14, 32, 17, 14, 31, NA, 7, 7, 23, 25, 19)

df <- data.frame(gender,arch1,prog1,arch2,prog2)
View(df)
library(psych)
describe(df)


for (i in 2:5){
  col_values <- na.exclude(df[[i]])
  col_mean <- mean(col_values)
  col_median <- median(col_values)
  std_dev <- describe(col_values)$sd
  
  difference_arr <- c()
  for (val in col_values){
    ans <- (val - col_mean)^3
    difference_arr <- append(difference_arr,ans)
  }
  
  pearson_approx = 3*((col_mean - col_median) / std_dev)
  
  summation_x_xi = sum(difference_arr)
  size_arr = length(col_values)
  
  
  skewness_ = summation_x_xi / ((size_arr - 1) * (std_dev^3))
  cat("Subject:",colnames(df)[i],", Skewness: ",skewness_,", Pearson: ",pearson_approx,", Difference (pearson-skewness): ",abs(skewness_-pearson_approx),"\n")
}


library(ggplot2)

figA <- ggplot(df, aes(x = arch1))+
  geom_density(adjust = 2)+
  labs(title = 'Density Distribution of arch1')
figB <- ggplot(df, aes(x = prog1))+
  geom_density(adjust = 2)+
  labs(title = 'Density Distribution of prog1')

figC <-ggplot(df, aes(x = arch2))+
  geom_density(adjust = 2)+
  labs(title = 'Density Distribution of arch2')
figD <-ggplot(df, aes(x = prog2))+
  geom_density(adjust = 2)+
  labs(title = 'Density Distribution of prog2')


library(dplyr)

df %>%
  ggplot(aes(x = gender, y = arch1, fill = gender)) +
  geom_boxplot()

df %>%
  ggplot(aes(x = gender, y = prog1, fill = gender)) +
  geom_boxplot() 

df %>%
  ggplot(aes(x = gender, y = arch2, fill = gender)) +
  geom_boxplot()

df %>%
  ggplot(aes(x = gender, y = prog2, fill = gender)) +
  geom_boxplot()

#group_data <- df %>% 
 # group_by(gender) %>% 
  #group_split()

#female_data <- group_data[[1]]
#male_data <- group_data[[2]]

# female
#tidy(stem(female_data[[2]])) #arch1
#tidy(stem(female_data[[3]])) #prog1
#tidy(stem(female_data[[4]])) #arch2
#tidy(stem(female_data[[5]])) #prog2

# male
#tidy(stem(male_data[[2]])) #arch1
#tidy(stem(male_data[[3]])) #prog1
#tidy(stem(male_data[[4]])) #arch2
#tidy(stem(male_data[[5]])) #prog2

#_____________Part 2_________________________
gender_scores <- read.csv("C:/Users/acer/Downloads/Gender_Scores - Sheet1.csv")
gender_scores %>%
  ggplot(aes(x = gender, y = scores, fill = gender)) +
  geom_boxplot()

group_data <- gender_scores %>% 
 group_by(gender) %>% 
group_split()

female_data <- group_data[[1]]
male_data <- group_data[[2]]

tidy(stem(female_data[[2]]))
tidy(stem(male_data[[2]]))
