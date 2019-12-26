library(ggplot2)
library(tidyr)

get_living_loc <- function() {
  return(c(1:40, 61:80, 101:120))  
}

get_non_living_loc <- function() {
  return(c(41:60, 81:100, 121:160))
}

get_all_loc <- function() {
  return(1:160)
}

living_labels <- c("banana", "cherry", "flower", "pear")
# 1.1


#read.csv(config::get())
data <- read.csv("40153557_features.csv")

# classify: nonliving = 0, living = 1
data$dummy.verticalness <- 0
data$dummy.verticalness[data$label %in% living_labels] <- 1

# Make training and test data sets. 80% training, 20% test.

# randomly shuffle rows:
data_shuffled = data[sample(nrow(data)),]

# see data on histogram
# COULD UPDATE WITH A BETTER LEGEND :)
veticalness_histogram <- ggplot(data_shuffled, aes(x = verticalness, fill = as.factor(dummy.verticalness))) +
  geom_histogram(binwidth = 0.05, alpha = 0.5, position = 'identity') +
  ggtitle("Verticalness for Living and Non-living Images") +
  labs(fill=c("Is living"))

print(veticalness_histogram)

# perform linear regression
glmfit <- glm(dummy.verticalness ~ verticalness, 
              data = data_shuffled,
              family = 'binomial')
print("1.1: Linear regression for verticalness:")
print(summary(glmfit))

# create fitted line
values <- seq(min(data_shuffled$verticalness), max(data_shuffled$verticalness), length.out = 1000)
fitted.curve <- data.frame(verticalness = values)
fitted.curve[["dummy.verticalness"]] = predict(glmfit, fitted.curve, type="response")

# plot logistic regression
verticalness_plot <- ggplot(training_data, aes(x=verticalness, y=dummy.verticalness)) + 
  geom_point(aes(colour = factor(dummy.verticalness)), 
             show.legend = T, position="dodge") +
  geom_line(data=fitted.curve, colour="orange", size=1) + 
  ggtitle("Linear regression for Living and Non-living Images Based On Vertical") +
  labs(fill=c("Is living")) 

print(verticalness_plot)
  

