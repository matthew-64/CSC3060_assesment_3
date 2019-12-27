library(ggplot2)
library(tidyr)

set.seed(3060)

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



##################  1.1 #########################
print("##################  1.1 #########################")

#read.csv(config::get())
DATA <- read.csv("40153557_features.csv")

# classify: nonliving = 0, living = 1
#DOES THIS DATA NEED SHUFFLED? I DON'T THINK SO
data_shuffled <- DATA
data_shuffled$dummy.verticalness <- 0
data_shuffled$dummy.verticalness[data_shuffled$label %in% living_labels] <- 1

# randomly shuffle rows:
print(str(data_shuffled))

data_shuffled = data_shuffled[sample(nrow(data_shuffled)),]
print(str(data_shuffled))
# see data on histogram
# COULD UPDATE WITH A BETTER LEGEND :)
veticalness_histogram <- ggplot(data_shuffled, aes(x = verticalness, fill = as.factor(dummy.verticalness))) +
  geom_histogram(binwidth = 0.05, alpha = 0.5, position = 'identity') +
  ggtitle("Verticalness for Living and Non-living Images") +
  #scale_fill_continuous(name = "blah, breaks = c(0,1), labels = c("123", "sdf"))
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
verticalness_plot <- ggplot(data_shuffled, aes(x=verticalness, y=dummy.verticalness)) + 
  geom_point(aes(colour = factor(dummy.verticalness)), 
             show.legend = T, position="dodge") +
  geom_line(data=fitted.curve, colour="orange", size=1) + 
  ggtitle("Linear regression for Living and Non-living Images Based On Vertical") +
  ylab("Probability image is living") +
  labs(fill=c("Is living")) 

print(verticalness_plot)
  


##################  1.2 #########################
print("##################  1.2 #########################")


# change p in 0.01 intervals:
#df_names <- c("cut-off value", "amount correct")
#names(p_values_df) <- df_names
p_data <- NULL
correct_data <- NULL
for (i in 1:100) {
  p <- (i)/100
  training_data <- data_shuffled
  training_data[["predicted_verticalness"]] = predict(glmfit, training_data, type="response")
  training_data[["predicted_correct"]] = 0
  training_data[["predicted_correct"]][training_data[["predicted_verticalness"]] > p] = 1

  correct_items = training_data[["predicted_correct"]] == training_data[["dummy.verticalness"]] 
  
  # proportion correct:
  correct <- nrow(training_data[correct_items,])/nrow(training_data)
  
  # append data to existing data
  p_data <- append(p_data, p)
  correct_data <- append(correct_data, correct)
}
# create df from data
correctness_cutoff_df <- data.frame(cut_off = p_data, correctness = correct_data)

# find largest correctness and corresponding cut off value
max_correctness_pos <- 0
max_correctness <- 0
for (i in 1:nrow(blah)) {
  if (correctness_cutoff_df[i, "correctness"] > max_correctness) {
    max_correctness <- correctness_cutoff_df[i, "correctness"]
    max_correctness_pos <- i
  }
}
max_cutoff_value <- correctness_cutoff_df[max_correctness_pos, "cut_off"]
print(paste("1.2: max correctness = ", max_correctness, "at a cut-off value of ", max_cutoff_value))

# graph results
correctness_cutoff_graph <- ggplot(data = correctness_cutoff_df, aes(x = cut_off, y = correctness)) +
  geom_point() +
  geom_hline(yintercept = max_correctness, linetype="dotted", 
             color = "red", size=0.5) +
  geom_vline(xintercept = max_cutoff_value, linetype="dotted", 
             color = "red", size=0.5) +
  ggtitle("Correctness of Linear Regression With Different Cut Off Points") +
  theme(plot.title = element_text(hjust = 0.5))

print(correctness_cutoff_graph)






##################  1.3 #########################
print("##################  1.3 #########################")

# create data frame with living and non-living categories
catorgised_data <- DATA
catorgised_data$catagory <- "non-living"
catorgised_data$catagory[nr_pix_graph_data$label %in% living_labels] <- "living"


# nr_pix graph
nr_pix_graph <- ggplot(catorgised_data, aes(x = nr_pix, fill = as.factor(catagory))) +
  geom_density(adjust = 1.5, alpha = 0.5) + 
  ggtitle("Density plot of\nNumber of pixels for Living and Non-living Images") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill=c("Living status"))

print(nr_pix_graph)


# width graph
width_graph <- ggplot(catorgised_data, aes(x = width, fill = as.factor(catagory))) +
  geom_density(adjust = 1.2, alpha = 0.5) + 
  ggtitle("Density plot of\nWidth for Living and Non-living Images") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill=c("Living status"))

print(width_graph)


# span graph
span_graph <- ggplot(catorgised_data, aes(x = span, fill = as.factor(catagory))) +
  geom_density(adjust = 1.5, alpha = 0.5) + 
  ggtitle("Density plot of\nSpan for Living and Non-living Images") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill=c("Living status"))

print(span_graph)


# create fresh data set and shuffle
data_shuffled <- DATA
data_shuffled$living_status <- 0
data_shuffled$living_status[nr_pix_graph_data$label %in% living_labels] <- 1
data_shuffled = data_shuffled[sample(nrow(data_shuffled)),]

# build 3-way logistic regression model

# 0 = non-living, 1 = living
#glm_data <- DATA
#glm_data$living_status <- 0
#glm_data$living_status[nr_pix_graph_data$label %in% living_labels] <- 1

# nr_pix linear regression
glmfit_nr_pix <- glm(living_status ~ nr_pix, 
                     data = data_shuffled, 
                     family = 'binomial')
print("1.3: Linear regression for nr_pix:")
print(summary(glmfit_nr_pix))


# preform 5-fold crossvalidation
NUM_K_FOLDS <- 5


# split into number of folds (in this case it is 5)
random_sequence <- cut(1:160, breaks = NUM_K_FOLDS)
print(str(random_sequence))


