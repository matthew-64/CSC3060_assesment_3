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



print("##################  1.1 #########################")

#read.csv(config::get())
DATA <- read.csv("40153557_features.csv")

# classify: nonliving = 0, living = 1
#DOES THIS DATA NEED SHUFFLED? I DON'T THINK SO
data_shuffled <- DATA
data_shuffled$dummy.verticalness <- 0
data_shuffled$dummy.verticalness[data_shuffled$label %in% living_labels] <- 1

# randomly shuffle rows:
data_shuffled = data_shuffled[sample(nrow(data_shuffled)),]

# visualise data on histogram
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
  


print("##################  1.2 #########################")


# change cut-off (p) value in 0.01 intervals:
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
for (i in 1:nrow(correctness_cutoff_df)) {
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






print("##################  1.3 #########################")

# create data frame with living and non-living categories
#catorgised_data <- DATA
catorgised_data$catagory <- "non-living"
catorgised_data$catagory[catorgised_data$label %in% living_labels] <- "living"


# height graph
height_graph <- ggplot(catorgised_data, aes(x = height, fill = as.factor(catagory))) +
  geom_density(adjust = 1.5, alpha = 0.5) + 
  ggtitle("Density plot of:\nHeight for Living and Non-living Images") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill=c("Living status"))
print(height_graph)


# width graph
width_graph <- ggplot(catorgised_data, aes(x = width, fill = as.factor(catagory))) +
  geom_density(adjust = 1.2, alpha = 0.5) + 
  ggtitle("Density plot of:\nWidth for Living and Non-living Images") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill=c("Living status"))

print(width_graph)


# span graph
span_graph <- ggplot(catorgised_data, aes(x = height, fill = as.factor(catagory))) +
  geom_density(adjust = 1.5, alpha = 0.5) + 
  ggtitle("Density plot of:\nSpan for Living and Non-living Images") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill=c("Living status"))

print(span_graph)


# create fresh data set and shuffle
data_shuffled <- DATA
data_shuffled$living_status <- 0
data_shuffled$living_status[catorgised_data$label %in% living_labels] <- 1
data_shuffled = data_shuffled[sample(nrow(data_shuffled)),]

# build 3-way logistic regression model
glmfit_3_way <- glm(living_status ~ height + width + span, 
                    data = data_shuffled, 
                    family = 'binomial')
print("1.3: Linear regression for nr_pix, width and span:")
print(summary(glmfit_3_way))


# set up 5-fold crossvalidation
NUM_K_FOLDS <- 5

# split into number of folds (in this case it is 5)
data_shuffled$folds <- cut(seq(1:nrow(data_shuffled)), 
                           breaks=NUM_K_FOLDS,
                           labels=FALSE)

# preform 5-fold crossvalidation with different cutoff points to find
# the cut off value that provides the highest accuray
cut_off_values <- c() # to record cut off that gives highest correctness
correctness_values_for_each_cut_off <- c() # to record correctness for each cut off value
correctectness_values <- c() # to store correctness for each fold

#data_shuffled$predicted_correct = 0
for (x in 1:100) {
  cut_off_value <- x/100
  cut_off_values <- append(cut_off_values, cut_off_value)
  
  # preform cross validation for particular cut off value
  for (i in 1:NUM_K_FOLDS) {
    # create training and validation data for particular fold
    train_items_this_fold  = data_shuffled[data_shuffled$folds != i,] 
    validation_items_this_fold = data_shuffled[data_shuffled$folds == i,]
    
    # fit linear regression model on this fold
    this_glmfit <- glm(living_status ~ height + width + span, 
                       data = train_items_this_fold, 
                       family = 'binomial')
    
    # validate current linear regression model
    validation_items_this_fold$prediction = predict(this_glmfit, validation_items_this_fold, type="response")
    validation_items_this_fold$predicted_correct = 0
    validation_items_this_fold$predicted_correct[validation_items_this_fold$prediction > cut_off_value] = 1
    
    # calculate correctness
    correct_items = validation_items_this_fold$predicted_correct == validation_items_this_fold$living_status
    num_correct = nrow(validation_items_this_fold[correct_items,])
    proportion_correct = num_correct / nrow(validation_items_this_fold)
    
    # record if individual image was correct for use in part 1.5
    
    
    # recoed correctness with corresponding number of folds
    correctectness_values <- append(correctectness_values, proportion_correct)
    #fold_num <- append(fold_num, i)
  }
  # record mean correctness value over all folds
  correctness_values_for_each_cut_off <- append(correctness_values_for_each_cut_off, mean(correctectness_values))
  correctectness_values <- c()
  
}
correctness_cutoff_df <- data.frame(cut_off = cut_off_values, correctness = correctness_values_for_each_cut_off)

# find largest correctness and corresponding cut off value
max_correctness_pos <- 0
max_correctness <- 0
for (i in 1:nrow(correctness_cutoff_df)) {
  if (correctness_cutoff_df[i, "correctness"] > max_correctness) {
    max_correctness <- correctness_cutoff_df[i, "correctness"]
    max_correctness_pos <- i
  }
}
max_cutoff_value <- correctness_cutoff_df[max_correctness_pos, "cut_off"]
print(paste("1.3: max correctness = ", max_correctness, "at a cut-off value of ", max_cutoff_value))


# Plot graph to visualise results of changing the cutoff value on correctness
correctness_cut_off_graph <- ggplot(data = correctness_cutoff_df, aes(x = cut_off_values, y = correctness_values_for_each_cut_off)) +
  geom_point() +
  scale_y_continuous(name="Correctness") +
  scale_x_continuous(name = "Cut Off Value") +
  ggtitle("Correctness of Linear Regression Model Under Differing Cut Off Values") +
  geom_hline(yintercept = max_correctness, linetype="dotted", color = "red", size=1) +
  geom_vline(xintercept = max_cutoff_value, linetype="dotted", color = "red", size=1)
print(correctness_cut_off_graph)


# MAYBE TURN THIS INTO A FUNCTION
#Preform k-fold cross validation with best suited cutoff point as calculated above
fold_num <- c() # to store fold
correct <- c() # store of each image is correct for use in part 1.5
for (i in 1:NUM_K_FOLDS) {
  # create training and validation data for particular fold
  train_items_this_fold  = data_shuffled[data_shuffled$folds != i,] 
  validation_items_this_fold = data_shuffled[data_shuffled$folds == i,]
  
  # fit linear regression model on this fold
  this_glmfit <- glm(living_status ~ height + width + span, 
                     data = train_items_this_fold, 
                     family = 'binomial')
  
  # validate current linear regression model
  validation_items_this_fold$prediction = predict(this_glmfit, validation_items_this_fold, type="response")
  validation_items_this_fold$predicted_correct = 0
  validation_items_this_fold$predicted_correct[validation_items_this_fold$prediction > max_cutoff_value] = 1
  
  # calculate correctness
  correct_items = validation_items_this_fold$predicted_correct == validation_items_this_fold$living_status
  correct <- append(correct, correct_items)
  num_correct = nrow(validation_items_this_fold[correct_items,])
  proportion_correct = num_correct / nrow(validation_items_this_fold)
  
  # recoed correctness with corresponding number of folds
  correctectness_values <- append(correctectness_values, proportion_correct)
  fold_num <- append(fold_num, i)
}
data_shuffled$predicted_correct = correct

# Understand corrrectness readings from 5-fold test
final_correctness_value <- mean(correctectness_values)
print(correctectness_values)
print(paste("Average correctness of linear regression model measured from 5-fold test:", final_correctness_value, 
            ",using a cut-off value of:", max_cutoff_value))

# Visualise correctness in a graph
correctness_5_fold_df <- data.frame(fold_num, correctectness_values)
correctness_5_fold_graph <- ggplot(data = correctness_5_fold_df, aes(x = fold_num, y = correctectness_values)) +
  geom_point() +
  scale_y_continuous(name="Correctness", limits=c(0, 1)) +
  scale_x_continuous(name = "Fold number") +
  ggtitle(paste("Correctness of Linear Regression Model via 5-fold Analysis\nWith Cut-off Value of ", max_cutoff_value)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = final_correctness_value,linetype="dotted", color = "red", size=1)

print(correctness_5_fold_graph)












print("##################  1.4 #########################")

# Create a vector brom binomial that has probability of 0.5
binom_probability <- 0.5
number_correct <- nrow(DATA) * final_correctness_value
model_range <- 1:nrow(DATA)
bimom_model <- dbinom(model_range, nrow(DATA), binom_probability)
plot(model_range, bimom_model, type="l", col="blue", lty=1, lwd=3, 
     xlab="sucesses", ylab="Density", 
     main=paste("Binomial Distribution\n n =" ,nrow(DATA), ", p =", binom_probability))

# Mark where the correctness of the model is
abline(v=(number_correct), col="red", lty=2, lwd=3)

# Work out p value for correctness
p_value <- pnorm(number_correct, nrow(DATA), binom_probability)
print(paste("1.4: P value for a correctness of ", final_correctness_value, "is", p_value))






print("##################  1.5 #########################")
write.csv(data_shuffled, "testing.csv")
get_incorrectness <- function(my_label) {
  label_subset <- subset(data_shuffled, label == my_label)
  #label_subset_incorrect <- label_subset[label_subset$predicted_correct == FALSE,]
  part_incorrect <- nrow(label_subset[!label_subset$predicted_correct,]) / nrow(label_subset)
  return(part_incorrect)
}

data_shuffled$living_status[catorgised_data$label %in% living_labels] <- 1
data_shuffled = data_shuffled[sample(nrow(data_shuffled)),]

# get list of labels to loop over
labels <- levels(as.factor(DATA$label))
incorrectness <- c()
length(labels)
# record incorrectness
# create data frame in order to plot results
living <- c()
for (i in 1:length(labels)) {
  this_label <- labels[i]
  this_incorrectness <- get_incorrectness(this_label)
  incorrectness <- append(incorrectness, this_incorrectness)
  if (this_label %in% living_labels) {
    living <- append(living, c("living"))
  } else {
    living <- append(living, c("non-living"))
  }
  print(paste("Incorrectness rate of:", this_label, "=", this_incorrectness))
}

# create data frame in order to plot results
incorrectness_df <- data.frame(label = labels, 
                               living_status = living, 
                               incorrectness = incorrectness)

#incorrectness_graph <- ggplot(data = incorrectness_df + aes(x = label, y = incorrectness)) +
#  geom_bar(stat="identity")
#print(incorrectness_graph)



# MAKE A BETTER GRAPH
plot(incorrectness_df$label, incorrectness_df$incorrectness)

# Determine if wineglass result is signifiant
# MAYBE CHANGE FROM HARDCODED VALUE
num_incorrect_wineglass <- 2
print(paste("P-value of wineglass result:", pbinom(num_incorrect_wineglass, 20, 0.5))))
