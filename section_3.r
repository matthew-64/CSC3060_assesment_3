library(caret)
library(rpart.plot)
library(rattle)
library(ipred)
library(randomForest)

set.seed(3060)

# loop every feature
# loop loop every coordinate
# find best feature

# set up data frame to hold data
data_col_names <- c("label",
                    "index",
                    "nr_pix",
                    "height",
                    "width",
                    "span",
                    "rows_with_5",
                    "cols_with_5",
                    "neigh1",
                    "neigh5",
                    "left2tile",
                    "right2tile",
                    "verticalness",
                    "top2tile",
                    "bottom2tile",
                    "horizontalness",
                    "positive_slope_3tile",
                    "negative_slope_3tile",
                    "nr_regions",
                    "nr_eyes",
                    "hollowness",
                    "max_black_3tile")

# Create dataframe from first feature csv file 
DATA <- read.table(feature_file_names[1], sep = "\t")

# bind data from remaining fines into existing dataframe
# read in the data from feature file names
for (i in 2:length(feature_file_names)) {
  temp_df <- read.table(feature_file_names[i], sep = "\t")
  DATA <- rbind(DATA, temp_df)
}

# asign feature names to the dataframe 
colnames(DATA) <- data_col_names

# Create a quique id for each image
DATA$ID = as.numeric(rownames(DATA)) # Maybe I dont need this


print("################## 3.1 #########################")

bag_size <- c(25, 50, 200, 400, 800)
first_8_features_formula = label ~ nr_pix + height + width + span + rows_with_5 + cols_with_5 +
  neigh1 + neigh5

# dataframe to store results for 3.1a and 3.1b
bagging_df <- data.frame(bag_size = c(), accuracy = c(), validation_method = c())

# a) Out of bag
for (bag in bag_size) {
  this_accuracy <- bagging(formula = first_8_features_formula, 
                           data = DATA,
                           nbag = 25,
                           coob = TRUE) # out of bag estimate 
  temp_df <- data.frame(bag_size = c(bag), accuracy = c(1 - this_accuracy$err), validation_method = c("OOB"))
  bagging_df <- rbind(bagging_df, temp_df)
  
}

# b) 5-fold cross validation
num_folds <- 5
train <- trainControl(method = "cv", number = num_folds)
for (bag in bag_size) {
  model <- train(first_8_features_formula,
                 data = DATA,
                 method = "treebag",
                 trControl = train,
                 nbagg = bag)
  temp_df <- data.frame(bag_size = c(bag), accuracy = c(model$results$Accuracy), validation_method = c("5_fold_CV"))
  bagging_df <- rbind(bagging_df, temp_df)
}


bagging_plot <- ggplot(data = bagging_df, aes(x = bag_size, y = accuracy, color = validation_method)) +
  geom_point() +
  geom_line() +
  ggtitle("Accuracy of Decision Trees Using Different Bag Sizes") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Bag Size") +
  ylab("Accuracy") +
  labs(color='Validation Method') 

print(bagging_plot)




print("################## 3.2 #########################")

num_predictors <- expand.grid(.mtry = c(2, 4, 6, 8))

rand_forest_df <- data.frame(num_predictor = c(), ntree = c(), accuracy = c())
for (i in c(1:16)) {
  print(i)
  this_ntree <- i * 25
  print(this_ntree)
  model <- train(first_8_features_formula,
                 data = DATA, 
                 method = "rf", 
                 ntree = this_ntree, 
                 tuneGrid = num_predictors,
                 trControl = train)
  
  temp_df <- data.frame(num_predictor = c(model$results$mtry), 
                        ntree = c(this_ntree), 
                        accuracy = c(model$results$Accuracy))
  rand_forest_df <- rbind(rand_forest_df, temp_df)
}

# print combination of factors that leads to the maximum accuracy
print("3.2 max accuracy result:")
print(rand_forest_df[rand_forest_df$accuracy == max(rand_forest_df$accuracy),])

# Show results on a plot
rand_forest_df_plot <- ggplot(data = rand_forest_df, 
                              mapping = aes(x = ntree, y = accuracy, color = as.factor(num_predictor))) +
  geom_point() +
  ggtitle("Random Forrest Model") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Number of trees") +
  ylab("Accuracy") +
  labs(color='Predictors Per Node') +
  geom_hline(yintercept=max(rand_forest_df$accuracy), linetype="dashed", color = "black") + 
  stat_smooth(method=lm)

print(rand_forest_df_plot)






print("################## 3.3 #########################")

num_folds <- 5
print("3.3: best value of tree and predictor number from part 3.2:")

most_accurate_rf_result <- rand_forest_df[rand_forest_df$accuracy == max(rand_forest_df$accuracy),]
print(most_accurate_rf_result)

optimal_ntree <- most_accurate_rf_result$ntree
optimal_num_predictor <- most_accurate_rf_result$num_predictor

# Record accuracy over 20 iterations of 5-fold cross validation with optimum number or trees and predictors per node
num_predictors <- expand.grid(.mtry = c(optimal_num_predictor))
rand_forest_accuracy_df <- data.frame(iteration = c(), accuracy = c())
train <- trainControl(method = "cv", number = num_folds)

for (i in 1:20) {
  model <- train(first_8_features_formula,
                 data = DATA, 
                 method = "rf", 
                 ntree = optimal_ntree, 
                 tuneGrid = num_predictors, # this needs changed
                 trControl = train)
  temp_df <- data.frame(iteration = c(i), accuracy = c(max(model$results$Accuracy)))
  rand_forest_accuracy_df <- rbind(rand_forest_accuracy_df, temp_df)
}

# find mean
print("3.3: mean")
mean_result <- mean(rand_forest_accuracy_df$accuracy)
print(mean_result)

# find standard erorr of sanmple
print("3.3: Standard deviation")
se_sample <- sd(rand_forest_accuracy_df$accuracy)
print(se_result)




print("################## 3.4 #########################")

# show how span density plot for each label:
span_graph <- ggplot(DATA, aes(x = span, fill = as.factor(label))) +
  geom_density(adjust = 1.5, alpha = 0.4) + 
  ggtitle("Density plot of:\nSpan for All Labels") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill=c("Label"))

print(span_graph)


set.seed(3060)

# Set up for random forest decision tree
num_predictors <- expand.grid(.mtry = c(optimal_num_predictor))
train <- trainControl(method = "cv", number = num_folds)

# Remove snap from formula
no_span_formula <- label ~ nr_pix + height + width + rows_with_5 + cols_with_5 + neigh1 + neigh5

rand_forest_accuracy_df <- data.frame(iteration = c(), accuracy = c())
for (i in 1:20) {
  print(i)
  model <- train(no_span_formula,
                 data = DATA, 
                 method = "rf", 
                 ntree = optimal_ntree, 
                 tuneGrid = num_predictors, # this needs changed
                 trControl = train)
  temp_df <- data.frame(iteration = c(i), accuracy = c(max(model$results$Accuracy)))
  rand_forest_accuracy_df <- rbind(rand_forest_accuracy_df, temp_df)
}


# find mean
print("3.3: mean")
mean_result <- mean(rand_forest_accuracy_df$accuracy)
print(mean_result)

# find standard erorr of sanmple
print("3.3: sample SE")
sd_result <- sd(rand_forest_accuracy_df$accuracy)
print(sd_result)


