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

living_labels <- c("banana", 
                   "cherry", 
                   "flower", 
                   "pear")

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
formula = label ~ nr_pix + height + width + span + rows_with_5 + cols_with_5 +
  neigh1 + neigh5

# a) Out of bag
bagging_df <- data.frame(bag_size = c(), accuracy = c())
for (bag in bag_size) {
  print(bag)
  #tc <- trainControl(method="boot", number=bag)
  #train(formula = label ~ nr_pix + height + width + span + rows_with_5 + cols_with_5 +
  #        neigh1 + neigh5,
  #      data=DATA, trControl=tc, method="treebag")
  this_accuracy <- bagging(formula = formula, 
                           data = DATA,
                           nbag = bag,
                           coob = TRUE) # out of bag estimate 
  temp_df <- data.frame(bag_size = c(bag), accuracy = c(1 - this_accuracy$err))
  bagging_df <- rbind(bagging_df, temp_df)
  
}
print(bagging_df)


# b) 5-fold cross validation
num_folds <- 5
train <- trainControl(method = "cv", number = num_folds)

model <- train(formula,
               data = DATA,
               method = "treebag",
               trControl = train)

print(model$results$Accuracy)




print("################## 3.2 #########################")


rand_forest <- randomForest(formula = formula, data = DATA)
num_predictors <- expand.grid(.mtry = c(2, 4, 6, 8))

train <- trainControl(method = "cv",
                      number = num_folds,
                      search = "grid")

train(formula,
               data = DATA, 
               method = "rf", 
               ntree = 50, 
               tuneGrid = num_predictors,
               trControl = train)

rand_forest_df <- data.frame(optimal_num_predictor = c(), ntree = c(), accuracy = c())
for (i in c(1:16)) {
  this_ntree <- i * 25
  print(this_ntree)
  model <- train(formula,
                 data = DATA, 
                 method = "rf", 
                 ntree = this_ntree, 
                 tuneGrid = num_predictors,
                 trControl = train)
  
  model_result <- model$results
  max_accuracy_result <- model_result[model_result$Accuracy == max(model_result$Accuracy),]
  temp_df <- data.frame(optimal_num_predictor = c(max_accuracy_result$mtry), 
                        ntree = c(this_ntree), 
                        accuracy = c(max_accuracy_result$Accuracy))
  rand_forest_df <- rbind(rand_forest_df, temp_df)
}

rand_forest_df_plot <- ggplot(data = rand_forest_df, 
                              mapping = aes(x = ntree, y = accuracy, color = as.factor(optimal_num_predictor))) +
  geom_point() +
  ggtitle("Random Forrest Model") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(color='NEW LEGEND TITLE') +
  xlab("Number of trees") +
  ylab("Accuracy")

rand_forest_df_plot






print("################## 3.3 #########################")

print("3.3: best value of tree and predictor number from part 3.2:")

best_rand_forest_df <- rand_forest_df[rand_forest_df$accuracy == max(rand_forest_df$accuracy),]
print(best_rand_forest_df)

print("While there are 2 answers that give the best number of trees refer to 3.2 in the report to see
why only a figure of ntree = 350")

optimal_ntree <- 350

best_rand_forest_df <- rand_forest_df[rand_forest_df$ntree == optimal_ntree,]
print(best_rand_forest_df)

optimal_num_predictor <- best_rand_forest_df$optimal_num_predictor

# Record accuracy over 20 iterations of 5-fold cross validation
num_predictors <- expand.grid(.mtry = c(optimal_num_predictor))
rand_forest_accuracy_df <- data.frame(iteration = c(), accuracy = c())
for (i in 1:20) {
  print(i)
  model <- train(formula,
                 data = DATA, 
                 method = "rf", 
                 ntree = optimal_ntree, 
                 tuneGrid = expand.grid(.mtry = c(optimal_num_predictor)), # this needs changed
                 trControl = train)
  temp_df <- data.frame(iteration = c(i), accuracy = c(max(model$results$Accuracy)))
  rand_forest_accuracy_df <- rbind(rand_forest_accuracy_df, temp_df)
}







