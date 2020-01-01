library(class)
library(ggplot2)

set.seed(3060)

LIVING_ITEMS <- c("banana", "cherry", "flower", "pear")
NON_LIVING_ITEMS <- c("envelope", "golfclub", "pencil", "wineglass")
ALL_ITEMS <- append(LIVING_ITEMS, NON_LIVING_ITEMS)
NUM_ITEMS_PER_IMAGE <- 500

# find out why print out below is not working
#   WHY DOES 3397 WINEGLASS INDEX == 3399 WINEGLASS INDEX?

# get feature file names 
feature_file_names <- list.files(path = "./doodle_data", 
                                 pattern = "*_features.csv",
                                 full.names = TRUE)

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


# 2.1
print("################## 2.1 #########################")

training_data <- DATA
training_data$living_status <- "non-living"
training_data$living_status[training_data$label %in% living_labels] <- "living"

# make dataframe to store and plot results of below
k_values_df_21 = data.frame(k=c(), accuracy=c())

# 1st 8 features
# for k = 1 to 59 (odd only)
for (i in 1:30) {
  k <- 2*i - 1
  #this_knn <- knn(training_data[3:10] ,training_data[3:10], training_data$living_status, k)
  this_knn <- knn(training_data[3:10] ,training_data[3:10], training_data$label, k)
  
  #correct_items <- training_data$living_status == this_knn
  accuracy <- sum(this_knn == training_data$label) / nrow(training_data)
  temp_df <- data.frame(k, accuracy)
  k_values_df_21 <- rbind(k_values_df_21, temp_df)
}

# plot results
k_values_graph <- ggplot(k_values_df_21, aes(x = k, y = accuracy)) + 
  geom_point() +
  geom_line() +
  ggtitle("Accuracy of KNN Model Using Different Values of k") +
  theme(plot.title = element_text(hjust = 0.5))
  
print(k_values_graph)

# print k that gives max accuracy
max_k_value_df = k_values_df_21[k_values_df_21$accuracy == max(k_values_df_21$accuracy),]
print(paste("Max accuracy =", max_k_value_df$accuracy, "at k =", max_k_value_df$k))
# print k that gives min accuracy
min_k_value_df = k_values_df_21[k_values_df_21$accuracy == min(k_values_df_21$accuracy),]
print(paste("Min accuracy =", min_k_value_df$accuracy, "at k =", min_k_value_df$k))



print("################## 2.2 #########################")

# shuffle amd section the data into 5 equal parts.
data_shuffled <- training_data
data_shuffled = training_data[sample(nrow(data_shuffled)),] # shuffle
data_shuffled$folds <- cut(seq(1:nrow(data_shuffled)), 
                           breaks=NUM_K_FOLDS,
                           labels=FALSE) # section

# 5-fold crossvalidation for knn with k = 1:59 (odd only)
num_folds <- 5

# data frame to store analysis
k_values_df_22 = data.frame(k=c(), inverse_k = c(), accuracy=c(), testing_set = c())

train_k_accuracy <- c()  # to store accurcy for each individual fold
test_k_accuracy <- c() # to store accurcy for each individual fold
for (i in 1:30) {
  k <- 2*i - 1
  
  for (fold in num_folds) {
    train_data  = data_shuffled[data_shuffled$folds != fold,]
    test_data = data_shuffled[data_shuffled$folds == fold,]
    
    # record train accuracy per fold
    train_knn <- knn(train_data[3:10] ,train_data[3:10], train_data$label, k)
    train_accuracy <- sum(train_knn == train_data$label) / nrow(train_data)
    train_k_accuracy <- append(train_k_accuracy, train_accuracy)
    
    # record test accuracy per fold
    test_knn <- knn(train_data[3:10] ,test_data[3:10], train_data$label, k)
    test_accuracy <- sum(test_knn == test_data$label) / nrow(test_data)
    test_k_accuracy <- append(test_accuracy, test_k_accuracy)
  }
  
  # save train data into df
  train_temp_df <- data.frame(k=k, inverse_k=(1/k), accuracy=mean(train_k_accuracy), testing_set="train")
  train_k_accuracy <- c() # reset for next iteration of c
  
  # save test data into df
  test_temp_df <- data.frame(k=k, inverse_k=(1/k), accuracy=mean(test_k_accuracy), testing_set="test")
  test_k_accuracy <- c() # reset for next iteration of c
  
  k_values_df_22 <- rbind(k_values_df_22, train_temp_df, test_temp_df)
}

# calculate max test accuracy and corresponding k (for knn) value
test_results <- k_values_df_22[k_values_df_22$testing_set == "test",]
max_test_accuracy <- max(test_results$accuracy)
print("2.2: max testing accuracy =")
print(max_test_accuracy)
print("At k values =")
print(test_results[test_results$accuracy == max_test_accuracy,]$k)

# print results to graph
test_train_graph <- ggplot(k_values_df_22, aes(x = inverse_k, y = accuracy, color = testing_set)) +
  geom_point() +
  geom_line() + 
  geom_hline(yintercept=max_test_accuracy, linetype="dashed", color = "black") +
  coord_trans(x = "log10") + # need to improve graph labelling in x axis
  ggtitle("Accuracy of KNN Model Using Different Values of k") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("1/k")

print(test_train_graph)

# plot the graph used for fig 1.2 but with a 1/k value on the x axis
# append a 1/k collum to the dataframe from 2.1
#k_values_df_21 <- cbind(k_values_df_21, inverse_k = k_values_df_22$inverse_k)
k_values_df_21$inverse_k = (1/k_values_df_21$k)
k_values_graph <- ggplot(k_values_df_21, aes(x = inverse_k, y = accuracy)) + 
  geom_point() +
  geom_line() +
  ggtitle("Accuracy of KNN Model Using Different Values of k") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_trans(x = "log10") +  # need to improve graph labelling in x axis
  xlab("1/k")

print(k_values_graph)


