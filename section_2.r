library(class)
library(ggplot2)

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
training_data <- DATA
training_data$living_status <- "non-living"
training_data$living_status[training_data$label %in% living_labels] <- "living"

# make dataframe to store and plot results of below
k_values_df = data.frame(k=c(), accuracy=c())

# 1st 8 features
# for k = 1 to 59 (odd only)
for (i in 1:30) {
  k <- 2*i - 1
  this_knn <- knn(training_data[3:10] ,training_data[3:10], training_data$living_status, k)
  correct_items <- training_data$living_status == this_knn
  accuracy <- sum(this_knn == training_data$living_status) / nrow(training_data)
  temp_df <- data.frame(k, accuracy)
  k_values_df <- rbind(k_values_df, temp_df)
}
print(k_values_df)

# plot results
k_values_graph <- ggplot(k_values_df, aes(x = k, y = accuracy)) + 
  geom_point() +
  geom_line() +
  ggtitle("Accuracy of KNN Model Using Different Values of k") +
  theme(plot.title = element_text(hjust = 0.5))
  
print(k_values_graph)



