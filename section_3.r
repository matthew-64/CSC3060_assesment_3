library(caret)
library(rpart.plot)
library(rattle)
library(ipred)


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
DATA$ID = as.numeric(rownames(DATA))


# 3.1
bagging_df <- data.frame(bag_size = c(), accuracy = c())
bag_size <- c(25, 50, 200, 400, 800)
for (bag in bag_size) {
  print(bag)
  this_accuracy <- bagging(formula = label ~ nr_pix + height + width + span + rows_with_5 + cols_with_5 +
                             neigh1 + neigh5, 
                           data = DATA,
                           nbag = bag,
                           coob = TRUE) # out of bag estimate 
  #temp_df <- data.frame(bag_size = c(bag), accuracy = c(this_accuracy))
  #bagging_df <- rbind(bagging_df, temp_df)
}
print(bagging_df)
num_folds <- 5


# a - out of bag

out_of_bag_df = data.frame(bag_size = c(), error)

set.seed(3060)
for (bag in bag_size) {
  for (i in 1:5) {
    # Look at doing this with caret
    blah <- bagging(formula = label ~ nr_pix + height + width + span + rows_with_5 + cols_with_5 +
                      neigh1 + neigh5, 
                    data = DATA,
                    nbag = 100,
                    coob = TRUE) # out of bag estimate 
    print(blah)
  }
}
