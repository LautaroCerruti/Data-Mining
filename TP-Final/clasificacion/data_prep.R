load("./data.RData")
load("./class.RData")
data_general <- data
class_general <- class

test_general_indices <- sample(1:nrow(data_general), size = (0.2 * nrow(data_general)))
test_general_x <- data_general[test_general_indices, ]
test_general_y <- class_general[test_general_indices]
train_general_x <- data_general[-test_general_indices, ]
train_general_y <- class_general[-test_general_indices]

save(test_general_x, file = "test_general_x.RData")
save(test_general_y, file = "test_general_y.RData")
save(train_general_x, file = "train_general_x.RData")
save(train_general_y, file = "train_general_y.RData")