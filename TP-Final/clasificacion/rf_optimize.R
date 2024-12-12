library("randomForest")

load("./train_general_x.RData")
load("./train_general_y.RData")

start_time <- Sys.time()

results_rf <- data.frame(Mtry = numeric(), Error = numeric(), stringsAsFactors = FALSE)

mtry_values <- floor((ncol(train_general_x)-1) * (1 / 2)^(0:5))
errors_avg <- c()
tree_count <- 1000
num_runs <- 5

for (mtryv in mtry_values) {
  errors <- numeric(num_runs)

  for (i in 1:num_runs) {
    rf_model <- randomForest(
      x = train_general_x,
      y= train_general_y,
      mtry = mtryv,
      ntree = tree_count
    )

    errors[i] <- rf_model$err.rate[nrow(rf_model$err.rate), "OOB"]
  }
  elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
  cat("RF - mtry", mtryv, "- tiempo", elapsed_time, "segundos - error", mean(errors), "\n")
  flush.console()

  errors_avg <- c(errors_avg, mean(errors))
  results_rf <- rbind(results_rf, data.frame(Mtry = mtryv, Error = errors_avg[length(errors_avg)]))
}

save(results_rf, file = "results_rf.RData")