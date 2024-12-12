library("xgboost")

k_fold_cv_xgboost <- function(data, class, folds, XGB.eta, XGB.nrounds, XGB.max.depth, num_folds = 5) {
  # Inicializar vector para almacenar la precisión de cada pliegue
  errors <- numeric(num_folds)

  for (i in 1:num_folds) {
      # Separar los datos de entrenamiento y validación
      test_indices <- which(folds == i, arr.ind = TRUE)
      test_x <- data[test_indices, ]
      train_x <- data[-test_indices, ]

      test_y <- class[test_indices]
      train_y <- class[-test_indices]

      m.xgb <- xgboost(
        data = as.matrix(train_x),
        label = as.integer(train_y) - 1,
        objective = "binary:hinge",
        nrounds = XGB.nrounds,
        early_stopping_rounds = 1000,
        eta = XGB.eta,
        max.depth = XGB.max.depth,
        colsample_bytree = 1,
        verbose = 0,
        subsample = 1
      )

      test_pred <- predict(m.xgb, as.matrix(test_x))
      error <- mean(test_pred != (as.integer(test_y) - 1))
      errors[i] <- error
    }

    # Retornar la precisión promedio de todos los pliegues
    mean_errors <- mean(errors)
    return(mean_errors)
}

load("./train_general_x.RData")
load("./train_general_y.RData")

start_time <- Sys.time()

results_xgb <- data.frame(LearningRate = numeric(), Prof = integer(), Error = numeric(), stringsAsFactors = FALSE)

nrounds <- 1000
learning_rates <- c(0.01, 0.05, 0.1, 0.3)
profs <- 1:20
num_folds <- 5
folds <- cut(seq(1, nrow(train_general_x)), breaks = num_folds, labels = FALSE)

for(i in 1:length(learning_rates)) {
  for(j in 1:length(profs)) {
    error_kfold <- k_fold_cv_xgboost(train_general_x, train_general_y, folds, learning_rates[i], nrounds, profs[j], num_folds)
    results_xgb <- rbind(results_xgb, data.frame(LearningRate = learning_rates[i], Prof = profs[j], Error = error_kfold))
    elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
    cat("XGB - lr", learning_rates[i], "- prof", profs[j], "- time", elapsed_time, "segundos - error", error_kfold, "\n")
    flush.console()
  }
}

save(results_xgb, file = "results_xgb.RData")