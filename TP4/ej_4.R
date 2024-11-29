library("xgboost")
library("randomForest")
library("e1071")
load("./TP4/TP4.Rdata")

k_fold_cv_xgboost <- function(data, column, folds, XGB.eta, XGB.nrounds, XGB.max.depth, num_folds = 5) {
  # Inicializar vector para almacenar la precisión de cada pliegue
  accuracies <- numeric(num_folds)

  for (i in 1:num_folds) {
      # Separar los datos de entrenamiento y validación
      test_indices <- which(folds == i, arr.ind = TRUE)
      test_data <- data[test_indices, ]
      train_data <- data[-test_indices, ]

      test_x <- test_data[, -ncol(test_data)]    # Todas las columnas menos la clase
      test_y <- test_data[[column]]

      m.xgb <- xgboost(
        data = as.matrix(train_data[, -ncol(train_data)]),
        label = as.integer(train_data[[column]]) - 1,
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
      accuracies[i] <- error
    }

    # Retornar la precisión promedio de todos los pliegues
    mean_accuracy <- mean(accuracies)
    return(mean_accuracy)
}

k_fold_cv_svm_RBF <- function(data, column, folds, c, gamma, num_folds = 5) {
  # Inicializar vector para almacenar la precisión de cada pliegue
  accuracies <- numeric(num_folds)

  for (i in 1:num_folds) {
      # Separar los datos de entrenamiento y validación
      test_indices <- which(folds == i, arr.ind = TRUE)
      test_data <- data[test_indices, ]
      train_data <- data[-test_indices, ]

      test_x <- test_data[, -ncol(test_data)]    # Todas las columnas menos la clase
      test_y <- test_data[[column]]

      svm_model <- svm(
        as.formula(paste(column, "~ .")),
        data = train_data,
        type = 'C-classification',
        kernel = "radial",
        cost = c,
        gamma = gamma
      )

      test_pred <- predict(svm_model, test_x)
      error <- mean(test_pred != test_y)
      accuracies[i] <- error
    }

    # Retornar la precisión promedio de todos los pliegues
    mean_accuracy <- mean(accuracies)
    return(mean_accuracy)
}

k_fold_cv_svm_polinomial <- function(data, column, folds, c, gamma, k, num_folds = 5) {
  # Inicializar vector para almacenar la precisión de cada pliegue
  accuracies <- numeric(num_folds)

  for (i in 1:num_folds) {
      # Separar los datos de entrenamiento y validación
      test_indices <- which(folds == i, arr.ind = TRUE)
      test_data <- data[test_indices, ]
      train_data <- data[-test_indices, ]

      test_x <- test_data[, -ncol(test_data)]    # Todas las columnas menos la clase
      test_y <- test_data[[column]]

      svm_model <- svm(
        as.formula(paste(column, "~ .")),
        data = train_data,
        type = 'C-classification',
        kernel = "polynomial",
        degree = k,
        cost = c,
        gamma = gamma
      )

      test_pred <- predict(svm_model, test_x)
      error <- mean(test_pred != test_y)
      accuracies[i] <- error
    }

    # Retornar la precisión promedio de todos los pliegues
    mean_accuracy <- mean(accuracies)
    return(mean_accuracy)
}

start_time <- Sys.time()

RRL <- RRL[sample(1:nrow(RRL)), ]

test_indices <- sample(1:nrow(RRL), size = (0.2 * nrow(RRL)))
test <- RRL[test_indices, ]
train <- RRL[-test_indices, ]

results_rrl_rf <- data.frame(Mtry = numeric(), Error = numeric(), stringsAsFactors = FALSE)

mtry_values <- floor((ncol(train)-1) * (1 / 2)^(0:6))
errors_avg <- c()
tree_count <- 1000
num_runs <- 5

for (mtryv in mtry_values) {
  errors <- numeric(num_runs)

  for (i in 1:num_runs) {
    rf_model <- randomForest(
      Tipo ~ .,
      data=train,
      mtry = mtryv,
      ntree = tree_count
    )

    errors[i] <- rf_model$err.rate[nrow(rf_model$err.rate), "OOB"]
  }
  elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
  cat("RF - mtry", mtryv, "- tiempo", elapsed_time, "segundos \n")
  flush.console()

  errors_avg <- c(errors_avg, mean(errors))
  results_rrl_rf <- rbind(results_rrl_rf, data.frame(Mtry = mtryv, Error = errors_avg[length(errors_avg)]))
}

save(results_rrl_rf, file = "results_rrl_rf.RData")

results_rrl_xgb <- data.frame(LearningRate = numeric(), Prof = integer(), Error = numeric(), stringsAsFactors = FALSE)

nrounds <- 1000
learning_rates <- c(0.01, 0.05, 0.1, 0.3)
profs <- 1:20
num_folds <- 5
folds <- cut(seq(1, nrow(train)), breaks = num_folds, labels = FALSE)

for(i in 1:length(learning_rates)) {
  for(j in 1:length(profs)) {
    error_kfold <- k_fold_cv_xgboost(train, "Tipo", folds, learning_rates[i], nrounds, profs[j], num_folds)
    results_rrl_xgb <- rbind(results_rrl_xgb, data.frame(LearningRate = learning_rates[i], Prof = profs[j], Error = error_kfold))

	elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
	cat("XGB - lr", learning_rates[i], "- prof", profs[j], "- time", elapsed_time, "segundos \n")
	flush.console()
  }
}

save(results_rrl_xgb, file = "results_rrl_xgb.RData")

results_rrl_svmrbf <- data.frame(Cost = numeric(),
                      Gamma = numeric(),
                      Error = numeric(),
                      stringsAsFactors = FALSE)

costs <- c(1, 10, 100, 1000, 10000, 100000)
gammas <- c(0.0000001, 0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10)
num_folds <- 5
folds <- cut(seq(1, nrow(train)), breaks = num_folds, labels = FALSE)

for(i in 1:length(costs)) {
  for(j in 1:length(gammas)) {
    suppressWarnings(error_kfold <- k_fold_cv_svm_RBF(train, "Tipo", folds, costs[i], gammas[j], num_folds))
    results_rrl_svmrbf <- rbind(results_rrl_svmrbf, data.frame(Cost = costs[i], Gamma = gammas[j], Error = error_kfold))

	elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
	cat("SVM RBF - cost", costs[i], "- gamma", gammas[j], "- time", elapsed_time, "segundos \n")
	flush.console()
  }
}

save(results_rrl_svmrbf, file = "results_rrl_svmrbf.RData")

results_rrl_svmp <- data.frame(Cost = numeric(), Gamma = numeric(), Degree = integer(), Error = numeric(), stringsAsFactors = FALSE)

costs <- c(1, 10, 100, 1000, 10000, 100000)
gammas <- c(0.0000001, 0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10)
degrees <- 1:5
num_folds <- 5
folds <- cut(seq(1, nrow(train)), breaks = num_folds, labels = FALSE)

for(i in 1:length(costs)) {
  for(j in 1:length(gammas)) {
    for(k in 1:length(degrees)) {
      suppressWarnings(error_kfold <- k_fold_cv_svm_polinomial(train, "Tipo", folds, costs[i], gammas[j], degrees[k], num_folds))
      results_rrl_svmp <- rbind(results_rrl_svmp, data.frame(Cost = costs[i], Gamma = gammas[j], Degree = degrees[k], Error = error_kfold))

	  elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
	  cat("SVM RBF - cost", costs[i], "- gamma", gammas[j], "- degree", degrees[k], "- time", elapsed_time, "segundos \n")
	  flush.console()
    }
  }
}

save(results_rrl_svmp, file = "results_rrl_svmp.RData")