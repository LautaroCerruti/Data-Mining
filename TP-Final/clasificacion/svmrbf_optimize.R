library("e1071")

k_fold_cv_svm_RBF <- function(data, class, folds, c, gamma, num_folds = 5) {
  # Inicializar vector para almacenar la precisión de cada pliegue
  errores <- numeric(num_folds)

  for (i in 1:num_folds) {
      # Separar los datos de entrenamiento y validación
      test_indices <- which(folds == i, arr.ind = TRUE)
      test_x <- data[test_indices, ]
      train_x <- data[-test_indices, ]

      test_y <- class[test_indices]
      train_y <- class[-test_indices]

      svm_model <- svm(
        x=train_x,
        y=train_y,
        type = 'C-classification',
        kernel = "radial",
        cost = c,
        gamma = gamma
      )

      test_pred <- predict(svm_model, test_x)
      error <- mean(test_pred != test_y)
      errores[i] <- error
    }

    # Retornar la precisión promedio de todos los pliegues
    mean_error <- mean(errores)
    return(mean_error)
}

load("./train_general_x.RData")
load("./train_general_y.RData")

start_time <- Sys.time()

results_svmrbf <- data.frame(Cost = numeric(),
                      Gamma = numeric(),
                      Error = numeric(),
                      stringsAsFactors = FALSE)

costs <- c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)
gammas <- c(0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000)
num_folds <- 5
folds <- cut(seq(1, nrow(train_general_x)), breaks = num_folds, labels = FALSE)

for(i in 1:length(costs)) {
  for(j in 1:length(gammas)) {
    suppressWarnings(error_kfold <- k_fold_cv_svm_RBF(train_general_x, train_general_y, folds, costs[i], gammas[j], num_folds))
    results_svmrbf <- rbind(results_svmrbf, data.frame(Cost = costs[i], Gamma = gammas[j], Error = error_kfold))

	elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
	cat("SVM RBF - cost", costs[i], "- gamma", gammas[j], "- time", elapsed_time, "segundos - error", error_kfold, "\n")
	flush.console()
  }
}

save(results_svmrbf, file = "results_svmrbf.RData")