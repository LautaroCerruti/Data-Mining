library("e1071")

k_fold_cv_svm_polinomial <- function(data, class, folds, c, gamma, k, num_folds = 5) {
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
        kernel = "polynomial",
        degree = k,
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

results_svmp <- data.frame(Cost = numeric(), Gamma = numeric(), Degree = integer(), Error = numeric(), stringsAsFactors = FALSE)

costs <- c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)
gammas <- c(0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000)
degrees <- 1:5
num_folds <- 5
folds <- cut(seq(1, nrow(train_general_x)), breaks = num_folds, labels = FALSE)

for(i in 1:length(costs)) {
  for(j in 1:length(gammas)) {
    for(k in 1:length(degrees)) {
      suppressWarnings(error_kfold <- k_fold_cv_svm_polinomial(train_general_x, train_general_y, folds, costs[i], gammas[j], degrees[k], num_folds))
      results_svmp <- rbind(results_svmp, data.frame(Cost = costs[i], Gamma = gammas[j], Degree = degrees[k], Error = error_kfold))

	  elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
	  cat("SVM P - cost", costs[i], "- gamma", gammas[j], "- degree", degrees[k], "- time", elapsed_time, "segundos - error", error_kfold, "\n")
	  flush.console()
    }
  }
}

save(results_svmp, file = "results_svmp.RData")