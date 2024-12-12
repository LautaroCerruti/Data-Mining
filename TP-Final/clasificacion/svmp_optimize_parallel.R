library(e1071)
library(future.apply)

k_fold_cv_svm_polinomial <- function(data, class, folds, c, gamma, k, num_folds = 5) {
  # Inicializar vector para almacenar el error de cada pliegue
  errores <- numeric(num_folds)
  
  for (i in 1:num_folds) {
    # Separar los datos de entrenamiento y validación
    test_indices <- which(folds == i, arr.ind = TRUE)
    test_x <- data[test_indices, ]
    train_x <- data[-test_indices, ]
    
    test_y <- class[test_indices]
    train_y <- class[-test_indices]
    
    # Entrenar el modelo SVM con el kernel polinomial
    svm_model <- svm(
      x = train_x,
      y = train_y,
      type = "C-classification",
      kernel = "polynomial",
      degree = k,
      cost = c,
      gamma = gamma
    )
    
    # Predecir los valores del conjunto de validación
    test_pred <- predict(svm_model, test_x)
    error <- mean(test_pred != test_y)
    errores[i] <- error
  }

  elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
  cat("SVM P - cost", c, "- gamma", gamma, "- degree", k, "- time", elapsed_time, "segundos - error", mean(errores), "\n", file = "results_svmp_parallel.txt", append = TRUE)
  
  # Retornar el error promedio de todos los pliegues
  mean_error <- mean(errores)
  return(mean_error)
}

# Cargar los datos
load("./train_general_x.RData")
load("./train_general_y.RData")

# Configurar hiperparámetros
costs <- c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)
gammas <- c(0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000)
degrees <- 1:5
num_folds <- 5
folds <- cut(seq(1, nrow(train_general_x)), breaks = num_folds, labels = FALSE)

# Crear todas las combinaciones de hiperparámetros
param_grid <- expand.grid(Cost = costs, Gamma = gammas, Degree = degrees, stringsAsFactors = FALSE)

start_time <- Sys.time()

# Configurar paralelismo
plan(multisession)  # Utiliza múltiples núcleos de la CPU

# Evaluar cada combinación de hiperparámetros en paralelo
results_svmp <- future_lapply(1:nrow(param_grid), function(idx) {
  params <- param_grid[idx, ]
  error_kfold <- k_fold_cv_svm_polinomial(
    train_general_x,
    train_general_y,
    folds,
    params$Cost,
    params$Gamma,
    params$Degree,
    num_folds
  )
  data.frame(Cost = params$Cost, Gamma = params$Gamma, Degree = params$Degree, Error = error_kfold)
})

# Combinar los resultados en un solo data frame
results_svmp <- do.call(rbind, results_svmp)

# Guardar los resultados
save(results_svmp, file = "results_svmp_parallel.RData")
