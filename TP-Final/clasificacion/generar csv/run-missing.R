# Librerías necesarias
library(e1071)
library(future.apply)

# Función para realizar validación cruzada con SVM polinomial
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
  cat("SVM P - cost", c, "- gamma", gamma, "- degree", k, "- time", elapsed_time, "segundos - error", mean(errores), "\n", file = "results_svmp_parallel_missing.txt", append = TRUE)
  
  # Retornar el error promedio de todos los pliegues
  mean_error <- mean(errores)
  return(mean_error)
}

# Cargar los datos
load("./train_general_x.RData")
load("./train_general_y.RData")

# Cargar combinaciones faltantes desde el archivo CSV
missing_combinations <- read.csv("missing_combinations.csv")

# Configurar número de pliegues
num_folds <- 5
folds <- cut(seq(1, nrow(train_general_x)), breaks = num_folds, labels = FALSE)

# Configurar paralelismo
plan(multisession)  # Utiliza múltiples núcleos de la CPU

# Evaluar cada combinación de hiperparámetros en paralelo
start_time <- Sys.time()
results_svmp <- future_lapply(1:nrow(missing_combinations), function(idx) {
  params <- missing_combinations[idx, ]
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

# Guardar los resultados en un archivo CSV
results_svmp <- do.call(rbind, results_svmp)
write.csv(results_svmp, "results_svmp_missing.csv", row.names = FALSE)