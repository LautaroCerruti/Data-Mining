library("randomForest")
library(knitr)

# Configuración inicial
start_time <- Sys.time()
path <- "./games_10min_wclass.csv"
original_data <- read.csv(path, header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Preprocesamiento inicial (antes de seleccionar la muestra)
original_data <- original_data[, -c(
  which(names(original_data) == "gameId"),
  which(names(original_data) == "blueFirstTowerLane"),
  which(names(original_data) == "blueDragnoType"),
  which(names(original_data) == "redFirstTowerLane"),
  which(names(original_data) == "redDragnoType"),
  which(names(original_data) == "blueFirstBlood"),
  which(names(original_data) == "redFirstBlood")
)]

# Crear carpeta para guardar resultados
output_dir <- "./resultados_experimentos"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Parámetros del experimento
n_experimentos <- 20
mtry_values <- c(5, 10)
tree_count <- 1000
num_runs <- 5

# Iterar el experimento
for (experiment_id in 1:n_experimentos) {
  # Selección aleatoria de 8000 puntos distintos
  sampled_data <- original_data[sample(nrow(original_data), 8000), ]
  class <- sampled_data$winner
  data <- sampled_data[, -which(names(sampled_data) == "winner")]

  # Asegurar que `class` sea factor
  if (!is.factor(class)) {
    class <- as.factor(class)
  }
  
  # Subcarpeta para cada experimento
  experiment_dir <- file.path(output_dir, paste0("experimento_", experiment_id))
  dir.create(experiment_dir)
  
  results_rrl_rf <- data.frame(Mtry = numeric(), Error = numeric(), stringsAsFactors = FALSE)
  
  for (mtryv in mtry_values) {
    errors <- numeric(num_runs)
    
    for (i in 1:num_runs) {
      rf_model <- randomForest(
        x = data,
        y = class,
        mtry = mtryv,
        ntree = tree_count
      )
      
      errors[i] <- rf_model$err.rate[nrow(rf_model$err.rate), "OOB"]
    }
    
    elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
    cat("Experimento", experiment_id, "- RF - mtry", mtryv, "- tiempo", elapsed_time, "segundos \n")
    flush.console()
    
    results_rrl_rf <- rbind(results_rrl_rf, data.frame(Mtry = mtryv, Error = mean(errors)))
  }
  
  # Guardar resultados y datos
  save(data, file = file.path(experiment_dir, "data.RData"))
  save(class, file = file.path(experiment_dir, "class.RData"))
  write.csv(results_rrl_rf, file.path(experiment_dir, "resultados.csv"), row.names = FALSE)
}

cat("Todos los experimentos han finalizado.\n")
