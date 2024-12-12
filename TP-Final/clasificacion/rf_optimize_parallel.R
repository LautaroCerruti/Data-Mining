library(randomForest)
library(future)
library(future.apply)

future.seed=TRUE

# Cargar los datos
load("./train_general_x.RData")
load("./train_general_y.RData")

# Configurar plan de ejecución paralela
plan(multisession)

start_time <- Sys.time()

results_rf <- data.frame(Mtry = numeric(), Error = numeric(), stringsAsFactors = FALSE)

mtry_values <- floor((ncol(train_general_x) - 1) * (1 / 2)^(0:5))
errors_avg <- c()
tree_count <- 1000
num_runs <- 5

# Función para calcular errores promedio para un valor de mtry
compute_errors <- function(mtryv) {
  errors <- future_sapply(1:num_runs, function(i) {
    rf_model <- randomForest(
      x = train_general_x,
      y = train_general_y,
      mtry = mtryv,
      ntree = tree_count
    )
    rf_model$err.rate[nrow(rf_model$err.rate), "OOB"]
  })
  mean(errors)
}

# Calcular errores para cada valor de mtry en paralelo
for (mtryv in mtry_values) {
  avg_error <- compute_errors(mtryv)

  elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
  cat("RF - mtry", mtryv, "- tiempo", elapsed_time, "segundos - error", avg_error, "\n")
  flush.console()

  errors_avg <- c(errors_avg, avg_error)
  results_rf <- rbind(results_rf, data.frame(Mtry = mtryv, Error = avg_error))
}

plan(sequential)

save(results_rf, file = "results_rf.RData")
