# Parámetros
costs <- c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)
gammas <- c(0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000)
degrees <- 1:5

# Generar todas las combinaciones posibles de cost, gamma y degree
all_combinations <- expand.grid(Cost = costs, Gamma = gammas, Degree = degrees)

# Cargar los datos del CSV
params <- read.csv("salida.csv")

# Asegurar que los nombres de columnas coincidan con el formato esperado
colnames(params) <- c("Cost", "Gamma", "Degree", "Error")

# Buscar combinaciones faltantes comparando filas completas
missing_combinations <- all_combinations[!apply(all_combinations, 1, function(row) {
  any(
    params$Cost == as.numeric(row["Cost"]) &
    params$Gamma == as.numeric(row["Gamma"]) &
    params$Degree == as.numeric(row["Degree"])
  )
}), ]

# Imprimir las combinaciones faltantes
print(missing_combinations)
write.csv(missing_combinations, "missing_combinations.csv", row.names = FALSE)