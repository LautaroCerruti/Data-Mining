library(MASS)
library(rpart)
library(class)
library(knitr)
library(future.apply)


gen_uniform_dataset <- function(data) {
  dataset_uniform <- c()
  n_row <- nrow(data)
  n_col <- ncol(data)

  data_pca <- prcomp(data)$x

  for (c in 1:n_col) {
    col <- runif(n_row, min = min(data_pca[,c]), max = max(data_pca[,c]))
    dataset_uniform <- cbind(dataset_uniform, col)
  }

  return(dataset_uniform)
}

gap_statistic <- function(data, maxK=10, B=100, nstart=20, iter.max = 100) {
  W_k <- c()
  Gaps <- c()
  s_k <- c()

  for (k in 1:(maxK+1)) {
    C_k <- suppressWarnings(kmeans(data, k, nstart=nstart, iter.max=iter.max))
    W_k[k] <- C_k$tot.withinss

    W_k_b <- c()
    for (b in 1:B) {
      d_b <- gen_uniform_dataset(data)
      C_k_b <- suppressWarnings(kmeans(d_b, k, nstart=nstart, iter.max=iter.max))
      W_k_b[b] <- C_k_b$tot.withinss
    }

    # Calculate Gap
    l <- mean(log(W_k_b))
    Gaps <- c(Gaps, (l - log(W_k[k])))

    # SD
    suma <- 0
    for (b in 1:B) {
      suma <- suma + (log(W_k_b[b])-l)^2
    }
    sd <- sqrt(suma/B)

    # S_k
    s_k[k] <- sd*(sqrt(1+1/B))
  }

  # Selecting k
  for (k in 1:maxK) {
    if (Gaps[k] >= (Gaps[k+1]-s_k[k+1])) {
      return(k)
    }
  }
  return(1)
}

stability_score <- function(n, cc1, cc2, ind1, ind2) {
  #pongo los clusters de nuevo en longitud n - quedan 0 los puntos fuera del sample.
  #Sumo 5 a las etiquetas para que valga el truco que la raiz de multiplicar las "clases" es un numero entero solo cuando tienen el mismo numero, vale para la cantidad de clusters que buscamos siempre.
  v1<-v2<-rep(0,n)
  v1[ind1]<-cc1+5
  v2[ind2]<-cc2+5
  #creo una matriz m con 1 donde los dos puntos estan en el mismo cluster, -1 en distinto cluster y 0 si alguno no esta, para cada clustering
  a<-sqrt(v1%*%t(v1))
  m1<-a / -a + 2*(a==round(a))
  m1[is.nan(m1)]<-0
  a<-sqrt(v2%*%t(v2))
  m2<-a / -a + 2*(a==round(a))
  m2[is.nan(m2)]<-0
  #calculo el score, los pares de puntos que estan en la misma situacion en los dos clustering dividido el total de pares validos.
  validos<-sum(v1*v2>0)
  score<-sum((m1*m2)[upper.tri(m1)]>0)/(validos*(validos-1)/2)
  return(score)
}

stability <- function(data, maxK=10, solutions=10, percentage=0.8, nstart=10, iter.max = 50) {
  n <- nrow(data)
  indexes <- lapply(1:solutions, function(i) sample(n, percentage * n))

  k_score <- numeric(maxK - 1)

  plan(multisession)

  for(k in 2:maxK) {
    clusters <- lapply(indexes, function(ind) {
      suppressWarnings(kmeans(data[ind, ], centers = k, nstart = nstart, iter.max = iter.max)$cluster)
    })

    scores <- unlist(future_lapply(1:(solutions - 1), function(i) {
      sapply((i + 1):solutions, function(j) {
        stability_score(n, clusters[[i]], clusters[[j]], indexes[[i]], indexes[[j]])
      })
    }))

    k_score[k - 1] <- mean(scores)
  }

  plan(sequential)

  # Busco el mayor k con el mayor score promedio
  k_greatest_score <- length(k_score) - which.max(rev(k_score)) + 2

  return(k_greatest_score)
}

load("./data.RData")
load("./class.RData")
start_time <- Sys.time()

data_scaled <- scale(data)
data_scaled_pca <- prcomp(data_scaled)
data_scaled_pca_scaled <- scale(data_scaled_pca$x)
data_pca <- prcomp(data)
data_pca_scaled <- scale(data_pca$x)

elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
cat("Start - tiempo", elapsed_time, "segundos \n")
flush.console()

# k_gap_data <- gap_statistic(data)
# elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
# cat("Finished gap data - tiempo", elapsed_time, "segundos \n")
# flush.console()
# save(
#   k_gap_data, 
#   file = "k_gap_data.RData"
# )

# k_gap_data_scaled <- gap_statistic(data_scaled)
# elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
# cat("Finished gap data_scaled - tiempo", elapsed_time, "segundos \n")
# flush.console()
# save(
#   k_gap_data_scaled, 
#   file = "k_gap_data_scaled.RData"
# )

# k_gap_data_scaled_pca <- gap_statistic(data_scaled_pca$x)
# elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
# cat("Finished gap data_scaled_pca - tiempo", elapsed_time, "segundos \n")
# flush.console()
# save(
#   k_gap_data_scaled_pca, 
#   file = "k_gap_data_scaled_pca.RData"
# )

# k_gap_data_scaled_pca_scaled <- gap_statistic(data_scaled_pca_scaled)
# elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
# cat("Finished gap data_scaled_pca_scaled - tiempo", elapsed_time, "segundos \n")
# flush.console()
# save(
#   k_gap_data_scaled_pca_scaled, 
#   file = "k_gap_data_scaled_pca_scaled.RData"
# )

# k_gap_data_pca <- gap_statistic(data_pca$x)
# elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
# cat("Finished gap data_pca - tiempo", elapsed_time, "segundos \n")
# flush.console()
# save(
#   k_gap_data_pca, 
#   file = "k_gap_data_pca.RData"
# )

# k_gap_data_pca_scaled <- gap_statistic(data_pca_scaled)
# elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
# cat("Finished gap data_pca_scaled - tiempo", elapsed_time, "segundos \n")
# flush.console()
# save(
#   k_gap_data_pca_scaled, 
#   file = "k_gap_data_pca_scaled.RData"
# )

k_stability_data <- stability(data)
elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
cat("Finished stability data - tiempo", elapsed_time, "segundos \n")
flush.console()
save(
  k_stability_data, 
  file = "k_stability_data.RData"
)

k_stability_data_scaled <- stability(data_scaled)
elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
cat("Finished stability data_scaled - tiempo", elapsed_time, "segundos \n")
flush.console()
save(
  k_stability_data_scaled, 
  file = "k_stability_data_scaled.RData"
)

k_stability_data_scaled_pca <- stability(data_scaled_pca$x)
elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
cat("Finished stability data_scaled_pca - tiempo", elapsed_time, "segundos \n")
flush.console()
save(
  k_stability_data_scaled_pca, 
  file = "k_stability_data_scaled_pca.RData"
)

k_stability_data_scaled_pca_scaled <- stability(data_scaled_pca_scaled)
elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
cat("Finished stability data_scaled_pca_scaled - tiempo", elapsed_time, "segundos \n")
flush.console()
save(
  k_stability_data_scaled_pca_scaled, 
  file = "k_stability_data_scaled_pca_scaled.RData"
)

k_stability_data_pca <- stability(data_pca$x)
elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
cat("Finished stability data_pca - tiempo", elapsed_time, "segundos \n")
flush.console()
save(
  k_stability_data_pca, 
  file = "k_stability_data_pca.RData"
)

k_stability_data_pca_scaled <- stability(data_pca_scaled)
elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
cat("Finished stability data_pca_scaled - tiempo", elapsed_time, "segundos \n")
flush.console()
save(
  k_stability_data_pca_scaled, 
  file = "k_stability_data_pca_scaled.RData"
)

cat("Todos los objetos se han guardado en 'k_results.RData'.\n")
