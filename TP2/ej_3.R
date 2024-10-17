#library(ggplot2)
#library(rpart)
#library(class)
if (!require(randomForest)) {
  install.packages("randomForest", dependencies = T)
}
library(randomForest)
if (!require(kernlab)) {
  install.packages("kernlab", dependencies = T)
}
library(kernlab)
library(MASS)

#-------------------------------------------------------------------------------------
# AVISO: codigo de demostracion
# No es optimo, no es la mejor solucion
#
#Funcion general para forward greedy feature selection
#input:
# x,y inputs y targets
# method es una funcion externa que estima el classification error para un dado clasificador con esos datos
# ... parametros extra para el metodo method
#output:
#list.feat <-numero de orden de importancia de las variables, primero la mas relevante
#-------------------------------------------------------------------------------------
forward.ranking <- function(x,y,method,verbosity=0,... )
{
	max.feat<-dim(x)[2]		#total de features
	num.feat<-1				#numero actual de features
	list.feat<-1:max.feat   #lista para guardar los features elegidos, inicializo como llegaron

	#ranking inicial: elijo la variable con menor error de prediccion
	class.error<-double(max.feat)   #inicializo el vector para guardar el error de cada modelo
	#para cada i, creo el dataset con esa variable sola, entreno un modelo y le mido el error, que lo guardo en class.error[i]
	for(i in 1:max.feat){
		x.train<-x[,i,drop=F]
		class.error[i] <- do.call(method, c(list(x.train, y), list(...)) )
	}
	#guardo la variable con minimo error como primera en mi lista de elegidas. Guardo una lista keep.feat con las que me quedan para seguir eligiendo.
	list.feat[1]<-which.min(class.error)
	keep.feat<-sort(class.error,decreasing=FALSE,index=T)$ix[-1]

	if(verbosity>1) cat("\nFirst feature: ",list.feat[1],"\n")

    #loop principal. A cada paso agrego todas las variables disponibles, de a una, le mido el error y me quedo con la de minimo error. Hasta llegar a meter todas.
	while(num.feat<max.feat){
        #class.error guarda el error de cada modelo. Son max.feat-num.feat modelos.
		class.error<-double(max.feat-num.feat)
		#para cada variable que me queda, la agrego a la lista del paso anterior, entreno el modelo y le mido el error.
		for(i in 1:(max.feat-num.feat)){
		    features<-c(list.feat[1:num.feat],keep.feat[i])
			x.train<-x[,features]
			class.error[i] <- do.call(method, c(list(x.train, y), list(...)) )
		}
		if(verbosity>2) cat("\nFeatures:\n",keep.feat,"\nErrors:\n",class.error)
		#me quedo con el modelo de minimo error, guardo ese feature en la lista de las elegidas, lo saco de la lista de las que quedan.
		best.index<-which.min(class.error)
		list.feat[num.feat+1]<-keep.feat[best.index]
		if(verbosity>1) cat("\n---------\nStep ",1+num.feat,"\nFeature ",best.index)

		keep.feat<-keep.feat[-best.index]
		if(verbosity>2) cat("\nNew search list: ",keep.feat)
		num.feat<-num.feat+1 #proximo paso
	}

	if(verbosity>0){
		cat("\nFinal ranking ")
		cat("\nFeatures: ",list.feat)
	}

 	return(list.feat)
}

#-------------------------------------------------------------------------------------
#Funcion general para backward greedy feature selection
#input:
# x,y inputs y targets
# method es una funcion externa que estima el classification error para un dado clasificador con esos datos
# ... parametros extra para el metodo method
#output:
#list.feat <-numero de orden de importancia de las variables, primero la mas relevante
#-------------------------------------------------------------------------------------
backward.ranking <- function(x, y, method, verbosity=0, ...) {
	max.feat <- dim(x)[2]      # total de features
	num.feat <- max.feat       # número actual de features
	list.feat <- 1:max.feat    # lista para guardar los features elegidos, inicializo con todos
	final.feat <- c()

	# Inicialmente, entreno el modelo con todas las variables y mido el error
	x.train <- x
	class.error <- double(max.feat)  # inicializo el vector para guardar el error de cada modelo
	full.error <- do.call(method, c(list(x.train, y), list(...)))

	if (verbosity > 1) cat("\nInitial error with all features: ", full.error, "\n")

	# Loop principal. En cada paso, elimino una variable, entreno el modelo y mido el error.
	while (num.feat > 1) {
		class.error <- double(num.feat)

		# Para cada variable que queda, la elimino, entreno el modelo y mido el error.
		for (i in 1:num.feat) {
			features <- list.feat[-i]  # quito una variable
			x.train <- x[, features,drop=F]
			class.error[i] <- do.call(method, c(list(x.train, y), list(...)))
		}

		if (verbosity > 2) cat("\nFeatures:\n", list.feat, "\nErrors:\n", class.error)

		# Me quedo con la eliminación que minimiza el aumento del error.
		worst.index <- which.min(class.error)
		if (verbosity > 1) cat("\n---------\nStep ", max.feat - num.feat + 1, "\nFeature removed: ", list.feat[worst.index], "\n")

		final.feat <- c(final.feat, list.feat[worst.index])
		list.feat <- list.feat[-worst.index]  # elimino el feature de la lista
		num.feat <- num.feat - 1  # actualizo el número de features

		if (verbosity > 2) cat("\nNew feature list: ", list.feat)
	}

	final.feat <- rev(c(final.feat, list.feat[1]))

	if (verbosity > 0) {
		cat("\nFinal ranking ")
		cat("\nFeatures: ", final.feat)
	}

	return(final.feat)
}

# Función para la selección de características usando el test Kruskal-Wallis
# input:
# x: matriz de features
# y: vector de respuesta (debe ser categórico para Kruskal-Wallis)
# verbosity: nivel de detalle en los mensajes impresos
# output:
# ranked.features: índices de las características ordenadas por importancia
kruskal.ranking <- function(x, y, verbosity = 0) {
  max.feat <- dim(x)[2]  # total de features
  statistics <- double(max.feat)  # vector para almacenar los p-valores de cada característica

  # Para cada característica, aplico el test Kruskal-Wallis y guardo el p-valor
  for (i in 1:max.feat) {
    feature <- x[, i]
    statistics[i] <- kruskal.test(feature ~ y)$statistic
    if (verbosity > 1) cat("\nFeature ", i, " statistic: ", statistics[i])
  }

  # Ordeno las características según el p-valor (menor p-valor es mayor importancia)
  ranked.features <- order(statistics, decreasing=T)

  if (verbosity > 0) {
		cat("\nFinal ranking ")
    cat("\nFeatures: ", ranked.features)
    if (verbosity > 1) cat("\nStatictics: ", statistics[ranked.features], "\n")
  }

  return(ranked.features)
}

# Función para selección de características usando Recursive Feature Elimination (RFE)
# input:
# x: matriz de features
# y: vector de respuesta
# method: función para entrenar un modelo que calcule la importancia de las características
# ...: parámetros adicionales para el método 'method'
# verbosity: nivel de detalle en los mensajes impresos
# output:
# R: índices de las características seleccionadas, ordenadas de más a menos importante
rfe.ranking <- function(x, y, method, verbosity = 0, ...) {
  p <- dim(x)[2]  # total de features
  list.feat <- 1:p
  R <- c()
  for(i in 1:p) {
	model <- do.call(method, c(list(x[, list.feat, drop = FALSE], y), list(...)))
	least.important <- model$feats[1]

	if (verbosity > 1) {
      cat("\nIteration with", length(list.feat), "features.")
	  cat("\nFeatures: ", list.feat)
	  cat("\nFeatures indexes: ", model$feats)
      cat("\nFeature importances: ", model$imp)
      cat("\nRemoving feature: ", list.feat[least.important], "\n")
    }

	R[p-i+1] <- list.feat[least.important]
	list.feat <- list.feat[-least.important]
  }

  if (verbosity > 0) {
		cat("\nFinal ranking ")
    cat("\nFeatures: ", R)
  }

  return(R)
}

#---------------------------------------------------------------------------
#random forest error estimation (OOB) for greedy search
#---------------------------------------------------------------------------
rf.est <- function(x.train,y,equalize.classes=TRUE,tot.trees=500,mtry=0)
{
	if(mtry<1) mtry<-floor(sqrt(dim(x.train)[2]))
	prop.samples<-table(y)
	if(equalize.classes) prop.samples<-rep(min(prop.samples),length(prop.samples))
	return( randomForest(x.train,y,mtry=mtry,ntree=tot.trees,sampsize=prop.samples)$err.rate[tot.trees] )
}

#---------------------------------------------------------------------------
#SVM error estimation (internal CV) for greedy search
#---------------------------------------------------------------------------
svm.est <- function(x.train,y,type="C-svc",kernel="vanilladot",C=1,cross = 4)
{
  invisible(capture.output(model <- ksvm(as.matrix(x.train), as.factor(y), type=type,kernel=kernel,C=C,cross = cross)))
	return ( model@cross )
}

#---------------------------------------------------------------------------
#random forest ranking method for rfe.
#---------------------------------------------------------------------------
imp.rf <- function(x.train,y,equalize.classes=TRUE,tot.trees=500,mtry=0)
{
	if(mtry<1) mtry<-floor(sqrt(dim(x.train)[2]))
	prop.samples<-table(y)
	if(equalize.classes) prop.samples<-rep(min(prop.samples),length(prop.samples))

	m.rf<-randomForest(x.train,y,ntree=tot.trees,mtry=mtry,sampsize=prop.samples,importance=TRUE)
	imp.mat<-importance(m.rf)
	imp.col<-dim(imp.mat)[2]-1
	rank.list<-sort(imp.mat[,imp.col],decreasing=FALSE,index=T)
	return(list(feats=rank.list$ix,imp=rank.list$x))
}


#---------------------------------------------------------------------------
#linear svm ranking method for rfe. Using kernlab. Multiclass
#---------------------------------------------------------------------------
imp.linsvm <- function(x.train,y,C=100)
{
	num.feat<-dim(x.train)[2]
	tot.problems<-nlevels(y)*(nlevels(y)-1)/2

	invisible(capture.output(m.svm <- ksvm(as.matrix(x.train), y, type="C-svc",kernel="vanilladot",C=C)))

	w<-rep(0.0,num.feat)
	for(i in 1:tot.problems) for(feat in 1:num.feat)
		w[feat]<-w[feat]+abs(m.svm@coef[[i]] %*% m.svm@xmatrix[[i]][,feat])
	rank.list<-sort(w,decreasing=FALSE,index=T)
	return(list(feats=rank.list$ix,imp=rank.list$x))
}

# Función para crear la matriz de covarianza
covM <- function(d, C) diag((C * sqrt(d))**2, d)

diagonales <- function(n, d, C) {
  # Clase 0
  c0 <- mvrnorm(n = n / 2, mu = rep(-1, d), Sigma = covM(d, C))
  c0 <- cbind(c0, 0)
  # Clase 1
  c1 <- mvrnorm(n = n / 2, mu = rep(1, d), Sigma = covM(d, C))
  c1 <- cbind(c1, 1)

  flist <- rbind(c0, c1)

  # Crear DataFrame
  df <- as.data.frame(flist)
  colnames(df) <- c(paste0("dim_", 0:(d - 1)), "class")

  df$class <- as.factor(df$class)

  return(df)
}

crea.ruido.diags<-function(n=100,d=2, min=-1, max=1){
  x<-runif(n*d,min=min,max=max)	#genero los datos
  dim(x)<-c(n,d)
  return(as.data.frame(x))
}

# Decidí que el rango de las variables uniformes sea de -1-(3*sd) a 1+(3*sd)
generateNoisyDiag <- function(d, n, C, nd) {
  datos_relevantes <- diagonales(n, d, C)
  ruido <- crea.ruido.diags(n, nd, -1-(C * sqrt(d)*3), 1+(C * sqrt(d)*3))
  datos <- cbind(cbind(datos_relevantes[,-(d+1)], ruido), as.factor(datos_relevantes[,d+1]))
  return(datos)
}

d <- 10
n <- 100
C <- 2
nd <- 90
num_reps <- 30
methods <- c("forward_RF", "forward_SVM", "backward_RF", "backward_SVM", "kruskal_wallis", "RFE_RF", "RFE_SVM")

resultados_aciertos <- matrix(0, nrow = num_reps, ncol = length(methods))
colnames(resultados_aciertos) <- methods
verbosity = 1
start_time <- Sys.time()

for (rep in 1:num_reps) {
  datos <- generateNoisyDiag(d, n, C, nd)

  # Aplicar cada método
  for (metodo in methods) {
    if (verbosity > 0) {
      elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
      cat("Rep", rep, "- method", metodo, "- tiempo", elapsed_time, "segundos \n")
      flush.console()
    }
    ranking <- switch(metodo,
                      "forward_RF" = {
                        # Código para el método forward usando Random Forest
                        forward.ranking(datos[, 1:(d + nd)], datos[,d+nd+1], method = "rf.est",tot.trees=100,equalize.classes=F)
                      },
                      "forward_SVM" = {
                        # Código para el método forward usando SVM
                        forward.ranking(datos[, 1:(d + nd)], datos[,d+nd+1], method = "svm.est")
                      },
                      "backward_RF" = {
                        # Código para el método backward usando Random Forest
                        backward.ranking(datos[, 1:(d + nd)], datos[,d+nd+1], method = "rf.est",tot.trees=100,equalize.classes=F)
                      },
                      "backward_SVM" = {
                        # Código para el método backward usando SVM
                        backward.ranking(datos[, 1:(d + nd)], datos[,d+nd+1], method = "svm.est")
                      },
                      "kruskal_wallis" = {
                        # Código para el método Kruskal-Wallis
                        kruskal.ranking(datos[, 1:(d + nd)], datos[,d+nd+1])
                      },
                      "RFE_RF" = {
                        # Código para RFE usando Random Forest
                        rfe.ranking(datos[, 1:(d + nd)], datos[,d+nd+1], method = "imp.rf",tot.trees=100,equalize.classes=F)
                      },
                      "RFE_SVM" = {
                        # Código para RFE usando SVM
                        rfe.ranking(datos[, 1:(d + nd)], datos[,d+nd+1], method = "imp.linsvm")
                      },
                      stop("Método no válido")
    )

    # Calcular el número de aciertos (cuántas de las 10 relevantes están en los primeros 10 lugares)
    aciertos <- sum(ranking[1:10] <= d)
    resultados_aciertos[rep, metodo] <- aciertos
  }
}

if (verbosity > 0) {
  elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
  cat("Termino en", elapsed_time, "segundos \n")
  flush.console()
}

save(resultados_aciertos, file = "resultados_aciertos.RData")