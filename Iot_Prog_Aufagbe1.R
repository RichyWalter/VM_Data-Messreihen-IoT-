#Programmierung Aufgabe-1
#Internet der Dinge
#Gruppe 1 
#Oliver Graetsch, Franz Huebner,Richard Arnold, Richard Walter


#Aufagbe 2
#read CSV
dfCPU <- read.csv(file="./data/cpu.csv",head=TRUE,sep=";",stringsAsFactors=F)
dfMem <- read.csv(file="./data/mem.csv",head=TRUE,sep=";",stringsAsFactors=F)
dfNet <- read.csv(file="./data/net.csv",head=TRUE,sep=";",stringsAsFactors=F)


#Aufgabe 3
#generate matrix
generateMatrix <-function(inputDF){
  countSample <- dim(inputDF)[1]
  countVM <- dim(inputDF)[2]
  matOut <- matrix(0, nrow= countVM, ncol=countSample)
  return(matOut) 
}

#fill the matrix with values
fillMatrix <- function(inDF, inMat){
  for (i in 1:(dim(inDF)[1])) {
    for (j in 1:(dim(inDF)[2])) {
      inMat[j,i] <- inDF[i, j]
    }
  }
  return(inMat)
}

matrixCPURaw <- generateMatrix(dfCPU)
matrixMemRaw <- generateMatrix(dfMem)
matrixNetRaw <- generateMatrix(dfNet)
matrixCPU <- fillMatrix(dfCPU, matrixCPURaw)
matrixMem <- fillMatrix(dfMem, matrixMemRaw)
matrixNet <- fillMatrix(dfNet, matrixNetRaw)


#Aufgabe 4
#A = matrix(t(c(1,2,4,64,32,8,16,128,256)),3,3)
#jede Spalte der matrix wird als messreihe betrachtet
#element der korrelationsmatrix ist der Korrelationswert für jeweil eine messreihe
calcCorrelation <- function(A){
  result = matrix(c(1:(ncol(A)*ncol(A))),ncol(A),ncol(A))
  for(i in 1:ncol(A)){
    for(j in 1:ncol(A)){
      result[i,j] = cor(A[,i],A[,j])
    }
  }
  return(result)
}
matrixCPUCOR <- calcCorrelation(matrixCPU)
matrixMemCOR <- calcCorrelation(matrixMem)
matrixNetCOR <- calcCorrelation(matrixNet)
#das ist nur ein test um das an einem einfachen Beispiel nachzuvollziehen


#Aufgabe 7a
#Durchschnittliche CPU-Auslastung berechnen
Means <- rowMeans(matrixCPU[,-1])
#Index der Elemente mit dem höchsten Durchschnitt ermittlen
firstElem <- (which(Means==sort(Means,partial=length(Means))[length(Means)]))
secondElem <- (which(Means==sort(Means,partial=length(Means)-1)[length(Means)-1]))
thirdElem <- (which(Means==sort(Means,partial=length(Means)-2)[length(Means)-2]))
fourthElem <- (which(Means==sort(Means,partial=length(Means)-3)[length(Means)-3]))
fifthElem <- (which(Means==sort(Means,partial=length(Means)-4)[length(Means)-4]))

#Verteilungsfunktion ermittlen
getDensityOfRow <- function(row){
  Vect <- vector()
  for (i in 1:dim(matrixCPU)[2]){
    Vect[i] <- matrixCPU[row,i]
  }
  densityOfRow <- density(Vect)
  return(densityOfRow)
}

#Ergebnisse ploten 
plot(getDensityOfRow(firstElem), col = 'green')
lines(getDensityOfRow(secondElem), col ='blue')
lines(getDensityOfRow(thirdElem), col ='red')
lines(getDensityOfRow(fourthElem), col ='magenta')
lines(getDensityOfRow(fifthElem), col ='orange')


#Aufgabe 8 
#Berechnung der Korrelation zwischen CPU und MEM Auslastung
testAufg8 <- cor(matrixCPU, matrixMem)
calcCorrelationTwoMat <- function(mat1, mat2){
  vecMat1 <- vector()
  vecMat2 <- vector()
  
  correlations <- vector()
  for (j in 1:dim(mat1)[1]){
    
    #Werte aus Matrizen in temporaere Vektoren uebertragen
    for (i in 1:dim(mat1)[2]){
      vecMat1[i] <- mat1[j,i]
      vecMat2[i] <- mat2[j,i]
    }
    #Korrelation berechnen un in einen Vektor schreiben
    correlations[j] <- cor(vecMat1, vecMat2)
  }
  return(correlations)
}
