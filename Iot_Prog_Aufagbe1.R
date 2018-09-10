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

#aufgabe 6

#Funktion um einen 3 dimensionalen Array aus den 3 Zeilenmatrizen zu generieren
generateThreeDimArray <- function(vmMatrix, samplesMatrix, ressourcenMatrix) {

    #array (zeile,spalte,dimension)
    threeDimArray <- array(c(vmMatrix, samplesMatrix, ressourcenMatrix), c(nrow(vmMatrix), ncol(vmMatrix), 3))

    return(threeDimArray)
}

#der 3d Array mit aus cpu mem und net
threeDimArray <- generateThreeDimArray(matrixCPU, matrixMem, matrixNet)

#hier fehlt noch die Einbindung einer Library und ein 3D-Plott


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
