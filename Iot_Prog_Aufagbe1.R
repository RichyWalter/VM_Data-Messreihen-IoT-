#Programmierung Aufgabe-1
#Internet der Dinge
#Gruppe 1: Oliver Graetsch, Franz Huebner,Richard Arnold, Richard Walter

#Einbindung von Plotly zur besseren Visualisierung der Metriken
library(plotly)


#Aufagbe 1-2  
#Einlesen der csv Dateien und Erzeugung der frames
dfCPU <- read.csv(file="./data/cpu.csv",head=TRUE,sep=";",stringsAsFactors=F)
dfMem <- read.csv(file="./data/mem.csv",head=TRUE,sep=";",stringsAsFactors=F)
dfNet <- read.csv(file="./data/net.csv",head=TRUE,sep=";",stringsAsFactors=F)


#Namen der Spalten f�r die Plots herausnehmen und speichern
colNamesCPU <- colnames(dfCPU)
colNamesMem <- colnames(dfMem)
colNamesNet <- colnames(dfNet)


#Aufgabe 3
#Generierung einer leeren Matrix mit den gegeben Groe�en
generateMatrix <-function(inputDF){
  countSample <- dim(inputDF)[1]
  countVM <- dim(inputDF)[2]
  matOut <- matrix(0, nrow= countVM, ncol=countSample)
  return(matOut) 
}

#Bef�llung der Matrix mit Werten
fillMatrix <- function(inDF, inMat){
  for (i in 1:(dim(inDF)[1])) {
    for (j in 1:(dim(inDF)[2])) {
      inMat[j,i] <- inDF[i, j]
    }
  }
  return(inMat)
}


#Erstellung der Matrizen
matrixCPURaw <- generateMatrix(dfCPU)
matrixMemRaw <- generateMatrix(dfMem)
matrixNetRaw <- generateMatrix(dfNet)

#Befuellung der Matrizen mit den Werten aus den globalen Variablen
matrixCPU <- fillMatrix(dfCPU, matrixCPURaw)
matrixMem <- fillMatrix(dfMem, matrixMemRaw)
matrixNet <- fillMatrix(dfNet, matrixNetRaw)

#Funktion um Plots zu den Matrizen zu erzeugen
createHeatmap <- function(inputMatrix, inputColNames, graphName) {

    #Font definieren
    f <- list(
        family = "Courier New, monospace",
        size = 18,
        color = "#7f7f7f"
    )

    #x-Achse Beschriften
    x <- list(
        title = "Messwerte",
        titlefont = f
    )

    #Plot generieren
    p <- plot_ly(
        y = inputColNames,
        z = inputMatrix, colors = colorRamp(c("green", "red")), type = "heatmap"
    ) %>%
    layout(title= graphName, xaxis = x)

    #HTML-File lokal erzeugen
    htmlwidgets::saveWidget(as.widget(p), paste(graphName,".html",sep = ""))

}

createHeatmap(matrixCPU,colNamesCPU,"CPUHeatmap")
createHeatmap(matrixMem,colNamesMem,"MEMORYHeatmap")
createHeatmap(matrixNet,colNamesNet,"NETHeatmap")

#Aufgabe 4

#A = matrix(t(c(1,2,4,64,32,8,16,128,256)),3,3)
#jede Spalte der matrix wird als messreihe betrachtet
#element der korrelationsmatrix ist der Korrelationswert f�r jeweil eine messreihe
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

#Aufgabe 5
myScale <- function(A){
  for(i in 1:ncol(A)){
    b =A[,i]
    A[,i] = (b - mean(b))/ sd(b) 
  }
  return (A)
}

matequal <- function(x, y)
  return(is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y))

#small test begin
m = matrix((1:9),3,3)
scale(m)
myScale(m)
if(matequal(scale(m),myScale(m))){
  print("cpu scale and myscale results are equal")
}else
  print("myScale() failed")

#end test

#vergleich beginn
matrixCPUScaled <- t(myScale(t(matrixCPU))) 
if(matequal(matrixCPUScaled,t(scale(t(matrixCPU))))){
  print("cpu scale and myscale results are equal")
}else{
  print("myScale() failed")
}
matrixCPUScaled[,1]
blub = t(scale(t(matrixCPU)))
blub[,1]

matrixMemScaled <- t(myScale(t(matrixMem)))
if(matequal(matrixMemScaled,t(scale(t(matrixMem))))){
  print("c scale and myscale reesults are equal")
}else
  print("myScale() failed")

matrixNetScaled <- t(myScale(t(matrixNet)))
if(matequal(matrixNetScaled,t(scale(t(matrixNet))))){
  print("Net scale and myscale reesults are equal")
}else
  print("myScale() failed")
#vergleich ende


#aufgabe 6

#Funktion um einen 3 dimensionalen Array aus den 3 Zeilenmatrizen zu generieren
generateThreeDimArray <- function(vmMatrix, samplesMatrix, ressourcenMatrix) {

    #array (zeile,spalte,dimension)
    threeDimArray <- array(c(vmMatrix, samplesMatrix, ressourcenMatrix), c(nrow(vmMatrix), ncol(vmMatrix), 3))

    return(threeDimArray)
}

#der 3d Array mit aus cpu mem und net
threeDimArray <- generateThreeDimArray(matrixCPU, matrixMem, matrixNet)




#Aufgabe 7a
#Durchschnittliche CPU-Auslastung berechnen
Means <- rowMeans(matrixCPU[,-1])
#Index der Elemente mit dem h�chsten Durchschnitt ermittlen
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
par(mfrow=c(2,3))
plot(getDensityOfRow(firstElem), col = 'green', main = 'erstes Element')
plot(getDensityOfRow(secondElem), col ='blue', main = 'zweites Element')
plot(getDensityOfRow(thirdElem), col ='red', main = 'drittes Element')
plot(getDensityOfRow(fourthElem), col ='magenta', main = 'viertes Element')
plot(getDensityOfRow(fifthElem), col ='orange', main = 'f�nftes Element')


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