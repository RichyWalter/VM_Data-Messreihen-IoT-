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


#Namen der Spalten fuer die Plots herausnehmen und speichern
colNamesCPU <- colnames(dfCPU)
colNamesMem <- colnames(dfMem)
colNamesNet <- colnames(dfNet)


#Aufgabe 3
#Generierung einer leeren Matrix mit den gegeben Groessen
generateMatrix <-function(inputDF){
  countSample <- dim(inputDF)[1]
  countVM <- dim(inputDF)[2]
  matOut <- matrix(0, nrow= countVM, ncol=countSample)
  return(matOut) 
}

#Befuellung der Matrix mit Werten
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
#element der korrelationsmatrix ist der Korrelationswert fuer jeweils eine messreihe
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

#HTML-File lokal erzeugen


#Aufgabe 7
#Index eines Elemtes im Vektor anhand seiner Groesse bestimmen
getRankingOfElement <- function(inputVector, position){
  #if(position != 0){
    position <- position - 1  
  #} 
  rankedElem <- (which(inputVector==sort(inputVector,partial=length(inputVector)-position)[length(inputVector)-position]))
  return(rankedElem)
}

#Dichtefunktion ermittlen
getDensityOfRow <- function(row, rowMatrix){
  Vect <- vector()
  for (i in 1:dim(rowMatrix)[2]){
    Vect[i] <- rowMatrix[row,i]
  }
  densityOfRow <- density(Vect)
  return(densityOfRow)
}

plotDensityOfFirstFiveElements <- function(indexVector, elemMatrix, plotTitle = 'First 5 Elements'){
  #Ergebnisse ploten
  # Dafuer zunaechst den Index der Elemente mit den hoechsten Werten ermittlen
  par(mfrow=c(2,3))
  plot(getDensityOfRow(getRankingOfElement(indexVector,1),elemMatrix), col = 'green', main = 'erstes Element')
  plot(getDensityOfRow(getRankingOfElement(indexVector,2),elemMatrix), col ='blue', main = 'zweites Element')
  plot(getDensityOfRow(getRankingOfElement(indexVector,3),elemMatrix), col ='red', main = 'drittes Element')
  plot(getDensityOfRow(getRankingOfElement(indexVector,4),elemMatrix), col ='magenta', main = 'viertes Element')
  plot(getDensityOfRow(getRankingOfElement(indexVector,5),elemMatrix), col ='orange', main = 'fuenftes Element')
  title(plotTitle,line = -1, outer = TRUE)
}

#Aufagbe 7a
#Durchschnittliche CPU-Auslastung berechnen 
means <- rowMeans(matrixCPU[,-1])
plotDensityOfFirstFiveElements(means, matrixCPU,'Dichtefunktion der CPU-Auslastung bei hoechstem Mittelwert')

#Aufgabe 7b
#Varianz der CPU-Auslatung
varianceCPU <- apply(matrixCPU, 1, var)
#Varianz der Speicher-Auslastung
varianceMem <- apply(matrixMem, 1, var)
#Varianz der Netz-Auslastung
varianceNet <- apply(matrixNet, 1, var)
#Plotten der Dichtefunktion der CPU-Auslastung für die VMs mit der höchsten Varianz
plotDensityOfFirstFiveElements(varianceCPU, matrixCPU,'Dichtefunktion der CPU-Auslastung bei hoechster CPU-Varianz')
plotDensityOfFirstFiveElements(varianceMem, matrixCPU,'Dichtefunktion der CPU-Auslastung bei hoechster Speicherauslastugs-Varianz')
plotDensityOfFirstFiveElements(varianceNet, matrixCPU,'Dichtefunktion der CPU-Auslastung bei hoechster Netzauslastungs-Varianz')


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