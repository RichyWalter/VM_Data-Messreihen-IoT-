#Programmierung Aufgabe-1
#Internet der Dinge
#Gruppe 1: Oliver Graetsch, Franz Huebner,Richard Arnold, Richard Walter

#Einbindung von Plotly zur besseren Visualisierung der Metriken
library(plotly)


#Aufagbe 1-2
#Lesen Sie die Messreihen und speichern Sie sie als Datenrahmen (Data Frame).

#Einlesen der csv Dateien und Erzeugung der frames
dfCPU <- read.csv(file="./data/cpu.csv",head=TRUE,sep=";",stringsAsFactors=F)
dfMem <- read.csv(file="./data/mem.csv",head=TRUE,sep=";",stringsAsFactors=F)
dfNet <- read.csv(file="./data/net.csv",head=TRUE,sep=";",stringsAsFactors=F)


#Namen der Spalten fuer die Plots herausnehmen und speichern
colNamesCPU <- colnames(dfCPU)
colNamesMem <- colnames(dfMem)
colNamesNet <- colnames(dfNet)

rowNamesCPU <- rownames(dfCPU)
rowNamesMem <- rownames(dfMem)
rowNamesNet <- rownames(dfNet)


#Aufgabe 3
#Generiere NxM Matrizen aus den Datenrahmen. 
#N ist die Anzahl der virtuellen Maschinen und M ist die Anzahl der Samples.
#Es gibt also drei Matrizen: eine fuer die CPU, eine fuer MEM und eine fuer NET. 



#Befuellung der Matrix mit Werten
fillMatrix <- function(inDF) {

    #Erstellung einer Leermatrix mit den gleichen Größen
    countSample <- dim(inDF)[1]
    countVM <- dim(inDF)[2]
    matOut <- matrix(0, nrow = countVM, ncol = countSample)

    #Befuellung der Rohmatrix mit den Werten aus dem DataFrame
    for (i in 1:(dim(inDF)[1])) {
        for (j in 1:(dim(inDF)[2])) {
            matOut[j,i] <- inDF[i, j]
        }
    }

    return(matOut)
}


#Befuellung der Matrizen mit den Werten aus den globalen Variablen
matrixCPU <- fillMatrix(dfCPU)
matrixMem <- fillMatrix(dfMem)
matrixNet <- fillMatrix(dfNet)

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
    htmlwidgets::saveWidget(as.widget(p),paste(graphName, ".html ", sep = ""))

}

createHeatmap(matrixCPU,colNamesCPU,"CPUHeatmap")
createHeatmap(matrixMem,colNamesMem,"MEMORYHeatmap")
createHeatmap(matrixNet,colNamesNet,"NETHeatmap")

#Aufgabe 4
#Generieren eine Korrelationsmatrix(ATA) für jede Rohmatrix. 
#Lesen Sie ueber eine Korrelationmatrix und erklaeren Sie ihre Bedeutung
#(Verwenden Sie statistische Werkzeuge fuer ihre Diskussion)

#Jede Spalte der Matrix wird als Messreihe betrachtet.
#Element der Korrelationsmatrix ist der Korrelationswert fuer jeweils eine Messreihe.
calcCorrelation <- function(A) {

    result = matrix(c(1:(ncol(A)*ncol(A))),ncol(A),ncol(A))

    for(i in 1:ncol(A)){
        for(j in 1:ncol(A)){
            result[i,j] = cor(A[,i],A[,j])
        }
    }

    return(result)
}


#Korrelationsmatrizen für die Ressourcen
matrixCPUCOR <- calcCorrelation(matrixCPU)
matrixMemCOR <- calcCorrelation(matrixMem)
matrixNetCOR <- calcCorrelation(matrixNet)


#erstellen der Korrelationsplots
createHeatmap(matrixCPUCOR, c(1:179), "CPUCorrelationHeatmap")
createHeatmap(matrixMemCOR, c(1:179), "MEMCorrelationHeatmap")
createHeatmap(matrixNetCOR, c(1:179), "NETCorrelationHeatmap")


#Aufgabe 5
#Erstellen Sie eine standardisierte Matrix fuer jede Rohmatrix. Es gibt eine Standardfunktion namens scale,
#die eine standartisierte Matrix generieren kann. Vergleichen Sie ihte Ausgabe mit der Ausgabe dieser Funktion.
#Dokumentieren Sie Ihre Beobachtung.



#Funktion um die Funktionen zu vergleichen
compareFunc <- function(whichMatrix) {

    #Eigene Funktion um standardisierte Matrix zu erzeugen
    myScale <- function(A) {

        for(i in 1:ncol(A)){
            b =A[,i]
            A[,i] = (b - mean(b))/ sd(b) 
        }

        return (A)
    }

    #Vergleich zweier Matrizen
    matEqual <- function(x, y) {

        #Fehlertoleranz von 10^-3 ist moeglich
        return(is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && isTRUE(all.equal(x, y, check.attributes = FALSE)))

    }

    #eigene scaled Matrizen erstellen
    matrixCPUScaled <- t(myScale(t(matrixCPU)))
    matrixMemScaled <- t(myScale(t(matrixMem)))
    matrixNetScaled <- t(myScale(t(matrixNet)))

    if (whichMatrix == "CPU") {
        x <- matrixCPUScaled
        y <- matrixCPU
        z <- "CPU"
    }else if(whichMatrix == "MEM") {
        x <- matrixMemScaled 
        y <- matrixMem
        z <- "MEM"
    }else if (whichMatrix == "NET") {
        x <- matrixNetScaled
        y <- matrixNet
        z <- "NET"
    }

    #Vergleich mit der geschriebenen Funktion
    if (matEqual(x, t(scale(t(y))))) {
        print(paste("Die Ergebnisse der integrierten Funktion und der eigenen Funktion sind gleich fuer die Matrizen", z))
    } else {
        print(paste("Eigene Funktion fehlerhaft fuer die Matrizen",y))
    }

}

#Funktion aufrufen um zu vergleichen
compareFunc("CPU")
compareFunc("MEM")
compareFunc("NET")



#Aufgabe 6
#Generieren Sie ein 3-d-Array aus den drei Zeilenmatrizen(VMS vs Samples vs Ressourcen)

#Funktion um ein 3 dimensionales Feld(Array) aus den 3 Zeilenmatrizen zu generieren
generateThreeDimArray <- function(vmMatrix, samplesMatrix, ressourcenMatrix) {

    #Array (Zeile,Spalte,Dimension)
    threeDimArray <- array(c(vmMatrix, samplesMatrix, ressourcenMatrix), c(nrow(vmMatrix), ncol(vmMatrix), 3))

    return(threeDimArray)
}

#Der 3d Array aus CPU, Mem und Net
threeDimArray <- generateThreeDimArray(matrixCPU, matrixMem, matrixNet)

#Aufgabe 7
#Plotten Sie die Dichtefunktion der CPU-Auslastung fuer die folgenden virtuellen Maschinen auf:

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

#Verteilungsfunktion der groessten Elemente einer Ressourcenmatrix plotten
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
#Die fuenf virtuellen Maschinen, deren durchschnittliche CPU-Auslastung am hoechsten ist

#Durchschnittliche CPU-Auslastung berechnen 
means <- rowMeans(matrixCPU[,-1])
plotDensityOfFirstFiveElements(means, matrixCPU,'Dichtefunktion der CPU-Auslastung bei hoechstem Mittelwert')

#Aufgabe 7b
#Die fuenf virtuellen Maschinen, deren Abweichungen am hoechsten sind

#Varianz der CPU-Auslatung
varianceCPU <- apply(matrixCPU, 1, var)
#Varianz der Speicher-Auslastung
varianceMem <- apply(matrixMem, 1, var)
#Varianz der Netz-Auslastung
varianceNet <- apply(matrixNet, 1, var)
#Plotten der Dichtefunktion der CPU-Auslastung fuer die VMs mit der hoechsten Varianz
plotDensityOfFirstFiveElements(varianceCPU, matrixCPU,'Dichtefunktion der CPU-Auslastung bei hoechster CPU-Varianz')
plotDensityOfFirstFiveElements(varianceMem, matrixCPU,'Dichtefunktion der CPU-Auslastung bei hoechster Speicherauslastugs-Varianz')
plotDensityOfFirstFiveElements(varianceNet, matrixCPU,'Dichtefunktion der CPU-Auslastung bei hoechster Netzauslastungs-Varianz')


#Aufgabe 8 
#Gibt es eine Korrelation zwischen der Auslastung von CPU und Mem? Demostrieren Sie quantitatiy

#Berechnung der Korrelation zwischen CPU und MEM Auslastung
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

correlationMatrix <- calcCorrelationTwoMat(matrixCPU, matrixMem)

#Barplott der Korrelationen fuer jede VM
barplot(correlationMatrix, ylim = c(-0.2, 0.2), names.arg = colNamesCPU, las = 2)

#function um den Plot zu erzeugen
createBarplot <- function(correlationInputMatrix, graphName) {

    nummer <- c(1:length(correlationInputMatrix))
    vec <- as.vector(correlationInputMatrix)
    data <- data.frame(nummer, vec)
    correlation <- correlationInputMatrix

    p <- plot_ly(data, x = ~nummer, y = ~correlation, name = 'Korrelationsverlauf', type = 'scatter', mode = 'markers')

    #HTML-File lokal erzeugen
    htmlwidgets::saveWidget(as.widget(p), paste(graphName, ".html ", sep = ""))

}

#Funktion zur Erstellung eines Boxplots
createBoxplot <- function(correlationInputMatrix, graphName) {

    p <- plot_ly(x = ~correlationInputMatrix,y= "Messreihe", type = 'box', boxpoints = 'all', jitter = 0.3, pointpos = -1.8)
    #HTML-File lokal erzeugen
    htmlwidgets::saveWidget(as.widget(p), paste(graphName, ".html ", sep = ""))

}


#Plots zu Aufgabe 8 erzeugen
createBarplot(correlationMatrix, "CPU-Mem_correlation")
createBoxplot(correlationMatrix, "CPU-Mem_corr_boxplot")