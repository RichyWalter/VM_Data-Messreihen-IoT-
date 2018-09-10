#Programmierung Aufgabe-1
#Internet der Dinge
#Gruppe 1 
#Oliver Graetsch, Franz Huebner,Richard Arnold, Richard Walter


#Aufagbe 1
#read CSV
#DF_cpu <- read.csv(file="./data/cpu.csv",head=TRUE,sep=";",stringsAsFactors=F)
#DF_mem <- read.csv(file="./data/mem.csv",head=TRUE,sep=";",stringsAsFactors=F)
#DF_net <- read.csv(file="./data/net.csv",head=TRUE,sep=";",stringsAsFactors=F)

#Aufgabe 4
A = matrix(t(c(1,2,4,64,32,8,16,128,256)),3,3)
A
#jede Spalte der matrix wird als messreihe betrachtet
#element der korrelationsmatrix ist der Korrelationswert für jeweil eine messreihe
calcKorrelationsMatrix <- function(A){
  cor(A)
}
calcCorrelation <- function(A){
  result = matrix(c(1:(ncol(A)*ncol(A))),ncol(A),ncol(A))
  for(i in 1:ncol(A)){
    for(j in 1:ncol(A)){
      result[i,j] = cor(A[,i],A[,j])
    }
  }
  return(result)
}
#das ist nur ein test um das an einem einfachen Beispiel nachzuvollziehen
calcKorrelationsMatrix(A)
result = calcCorrelation(A)
result