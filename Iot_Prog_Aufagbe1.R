#Programmierung Aufgabe-1
#Internet der Dinge
#Gruppe 1 
#Oliver Graetsch, Franz Huebner,Richard Arnold, Richard Walter


#Aufagbe 1
#read CSV
DF_cpu <- read.csv(file="./data/cpu.csv",head=TRUE,sep=";",stringsAsFactors=F)
DF_mem <- read.csv(file="./data/mem.csv",head=TRUE,sep=";",stringsAsFactors=F)
DF_net <- read.csv(file="./data/net.csv",head=TRUE,sep=";",stringsAsFactors=F)