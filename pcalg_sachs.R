rm(list=ls())

library(pcalg)
library(readxl)
library(corrplot)

#load the discretized interventional data by Marco Scutari
isachs = read.table("sachs_data_processed/sachs.interventional.txt",header = TRUE)

# isachs = isachs[isachs$INT != 0, ]

target_l = unique(isachs$INT)

isachs$index = rep(0, nrow(isachs))

for (i in 1:length(target_l)){
  
  isachs[isachs$INT == target_l[i],]$index = i
  
}

target_l = as.list(target_l)
target_l[[2]] = integer(0)

score = new("GaussL0penIntScore", isachs[, 1:11],
            targets = target_l, target.index = isachs$index)

gies.fit = gies(score)
plot(gies.fit$essgraph)


# check edges

for (i in gies.fit$essgraph$`.->.in.edges`$Raf){
  
  print(paste(gies.fit$essgraph$`.->.nodes`[i], " --> ", "Raf"))
  
}

for (i in gies.fit$essgraph$`.->.in.edges`$Mek){
  
  print(paste(gies.fit$essgraph$`.->.nodes`[i], " --> ", "Mek"))
  
}

for (i in gies.fit$essgraph$`.->.in.edges`$Plcg){
  
  print(paste(gies.fit$essgraph$`.->.nodes`[i], " --> ", "Plcg"))
  
}

for (i in gies.fit$essgraph$`.->.in.edges`$PIP2){
  
  print(paste(gies.fit$essgraph$`.->.nodes`[i], " --> ", "PIP2"))
  
}

for (i in gies.fit$essgraph$`.->.in.edges`$PIP3){
  
  print(paste(gies.fit$essgraph$`.->.nodes`[i], " --> ", "PIP3"))
  
}

for (i in gies.fit$essgraph$`.->.in.edges`$Erk){
  
  print(paste(gies.fit$essgraph$`.->.nodes`[i], " --> ", "Erk"))
  
}

for (i in gies.fit$essgraph$`.->.in.edges`$Akt){
  
  print(paste(gies.fit$essgraph$`.->.nodes`[i], " --> ", "Akt"))
  
}

for (i in gies.fit$essgraph$`.->.in.edges`$PKA){
  
  print(paste(gies.fit$essgraph$`.->.nodes`[i], " --> ", "PKA"))
  
}

for (i in gies.fit$essgraph$`.->.in.edges`$PKC){
  
  print(paste(gies.fit$essgraph$`.->.nodes`[i], " --> ", "PKC"))
  
}

for (i in gies.fit$essgraph$`.->.in.edges`$P38){
  
  print(paste(gies.fit$essgraph$`.->.nodes`[i], " --> ", "P38"))
  
}

for (i in gies.fit$essgraph$`.->.in.edges`$Jnk){
  
  print(paste(gies.fit$essgraph$`.->.nodes`[i], " --> ", "Jnk"))
  
}
