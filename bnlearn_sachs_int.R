rm(list=ls())

library(bnlearn)
library(tidyverse)

# This code follows:
#Scutari, M. (2022, November 30). 
# Reproducing the causal signalling network in Sachs et al., Science (2005).
# Bnlearn - an R Package for Bayesian Network Learning and Inference. 
# https://www.bnlearn.com/research/sachs05/


#load the interventional data
isachs = read.table("sachs_data_processed/sachs.interventional.txt",header = TRUE, colClasses = "factor")

summary(isachs$INT)

# create whitelist
wh = matrix(c(rep("INT", 11), names(isachs)[1:11]), ncol = 2)

dag.wh = bnlearn::tabu(isachs, whitelist = wh, score = 'bde', 
                       iss = 10, tabu = 50)
bnlearn::graphviz.plot(dag.wh, layout = "dot")

tiers = list("INT", names(isachs)[1:11])
bl = bnlearn::tiers2blacklist(tiers = tiers)
dag.tiers = tabu(isachs, blacklist = bl, score = 'bde', iss = 1, tab = 50)
bnlearn::graphviz.plot(dag.tiers, layout = "dot")


INT = sapply(1:11, function(x){
  which(isachs$INT == x)
})

nodes = names(isachs)[1:11]
names(INT) = nodes

start = random.graph(nodes = nodes, 
                     method = "melancon",
                     num = 500,
                     burn.in = 10^5, every = 100)

netlist = lapply(start, function(net){
  tabu(isachs[, 1:11], score = "mbde", exp = INT,
       iss = 1, start = net, tabu = 50)})

intscore = custom.strength(netlist, nodes = nodes, cpdag = FALSE)
dag.mbde = averaged.network(intscore)
bnlearn::graphviz.plot(dag.mbde, layout = "dot")

# get the sachs network
sachs.modelstring <- paste("[PKC][PKA|PKC][Raf|PKC:PKA][Mek|PKC:PKA:Raf]",
                          "[Erk|Mek:PKA][Akt|Erk:PKA][P38|PKC:PKA]",
                          "[Jnk|PKC:PKA][Plcg][PIP3|Plcg][PIP2|Plcg:PIP3]", sep = "")

dag.sachs = bnlearn::model2network(sachs.modelstring)
bnlearn::graphviz.plot(dag.sachs, layout = "dot")

unlist(compare(dag.sachs, dag.mbde))
