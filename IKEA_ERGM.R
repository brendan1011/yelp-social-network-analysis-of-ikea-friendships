# install libraries
needed_packages <- c("sna", "network", "ergm", "coda", "statnet","tidyverse","igraph","intergraph","GGally","stargazer")                      
# Extract not installed packages
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
# Install not installed packages
if(length(not_installed)) install.packages(not_installed) 

library(tidyverse)
library(ergm)
library(sna)
library(coda)
library(statnet)
library(GGally)
library(ggplot2)
# library(igraph)
# library(intergraph)

setwd("/home/kent/college/Semester3/THUR_1830_SNA/pycharm_project")

# IKEA.adjacency <- as.matrix(read.csv("IKEA_adjacency.csv"))
# dim(IKEA.adjacency)
# IKEA.adjacency <- IKEA.adjacency[,2:1505]
# dim(IKEA.adjacency)
# table(IKEA.adjacency)
# 
# IKEA.mat <- as.matrix(read.csv("IKEA.csv"))
# dim(IKEA.mat)
# IKEA.net <- network(IKEA.mat, matrix.type = "edge", directed = FALSE)
# 
# IKEA.net <- network(IKEA.adjacency, matrix.type = "adjacency")

# plot(IKEA.net)

IKEA.edge <- as.data.frame(read.csv("IKEA.csv"))
dim(IKEA.edge)
IKEA.attrib <- read.csv("df_IKEA_users_wth_friends.csv")
colnames(IKEA.attrib)

# Simple example network similar to your data?
# org.unit <- data.frame(name =c("Jane",     "Tom",     "David",       "Jay", "Brian", "Christian",  "Tim"),
#                        grade=c(    11,        11,            9,          9,       8,           7,      5),
#                        org  =c(  "HR", "Finance", "Marketing", "Marketing",   "GTO",       "GTO", "Bank"))
# 
# relations <- data.frame(from=c("Jane",  "Jane",  "Jane",       "Jay",   "Jay", "David"),
#                         to = c( "Jay", "Brian", "David", "Christian", "David", "Christian"))

# Make a graph from the relations
IKEA.igraph <- igraph::graph.data.frame(IKEA.edge, directed=FALSE)

# Set vertex atributes
igraph::V(IKEA.igraph)$average_stars <- sapply(igraph::V(IKEA.igraph)$name, function(x) IKEA.attrib $average_stars[IKEA.attrib $user_id == x])
igraph::V(IKEA.igraph)$funny <- sapply(igraph::V(IKEA.igraph)$name, function(x) IKEA.attrib $funny[IKEA.attrib $user_id == x])
igraph::V(IKEA.igraph)$fans <- sapply(igraph::V(IKEA.igraph)$name, function(x) IKEA.attrib $fans[IKEA.attrib $user_id == x])
igraph::V(IKEA.igraph)$cool <- sapply(igraph::V(IKEA.igraph)$name, function(x) IKEA.attrib $cool[IKEA.attrib $user_id == x])
igraph::V(IKEA.igraph)$useful <- sapply(igraph::V(IKEA.igraph)$name, function(x) IKEA.attrib $useful[IKEA.attrib $user_id == x])
igraph::V(IKEA.igraph)$state <- sapply(igraph::V(IKEA.igraph)$name, function(x) IKEA.attrib $state[IKEA.attrib $user_id == x])
igraph::V(IKEA.igraph)$west <- sapply(igraph::V(IKEA.igraph)$name, function(x) IKEA.attrib $west[IKEA.attrib $user_id == x])
# igraph::V(gD)$org.unit <- sapply(igraph::V(gD)$name, function(x) as.character(org.unit$org[org.unit$name == x]))
plot(IKEA.igraph)

# Look at it:
# igraph::V(gD)$grade


plot(IKEA.igraph ,
     displaylabels = FALSE,
     vertex.cex = 1, 
     vertex.col = 'state'
)


IKEA.net = intergraph::asNetwork(igraph::simplify(IKEA.igraph))
set.network.attribute(IKEA.net, "multiple", FALSE)

IKEA.igraph.florida <- igraph::delete_vertices(IKEA.igraph, igraph::V(IKEA.igraph)$state != "FL")
IKEA.net.florida = intergraph::asNetwork(igraph::simplify(IKEA.igraph.florida))
set.network.attribute(IKEA.net.florida, "multiple", FALSE)

IKEA.igraph.OR <- igraph::delete_vertices(IKEA.igraph, igraph::V(IKEA.igraph)$state != "OR")
IKEA.net.OR = intergraph::asNetwork(igraph::simplify(IKEA.igraph.OR))
set.network.attribute(IKEA.net.OR, "multiple", FALSE)

plot(IKEA.net, 
     vertex.col = "state", 
     vertex.cex = 1,
     pad = 3)


plot(IKEA.net
     , displaylabels = FALSE
     , vertex.cex = sna::degree(IKEA.net) / 200
     , vertex.col = 'state'
     , vertex.sides = ifelse(IKEA.net %v% 'west', 4, 50)
     , pad = 1
)


# ggnet2(IKEA.net, color = "state", palette = "Set2", alpha = 0.75, size = 4, edge.alpha = 0.5)
ggnet2(IKEA.net, color = "state", palette = "Set2", alpha = 0.85, size = "degree", edge.alpha = 0.5) +
  coord_equal() +
  guides(size = "none")

ggnet2(IKEA.net.florida, color = "state", palette = "Set2", alpha = 0.85, size = "degree", edge.alpha = 0.5) +
  coord_equal() +
  guides(size = "none")

ggnet2(IKEA.net.OR, color = "state", palette = "Set3", alpha = 0.85, size = "degree", edge.alpha = 0.5) +
  coord_equal() +
  guides(size = "none")


m1 = ergm(IKEA.net ~ edges)
summary(m1)
#models info
# names(m1)
#list of coefficient
coef(m1)
#use plogis to get the probability associated with a a coefficient
#the probability of the edges coefficient is:
plogis(coef(m1)[[1]])
plogis(coef(m1)[['edges']])
#can be also computed with the usual formula
#p = e^logit(p)/(1+e^logit(p))
exp(coef(m1)[1])/(1+exp(coef(m1)[1]))
#it should be equal to network density
network.density(IKEA.net)

summary(IKEA.net ~ edges + nodecov("average_stars"))
summary(IKEA.net ~ edges + absdiff("average_stars"))
summary(IKEA.net ~ edges + nodefactor("state"))
summary(IKEA.net ~ edges + nodematch("state"))

m2 = ergm(IKEA.net ~ edges + triangles + )
summary(m2)

summary(IKEA.net ~ edges + absdiff("average_stars"))
m2 = ergm(IKEA.net ~ edges + absdiff("average_stars"))
summary(m2)
plogis(coef(m2)[['edges']])
plogis(coef(m2)[['edges']] + coef(m2)[['absdiff.average_stars']])

m2 = ergm(IKEA.net ~ edges + nodecov("average_stars"))
summary(m2)
plogis(coef(m2)[['edges']])
plogis(coef(m2)[['edges']] + coef(m2)[['nodecov.average_stars']])

m3 = ergm(IKEA.net ~ edges + nodematch("state"))
summary(m3)
plogis(coef(m3)[['edges']])
plogis(coef(m3)[['edges']] + coef(m3)[['nodematch.state']])

m3 = ergm(IKEA.net ~ edges + nodematch("west"))
summary(m3)
plogis(coef(m3)[['edges']])
plogis(coef(m3)[['edges']] + coef(m3)[['nodematch.west']])

m4 = ergm(IKEA.net ~ edges + nodematch("west") + nodecov("funny") + nodecov("cool") + nodecov("useful")  + nodematch("state"))
summary(m4)
plogis(coef(m4)[['edges']] + coef(m4)[['nodematch.west']]  + coef(m4)[['nodecov.funny']]  + coef(m4)[['nodecov.cool']] + coef(m4)[['nodecov.useful']])
plogis(coef(m4)[['edges']] + coef(m4)[['nodematch.west']]  + coef(m4)[['nodecov.funny']]  + coef(m4)[['nodecov.cool']] + coef(m4)[['nodecov.useful']] + coef(m4)[['nodematch.state']] )

m5 = ergm(IKEA.net ~ edges + kstar(2))
summary(m5)
plogis(coef(m5)[['edges']])
plogis(coef(m5)[['edges']] + coef(m5)[['degree']])