####LIBRERIE####
# --------
library(igraph)
library(readr)
library(qgraph)
library(network)
library(wesanderson)
library(ergm)







####FUNZIONE####
# --------
get.color = function(net, E, tri) {
  if (E == 1) {
    P = E(net)$weight
    P[is.na(P)] = 5
    R = matrix(wes_palettes$GrandBudapest2,
               ncol = 4,
               nrow = length(E(net)$weight))
    
    E(net)$color = R[P]
    
    E(net)$color[is.na(E(net)$weight)] = NA
    return(E(net)$color)
    
  } else if (E == 2) {
    P = tri + 1
    P[is.na(P)] = 2
    R = matrix(rep(wes_palettes$Zissou1, length(tri)),
               ncol = 5,
               nrow = length(tri))
    
    V(net)$color = R[P + 3]
    
    V(net)$color[is.na(tri)] = NA
    return(V(net)$color)
  } else if (E == 3) {
    P = tri + 1
    P[is.na(P)] = 2
    R = matrix(rep(wes_palettes$Darjeeling1, length(tri)),
               ncol = ,
               nrow = length(tri))
    
    V(net)$color = R[P]
    
    V(net)$color[is.na(tri)] = NA
    return(V(net)$color)
  }
  
  
  
}







####DATI####

#set directory####
# --------
setwd("C:\\Users\\chiar\\Desktop\\TESI TROVA NOME\\Codice\\Dati")

#set data####
# --------
Y = as.matrix(read_csv("LONDON_GANG.csv", col_types = cols(X1 = col_skip())))

Y[Y > 1 & is.na(Y) != 1] = 1

#generate graph####
# --------
gang = graph_from_adjacency_matrix(Y, mode = c("undirected"))
n = vcount(gang)

#set other data####
# --------
attr = read_csv("LONDON_GANG_ATTR.csv")
attr = attr[, -c(8, 9)]

#check####
# --------
head(attr)

#add vertex attributes
# --------
V(gang)$Residence = attr$Residence
V(gang)$Prison = attr$Prison
V(gang)$Birthplace = attr$Birthplace
V(gang)$Age = attr$Age
V(gang)$Arrests = attr$Arrests
V(gang)$Conviction = attr$Convictions




####VISUALIZZAZIONE####

#add visualizzation option####
# --------
V(gang)$size <- 5
V(gang)$frame.color <- wes_palettes$Zissou1[1]
V(gang)$color <- wes_palettes$Zissou1[2]
V(gang)$label <- V(gang)$name
E(gang)$color = wes_palettes$Zissou1[3]
set.seed(111)

#set two layout####
# --------
l1 = layout_as_tree(gang)
l2 = layout_with_fr(gang)


#Grafo with fr####
# --------
dev.new()

plot.igraph(
  gang,
  layout = l1,
  vertex.label.color = "black",
  main = "Network dei membri criminali della gang di londra",
  asp = 0
)



#Grafo as tree####
# --------
dev.new()
plot.igraph(
  gang,
  layout = l2,
  vertex.label.color = "black",
  main = "Network dei membri criminali della gang di londra",
  asp = 0
)
