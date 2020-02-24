####DENSITA'####
# --------
gang.rho = graph.density(gang)
round(gang.rho, 3)


####TRANSITIVITA'####
# --------
gang.c = transitivity(gang)
round(gang.c, 3)

####SUMMARY ATTRIBUTI####

attach(attr)
table(attr$Residence)
table(attr$Prison)
table(attr$Birthplace)
summary(attr$Age)
summary(attr$Arrests)
summary(attr$Convictions)

#Matrice di correlazione####
cor(attr[,-1])

####CENTRALITA'####



####DEGREE CENTRALITY####

#----
cid.gang = rowSums(Y)
cid.st.gang = degree(gang, normalized = T)



####ODDS####

#transitivity
#----
gang.c = transitivity(gang)
gang.c


#Normalized for the comparison

# transitivity -- success and failure probabilities
#----
tr.s = c(gang.c)
tr.f = 1 - c(gang.c)

# density -- success and failure probabilities
#----
de.s = c(gang.rho)
de.f = 1 - c(gang.rho)

# compute the log-odds ratio
#----
odds.tr = tr.s / tr.f
odds.de = de.s / de.f
logodds = log(odds.tr) - log(odds.de)
round(logodds, 3)



#Grafico l1####
#----
dev.new()
par(mfrow=c(3,3))
ord = order(cid.st.gang, decreasing = T)
V(gang)$color = wes_palettes$Zissou1[2]
V(gang)$color[ord[1:4]] = wes_palettes$Zissou1[5]
set.seed(111)
plot(
  gang,
  layout = l1,
  vertex.size = cid.st.gang * 20,
  vertex.label.cex = 1.5,
  overlap = F,
  main = "Degree centrality",
  vertex.label.color = "black",
  asp = 0
)

#Grafico l2####
#----
ord = order(cid.st.gang, decreasing = T)
V(gang)$color = wes_palettes$Zissou1[2]
V(gang)$color[ord[1:4]] = wes_palettes$Zissou1[5]
set.seed(111)
plot(
  gang,
  layout = l2,
  vertex.size = cid.st.gang * 20,
  vertex.label.cex = 1.5,
  overlap = F,
  main = "Degree centrality",
  vertex.label.color = "black",
  asp = 0
)


#Boxplot####
#----
par(mfrow = c(2, 2))
boxplot(
  cid.st.gang ~ (gage),
  main = "Età ",
  col = get.color(gang, E = 4, 1:7),
  names = c("<18", "18", "19", "20", "21-23", ">23")
)

boxplot(
  cid.st.gang ~ (garr),
  main = "Arresti ",
  col = get.color(gang, E = 3, c(1, 2, 3, 4)),
  names = c("<=5", "5-11", "12-16", ">16")
)

boxplot(
  cid.st.gang ~ (gconv),
  main = "Condanne ",
  col = get.color(gang, E = 4, 1:7),
  names = c("0", "1", "2", "3", "4", "5-8", "9", ">9")
)


boxplot(
  cid.st.gang ~ (attr$Birthplace),
  main = "Luogo di nascita",
  col = get.color(gang, E = 3, c(1, 2, 3, 4)),
  names = c("Africa dell'ovest", "Caraibi", "UK", "Africa dell'est")
)


####CLOSENESS CENTRALITY####


cic.st.gang = closeness(gang, normalized = T)


#Grafico l1####
#----
V(gang)$color = wes_palettes$Zissou1[2]
V(gang)$color[ord[1:4]] = wes_palettes$Zissou1[5]
ord = order(cic.st.gang, decreasing = T)
dev.new()

plot(
  gang,
  vertex.label.color = "black",
  layout = l1,
  vertex.size = cic.st.gang * 10,
  edge.arrow.size = 0.5,
  vertex.label.cex = 1.5,
  main = "Closeness centrality",
  asp = 0
)

#Grafico l2####
#----
set.seed(111)
plot(
  gang,
  vertex.label.color = "black",
  layout = l2,
  vertex.size = cic.st.gang * 10,
  edge.arrow.size = 0.5,
  vertex.label.cex = 1.5,
  main = "Closeness centrality",
  asp = 0
)


#####BOXPLOT####
#----
par(mfrow = c(2, 2))
boxplot(
  cic.st.gang ~ (gage),
  main = "Età",
  col = get.color(gang, E = 4, 1:7),
  names = c("<18", "18", "19", "20", "21-23", ">23")
)

boxplot(
  cic.st.gang ~ (garr),
  main = "Arresti",
  col = get.color(gang, E = 3, c(1, 2, 3, 4)),
  names = c("<=5", "5-11", "12-16", ">16")
)

boxplot(
  cic.st.gang ~ (gconv),
  main = "Condanne",
  col = get.color(gang, E = 4, 1:7),
  names = c("0", "1", "2", "3", "4", "5-8", "9", ">9")
)


boxplot(
  cic.st.gang ~ (attr$Birthplace),
  main = "Luogo di nascita",
  col = get.color(gang, E = 3, c(1, 2, 3, 4)),
  names = c("Africa dell'ovest", "Caraibi", "UK", "Africa dell'est")
)


####EIGENVECTOR CENTRALITY####

# standardized eigen vector centrality
#----
cie.st.gang = eigen_centrality(gang, scale = T)$vector


#Grafico l1####
#----
V(gang)$color = wes_palettes$Zissou1[2]
V(gang)$color[ord[1:3]] = wes_palettes$Zissou1[5]
ord = order(cie.st.gang, decreasing = T)
set.seed(111)
plot(
  vertex.label.color = "black",
    gang,
  vertex.size = cie.st.gang * 10,
  layout = l1,
  edge.arrow.size = 0.5,
  
  main = "Eingenvector centrality",
  asp = 0
)




#Grafico l2####
#----
set.seed(111)
plot(
    gang,
    vertex.label.color = "black",
      vertex.size = cie.st.gang * 10,
  layout = l2,
  edge.arrow.size = 0.5,
  main = "Eingenvector centrality",
  asp = 0
)



#####BOXPLOT####
#----
par(mfrow = c(2, 2))
boxplot(
  cie.st.gang ~ (gage),
  main = "Età",
  col = get.color(gang, E = 4, 1:7),
  names = c("<18", "18", "19", "20", "21-23", ">23")
)

boxplot(
  cie.st.gang ~ (garr),
  main = "Arresti",
  col = get.color(gang, E = 3, c(1, 2, 3, 4)),
  names = c("<=5", "5-11", "12-16", ">16")
)

boxplot(
  cie.st.gang ~ (gconv),
  main = "Condanne",
  col = get.color(gang, E = 4, 1:7),
  names = c("0", "1", "2", "3", "4", "5-8", "9", ">9")
)


boxplot(
  cie.st.gang ~ (attr$Birthplace),
  main = "Luogo di nascita",
  col = get.color(gang, E = 3, c(1, 2, 3, 4)),
  names = c("Africa dell'ovest", "Caraibi", "UK", "Africa dell'est")
)


#####NETWORK CENTRALIZATION####
#----
centralize(cid.gang, theoretical.max = (n - 1) * (n - 2))
centralize(cic.st.gang, theoretical.max = n - 1)
centralize(cib.st.gang, theoretical.max = 0.5 * (n - 1) * (n - 2))