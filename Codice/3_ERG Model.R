####SETTING####
#----
# to start, let us transform the adjacency matrix in a network object
diag(Y) = NA
net = network(Y , direct = F)
class(net)
net
summary(net)


####MODELLO NULLO####
#----
mod0 = brg = ergm(net ~ edges, estimate = "MLE")
summary(brg)


odds = exp(brg$coef)
odds

########

####MARKOV GRAPH MODEL####

# add network and edge attributes
# ----
net %v% "Residence" = attr$Residence
net %v% "Prison" = attr$Prison
net %v% "Birthplace" = attr$Birthplace
net %v% "Age" = attr$Age
net %v% "Arrests" = attr$Arrests
net %v% "Conviction" = attr$Convictions


####MODELLO ARCO GWESP####
#----
modcov1 = ergm(net ~ edges + gwesp(decay = 0.3, fixed = T) +
                 nodematch("Birthplace")+nodemain("Birthplace") ,
               control = control.ergm(seed = 1))

####MODELLO NO LUOGO NASCITA####
#----
modcov2 = ergm(
  net ~ edges + gwesp(decay = 0.3, fixed = T) + nodecov("Age") +
    nodecov("Arrests") + nodecov("Conviction"),
  control = control.ergm(seed = 1)
)

####MODELLO COMPLETO####
#----
modcov3 = ergm(
  net ~ edges + gwesp(decay = 0.3, fixed = T) +
    nodematch("Birthplace") + nodecov("Age") +
    nodecov("Arrests") + nodecov("Conviction")+nodemain("Birthplace"),
  control = control.ergm(seed = 1)
)

####SUMMARY MODELLI####
#----
summary(brg)
summary(modcov1)
summary(modcov2)
summary(modcov3)

####MCMC DIAGNOSTIC####
#----
mcmc.diagnostics(modcov3)

####GODNESS OF FITNESS####
#----
# in terms of geodesic distance
dev.new()
gof.mod = gof(modcov3 ~ distance + triadcensus)
par(mfrow = c(1, 2), mar = c(5, 4, 3, 3))
plot(gof.mod)
