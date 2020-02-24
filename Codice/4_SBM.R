#CAMBIA VERSIONE R

####LIBRERIA####
#----
library(mixer)



####FUNZIONE####
#----
assegna_gruppi = function(distr) {
  gruppi = NULL
  gruppi[distr <= quantile(distr)[2]] = 1
  gruppi[distr > quantile(distr)[2] & distr <= quantile(distr)[3]] = 2
  gruppi[distr > quantile(distr)[3] & distr <= quantile(distr)[4]] = 3
  gruppi[distr > quantile(distr)[4] & distr <= quantile(distr)[5]] = 4
  
  return(gruppi)
}



####STOCHASTIC BLOCK MODEL####
#----
diag(Y) = 0

# estimate parameters of the SBM
#----
sbm = mixer(Y,
            qmin = 2,
            qmax = 10,
            directed = F)

# let us select the optimal model
#----
out = getModel(sbm)

# how many blocks?
#----
out$q

# prior block probabilities
#----
round(out$alpha, 3)

# tie  probabilities
#----
round(out$Pis, 3)
# what can we say?

# how are nodes classified?
#----
bl = apply(out$Taus, 2, which.max)
bl


####ATTRIBUTI####
#----
attr <-
  read.csv("C:/Users/chiar/Desktop/Progetto Londra/Nuova cartella/LONDON_GANG.txt")


table(bl, attr$Birthplace)


#--Specify layout for 2 plots (1 for science category legend)
layout(matrix(1:2, 2, 1, byrow = TRUE), heights = c(1, 0.4))

#--Since the X & Y data are factors, "plot" produces a "spineplot()"




degree = assegna_gruppi(cid.gang)

closer = assegna_gruppi(cic.st.gang)
eigenc = assegna_gruppi(cie.st.gang)


dev.new()
# Widen right margin slightly, to fit horizontal labels
plot(
  as.factor(bl) ~ as.factor(degree) ,
  data = attr,
  col = c("#B3E2CD", "#FDCDAC", "#CBD5E8", "light blue"),
  main = "Centralità di grado",
  xlab = "Quantili",
  ylab = "Gruppi latenti"
)

plot(
  as.factor(bl) ~ as.factor(closer) ,
  data = attr,
  col = c("#B3E2CD", "#FDCDAC", "#CBD5E8", "light blue"),
  main = "Closness centrality",
  xlab = "Quantili",
  ylab = "Gruppi latenti"
)


plot(
  as.factor(bl) ~ as.factor(eigenc) ,
  data = attr,
  col = c("#B3E2CD", "#FDCDAC", "#CBD5E8", "light blue"),
  main = "Eigenvector centrality",
  xlab = "Quantili",
  ylab = "Gruppi latenti"
)
