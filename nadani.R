library(psych)
library(lavaan)


# Nastavení pro test IST-2000-R (korelace a reliability jsou přebrány z německé verze) ---------------------------------------------------------------

set.seed(211)

lower <- "
.52 
.47	.59 
.34	.49	.44 
.30	.45	.41	.61 
.34	.45	.44	.69	.63 
.27	.39	.42	.40	.43	.49 
.20	.29	.30	.32	.38	.42	.54 
.25	.39	.35	.32	.43	.42	.48	.48 
"

cortab <- getCov(lower, names=c("SC","VA","VS","CA","NS","SI","FS","CU","MA"), diagonal = F) ## vytvoří korelační tabulku

prior = c(0,1) ## vzorek dětí v poradně; v z-skórech, formát (M,SD)

# Reliabilita -------------------------------------------------------------


rel <- c(.69,.70,.77,.87,.91,.89,.77,.81,.73)

relindexy <- c(
  1- sum(1-rel[7:9])/(1- sum(1-rel[7:9]) + 2*sum(lower.tri(cortab[7:9,7:9]))) , 
  1- sum(1-rel[4:6])/(1- sum(1-rel[4:6]) + 2*sum(lower.tri(cortab[4:6,4:6]))) , 
  1- sum(1-rel[7:9])/(1- sum(1-rel[7:9]) + 2*sum(lower.tri(cortab[7:9,7:9]))) , 
  1- sum(1-rel)/(1- sum(1-rel) + 2*sum(lower.tri(cortab)))
)


# Simulace ----------------------------------------------------------------

simulace <- prior[1] + prior[2]*sim.correlation(cortab, n=10000, data=T)
index <- cbind(rowMeans(simulace[,1:3]), rowMeans(simulace[,4:6]), rowMeans(simulace[,7:9]), rowMeans(simulace))

simulace_horni <- t(t(simulace) + 1.96*sqrt(1-rel))
simulace_dolni <- t(t(simulace) - 1.96*sqrt(1-rel))

index_horni <- t(t(index) + 1.96*sqrt(1-relindexy))
index_dolni <- t(t(index) - 1.96*sqrt(1-relindexy))


# Identifikován na základě indexu -----------------------------------------

identifikovan <- cbind(
  bodove = as.logical(rowSums(index > 2)),
  horni = as.logical(rowSums(index_horni > 2)),
  dolni = as.logical(rowSums(index_dolni > 2))
)

colMeans(identifikovan) ## vypočte podíl identifikovaných dětí


# Přidání kritéria subtestu ---------------------------

identifikovan_subtest <- as.logical(rowSums(simulace > 2))
mean(identifikovan_subtest)

identifikovan_subtest_dolni <- as.logical(rowSums(t(t(simulace)-1.96*rel) > 2))

identifikovan2 <- cbind(
  bodove = as.logical(identifikovan[,1]+identifikovan_subtest),
  horni = as.logical(identifikovan[,2]+identifikovan_subtest),
  dolni = as.logical(identifikovan[,3]+identifikovan_subtest)
)
colMeans(identifikovan2) ## vypočte podíl identifikovaných dětí

identifikovan2_dolni <- cbind(
  bodove = as.logical(identifikovan[,1]+identifikovan_subtest_dolni),
  horni = as.logical(identifikovan[,2]+identifikovan_subtest_dolni),
  dolni = as.logical(identifikovan[,3]+identifikovan_subtest_dolni)
)
colMeans(identifikovan2_dolni) ## vypočte podíl identifikovaných dětí

