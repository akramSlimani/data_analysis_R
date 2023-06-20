#trouver la valeur de x telle que P(X ≤ x) = 0.975
qnorm(0.975)

#densite de probabilite
dnorm(0)

#alculer la fonction de répartition de 
#la distribution normale pour une valeur donnée
pnorm(1.96)

#génère un vecteur de 20 nombres aléatoires
#d'une distribution normale standard
rnorm(20)

#10 nombres aléatoires provenant d'une distribution normale
#avec une moyenne de 5 et un écart-type de 0.5.
rnorm(10,mean=5,sd=0.5)

x=seq(-3,3,0.1);pdf=dnorm(x);plot(x,pdf,type="l");
runif(3)
rt(5,10)

x1 <- seq(-3, 3, 0.1)
y=pnorm(x1)
plot(x1, y, type = "l", xlab = "x", ylab = "F(x)", main = "Fonction de répartition d'une loi normale centrée réduite")

x2 <- seq(-15, 17, by = 0.1)
densite <- dnorm(x2, mean = 1, sd = 4)
f_repartition <- pnorm(x2, mean = 1, sd = 4)
# Diviser le graphique en deux colonnes
par(mfrow = c(1, 2))
plot(x2, densite, type = "l", main = "Densité de probabilité")
plot(x2, f_repartition, type = "l", main = "Fonction de répartition")

#un échantillon aléatoire de 10 valeurs qui suit une distribution binomiale 
#avec une probabilité de succès de 0,5 et une taille de 1
rbinom(n=10, p=0.5, size=1)
#P(X ≤ 4) avec X ~ B(5, 0.5)
pbinom(4,5,0.5)

#la probabilité que X = 3, où X suit 
#une loi binomiale de paramètres size = 5 et prob = 0,5.
dbinom(3,5,0.5)
#calcule le quantile d'ordre 0,975 de la distribution
#de Student avec 15 degrés de liberté
qt(0.975,15)
# calcule le quantile d'ordre 0.95 de la distribution 
#du chi2 avec 7 degrés de liberté
qchisq(0.95,7)

qnorm(0.975)
qt(0.95,10)
qt(0.95, df = 10)
dbinom(4, 5, 0.3)
dbinom(4, size = 5, prob = 0.3)
qchisq(0.95,10)

x3 <- 0:25
y <- dpois(x3, lambda = 2.4)
result <- cbind(x, y)
print(result)
barplot(y, col="red", space =2)

# Définition du paramètre lambda
lambda <- 4

# Calcul de la loi de probabilité pour x allant de 0 à 25
x <- 0:25
proba <- dpois(x, lambda)

print(proba)
barplot(y, col="red", space =2)

x = rpois(20, 3)
fn=ecdf(x)
plot(fn, verticals=TRUE, do.points=FALSE)

n=100
x = rnorm(n)  
m = cumsum(x)/(1:n)
v=1
q = qnorm(0.975)
ic1= m+q*sqrt(v)/sqrt(1:n)

ic2= m-q*sqrt(v)/sqrt(1:n)

plot(m, type="l")
abline(h=0, col="red")
lines(ic1,col="blue",lty="dotted");lines(ic2,col="blue",lty="dotted")




n <- 100
p <- 0.5
x <- rbinom(n, 1, p)
#mean(x) egal a peu pres la probabilite de succes p
mean(x)


#3.3-------2-------
nbeech<- 50000
tailleech<- 10
moyennes<-rep(0,nbeech)
for(i in 1:nbeech){
  +ech<- rbinom(n=tailleech,p=0.5,size=1)
  +moyennes[i] <- mean(ech)
}

hist(moyennes,freq=FALSE)


nbeech<- 50000
tailleech<- 2
moyennes<-rep(0,nbeech)
for(i in 1:nbeech){
  ech<- rbinom(n=tailleech,p=0.5,size=1)
  moyennes[i] <- mean(ech)
}
hist(moyennes,freq=FALSE)

mean(moyennes)
#var(moyennes) = 0.5*(1-0.5)/2 ///p(1-p)/n
var(moyennes)
0.5*(1-0.5)/2

nbeech<- 50000
tailleech<- 30
moyennes<-rep(0,nbeech)
for(i in 1:nbeech){
  ech<- rbinom(n=tailleech,p=0.5,size=1)
  moyennes[i] <- mean(ech)
}
hist(moyennes,freq=FALSE)
