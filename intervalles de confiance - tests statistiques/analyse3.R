poids<-poulpeF$Poids
summary(poids)
var(poids)
sd(poids)
hist(poids)
hist(poids,freq=FALSE,plot=FALSE)
boxplot(poids)
sum(poids > 1500)
sum(sort(poids >1500))
sort(poids)

t.test(poids,conf.level = 0.95)$conf.int
t.test(poids,conf.level = 0.90)$conf.in
t.test(poids,conf.level = 0.99)$conf.in

#sondage et satisfaction
# z contient tous les elements de la colonne engagement
z<-sondage$engagements
sum(z)
length(z)

prop.test(sum(z), length(z), conf.level=0.95)$conf.int


taille_filles<-c(156,164,170,165,158,159,168,172,174,162,159,167,169,170,179)
t.test(taille_filles, mu=165)
t.test(taille_filles, mu=165, alternative="less")
t.test(taille_filles, mu=165, alternative="greater")
#les hypothèses a tester sont 
#Hypothèse nulle (H0) : Le nouveau médicament anti-cancéreux n'a pas d'effet 
#significatif sur le taux de développement de la maladie chez les souris.

#Hypothèse alternative (H1) : Le nouveau médicament anti-cancéreux a un effet 
#significatif sur le taux de développement de la maladie chez les souris.
prop.test(17,100,0.20,alternative="less",correct=FALSE)
prop.test(17,100,0.20,alternative="less")


#--------------------
TAB<-matrix(c(10,15,40,135),ncol=2)
rownames(TAB)<-c("Age < 20", "Age > 20")
colnames(TAB)<-c("Poids < 2500g", "Poids > 2500g")
TAB
chisq.test(TAB)

