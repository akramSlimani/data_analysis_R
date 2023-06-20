#matrice de corrélation
cor(ozone.m)
#arrondir a 3 chiffres apres la virgule
core<-round(cor(ozone.m),3)
core

cor(ozone$maxO3,ozone$T9)
cor(ozone$maxO3,ozone$T12)
cor(ozone$maxO3,ozone$T15)
cor(ozone$maxO3,ozone$Ne9)
cor(ozone$maxO3,ozone$Ne12)
cor(ozone$maxO3,ozone$Ne15)
cor(ozone$maxO3,ozone$Vx9)
cor(ozone$maxO3,ozone$Vx12)
cor(ozone$maxO3,ozone$Vx15)
cor(ozone$maxO3,ozone$maxO3v)
#la variable la plus corrélée à maxO3 est T12
plot(maxO3~T12,data=ozone)
points(mean(ozone$T12),mean(ozone$maxO3),col="Red")
reg.simple<- lm(maxO3~T12,data=ozone)
reg.simple
abline(reg.simple)
summary(reg.simple)
reg.simple$coefficients
coef(reg.simple)
reg.simple$fitted.values
reg.simple$residuals
residuals(reg.simple)
reg.simple$residuals
rstandard(reg.simple)
res.simple<-rstudent(reg.simple)
plot(reg.simple$residuals)
lm(maxO3~maxO3v, data=ozone)
lm(maxO3~T12, data=ozone)
plot(maxO3~maxO3v,data=ozone)


reg<- lm(maxO3~maxO3v,data=ozone)
reg
abline(reg, col="Red")
points(mean(ozone$maxO3),mean(ozone$maxO3v),col="Red")
reg$residuals
hist(reg$residuals)


diabet <- data.frame(patient, blood.glucose, short.velocity)
diabet
patient<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)
blood.glucose<-c(15.30,10.80,8.10,19.50,7.20,5.30,9.30,11.10,7.50,12.20,6.70,5.20,19.00,15.10,6.70,4.20,10.30,12.50,16.10,13.30,4.90,8.80,9.50)
short.velocity<-c(1.76,1.34,1.27,1.47,1.27,1.49,1.31,1.09,1.18,1.22,1.25,1.19,1.95,1.28,1.52,1.12,1.37,1.19,1.05,1.32,1.03,1.12,1.70)
cor(blood.glucose,short.velocity)
plot(short.velocity~blood.glucose,data=diabet)
points(mean(diabet$blood.glucose),mean(diabet$short.velocity),col="Red")
regr<- lm(short.velocity~blood.glucose,data=diabet)
regr
abline(regr, col="Red")
x<-aov(short.velocity~blood.glucose)
x

regexo3<-lm(blood.glucose~short.velocity, data=diabet)
anova_result <- anova(regexo3)
anova_result

regr<- lm(short.velocity~blood.glucose,data=diabet)
regr
abline(regr, col="Red")
