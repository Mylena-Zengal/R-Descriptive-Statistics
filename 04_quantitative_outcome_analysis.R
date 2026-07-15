### Load the dataset
################################################################################


poidsNais <- read.table("poidsNais.txt",header=TRUE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

head(poidsNais)

# Birth weight according to maternal smoking status
################################################################################

Table <- table(poidsNais$cigjourcl)
Table  # counts for cigjourcl

# Option 1
prop.table(Table)
# Option 2
100*Table/sum(Table)
# Display percentages with two decimal places
round(100*Table/sum(Table), 2)
# Display percentages with the % symbol
paste0(round(100*Table/sum(Table), 2),"%")

# Scatter plot
plot(poidsnais~cigjourNum, 
xlab="Nombre de cigarettes par jour", ylab="Poids ? la naissance", 
data=poidsNais)

# Boxplot
boxplot(poidsnais~cgjour2cl, 
xlab="Consommation de cigarettes (Oui/Non)", ylab="Poids ? la naissance", 
data=poidsNais)

# Birth weight distribution
################################################################################# Option 1

library(RcmdrMisc)
numSummary(poidsNais[,"poidsnais"], groups=poidsNais$cgjour2cl, 
statistics=c("mean", "sd", "quantiles"), quantiles=c(0,.25,.5,.75,1))
# Option 2
by(poidsNais[,c("poidsnais")], poidsNais[,"cgjour2cl"], summary)
# Option 3
tapply(poidsNais$poidsnais, poidsNais$cgjour2cl, summary)

# Comparison of variances
################################################################################

tapply(poidsNais$poidsnais, poidsNais$cgjour2cl,  var, na.rm=TRUE)

var.test(poidsnais ~ cgjour2cl, alternative='two.sided', conf.level=.95, 
data=poidsNais)

t.test(poidsnais~cgjour2cl, alternative='two.sided', conf.level=.95, 
var.equal=TRUE, data=poidsNais)

t.test(poidsnais~cgjour2cl, alternative='two.sided', conf.level=.95, 
var.equal=FALSE, data=poidsNais)

# Linear regression model
################################################################################

modeleCigarette <- lm(poidsnais ~ cgjour2cl,data=poidsNais)
summary(modeleCigarette)

# Gestational age at birth
################################################################################

numSummary(poidsNais[,"Agegest"], statistics=c("mean", "sd", "quantiles"), quantiles=c(0,.25,.5,.75,1))
summary(poidsNais[,"Agegest"])

# Boxplot
boxplot(poidsnais~Agegest, xlab="Age gestationnel ? la naissance", ylab="Poids ? la naissance", 
data=poidsNais)

# Linear regression model
Modele1 <- lm(poidsnais ~ Agegest, data=poidsNais)
summary(Modele1)

Modele2 <- lm(poidsnais ~ agegestc, data=poidsNais)
summary(Modele2)

# Linear regression using a centered variable
summary(poidsNais$poidsnais)
poidsNais$poidsnaisCR <- (poidsNais$poidsnais-3500)/500

Modele3 <- lm(poidsnaisCR ~ agegestc, data=poidsNais)
summary(Modele3)

# Maternal weight gain during pregnancy
################################################################################

summary(poidsNais$prisepds)

plot(poidsnais~prisepdsc, data=poidsNais)
abline(lm(poidsnais~prisepdsc, data=poidsNais))

ModelePoids <- lm(poidsnais ~ prisepdsc, data=poidsNais)
shapiro.test(residuals(ModelePoids))

summary(ModelePoids)

# Is there an outlier in the dataset?
plot(poidsnais ~ prisepdsc, data=poidsNais)
modeleAvec <- lm(poidsnais ~ prisepdsc, data=poidsNais) 
poidsNais2 <- poidsNais[poidsNais$poidsnais <5000,]
modeleSans <- lm(poidsnais ~ prisepdsc, data=poidsNais2)
abline(modeleAvec,col="blue")
abline(modeleSans,col="darkorange3",lty=2)

par(mfrow = c(2,2))
plot(modeleAvec)

# High leverage point
poidsNais3 <- poidsNais
poidsNais3$prisepdsc[42] <- poidsNais3$prisepdsc[42]+5000
plot(poidsnais ~ prisepdsc, data=poidsNais3)
modeleLevier <- lm(poidsnais ~ prisepdsc, data=poidsNais3)
abline(modeleSans,col="blue")
abline(modeleLevier,col="darkorange3",lty=2)

plot(modeleLevier,4)

# Infant sex
################################################################################

Table <- table(poidsNais$sexe)
Table
round(100*Table/sum(Table), 2)

boxplot(poidsnais~sexe_num, xlab="Sexe", ylab="Poids ? la naissance", 
data=poidsNais)

tapply(poidsNais$poidsnais, poidsNais$sexe, var, na.rm=TRUE)

var.test(poidsnais ~ sexe, alternative='two.sided', conf.level=.95, data=poidsNais)
t.test(poidsnais~sexe, alternative='two.sided', conf.level=.95, var.equal=TRUE, data=poidsNais)

# Welch's t-test is preferred
t.test(poidsnais~sexe, alternative='two.sided', conf.level=.95, 
var.equal=FALSE, data=poidsNais)
  
  
# Multiple linear regression
################################################################################

ModeleMultiple <- lm(poidsnais ~ agegestc + cgjour2cl + prisepdsc + sexe, 
data=poidsNais)

summary(ModeleMultiple)

# Studentized residuals versus predicted birth weight
poidsNais$Residus <- rstudent(ModeleMultiple)
poidsNais$residus <- residuals(ModeleMultiple)
poidsNais$Residus/poidsNais$residus

mod1 <- lm(poidsnais ~ agegestc + cgjour2cl + prisepdsc + sexe, data=poidsNais[-1,])

X <- model.matrix(ModeleMultiple)
hat <- X%*%solve(t(X)%*%X)%*%t(X)

poidsNais$Residus[1]/
  

poidsNais$Predit <- predict(ModeleMultiple)
plot(Residus~Predit, xlab="Poids pr?dit ? la naissance", 
ylab="R?sidus de Student", data=poidsNais)

# Residuals should be centered around zero
abline(lm(Residus~Predit,data=poidsNais),col="blue")

# Residuals by predicted birth weight categories
LesQuantiles <- quantile(poidsNais$Predit, prob=seq(0,1,by=0.25))
LesQuantiles

poidsNais$LesGroupes <- cut(poidsNais$Predit, breaks=LesQuantiles, include.lowest=TRUE)
summary(poidsNais$LesGroupes)

boxplot(Residus ~ LesGroupes, data=poidsNais)

# Multiple linear regression (additional analyses)
################################################################################

# Does maternal weight gain have a nonlinear relationship?
plot(Residus~prisepdsc, xlab="Prise de poids de la m?re", 
ylab="R?sidus de Student", data=poidsNais)

# Are residuals centered around zero?
abline(lm(Residus~prisepdsc,data=poidsNais),col="blue")

# Are residuals centered around zero?
poidsNais$prisepdsc2 <- poidsNais$prisepdsc*poidsNais$prisepdsc
ModeleMultiple <- lm(poidsnais ~ agegestc + cgjour2cl + prisepdsc + prisepdsc2 + sexe, 
data=poidsNais)
summary(ModeleMultiple)

# Add an interaction term
ModeleMultiple <- lm(poidsnais ~ agegestc + cgjour2cl + prisepdsc + prisepdsc:sexe + sexe, 
data=poidsNais)
summary(ModeleMultiple)

################################################################################
################################################################################

  