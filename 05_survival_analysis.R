# Load Exercise 2 dataset

LesDonnees <- read.table(file="donnees_exo2.txt",sep="\t",header=T)

# Always check factor variables in R
LesDonnees$sexe=factor(LesDonnees$sexe)

# Create a survival object
SurvObj=Surv(LesDonnees$Suivi,LesDonnees$etat)

# By default, right-censored survival data are used (type="right").
# SurvObj=Surv(LesDonnees$Suivi,LesDonnees$etat,type="right")

# Kaplan-Meier survival curve
km <- survfit(SurvObj ~ tcoeur, data=LesDonnees, conf.type="log", conf.int=0.95, type="kaplan-meier", error="greenwood")
plot(km, xlab="Temps (mois)", ylab="Survie", col=c("black","red"), mark.time=TRUE)

# Cox model adjusted for heart transplantation
mod0=coxph(SurvObj ~ tcoeur, data=LesDonnees,method="efron",robust=F)
summary(mod0)

# Cox model adjusted for heart transplantation, age and sex
mod1=coxph(SurvObj ~ tcoeur + age + sexe, data=LesDonnees, method="efron", robust=F)
summary(mod1)

# Cox model adjusted for heart transplantation, age, sex and diabetes
mod2=coxph(SurvObj ~ tcoeur + age + sexe + diab2, data=LesDonnees, method="efron", robust=F)
summary(mod2)



##########################################################################################################
##########################################################################################################

# Load Exercise 3 dataset
LesDonnees <- read.table(file="donnees_exo3.txt",sep="\t",header=T)

# Create a survival object
SurvObj <- Surv(LesDonnees$suivimois,LesDonnees$etat,type="right") 

# Kaplan-Meier survival curve by treatment group
fit <- survfit(SurvObj ~ groupe,type="kaplan-meier",error="greenwood",data=LesDonnees)
plot(fit, xlab="Temps (mois)", ylab="Survie", col=c("black","red"), mark.time=TRUE)
summary(fit)

# Log-rank test
survdiff(SurvObj ~ groupe, data=LesDonnees)

##########################################################################################################
##########################################################################################################

# Load Exercise 4 dataset
LesDonnees <- read.table(file="donnees_exo4.txt",sep="\t",header=T)

# Create a survival object
SurvObj <- Surv(LesDonnees$suivi,LesDonnees$rechute,type="right")

# Kaplan-Meier model by treatment group
fit <- survfit(SurvObj ~ trt,type="kaplan-meier", error="greenwood", data=LesDonnees)
summary(fit)

# Log-rank test
survdiff(SurvObj ~ trt, data=LesDonnees)
plot(fit, xlab="Temps (semaines)", ylab="Survie", col=c("black","red"), lty=2:3, mark.time=TRUE)
legend("topright", legend=c("Placebo","6-MP"), lty=2:3, col=c("black","red"), horiz=FALSE, bty='n')

# Kaplan-Meier model by sex
fit <- survfit(SurvObj ~ sexe,type="kaplan-meier", error="greenwood", data=LesDonnees)
summary(fit)
survdiff(SurvObj ~ sexe, data=LesDonnees)
plot(fit, xlab="Temps (semaines)", ylab="Survie", col=c("black","red"), lty=2:3, mark.time=TRUE)
legend("top", legend=c("Femmes","Hommes"), col=c("black","red"), lty=2:3, horiz=FALSE, bty='n')

# Kaplan-Meier model by treatment among women
LesDonneesFemmes <- LesDonnees[LesDonnees$sexe==0,] 
SurvObj <- Surv(LesDonneesFemmes$suivi,LesDonneesFemmes$rechute,type="right")
fit <- survfit(SurvObj ~ trt,type="kaplan-meier", error="greenwood", data=LesDonneesFemmes)
summary(fit)
survdiff(SurvObj ~ trt, data=LesDonneesFemmes)
plot(fit, xlab="Temps (semaines)", ylab="Survie", col=c("black","red"), lty=2:3, mark.time=TRUE)
legend("topright", legend=c("Placebo, Femmes","6-MP, Femmes"), lty=2:3, col=c("black","red"), horiz=FALSE, bty='n')
  
# Kaplan-Meier model by treatment among men
LesDonneesHommes <- LesDonnees[LesDonnees$sexe==1,] 
SurvObj <- Surv(LesDonneesHommes$suivi,LesDonneesHommes$rechute,type="right")
fit <- survfit(SurvObj ~ trt,type="kaplan-meier", error="greenwood", data=LesDonneesHommes)
summary(fit)
survdiff(SurvObj ~ trt, data=LesDonneesHommes)
plot(fit, xlab="Temps (semaines)", ylab="Survie", col=c("black","red"), lty=2:3, mark.time=TRUE)
legend("topright", legend=c("Placebo, Hommes","6-MP, Hommes"), lty=2:3, col=c("black","red"), horiz=FALSE, bty='n') 

# Stratified log-rank test by sex 
SurvObj <- Surv(LesDonnees$suivi,LesDonnees$rechute,type="right")
survdiff(SurvObj ~ trt+strata(sexe), data=LesDonnees) 

# Additive decomposition of the stratified log-rank test
SurvObj <- Surv(LesDonneesFemmes$suivi,LesDonneesFemmes$rechute,type="right")
survdiff(SurvObj ~ trt, data=LesDonneesFemmes) 
SurvObj <- Surv(LesDonneesHommes$suivi,LesDonneesHommes$rechute,type="right")
survdiff(SurvObj ~ trt, data=LesDonneesHommes) 

# Cox proportional hazards model
SurvObj <- Surv(LesDonnees$suivi,LesDonnees$rechute,type="right")

# Explicit reference categories
LesDonnees$Femme = 1-LesDonnees$sexe
LesDonnees$trt6MP = LesDonnees$trt-1
ModeleCox <- coxph(SurvObj ~ trt6MP + Femme + trt6MP:Femme, data=LesDonnees, method="efron", robust=F)
summary(ModeleCox)

# Cox model without interaction term
ModeleCox <- coxph(SurvObj ~ trt6MP + Femme , data=LesDonnees, method="efron", robust=F)
summary(ModeleCox)

# ---------------------------------------------------------- #
# ---------------------------------------------------------- #
