##### Practical Session: Case-Control Study and Logistic Regression #####

setwd(Dir)
Tuyns <- read.table("donneesTuyns.txt",header=TRUE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

# Load the dataset

## Alcohol consumption
# Contingency table
Table <- xtabs(~Alcool+Cas, data=Tuyns)
# Chi-squared test
Test <- chisq.test(Table, correct=FALSE)
# Column percentages
prop.table(Table,margin=2)*100# Column Percentages
#Odds-ratio
library(epiDisplay)
cc(Tuyns$Cas,Tuyns$Alcool)


## Tabac consumption
# Contingency table
Table <- xtabs(~Tabac+Cas, data=Tuyns)
# Chi-squared test
Test <- chisq.test(Table, correct=FALSE) 
# Column percentages
prop.table(Table,margin=2)*100# Column Percentages
prop.table(Table,margin=2)*100# Column Percentages
#Odds-ratio
cc(Tuyns$Cas,Tuyns$Tabac)


# Association between tobacco and alcohol consumption
Table <- xtabs(~Tabac+Alcool, data=Tuyns)

# Chi-squared test
Test <- chisq.test(Table, correct=FALSE)

# Mantel-Haenszel adjusted odds ratio
mhor(Tuyns$Cas,Tuyns$Alcool,Tuyns$Strate)
mhor(Tuyns$Cas,Tuyns$Tabac,Tuyns$Strate)


# Logistic regression models
# Create a binary outcome variable
Tuyns$CasNum=ifelse(Tuyns$Cas=="Oui",1,0)
table(Tuyns$Cas,Tuyns$CasNum)

# Univariate logistic regression: alcohol consumption
modAlcool <- glm(CasNum ~ Alcool, family=binomial(logit), data=Tuyns)
summary(modAlcool)
logistic.display(modAlcool)

# Univariate logistic regression: tobacco consumption
modTabac <- glm(CasNum ~ Tabac, family=binomial(logit), data=Tuyns)
summary(modTabac)
logistic.display(modTabac)

# Logistic regression adjusted for matching strata
modAlcoolStrate <- glm(CasNum ~ Alcool +Strate, family=binomial(logit), data=Tuyns)
summary(modAlcoolStrate)
logistic.display(modAlcoolStrate,simplified=TRUE)

# Tobacco model adjusted for matching strata
modTabacStrate <- glm(CasNum ~ Tabac +Strate, family=binomial(logit), data=Tuyns)
summary(modTabacStrate)
logistic.display(modTabacStrate,simplified=TRUE)

# Multivariable logistic regression
modAlcoolTabacStrate <- glm(CasNum ~ Alcool + Strate +Tabac, family=binomial(logit), data=Tuyns)
summary(modAlcoolTabacStrate)
logistic.display(modAlcoolTabacStrate,simplified=TRUE)

# Logistic regression including an interaction term
modInter <- glm(CasNum ~ Alcool*Tabac +Strate, family=binomial(logit), data=Tuyns)
summary(modInter)

