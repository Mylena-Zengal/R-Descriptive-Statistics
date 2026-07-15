#setwd()

### Load the dataset
prima <- read.table("Mortalite_1sem.txt",
  header=TRUE, sep="\t", na.strings="", dec=".")


###******** COMPARISON OF MORTALITY RISK ACCORDING TO SEX ****************###
## Contingency table and Chi-squared test
.Table <- xtabs(~ Sexe + Deces, data = prima)
.Table
prop.table(.Table, 1)
chisq.test(.Table, correct=FALSE)

## Relative risk according to sex
library(epiDisplay)
cs(prima$Deces,prima$Sexe)

# Order of the levels of the sex variable
table(prima$Sexe)

# Set men as the reference category
prima$Sexe <- factor(prima$Sexe, levels = c("Homme", "Femme"))
table(prima$Sexe)
cs(prima$Deces,prima$Sexe)


###********** COMPARISON OF MORTALITY RISK ACCORDING TO OTHER RISK FACTORS ****************###
### Age
## Contingency table
.Table <- xtabs(~ AgeTer + Deces, data=prima)
.Table
prop.table(.Table, 1)
chisq.test(.Table, correct=FALSE)


## Trend test
source("fonctions_epidemio.R")
trend.test(prima$Deces,prima$AgeTer)



### Killip stage
## Contingency table
.Table <- xtabs(~Killip+Deces, data=prima)
.Table
prop.table(.Table, 1)
chisq.test(.Table, correct = F)

## Trend test
trend.test(prima$Deces,prima$Killip)



### Infarct location
.Table <- xtabs(~LocaInf + Deces, data=prima)
.Table
prop.table(.Table, 1)
chisq.test(.Table, correct=FALSE)



###****************** AGE-ADJUSTED RELATIVE RISK ***********************###
### Contingency table of death and sex according to age group
.Table <- xtabs(~Sexe+Deces+AgeTer, data=prima)
.Table
prop.table(.Table,1 )

Table1 <- xtabs(~Sexe+Deces, data=prima[prima$AgeTer=="(0) <= 62 ans",])
Table1
prop.table(Table1,1 )

Table2 <- xtabs(~Sexe+Deces, data=prima[prima$AgeTer=="(1) 62-75 ans",])
Table2
prop.table(Table2,1 )

Table3 <- xtabs(~Sexe+Deces, data=prima[prima$AgeTer=="(2) > 75 ans",])
Table3
prop.table(Table3,1 )

### Relative risk by age group and age-adjusted relative risk
mhrr(prima$Deces,prima$Sexe,prima$AgeTer)

