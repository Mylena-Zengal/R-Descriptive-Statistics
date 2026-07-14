### Import the dataset
prevalence <- read.table("Prevalence.txt", header = T, sep = "\t")


###******************** PREVALENCE ESTIMATION *************************###
### Point estimate of prevalence
table(prevalence$InfNoso)
prop.table(table(prevalence$InfNoso))

### 95% confidence interval
# CI for the proportion without infection
prop.test(table(prevalence$InfNoso))

# CI for the proportion with infection
prevalence$InfNoso <- factor(prevalence$InfNoso, levels = c(1, 0))
table(prevalence$InfNoso)
prop.test(table(prevalence$InfNoso))


###******** PREVALENCE ACCORDING TO A RISK FACTOR ********************###
### Using a contingency table
table(prevalence$Cancer, prevalence$InfNoso)
prop.table(table(prevalence$Cancer, prevalence$InfNoso), 1)

### Using the xtabs() function
prop.table(xtabs( ~ Cancer + InfNoso, data = prevalence), 1)
prop.test(table(prevalence$Cancer, prevalence$InfNoso))


#####################################################################################################


### Import the dataset
incidence <- read.table("Incidence5.txt", header=TRUE, sep="\t", dec=".")


###******************** INCIDENCE ESTIMATION *************************###
### Total follow-up time
sum(incidence$Suivi5ans)

### Number of incident cases
sum(incidence$Malade5ans)

### Incidence rate
sum(incidence$Malade5ans)/sum(incidence$Suivi5ans)



#####################################################################################################


### Import the dataset
montana <- read.table("Montana.txt", header=TRUE, sep="\t", dec = ".")


###*************************** CRUDE INCIDENCE RATES ***********************************###
### Calculate the number of cases by period
## Table containing case counts only
cas <- montana[,c(3,5,7,9,11,13,15)]

## Total number of cases for each period
## (sum across all age groups)
sum_cas_brut <- apply(cas,2,sum)
sum_cas_brut


### Calculate person-years by period
## Table containing person-years only
pa <- montana[,c(2,4,6,8,10,12,14)]

## Total person-years for each period
## (sum across all age groups)
sum_pa_brut <- apply(pa,2,sum)
sum_pa_brut


### Crude incidence rates
## Calculation
taux_brut <- sum_cas_brut/sum_pa_brut*1000
taux_brut

## Plot crude incidence rates over time
periode <- 1:7
tableau_brut <- cbind.data.frame(periode,taux_brut)

plot(tableau_brut$periode, tableau_brut$taux_brut, type="b", 
  lty=1, ylab="Taux d'incidence", xlab = "Période", pch=1)




###************************ AGE-STANDARDIZED INCIDENCE RATES *******************************###
### Incidence rates by period and age group
taux_periode_age <- cas/pa*1000

### Age-specific incidence rates weighted
### by the reference population
pop1950 <- montana[,16]
taux_periode_age_ponderes <- taux_periode_age*pop1950


### Standardized incidence rates by period
taux_standardise <- apply(taux_periode_age_ponderes,2,sum)
taux_standardise

tableau_standardise <- cbind.data.frame(periode,taux_brut,taux_standardise)


## Plot standardized and crude incidence rates
plot(tableau_standardise$periode, tableau_standardise$taux_standardise, 
  type="b", lty=1, ylab="Taux d'incidence", xlab = "Période", pch=2, 
  col = "red", ylim = c(19, 31))
lines(tableau_standardise$periode, tableau_standardise$taux_brut, 
  type = "b", pch = 1 , col = "black")
legend("topright", legend = c("taux standardisés", "taux bruts"), 
 col = c("red", "black"), pch = c(2, 1), lty = 1)


