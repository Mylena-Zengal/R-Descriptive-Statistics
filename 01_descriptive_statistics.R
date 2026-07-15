# Working directory
setwd("C:/Users/user/OneDrive/Documents/STAGE 2027/EXO LOGICIEL R/3 TDTP1 Description Estimation-20260702/TP/donnees/donnees")

# Import dataset
mortalite <- read.table("Mortaliteunan.txt", sep = "\t", dec = ".", header = TRUE, na.strings = " ")

# Explore dataset
dim(mortalite)
head(mortalite)
summary(mortalite$Age)

# Descriptive statistics
summary(mortalite$Age)
sd(mortalite$Age)
quantile(mortalite$Age, prob = c(0.025, 0.975))

# Histogram
hist(mortalite$Age, col = "darkgray", xlab = "Age", ylab = "Frequency", breaks = 35)

# Description of age by sex
tapply(mortalite$Age, mortalite$Sexe, summary)

# Description of sex
table(mortalite$Sexe)
prop.table(table(mortalite$Sexe))

# Percentages of sex distribution
prop.table(table(mortalite$Sexe)) * 100
