#n.e.c not elsewhere classified
# Empirical Research Final Project

# First we will decrease the size of our data file, while maintain 15 percent
# of the variables, which are randomly selected. check!
attach(usa_00004_csv)
select1 <- (runif(dim(usa_00004_csv)[1]) < 0.15)
PUMS_2020 <- subset(usa_00004_csv,select1)
glimpse(PUMS_2020)
head(PUMS_2020)
dim(PUMS_2020)
summary(PUMS_2020)
detach()
rm(usa_00004_csv,select1)

library(dplyr)
library(ggplot2)
library(tidyverse)
library(coefplot)
library(stargazer)
library(table1)

# 2020
# There are only 5 disabilities, dependent variables.

# All disabilities
PUMS_2020$Any_Disability <- as.logical((PUMS_2020$DIFFCARE == 2) + (PUMS_2020$DIFFMOB == 2) + (PUMS_2020$DIFFREM == 2) + (PUMS_2020$DIFFEYE == 2) + (PUMS_2020$DIFFHEAR == 2))
summary(PUMS_2020$Any_Disability)


# Difficult Self care
PUMS_2020$Difficult_Self_care <- PUMS_2020$DIFFCARE == 2
summary(as.numeric(PUMS_2020$Difficult_Self_care))
summary(PUMS_2020$Difficult_Self_care)

# Difficult Independent Living
PUMS_2020$Difficult_Independent_Living <- PUMS_2020$DIFFMOB == 2
summary(as.numeric(PUMS_2020$Difficult_Independent_Living))
summary(PUMS_2020$Difficult_Independent_Living)

# Difficult Cognitive Function
PUMS_2020$Difficult_Cognitive_Function <- PUMS_2020$DIFFREM == 2
summary(as.numeric(PUMS_2020$Difficult_Cognitive_Function))
summary(PUMS_2020$Difficult_Cognitive_Function)

# Difficult Vision
PUMS_2020$Difficult_Vision <- PUMS_2020$DIFFEYE == 2
summary(as.numeric(PUMS_2020$Difficult_Vision))
summary(PUMS_2020$Difficult_Vision)

# Difficult Hearing
PUMS_2020$Difficult_Hearing <- PUMS_2020$DIFFHEAR == 2
summary(as.numeric(PUMS_2020$Difficult_Hearing))
summary(PUMS_2020$Difficult_Hearing)

# When we create the binary variable, remember what we set it to. 
# Example: no/yes, False/True, 0/1

#RACES

#White
PUMS_2020$White <- as.numeric(PUMS_2020$RACE == 1)
summary(PUMS_2020$White)

#Black
PUMS_2020$Black <-as.numeric(PUMS_2020$RACE == 2)
summary(PUMS_2020$Black)

#Asian_Jap_Chin_Other
PUMS_2020$Asian <- as.numeric((PUMS_2020$RACE >= 4) & (PUMS_2020$RACE <= 6))
summary(PUMS_2020$Asian)

#Other Races
PUMS_2020$Other_Races <- as.numeric(PUMS_2020$RACE == 7)
summary(PUMS_2020$Other_Races)

#Hispanic
PUMS_2020$Hispanic <- as.numeric((PUMS_2020$HISPAND >= 100) & (PUMS_2020$HISPAND <= 499))
summary(PUMS_2020$Hispanic)

# 5 major white races White Ancestry

# Western Europeans
PUMS_2020$Western_Europeans_White <- as.numeric((PUMS_2020$ANCESTR1 >= 001) & (PUMS_2020$ANCESTR1 <= 098) & PUMS_2020$White)
summary(PUMS_2020$Western_Europeans_White)

# Eastern Europeans
PUMS_2020$Eastern_Europeans_White <- as.numeric((PUMS_2020$ANCESTR1 >= 100) & (PUMS_2020$ANCESTR1 <= 179) & PUMS_2020$White)
summary(PUMS_2020$Eastern_Europeans_White)

# North African and Middle Eastern (Southwest Asia)
PUMS_2020$Middle_Eastern_North_African_White <- as.numeric((PUMS_2020$ANCESTR1 >= 400) & (PUMS_2020$ANCESTR1 <= 496) & PUMS_2020$White)
summary(PUMS_2020$Middle_Eastern_North_African_White)

#North American
PUMS_2020$North_American_White <- as.numeric((PUMS_2020$ANCESTR1 >= 900) & (PUMS_2020$ANCESTR1 <= 994) & PUMS_2020$White)
summary(PUMS_2020$North_American_White)

# White Residuals: Mixed, Uncodable and Other
PUMS_2020$Other_White <- as.numeric((PUMS_2020$ANCESTR1 >= 995) & (PUMS_2020$ANCESTR1 <= 998) & PUMS_2020$RACE == 1)
summary(PUMS_2020$Other_White)

#BLACK RACES

# Black North American
PUMS_2020$Black_NorthAmerican <- as.numeric((PUMS_2020$ANCESTR1 >= 900) & (PUMS_2020$ANCESTR1 <= 994) & PUMS_2020$Black)
summary(PUMS_2020$Black_NorthAmerican)

# Black North African, Southwest Asian and Sub-Saharan Africa # (AFRICAN)
PUMS_2020$Black_African <- as.numeric((PUMS_2020$ANCESTR1 >= 400) & (PUMS_2020$ANCESTR1 <= 599) & PUMS_2020$Black)
summary(PUMS_2020$Black_African)

# Black West Indies/West Indian
PUMS_2020$Black_WestIndian <- as.numeric((PUMS_2020$ANCESTR1 >= 300) & (PUMS_2020$ANCESTR1 <= 337) & PUMS_2020$Black)
summary(PUMS_2020$Black_WestIndian)

# Black Central/South American
PUMS_2020$Black_Central_South_American <- as.numeric((PUMS_2020$ANCESTR1 >= 360) & (PUMS_2020$ANCESTR1 <= 380) & PUMS_2020$Black)
summary(PUMS_2020$Black_Central_South_American)

# Black European (Western,eastern,central, north, south)
PUMS_2020$Black_European <- as.numeric((PUMS_2020$ANCESTR1 >= 001) & (PUMS_2020$ANCESTR1 <= 195) & PUMS_2020$Black)
summary(PUMS_2020$Black_European)

# Other Black
PUMS_2020$Other_Black <- as.numeric((PUMS_2020$ANCESTR1 >= 995) & (PUMS_2020$ANCESTR1 <= 998) & PUMS_2020$Black)
summary(PUMS_2020$Other_Black)

#ASIANS
# Chinese Asian
PUMS_2020$Asian_Chinese <- as.numeric(PUMS_2020$RACE == 4) 
summary(PUMS_2020$Asian_Chinese)

# added & Asian to the end, however gives the same result.

# Filipino Asian
PUMS_2020$Filipino <- as.numeric(PUMS_2020$RACED == 600)
summary(PUMS_2020$Filipino)

# Asian Indian
PUMS_2020$Asian_Indian <- as.numeric(PUMS_2020$RACED == 610)
summary(PUMS_2020$Asian_Indian)

# Vietnamese
PUMS_2020$Vietnamese <- as.numeric(PUMS_2020$RACED == 640)
summary(PUMS_2020$Vietnamese)

# Korean
PUMS_2020$Korean <- as.numeric(PUMS_2020$RACED == 620)
summary(PUMS_2020$Korean)

#Japanese
PUMS_2020$Japanese <- as.numeric(PUMS_2020$RACE == 5)
summary(PUMS_2020$Japanese)

# Other Asian
PUMS_2020$Other_Asian <- as.numeric(PUMS_2020$RACE == 6)
summary(PUMS_2020$Other_Asian)


# HISPANICS

# Mexican
PUMS_2020$Mexican <- as.numeric(PUMS_2020$HISPAN == 1)
summary(PUMS_2020$Mexican)

# Puerto_Rican
PUMS_2020$Puerto_Rican <- as.numeric(PUMS_2020$HISPAN == 2)
summary(PUMS_2020$Puerto_Rican)

#Cuban
PUMS_2020$Cuban <- as.numeric(PUMS_2020$HISPAN == 3)
summary(PUMS_2020$Cuban)

# Central American
PUMS_2020$Central_American_Hispanic <- as.numeric((PUMS_2020$HISPAND >= 401) & (PUMS_2020$HISPAND <= 417))
summary(PUMS_2020$Central_American_Hispanic)

# South American
PUMS_2020$South_American_Hispanic <- as.numeric((PUMS_2020$HISPAND >= 420) & (PUMS_2020$HISPAND <= 431))
summary(PUMS_2020$South_American_Hispanic)

#Other Hispanic 
PUMS_2020$Other_Hispanic <- as.numeric((PUMS_2020$HISPAND >= 498) & (PUMS_2020$HISPAND <= 499))
summary(PUMS_2020$Other_Hispanic)


# Demographic
#AGE
summary(PUMS_2020$AGE)
#SEX (1 male / 2 female)
summary(PUMS_2020$SEX)

#Marital Status
# Married(Spouse present/absent)
PUMS_2020$Married <- as.numeric((PUMS_2020$MARST >= 1) & (PUMS_2020$MARST <= 2))
summary(PUMS_2020$Married)

#Separated/Divorced
PUMS_2020$Seperated_Divorced <- as.numeric((PUMS_2020$MARST >= 3) & (PUMS_2020$MARST <= 4))
summary(PUMS_2020$Seperated_Divorced)

#Widowed
PUMS_2020$Widowed <- as.numeric(PUMS_2020$MARST == 5)
summary(PUMS_2020$Widowed)

#Never Married/Single
PUMS_2020$Never_Married_Single <- as.numeric(PUMS_2020$MARST == 6)
summary(PUMS_2020$Never_Married_Single)


#REGIONS
#Northeast
PUMS_2020$Northeast <- (PUMS_2020$REGION >= 11) & (PUMS_2020$REGION <= 13)
summary(PUMS_2020$Northeast)

#Midwest
PUMS_2020$Midwest <- (PUMS_2020$REGION >= 21) & (PUMS_2020$REGION <= 23)
summary(PUMS_2020$Midwest)
summary(as.numeric(PUMS_2020$Midwest))

#South
PUMS_2020$South <- (PUMS_2020$REGION >= 31) & (PUMS_2020$REGION <= 34)
summary(PUMS_2020$South)
summary(as.numeric(PUMS_2020$South))

#West
PUMS_2020$West <- (PUMS_2020$REGION >= 41) & (PUMS_2020$REGION <= 43)
summary(PUMS_2020$West)
summary(as.numeric(PUMS_2020$West))

#Educational Level
#Less than High school Diploma
PUMS_2020$Less_than_HS <- (PUMS_2020$EDUCD >= 002) & (PUMS_2020$EDUCD <= 061)
summary(PUMS_2020$Less_than_HS)
summary(as.numeric(PUMS_2020$Less_than_HS))

# High School Diploma
PUMS_2020$HS <- (PUMS_2020$EDUCD >= 062) & (PUMS_2020$EDUCD <= 064)
summary(PUMS_2020$HS)
summary(as.numeric(PUMS_2020$HS))

#Some College or Associates
PUMS_2020$Some_college <- (PUMS_2020$EDUCD >= 065) & (PUMS_2020$EDUCD <= 100)
summary(PUMS_2020$Some_college)
summary(as.numeric(PUMS_2020$Some_college))


# Bachelor Degree/ Master/Doctoral
PUMS_2020$Bachelor_Higher <- (PUMS_2020$EDUCD >= 101) & (PUMS_2020$EDUCD <= 116)
summary(PUMS_2020$Bachelor_Higher)
summary(as.numeric(PUMS_2020$Bachelor_Higher))

#No Health Insurance
PUMS_2020$No_Insurance <- PUMS_2020$HCOVANY == 1
summary(PUMS_2020$No_Insurance)

# Is Linguistically Isolated
PUMS_2020$Linguistic_Isolation <- as.numeric(PUMS_2020$LINGISOL == 2)
summary(PUMS_2020$Linguistic_Isolation)

#Not a citizen
PUMS_2020$Not_Citizen <- as.numeric((PUMS_2020$CITIZEN >= 3) & (PUMS_2020$CITIZEN <= 4))
summary(PUMS_2020$Not_Citizen)


# Next we will create a factors with the 4 major races and the white ancestry variables

PUMS_2020$Ancestry <- factor((PUMS_2020$White + 2*PUMS_2020$Black + 3*PUMS_2020$Asian + 4*PUMS_2020$Hispanic 
                                + 5*PUMS_2020$Western_Europeans_White + 6*PUMS_2020$Eastern_Europeans_White + 7*PUMS_2020$Middle_Eastern_North_African_White +
                                  8*PUMS_2020$North_American_White + 9*PUMS_2020$Other_White), 
                               levels=c(1,2,3,4,5,6,7,8,9), labels = c("White","Black","Asian","Hispanic", "White W European Ancestry",
                                                                       "White E European Ancestry","White MENA Ancestry","White N American Ancestry", "Other White"))

# Can change to either mean/standard deviation or proportion by changing the created variables
# from logical to numeric 
PUMS_2020$Ancestry_White <- factor((PUMS_2020$Western_Europeans_White + 2*PUMS_2020$Eastern_Europeans_White +
                                      3*PUMS_2020$Middle_Eastern_North_African_White + 4*PUMS_2020$North_American_White + 5*PUMS_2020$Other_White), 
                                   levels=c(1,2,3,4,5), labels = c( "Western European","Eastern European","MENA","North American", "Other White"))

# Export save as web page, print pdf, landscape save as tabloid
PUMS_2020$ALL_Ancestry <- factor(( PUMS_2020$Black_NorthAmerican
                                   + 1*PUMS_2020$Black_African + 2*PUMS_2020$Black_WestIndian + 3*PUMS_2020$Black_Central_South_American
                                   + 4*PUMS_2020$Black_European + 5*PUMS_2020$Other_Black + 6*PUMS_2020$Asian_Chinese 
                                   + 7*PUMS_2020$Filipino + 8*PUMS_2020$Asian_Indian + 9*PUMS_2020$Vietnamese + 10*PUMS_2020$Korean +
                                     11*PUMS_2020$Japanese + 12*PUMS_2020$Other_Asian + 13*PUMS_2020$Mexican + 14*PUMS_2020$Puerto_Rican
                                   + 15*PUMS_2020$Cuban + 16*PUMS_2020$Central_American_Hispanic + 17*PUMS_2020$South_American_Hispanic
                                   + 18*PUMS_2020$Other_Hispanic), 
                                 levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19),
                                 labels = c( "Black North American", "Black_African", 
                                             "Black West Indian", "Black Central South American", "Black_European", "Other_Black", "Asian Chinese",
                                             "Filipino", "Asian Indian", "Vietnamese", "Korean", "Japanese", "Other Asian", "Mexican", "Puerto_Rican",
                                             "Cuban", "Central American Hispanic", "South_American_Hispanic",  "Other Hispanic"))

summary(PUMS_2020)


table1::label(PUMS_2020$Difficult_Self_care) <- "Disability Self-Care"
table1::label(PUMS_2020$Difficult_Independent_Living) <- "Disability Independent Living"
table1::label(PUMS_2020$Difficult_Cognitive_Function ) <- "Disability Cognitive Thinking"
table1::label(PUMS_2020$Difficult_Vision) <- "Disability Vision"
table1::label(PUMS_2020$Difficult_Hearing) <- "Disability Hearing"
table1::label(PUMS_2020$Seperated_Divorced) <- "Seperated/Divorced"
table1::label(PUMS_2020$Never_Married_Single) <- "Never Married/Single"
table1::label(PUMS_2020$Northeast ) <- "Northeast Region"
table1::label(PUMS_2020$Midwest) <- "Midwest Region"
table1::label(PUMS_2020$South) <- "South Region"
table1::label(PUMS_2020$West) <- "West Region"
table1::label(PUMS_2020$Less_than_HS) <- "Less Than High School Diploma"
table1::label(PUMS_2020$HS ) <- "High School Diploma"
table1::label(PUMS_2020$Some_college) <- "Some College/Associates Degree"
table1::label(PUMS_2020$Bachelor_Higher) <- "Bachelor's Degree or Higher"
table1::label(PUMS_2020$West) <- "West Region"
table1::label(PUMS_2020$South) <- "South Region"
table1::label(PUMS_2020$No_Insurance) <- "No Health Insurance"
table1::label(PUMS_2020$Linguistic_Isolation ) <- "Limited English Proficiency"
table1::label(PUMS_2020$Not_Citizen) <- "Not a U.S. Citizen"


#SUBSET, PEOPLE OVER 40
use_varb <- (PUMS_2020$AGE >= 40) 
dat_use <- subset(PUMS_2020,use_varb) 
rm(use_varb, PUMS_2020)

# The following code will create a table with the summary statistics of all the listed variables we 
# plan on incorporating into our analysis.

# 40 years old
table1(~ Difficult_Self_care + Difficult_Independent_Living + Difficult_Cognitive_Function + Difficult_Vision +
         Difficult_Hearing + AGE + SEX + Married + Seperated_Divorced + Widowed + Never_Married_Single 
       + Northeast + Midwest + South + West + Less_than_HS + HS + Some_college + Bachelor_Higher
       + No_Insurance + Linguistic_Isolation + Not_Citizen | Ancestry, data = dat_use)

table1(~ Any_Disability + Difficult_Self_care + Difficult_Independent_Living + Difficult_Cognitive_Function + Difficult_Vision +
         Difficult_Hearing | Ancestry_White, data = dat_use)

table1::table1(~Difficult_Self_care + Difficult_Independent_Living + Difficult_Cognitive_Function + Difficult_Vision +
                 Difficult_Hearing + AGE + SEX + Married + Seperated_Divorced + Widowed + Never_Married_Single 
               + Northeast + Midwest + South + West + Less_than_HS + HS + Some_college + Bachelor_Higher
               + No_Insurance + Linguistic_Isolation + Not_Citizen | ALL_Ancestry, data = dat_use)

# The first model reveals heterogeneity that exists between main racial groups and
# where we use the dependent variable based on people that are disabled.
# Logistic Regression For Main Racial Groups
# code dependent variable to a FALSE or TRUE to do logit and ols regression.

model_logit1 <- glm(Difficult_Self_care ~ White + Black + Asian + Hispanic, family = binomial, data = dat_use)
summary(model_logit1)

model_logit2 <- glm(Difficult_Independent_Living ~ White + Black + Asian + Hispanic, family = binomial, data = dat_use)
summary(model_logit2)

model_logit3 <- glm(Difficult_Cognitive_Function ~ White + Black + Asian + Hispanic, family = binomial, data = dat_use)

summary(model_logit3)

model_logit4 <- glm(Difficult_Vision ~ White + Black + Asian + Hispanic , family = binomial, data = dat_use)

summary(model_logit4)

model_logit5 <- glm(Difficult_Hearing ~ White + Black + Asian + Hispanic, family = binomial, data = dat_use)

summary(model_logit5)


coefplot:::multiplot(model_logit1,model_logit2,model_logit3, model_logit4, model_logit5, decreasing = TRUE,
                     intercept = FALSE,ylab = "Major Races", names = c("Self-care", "Independent Living","Cognitive Function","Vision","Hearing"),
                     title = "Disability By Major Races")



stargazer(model_logit1,model_logit2, model_logit3,model_logit4, model_logit5, out = "Logit Model 1, Major Racial Groups.html",
          dep.var.labels = c("Self-Care Disability", "Independent Living Disability", "Cognitive Function Disability", 
                             "Vision Disability", "Hearing Disability"),       title = "Logit Model 1, Major Racial Groups", type = "text")


# Model 2: Major Races With Demographic and Socio-Economic Variables

model_logit6 <- glm(Difficult_Self_care ~ White + Black + Asian + Hispanic+ AGE + I(AGE^2) + SEX + Married + Seperated_Divorced + Widowed 
                    + Northeast + Midwest + South + HS + Some_college + Bachelor_Higher + No_Insurance + Linguistic_Isolation 
                    + Not_Citizen, family = binomial, data = dat_use)

summary(model_logit6)

coefplot(model_logit6, innerCI = 1, outerCI = 0, intercept = FALSE, title = "Self-Care Disability By Major Races With Covariates",
         ylab = "Covariates", xlab = "Self Care Disability", decreasing = TRUE) 

model_logit7 <- glm(Difficult_Independent_Living ~ White + Black + Asian + Hispanic + AGE + I(AGE^2) + SEX + Married + Seperated_Divorced + Widowed 
                    + Northeast + Midwest + South + HS + Some_college + Bachelor_Higher + No_Insurance + Linguistic_Isolation 
                    + Not_Citizen, family = binomial, data = dat_use)
summary(model_logit7)

coefplot(model_logit7, innerCI = 1, outerCI = 0, intercept = FALSE, title = "Independent Living Disability By Major Races With Covariates",
         ylab = "Covariates", xlab = "Independent Living Disability", decreasing = TRUE) 

model_logit8 <- glm(Difficult_Cognitive_Function ~ White + Black + Asian + Hispanic+ AGE + I(AGE^2) + SEX + Married + Seperated_Divorced + Widowed 
                    + Northeast + Midwest + South + HS + Some_college + Bachelor_Higher + No_Insurance + Linguistic_Isolation 
                    + Not_Citizen, family = binomial, data = dat_use)
summary(model_logit8)

coefplot(model_logit8, innerCI = 1, outerCI = 0, intercept = FALSE, title = "Cognitive Function Disability By Major Races With Covariates",
         ylab = "Covariates", xlab = "Cognitive Function", decreasing = TRUE) 

model_logit9 <- glm(Difficult_Vision ~ White + Black + Asian + Hispanic + AGE + I(AGE^2) + SEX + Married + Seperated_Divorced
                    + Widowed + Northeast + Midwest + South + HS + Some_college + Bachelor_Higher 
                    + No_Insurance + Linguistic_Isolation  + Not_Citizen, family = binomial, data = dat_use)
summary(model_logit9)

coefplot(model_logit9, innerCI = 1, outerCI = 0, intercept = FALSE, title = "Difficult Vision Disability By Major Races With Covariates",
         ylab = "Covariates", xlab = "Difficult Vision Disability", decreasing = TRUE) 

model_logit10 <- glm(Difficult_Hearing ~ White + Black + Asian + Hispanic + AGE + I(AGE^2) + SEX + Married + Seperated_Divorced + Widowed 
                     + Northeast + Midwest + South + HS + Some_college + Bachelor_Higher + No_Insurance + Linguistic_Isolation 
                     + Not_Citizen, family = binomial, data = dat_use)
summary(model_logit10)

coefplot(model_logit10, innerCI = 1, outerCI = 0, intercept = FALSE, title = "Hearing Disability By Major Races With Covariates",
         ylab = "Covariates", xlab = "Hearing Disability", decreasing = TRUE) 

stargazer(model_logit6, model_logit7, model_logit8, model_logit9, model_logit10, out = "Logit Model 2, Major Racial Groups.html",
          dep.var.labels = c("Self-Care Disability", "Independent Living Disability", "Cognitive Function Disability", 
                             "Vision Disability", "Hearing Disability"),       title = "Logit Model 2, Major Racial Groups", type = "text")

# Regressions with ALL ancestries
model_logit11 <- glm(Difficult_Self_care ~ Western_Europeans_White + 
                       Eastern_Europeans_White  + Middle_Eastern_North_African_White + North_American_White + Other_White 
                     + Black_NorthAmerican + Black_African + Black_WestIndian + Black_Central_South_American + Black_European 
                     + Other_Black+ Asian_Chinese + Filipino + Asian_Indian + Vietnamese + Korean + Japanese + Mexican
                     + Puerto_Rican + Cuban + Central_American_Hispanic + South_American_Hispanic + Other_Hispanic
                     , family = binomial, data = dat_use)
summary(model_logit11)

coefplot(model_logit11, innerCI = 1, outerCI = 0, intercept = FALSE, title = "Self-Care Disability Major Racial Groups By Ancestries, No Covariates",
         ylab = "Covariates", xlab = "Self Care Disability", decreasing = TRUE) 


model_logit12 <- glm(Difficult_Independent_Living ~ Western_Europeans_White + 
                       Eastern_Europeans_White + Middle_Eastern_North_African_White + North_American_White + Other_White 
                     + Black_NorthAmerican + Black_African + Black_WestIndian + Black_Central_South_American + Black_European 
                     + Other_Black+ Asian_Chinese + Filipino + Asian_Indian + Vietnamese + Korean + Japanese + Mexican
                     + Puerto_Rican + Cuban + Central_American_Hispanic + South_American_Hispanic + Other_Hispanic
                     , family = binomial, data = dat_use)

summary(model_logit12)

coefplot(model_logit12, innerCI = 1, outerCI = 0, intercept = FALSE, title = "Independent Living Disability Major Racial Groups By Ancestries, No Covariates",
         ylab = "Covariates", xlab = "Independent Living Disability", decreasing = TRUE) 

model_logit13 <- glm(Difficult_Cognitive_Function ~ Western_Europeans_White + 
                       Eastern_Europeans_White + Middle_Eastern_North_African_White + North_American_White + Other_White 
                     + Black_NorthAmerican + Black_African + Black_WestIndian + Black_Central_South_American + Black_European 
                     + Other_Black+ Asian_Chinese + Filipino + Asian_Indian + Vietnamese + Korean + Japanese + Mexican
                     + Puerto_Rican + Cuban + Central_American_Hispanic + South_American_Hispanic + Other_Hispanic
                     , family = binomial, data = dat_use)

summary(model_logit13)
coefplot(model_logit13, innerCI = 1, outerCI = 0, intercept = FALSE, title = "Cognitive Function Disability Major Racial Groups By Ancestries, No Covariates",
         ylab = "Covariates", xlab = "Cognitive Function Disability", decreasing = TRUE) 


model_logit14 <- glm(Difficult_Vision ~ Western_Europeans_White + 
                       Eastern_Europeans_White  + Middle_Eastern_North_African_White + North_American_White + Other_White 
                     + Black_NorthAmerican + Black_African + Black_WestIndian + Black_Central_South_American + Black_European 
                     + Other_Black+ Asian_Chinese + Filipino + Asian_Indian + Vietnamese + Korean + Japanese + Mexican
                     + Puerto_Rican + Cuban + Central_American_Hispanic + South_American_Hispanic + Other_Hispanic, family = binomial, data = dat_use)


summary(model_logit14)

coefplot(model_logit14, innerCI = 1, outerCI = 0, intercept = FALSE, title = "Vision Disability Major Racial Groups By Ancestries, No Covariates",
         ylab = "Covariates", xlab = "Vision Disability", decreasing = TRUE)

model_logit15 <- glm(Difficult_Hearing ~ Western_Europeans_White + 
                       Eastern_Europeans_White + Middle_Eastern_North_African_White + North_American_White + Other_White 
                     + Black_NorthAmerican + Black_African + Black_WestIndian + Black_Central_South_American + Black_European 
                     + Other_Black+ Asian_Chinese + Filipino + Asian_Indian + Vietnamese + Korean + Japanese + Mexican
                     + Puerto_Rican + Cuban + Central_American_Hispanic + South_American_Hispanic + Other_Hispanic
                     , family = binomial, data = dat_use)

coefplot(model_logit15, innerCI = 1, outerCI = 0, intercept = FALSE, title = "Hearing Disability Major Racial Groups By Ancestries, No Covariates",
         ylab = "Covariates", xlab = "Hearing Disability", decreasing = TRUE) 

summary(model_logit15)

stargazer(model_logit11,model_logit12, model_logit13,model_logit14, model_logit15, 
          dep.var.labels = c("Self-Care Disability", "Independent Living Disability", "Cognitive Function Disability", "Vision Disability", "Hearing Disability"),
          title = "Logit Model 1, All Ancestry Groups No Covariates",  type = "text", out = "Logit Model 1, All Ancestry Groups No Covariates.html")


# Regressions with different ancestries, and ALL demographic and socio-economic factors
model_logit16 <- glm(Difficult_Self_care ~ Western_Europeans_White + Eastern_Europeans_White + Middle_Eastern_North_African_White 
                     + North_American_White + Other_White + Black_NorthAmerican + Black_African + Black_WestIndian 
                     + Black_Central_South_American + Black_European + Other_Black+ Asian_Chinese + Filipino + Asian_Indian 
                     + Vietnamese + Korean + Japanese + Mexican + Puerto_Rican + Cuban + Central_American_Hispanic 
                     + South_American_Hispanic + Other_Hispanic + AGE + I(AGE^2) + SEX + Married + Seperated_Divorced + Widowed 
                     + Northeast + Midwest + South + HS + Some_college + Bachelor_Higher + No_Insurance + Linguistic_Isolation 
                     + Not_Citizen, family = binomial, data = dat_use)

summary(model_logit16)

coefplot(model_logit16, innerCI = 1, outerCI = 0, intercept = FALSE, title = "Self Care Disability Major Racial Groups By Ancestries, With Covariates",
         ylab = "Races and Covariates", xlab = "Self Care Disability", decreasing = TRUE) 
# for every unit of age squared that increases, the more likely to one is to have a disability

model_logit17 <- glm(Difficult_Independent_Living ~ Western_Europeans_White + Eastern_Europeans_White + Middle_Eastern_North_African_White 
                     + North_American_White + Other_White + Black_NorthAmerican + Black_African + Black_WestIndian 
                     + Black_Central_South_American + Black_European + Other_Black+ Asian_Chinese + Filipino + Asian_Indian 
                     + Vietnamese + Korean + Japanese + Mexican + Puerto_Rican + Cuban + Central_American_Hispanic 
                     + South_American_Hispanic + Other_Hispanic + AGE + I(AGE^2) + SEX + Married + Seperated_Divorced + Widowed 
                     + Northeast + Midwest + South + HS + Some_college + Bachelor_Higher + No_Insurance + Linguistic_Isolation 
                     + Not_Citizen, family = binomial, data = dat_use)

summary(model_logit17)

coefplot(model_logit17, innerCI = 1, outerCI = 0, intercept = FALSE, title = "Independent Living Disability Major Racial Groups By Ancestries, With Covariates",
         ylab = "Races and Covariates", xlab = "Independent Disability", decreasing = TRUE) 


model_logit18 <- glm(Difficult_Cognitive_Function ~ Western_Europeans_White + Eastern_Europeans_White + Middle_Eastern_North_African_White 
                     + North_American_White + Other_White + Black_NorthAmerican + Black_African + Black_WestIndian 
                     + Black_Central_South_American + Black_European + Other_Black+ Asian_Chinese + Filipino + Asian_Indian 
                     + Vietnamese + Korean + Japanese + Mexican + Puerto_Rican + Cuban + Central_American_Hispanic 
                     + South_American_Hispanic + Other_Hispanic + AGE + I(AGE^2) + SEX + Married + Seperated_Divorced + Widowed 
                     + Northeast + Midwest + South + HS + Some_college + Bachelor_Higher + No_Insurance + Linguistic_Isolation 
                     + Not_Citizen, family = binomial, data = dat_use)

summary(model_logit18)

coefplot(model_logit18, innerCI = 1, outerCI = 0, intercept = FALSE, title = "Cognitive Function Disability Major Racial Groups By Ancestries, With Covariates",
         ylab = "Races and Covariates", xlab = "Cognitive Function Disability", decreasing = TRUE) 


model_logit19 <- glm(Difficult_Vision ~ Western_Europeans_White + Eastern_Europeans_White + Middle_Eastern_North_African_White 
                     + North_American_White + Other_White + Black_NorthAmerican + Black_African + Black_WestIndian 
                     + Black_Central_South_American + Black_European + Other_Black+ Asian_Chinese + Filipino + Asian_Indian 
                     + Vietnamese + Korean + Japanese + Mexican + Puerto_Rican + Cuban + Central_American_Hispanic 
                     + South_American_Hispanic + Other_Hispanic + AGE + I(AGE^2) + SEX + Married + Seperated_Divorced + Widowed 
                     + Northeast + Midwest + South + HS + Some_college + Bachelor_Higher + No_Insurance + Linguistic_Isolation 
                     + Not_Citizen, family = binomial, data = dat_use)

summary(model_logit19)

coefplot(model_logit19, innerCI = 1, outerCI = 0, intercept = FALSE, title = "Vision Disability Major Racial Groups By Ancestries, With Covariates",
         ylab = "Races and Covariates", xlab = "Vision Disability", decreasing = TRUE) 

model_logit20 <- glm(Difficult_Hearing ~ Western_Europeans_White + Eastern_Europeans_White + Middle_Eastern_North_African_White 
                     + North_American_White + Other_White + Black_NorthAmerican + Black_African + Black_WestIndian 
                     + Black_Central_South_American + Black_European + Other_Black+ Asian_Chinese + Filipino + Asian_Indian 
                     + Vietnamese + Korean + Japanese + Mexican + Puerto_Rican + Cuban + Central_American_Hispanic 
                     + South_American_Hispanic + Other_Hispanic + AGE + I(AGE^2) + SEX + Married + Seperated_Divorced + Widowed 
                     + Northeast + Midwest + South + HS + Some_college + Bachelor_Higher + No_Insurance + Linguistic_Isolation 
                     + Not_Citizen, family = binomial, data = dat_use)

summary(model_logit20)

coefplot(model_logit20, innerCI = 1, outerCI = 0, intercept = FALSE, title = "Hearing Disability Major Racial Groups By Ancestries, With Covariates",
         ylab = "Races and Covariates", xlab = "Hearing Disability", decreasing = TRUE) 


stargazer(model_logit16,model_logit17, model_logit18,model_logit19, model_logit20, 
          dep.var.labels = c("Self-Care Disability", "Independent Living Disability", "Cognitive Function Disability", "Vision Disability", "Hearing Disability"),
          title = "Logit Model 2, All Ancestry Groups With Covariates",  type = "text", out = "Logit Model 2, All Ancestry Groups With Covariates.html")

#Looking just at white ancestry
stargazer(model_logit16,model_logit17, model_logit18,model_logit19, model_logit20, 
          dep.var.labels = c("Self-Care Disability", "Independent Living Disability", "Cognitive Function Disability", "Vision Disability", "Hearing Disability"),
          title = "Logit Model 2, All Ancestry Groups With Covariates",  type = "text",
          omit = c("Black_NorthAmerican","Black_African", "Black_WestIndian","Black_Central_South_American ", "Black_European", 
                   "Other_Black", "Asian_Chinese", "Filipino", "Asian Indian", "Vietnamese", "Korean", "Japanese", "Mexican", "Puerto_Rican",
                   "Cuban", "Central_American_Hispanic", "South_American_Hispanic", "Other_Hispanic", "AGE", "I(AGE2)", "SEX", 
                   "Married", "Seperated_Divorced", "Widowed", "Northeast"))



# The coefficients function restricts the view to those of white ancestry.
coefplot:::multiplot(model_logit16,model_logit17,model_logit18, model_logit19, model_logit20, decreasing = TRUE,
                     intercept = FALSE, newNames = c(Western_Europeans_White= "Western European", Eastern_Europeans_White = "Eastern European",
                                                     Middle_Eastern_North_African_White = "MENA", North_American_White = "North American",Other_White = "Other White"),
                     ylab = "White Ancestry", names = c("Self-care", "Independent Living","Cognitive Function","Vision","Hearing"),
                     title = "Disability By White Ancestry",  coefficients=c("Western_Europeans_White", "Eastern_Europeans_White",
                                                                             "Middle_Eastern_North_African_White", "North_American_White", "Other_White"))


#PIE Chart
table(PUMS_2020$Ancestry_White)
pie(table(PUMS_2020$Ancestry_White),
    labels = paste(round(prop.table(table(PUMS_2020$Ancestry_White))*100), "%", sep = ""), 
    col = c("dodgerblue", "red", "cornsilk", "orange", "forestgreen"), main = "Proportion of White Ancestry")
legend("topright", legend = c("Western European", "Eastern European", "MENA","North American","Other White"), 
       fill =   c("dodgerblue", "red", "cornsilk", "orange", "forestgreen"), title = "Categories", cex = .7)


table1 <- table(dat_use$Ancestry_White,dat_use$Difficult_Vision)
table1
# Proportions of people that are vaccinated from table.

x=c(table1[1,2]/(table1[1,1]+table1[1,2]),
    table1[2,2]/(table1[2,1]+table1[2,2]),
    table1[3,2]/(table1[3,1]+table1[3,2]),
    table1[4,2]/(table1[4,1]+table1[4,2]),
    table1[5,2]/(table1[5,1]+table1[5,2]))
x
disabilityV_prop_table<-data.frame(row.names=row.names(table1), Prop_Disability_Vision=x)
disabilityV_prop_table
require(ggplot2)
ggplot(data=disabilityV_prop_table, aes(y=row.names(disabilityV_prop_table), x=Prop_Disability_Vision, fill=row.names(disabilityV_prop_table))) + 
  geom_bar(stat="identity") + scale_fill_brewer(palette = "Dark2") + ggtitle("Proportion Of Vision Disability White Ancestry Groups") + theme(legend.position = "none") + labs( x="Vision Disability", y="Ancestry")


# The older on get the increased likelihood for having hearing difficulty
table1 <- table(dat_use$Ancestry_White ,dat_use$Difficult_Hearing)
table1
# Proportions of people that are vaccinated from table.

x=c(table1[1,2]/(table1[1,1]+table1[1,2]),
    table1[2,2]/(table1[2,1]+table1[2,2]),
    table1[3,2]/(table1[3,1]+table1[3,2]),
    table1[4,2]/(table1[4,1]+table1[4,2]),
    table1[5,2]/(table1[5,1]+table1[5,2]))
x
disabilityH_prop_table<-data.frame(row.names=row.names(table1), Prop_Disability_Hearing=x)
disabilityH_prop_table
library(ggplot2)
ggplot(data=disabilityH_prop_table, aes(y=row.names(disabilityH_prop_table), x=Prop_Disability_Hearing, fill=row.names(disabilityH_prop_table))) + 
  geom_bar(stat="identity") +ggtitle("Proportion Of Hearing Disability By White Ancestry") + theme(legend.position = "none") + labs( x="Hearing Disability", y="Ancestry")


# focus major plots on white demographic ancestry

#works
attach(dat_use)

library(dplyr)
library(ggplot2)
library(scales)
library(plyr)

attach(PUMS_2020)
mu <- ddply(dat_use, "Ancestry_White", summarise, grp.mean=mean(AGE))
head(mu)
# When using thest graph variable must not be a factor.
detach()
fit <- glm(
  data = dat_use,
  family = binomial,
  Difficult_Self_care ~ 1 + AGE)
summary(fit)
library(broom)
tidy(fit) %>% 
  knitr::kable()

c(coef(fit)[2], confint(fit)[2, ]) 

nd <- tibble(AGE = seq(from = 40, to = 100, length.out = 100))
p <-
  # compute the fitted lines and SE's
  predict(fit,
          newdata = nd,
          type = "link",
          se.fit = TRUE) %>% 
  # wrangle
  data.frame() %>% 
  mutate(ll = fit - 1.96 * se.fit,
         ul = fit + 1.96 * se.fit) %>% 
  select(-residual.scale, -se.fit) %>% 
  mutate_all(plogis) %>%
  bind_cols(nd)
p %>% 
  ggplot(aes(x =AGE, y = fit)) +
  geom_ribbon(aes(ymin = ll, ymax = ul),
              alpha = 1/2) +
  geom_line() +
  geom_jitter(size = 1/4, alpha = 1/2, height = 0.05) +
  scale_y_continuous("Probability", 
                     expand = c(0, 0), limits = 0:1) + ggtitle("Probability of Self Care Disability") 


ggplot(dat_use, 
       aes(x = AGE, y = DIFFCARE, color = Ancestry_White)) +
  geom_point() + 
  ggtitle("Probability of Difficult Self-Care Disability Based On Ancestry") + 
  xlab("AGE") + ylab("Likelihood Of Disability") +
  theme_minimal() + 
  geom_smooth(aes(color = Ancestry_White), se = FALSE) 


ggplot(dat_use, aes(x = AGE,
                    fill = Difficult_Vision)) +
  geom_histogram(position = "dodge") +
 facet_grid(~Ancestry_White) +
  labs(y= "Count",
       x = "Age",
       fill = "Difficult Vision Disability") + ggtitle("Number of White People With Vision Disability By Ancestry") +
  scale_fill_manual(labels = c("No", 
                               "Yes"), 
                    values = c("forestgreen",
                               "darkred")) + theme_minimal()














table1 <- table(dat_use$SEX, dat_use$Difficult_Vision)
table1

# plot 5 (non-interactive plot)
plot(table1, main="Difficult Vision Disability By Gender",
     xlab="Male                 Female",
     ylab="Vision Disability", 
     col = c("grey", "blue"))

# Proportions of non-young people(FALSE) and young people(TRUE) that were in accident.
x=c(table1[1,2]/(table1[1,1]+table1[1,2]),
    table1[2,2]/(table1[2,1]+table1[2,2]))
x


Vision_disability_prop_table<-data.frame(row.names= c("Male", "Female"), Vision_disability=x)
Vision_disability_prop_table
ggplot(data=Vision_disability_prop_table, aes(y=row.names(Vision_disability_prop_table), x=Vision_disability, fill=row.names(Vision_disability_prop_table))) + 
                    geom_bar(stat="identity") + scale_fill_brewer(palette = "Dark2") +ggtitle("Difficult Vision Disability By Gender") + theme(legend.position = "none") + labs( x="Proportion Of Visually Disabled", y="Gender") 

ggplot(aes(x = Ancestry_White , y = "Proportion"), 
                          data = dat_use) +
                     geom_col(aes(),size = 2, alpha = .5, color="lightblue") +
                     xlab("Ancesrty White") + 
                     ylab("Proportion") +
                     ggtitle("Proportion of Ancestry From White Racial Group") +
                     theme_linedraw()
