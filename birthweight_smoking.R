library(dplyr)
library(readxl)
birthweight_smoking <- read_excel("birthweight_smoking.xlsx")

smokerReg <- lm(birthweight ~ smoker, data = birthweight_smoking)
summary ( smokerReg )

smokerAlcoholNprevistReg <- lm(birthweight ~ smoker + alcohol + nprevist, data = birthweight_smoking)
summary ( smokerAlcoholNprevistReg )
smokerAlcoholNprevistReg$coefficients["(Intercept)"]+smokerAlcoholNprevistReg$coefficients["smoker"]+8*smokerAlcoholNprevistReg$coefficients["nprevist"]


# install packages only the first time .
install.packages( c ("sandwich" , "lmtest"))

library(sandwich)
library(lmtest)

coeftest (smokerAlcoholNprevistReg , vcov = vcovHC (smokerAlcoholNprevistReg , type = "HC1"))

Y <- lm(smoker~alcohol+nprevist, birthweight_smoking)$residuals
X <- lm(birthweight~alcohol+nprevist, birthweight_smoking)$residuals
resids <- lm(X~Y)
summary(resids)
