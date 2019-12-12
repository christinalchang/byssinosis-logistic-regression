## ----setup, include=FALSE--------------------------------------------
knitr::opts_chunk$set(echo = FALSE, include = TRUE)


## ---- include = FALSE------------------------------------------------
# Import libraries.
library(bestglm)
library(lmtest)
library(car)

# Read the data.
df = read.csv("byssinosis.csv")


## --------------------------------------------------------------------
# Model fitting
model = glm(formula = cbind(Byssinosis,Non.Byssinosis) ~ ., 
            family = binomial(), data = df)
summary(model)


## ---- include = FALSE------------------------------------------------
# Consider interaction terms.
model = glm(formula = cbind(Byssinosis,Non.Byssinosis) ~ (.)^2, 
            family = binomial(), data = df)
summary(model)


## --------------------------------------------------------------------
# Model selection 
fullmodel = glm(formula = cbind(Byssinosis,Non.Byssinosis) ~ ., 
            family = binomial(), data = df)
nullmodel = glm(formula = cbind(Byssinosis,Non.Byssinosis) ~ 1, 
            family = binomial(), data = df)


## ---- include = FALSE------------------------------------------------
# Forward AIC
forwardAIC = step(nullmodel,
                  scope = list(lower = nullmodel,
                               upper = fullmodel),
                  direction = "forward")
# Backward AIC
backwardAIC = step(fullmodel,
                  scope = list(lower = nullmodel,
                               upper = fullmodel),
                  direction = "backward")

# Bidirectional AIC
bidirectAIC = step(fullmodel,
                  scope = list(lower = nullmodel,
                               upper = fullmodel),
                  direction = "both")


## --------------------------------------------------------------------
forwardAIC


## --------------------------------------------------------------------
# LR test for interactions
fitf = glm(formula = cbind(Byssinosis,Non.Byssinosis) ~ 
             (Employment + Smoking + Workspace)^2, 
            family = binomial(), data = df)
fit0 = glm(formula = cbind(Byssinosis,Non.Byssinosis) ~ 
             Employment + Smoking + Workspace, 
            family = binomial(), data = df)

lrtest(fitf,fit0)


## --------------------------------------------------------------------
# Pearson's residuals
res = residuals(fit0, "pearson")
hist(res, main = "Plot of Pearson's Residuals", xlab = "Residual")


## --------------------------------------------------------------------
# DFbeta plots
dfbetaPlots(fit0)


## --------------------------------------------------------------------
# DFfits plot
plot(dffits(fit0), main = "DFfits Plot")


## --------------------------------------------------------------------
df[which(abs(dffits(fit0)) > 1),]


## ---- ref.label=knitr::all_labels(),echo=TRUE,eval=FALSE,include =TRUE----
## NA

