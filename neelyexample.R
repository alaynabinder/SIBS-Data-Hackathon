# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ SIBS Lecture | Building Prediction Models ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# # - Install required packages
# # --- Note: If packages already installed, do NOT uncomment this code!
# install.packages('mplot')     <--- For bodyfat dataset 
# install.packages('scales')    <--- For transparent plotting point shading
# install.packages('car')       <--- For variance inflation factor function
# install.packages('bestglm')   <--- For best subset selection function
# install.packages("glmnet")    <--- For penalized regression function

# - Load required packages 
library(mplot)
library(scales)
library(car)
library(bestglm)
library(glmnet)

# ------------------------------------------------------------------------------------
# Body Fat Data: 
# Percentage of body fat, age, weight, height, and ten body circumference measurements 
# (abdomen, neck, chest, hip, thigh, knee, ankle, bicep, forearm, wrist) were recorded 
# for 128 men. Body fat percentage was estimated through an underwater weighing technique 
# which is expensive and time consuming. 

# Data Dictionary: 
# A data frame with 128 observations on 15 variables.
#      Variable   Description 
# (1)  Id         Identifier
# (2)  Bodyfat    Bodyfat percentage
# (3)  Age        Age (years)
# (4)  Weight     Weight (kg)
# (5)  Height     Height (inches)
# (6)  Neck       Neck circumference (cm)
# (7)  Chest      Chest circumference (cm)
# (8)  Abdo       Abdomen circumference (cm) "at the umbilicus and level with the iliac crest"
# (9)  Hip        Hip circumference (cm)
# (10) Thigh      Thigh circumference (cm)
# (11) Knee       Knee circumference (cm)
# (12) Ankle      Ankle circumference (cm)
# (13) Bic        Extended biceps circumference (cm)
# (14) Fore       Forearm circumference (cm)
# (15) Wrist      Wrist circumference (cm) "distal to the styloid processes"

# - Load from mplot package: 
data(bodyfat)
summary(bodyfat)

# ------------------------------------------------------------------------------------
# - Scatter plot of X = Abodmen Circumference vs. Y = % Body Fat

png("plots/scatter_bodyfat_adbomen.png",width = 8, height = 6, units = 'in', res = 300)
par(mar = c(5,4,2,2))
plot(x = bodyfat$Abdo, 
     y = bodyfat$Bodyfat,
     main = '',
     xlab = 'Abdomen circumference (cm)',
     ylab = '% Body Fat',
     pch = 21, 
     cex = 1.5,
     col = 'blue',
     bg = alpha('blue',alpha = 0.4))
dev.off()

# ------------------------------------------------------------------------------------
# - Fit linear regression model for Y = % Body Fat ~ X = Abodmen Circumference

fit1 <- lm(Bodyfat ~ Abdo, data = bodyfat)
summary(fit1)

# ------------------------------------------------------------------------------------
# - Get a prediction for X = 100 cm 

fit1$coefficients

pred.y <- -43.1905782 + (0.6741128 * 95)
pred.y 

predict(fit1,newdata = data.frame(Abdo = 95))

# ------------------------------------------------------------------------------------
# - Scatter plots for other available covariates 

png("plots/scatter_bodyfat_rest_covs.png",width = 10, height = 8, units = 'in', res = 300)

# --- Change plotting parameters
par(mfrow = c(3,4),     # Show more than 1 plot in figure; fill by rows in 3 x 4 array
    mar = c(2,2,4,2),   # Default margin lines are c(5,4,4,2) for top,left,right,bottom; 
    # reduce for all but top because including main title 
    oma = c(1,3,1,1))   # Default outer margin lines are c(1,1,1,1) (only use for multi-plot figures); 
# increasing for left to include y-axis label for all plots 

# --- Loop through all covariate names 
for (i in c("Age","Weight","Height","Neck","Chest","Hip",
            "Thigh","Knee","Ankle","Bic","Fore","Wrist")) {
  
  # --- Plot for each variable name (same plotting code as above)
  plot(x = bodyfat[,i], 
       y = bodyfat[,"Bodyfat"],
       main = i,
       xlab = '',
       ylab = '',
       pch = 21, 
       cex = 1.5,
       col = 'blue',
       bg = alpha('blue',alpha = 0.4))
}

# --- Add outer margin text for global y-axis label 
mtext('% Body Fat', side = 2, line = 1, outer = T)

# --- Reset plotting parameters to default
par(mfrow = c(1,1), mar = c(5,4,4,2), oma = c(1,1,1,1))

dev.off()

# ------------------------------------------------------------------------------------
# - Full model: includes all available covariates 

fit2 <- lm(Bodyfat ~ Abdo + Age + Weight + Height + Neck + Chest + Hip + 
             Thigh + Knee + Ankle + Bic + Fore + Wrist, data = bodyfat)

summary(fit2)
t(t(vif(fit2)))

# ------------------------------------------------------------------------------------
# - Remove covariates to resolve multi-collinearity issue 

# --- Remove covariates weight 
fit3 <- lm(Bodyfat ~ Abdo + Age + Height + Neck + Chest + Hip + 
             Thigh + Knee + Ankle + Bic + Fore + Wrist, data = bodyfat)

summary(fit3)
t(t(vif(fit3)))

# --- Remove hip circumference 
fit4 <- lm(Bodyfat ~ Abdo + Age + Height + Neck + Chest + 
             Thigh + Knee + Ankle + Bic + Fore + Wrist, data = bodyfat)

summary(fit4)
t(t(vif(fit4)))

# --- Remove chest circumference 
fit5 <- lm(Bodyfat ~ Abdo + Age + Height + Neck + 
             Thigh + Knee + Ankle + Bic + Fore + Wrist, data = bodyfat)

summary(fit5)
t(t(vif(fit5)))

# --- Remove knee circumference 
fit6 <- lm(Bodyfat ~ Abdo + Age + Height + Neck + 
             Thigh + Ankle + Bic + Fore + Wrist, data = bodyfat)

summary(fit6)
t(t(vif(fit6)))

# --- Remove thigh circumference 
fit7 <- lm(Bodyfat ~ Abdo + Age + Height + Neck + 
             Knee + Ankle + Bic + Fore + Wrist, data = bodyfat)

summary(fit7)
t(t(vif(fit7)))

# ------------------------------------------------------------------------------------
# - Stepwise Selection 

# --- Forward Selection
# - Define starting model as the "null" model with no covariates
start.mod <- lm(Bodyfat ~ 1 , data = bodyfat)

# - Define stopping model as the "full" model with all potential covariates
#   Note: Need to wrap lm() function call in a formula() function call
stop.mod <- formula(lm(Bodyfat ~ Abdo + Age + Weight + Height + Neck + Chest + Hip + 
                         Thigh + Knee + Ankle + Bic + Fore + Wrist, data = bodyfat))

# - Perform forward selection using the step() function
alpha.crit <- 0.2
ffit <- step(start.mod,
             scope=stop.mod,
             direction="forward",
             test="F",
             k=qchisq(1-alpha.crit,1))
summary(ffit)

# --- Backward Selection
# - Define starting model as the "full" model with all covariates
start.mod <- lm(Bodyfat ~ Abdo + Age + Weight + Height + Neck + Chest + Hip + 
                  Thigh + Knee + Ankle + Bic + Fore + Wrist, data = bodyfat)

# - Perform backward selection using the step() function
alpha.crit <- 0.2
bfit <- step(start.mod,
             direction="backward",
             test="F",
             k=qchisq(1-alpha.crit,1))
summary(bfit)



# ------------------------------------------------------------------------------------
# - Best subset selection 

# - Create data object for bestglm() function: 
#   data frame can only include covariates to be considered in selection; 
#   outcome variable has to be listed last and must have the name y
bodyfat.bglm <- bodyfat[,c("Age","Weight","Height","Neck","Chest","Abdo","Hip",
                           "Thigh","Knee","Ankle","Bic","Fore","Wrist","Bodyfat")]
names(bodyfat.bglm)[14] <- "y"
head(bodyfat.bglm)

# --- Best subset selection using AIC
bsub.aic.fit <- bestglm(bodyfat.bglm,
                        IC = "AIC",                 
                        family=gaussian)

summary(bsub.aic.fit$BestModel)
bsub.aic.fit$Subsets

# --- Best subset selection using BIC
bsub.bic.fit <- bestglm(bodyfat.bglm,
                        IC = "BIC",                 
                        family=gaussian)

summary(bsub.bic.fit$BestModel)
bsub.bic.fit$Subsets

# ------------------------------------------------------------------------------------
# - Penalized regression 

# - Create data objects for glmnet() and cv.glment functions: 
#   data objects with covariates (xx) and outcome (yy) need to be separate; 
#   outcome saved in a vector object;
#   covariates saved in a matrix object (not a data frame!) and only include 
#   covariates to be considered in selection
yy <- bodyfat[,"Bodyfat"]
xx <- as.matrix(bodyfat[,c("Age","Weight","Height","Neck","Chest","Abdo","Hip",
                           "Thigh","Knee","Ankle","Bic","Fore","Wrist")])
yy[1:6]
head(xx)

# --- LASSO regression
# - Use glmnet() function to get solutions path plot; 
#   because glmnet() fits both LASSO and Ridge regression, use alpha 
#   argument to specify which penalty to use (alpha = 1 --> LASSO)
fit.lasso <- glmnet(xx, yy, alpha=1, standardize=TRUE)
plot(fit.lasso, label=TRUE, xvar="lambda")
cbind(1:13,colnames(xx))

# - Use cv.glment() function to perform k-fold cross validation
#   to select the final model; need to set the seed so random 
#   can be reproduced each time code is run
set.seed(12345)
cv.lasso <- cv.glmnet(xx, yy, alpha=1, standardize=TRUE, nfolds=5)
plot(cv.lasso)

# - Value of lamda that minimizes MSPE: 
cv.lasso$lambda.min; log(cv.lasso$lambda.min)

# - Regression coefficients for selected model
lasso.coef <- coef(cv.lasso, s=cv.lasso$lambda.min)
lasso.coef
as.vector(lasso.coef)

# - Compare to standard regression fit for selected covariates;
#   not the same! Penalized regression shrink the betas! 
fit8 <- lm(Bodyfat ~ Age + Height + Neck + Abdo + Hip + 
             Knee + Ankle + Fore + Wrist, data = bodyfat)
summary(fit8)

# --- Ridge regression
# - Use glmnet() function to get solutions path plot; 
#   because glmnet() fits both LASSO and Ridge regression, use alpha 
#   argument to specify which penalty to use (alpha = 0 --> Ridge)
fit.ridge <- glmnet(xx, yy, alpha=0, standardize=TRUE)
plot(fit.ridge, label=TRUE, xvar="lambda")
cbind(1:13,colnames(xx))

# - Use cv.glment() function to perform k-fold cross validation
#   to select the final model; need to set the seed so random 
#   can be reproduced each time code is run
set.seed(12345)
cv.ridge <- cv.glmnet(xx, yy, alpha=0, standardize=TRUE, nfolds=10)
plot(cv.ridge)

# - Value of lamda that minimizes MSPE: 
cv.ridge$lambda.min; log(cv.ridge$lambda.min)

# - Regression coefficients for selected model; 
#   unlike LASSO, all of the beta's are non-zero
ridge.coef <- coef(cv.ridge, s=cv.ridge$lambda.min)
ridge.coef

# - Compare to standard regression fit for selected covariates;
#   not the same! Penalized regression shrink the betas! 
summary(fit2)

# ------------------------------------------------------------------------------------
# End of Program