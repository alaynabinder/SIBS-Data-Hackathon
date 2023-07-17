# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ SIBS Lecture | Evaluating Risk Prediction Models ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# # - Install required packages
# # --- Note: If packages already installed, do NOT uncomment this code!
# install.packages('ggplot2')
# install.packages('pROC')
# install.packages('mmpf')

# - Load required packages 
library(ggplot2)
library(pROC)
library(mmpf)

# ------------------------------------------------------------------------------------
# Bronchopulmonary Dysplasia Data Example: 
# BPD is a chronic lung disease that can develop among infants who receive prolonged 
# mechanical ventilation. As such, infants who require intubation early in life are 
# at high risk of developing BPD. Researhers wanted to determine if there was an 
# an association between the probability of developing BPD and an infants birthweight. 
# To study this relationship, they selected a random sample of 223 infants who were 
# confined to the NICU for at least the first 30 days of life, who required intubation
# in the first 12 hours of life, and who survived for at least 30 day. 

# Data Dictionary: 
# 1. birthweight    birthweight of baby in grams
# 2. BPD            binary indicator of presence vs. absence of bronchopulmonary 
#                   dysplasia (BPD): 0=absent, 1=present

# - Download and load the dataset used in lecture: 
download.file("http://www.duke.edu/~sgrambow/crp241data/bpd_data.RData",
              destfile = "bpd_data.RData",quiet=TRUE,mode="wb",cacheOK=FALSE)
load("bpd_data.RData")
summary(bpd)

# ------------------------------------------------------------------------------------
# - Fit a logistic regression model for BPD ~ birthweight 

# --- Note: Default estimates are on the log-odds scale
fit <- glm(BPD ~ birthweight, 
           data=bpd, 
           family='binomial')
summary(fit)
confint.default(fit)

# --- To get estimates on odds scale, must exponentiate
exp(fit$coef)
exp(confint.default(fit))

# --- To get estimates on odds scale per 100grams, must multiply by 100 and exponentiate
exp(100*fit$coef)
exp(100*confint.default(fit))

# ------------------------------------------------------------------------------------
# - Get predicted probabilities of BPD

p.hats <- predict(fit,type='response')

# --- Check computation of p.hats for first observation 
head(bpd)

# ----- With rounding errors ... 
xbeta <- 4.0342913 + (-0.0042291*850)   # Manually compute linear predictor 
p.x <- exp(xbeta)/(1+exp(xbeta)); p.x   # Manually compute P(Y=1|X)
p.hats[1]                               # Compare to output from predict()  

# ----- Without rounding errors ... 
xbeta <- c(1,850) %*% fit$coef          # Manually compute linear predictor 
p.x <- exp(xbeta)/(1+exp(xbeta)); p.x   # Manually compute P(Y=1|X)
p.hats[1]                               # Compare to output from predict() 

# --- Add predicted probabilities to data set
bpd$p.hats <- p.hats

# ------------------------------------------------------------------------------------
# - Estimate calibration-in-the-large 

mean(bpd$p.hats)        # Mean P(Y=1|X)
mean(bpd$BPD)           # Observed P(Y=1)

# ------------------------------------------------------------------------------------
# - Create calibration plot  

# --- Summarize predicted probabilities
summary(bpd$p.hats)
hist(bpd$p.hats)

# --- Compute deciles of predicted probabilities
dec <- quantile(bpd$p.hats,            # Variable to be summarized
                probs=seq(0,1,by=0.1), # Vector of percentile values 
                type=3)                # Use same algorithm as SAS
dec 

# --- Create decile group variable 
bpd$dec_grp <- cut(bpd$p.hats,         # Predicted probabilities (var to group)
                   breaks = dec,       # Cut points (group intervals)
                   include.lowest = T, # Include smallest value 
                   labels = 1:10)      # Labels for groups

# ----- Check that decile groups created correctly 
table(bpd$dec_grp)                     # Number of observations in each decile group
prop.table(table(bpd$dec_grp))         # Proportion of observations in each decile group

# --- Compute mean predicted probability and event rate by decile group
agg <- aggregate(cbind(BPD,p.hats) ~ dec_grp, # Aggregate A,B by C
                 data = bpd,                  # From data set
                 FUN = 'mean')                # Using this summary function 
agg

# ----- Check computations for decile group 5
mean(bpd$BPD[bpd$dec_grp == 5])
mean(bpd$p.hats[bpd$dec_grp == 5])

# --- Create calibration plot 
plot(agg$p.hats,                         # x-coor = mean pred prob in dec group
     agg$BPD,                            # y-coor = obs event rate in dec group
     main = 'Calibration Plot',          # Add main title 
     ylab = 'Observed Event Rate',       # Add y-axis label
     xlab = 'Predicted Probabilities',   # Add x-axis label
     pch = 19,                           # Plotting character = solid dot
     col = 'orangered',                  # Color of plotting character 
     cex = 2)                            # Size of plotting character (base = 1)

# Add identity line
abline(a = 0,                            # a = intercept 
       b = 1)                            # b = slope

# Add fitted regression line
cal.fit <- lm(BPD ~ p.hats, data = agg) # Fit linear model to plot data 
abline(cal.fit,                         # Using intercept and slope from linear model fit
       lty = 2,                         # Dashed line
       col = 'royalblue',               # Color of plotting line
       lwd = 3)                         # Thickness of plotting line (base = 1)

summary(cal.fit)                        # Compute calibration intercept and slope
confint(cal.fit)                        # Compute  95% confidence intervals

# ------------------------------------------------------------------------------------
# - Plot density of predicted probabilities by event status

ggplot(bpd,                           # Data set to pull variables from 
       aes(p.hats,                    # Variable density to plot 
           fill=as.factor(BPD))) +    # Variable to stratify by (has to be a factor)
  geom_density(alpha = 0.2) +           # Transparency of plotting colors 
  scale_fill_manual(                    # Set plotting colors
    values=c("orangered", "royalblue"))

# ------------------------------------------------------------------------------------
# - Compute c-index 

set1 <- bpd[bpd$BPD == 1, 2:3]            # Subset of subjects with events
set0 <- bpd[bpd$BPD == 0, 2:3]            # Subset of subject without events

cart.prod <- cartesianExpand(set1,set0)   # Create all pairs of pred probs
head(cart.prod)                           # What variables are present? 
dim(cart.prod)                            # Number of pairs
n.pairs <- dim(set1)[1] * dim(set0)[1]    # Double check ... 
n.pairs 

c.pairs <- sum(cart.prod$p.hats.x > cart.prod$p.hats.y)
c.pairs                                   # Number of concordant pairs 

d.pairs <- sum(cart.prod$p.hats.x < cart.prod$p.hats.y)
d.pairs                                   # Number of discordant pairs

t.pairs <- sum(cart.prod$p.hats.x == cart.prod$p.hats.y)
t.pairs                                   # Number of tied pairs 

c.index <- (c.pairs + 0.5*t.pairs) / n.pairs
c.index                                   # Compute c-index 

# ------------------------------------------------------------------------------------
# - Create ROC curve plot and compute AUC 

roc.mod <- roc(bpd$BPD,       # Observed outcome variable (Y) 
               bpd$p.hats)    # Predicted probabilities (P.hat(Y=1|X))
plot.roc(roc.mod)             # Plot ROC curve
auc(roc.mod)                  # Compute AUC
ci.auc(roc.mod)               # Compute 95% confidence interval for AUC

# ------------------------------------------------------------------------------------
# End of Program