# Question 1
# A random sample of 99 men's age and height
set.seed(2020)
age <- c(sample(18:30, 99, replace=T))
height <- c(160 + 0.5*age + rnorm(99,0,10))
fit <- lm(height~age)
print("Summary for the regression on the first 99 data points")
summary(fit)
# The Curious Case of Benjamin Button
age_100 <- c(age, 80)
height_100 <- c(height, 100)
fit2 <- lm(height_100~age_100)
print("Summary for the regression on the 100 data points")
summary(fit2)
plot(age_100, height_100, main = "Scatter plot of height and age", 
     xlab="Age", ylab="Height")
abline(fit2, col = "blue", lwd=2)
abline(fit, col = "yellow", lwd=2)

# Question 2
library(arm)
library(Matching)
# The dataset still loads though
data(lalonde)
# Get dataset with only control group
to_be_excluded <- which(lalonde$treat == 1)
df <- lalonde[-to_be_excluded, ]
# Fit the linear model
lm1 <- lm(re78~age+educ+re74+re75+I(educ*re74)+I(educ*re75)+I(age*re74)
          +I(age*re75)+I(re74*re75), data=df)
# Simulate 10000 predictions
sim_results <- sim(lm1, n.sims = 10000)
age_values <- min(df$age):max(df$age)
age_range <- length(age_values)
# Hold educ, re74, and re75 at their medians
simulated_ys_median <- list()
educ_val_median = median(df$educ)
re74_val_median = median(df$re74)
re75_val_median = median(df$re75)
for (i in 1:10000) {
  simulated_ys_median[[i]] <- 
    sim_results@coef[i,1]*rep(1, age_range) + 
    sim_results@coef[i,2]*age_values + 
    sim_results@coef[i,3]*rep(educ_val_median, age_range)+
    sim_results@coef[i,4]*rep(re74_val_median, age_range)+
    sim_results@coef[i,5]*rep(re75_val_median, age_range)+
    sim_results@coef[i,6]*rep(educ_val_median*re74_val_median,age_range)+
    sim_results@coef[i,7]*rep(educ_val_median*re75_val_median,age_range)+
    sim_results@coef[i,8]*age_values*rep(re74_val_median, age_range)+
    sim_results@coef[i,9]*age_values*rep(re75_val_median, age_range)+
    sim_results@coef[i,10]*rep(re74_val_median*re75_val_median,age_range)+
    rnorm(age_range, 0, sim_results@sigma[i])
}
median_left <- vector()
median_right <- vector()
median_mean <- vector()

for (i in 1:age_range) {
  ith.unit <- sapply(simulated_ys_median, function (x) x[i])
  ci <- quantile(ith.unit, c(0.025, 0.975))
  median_mean[i] <- mean(ith.unit)
  median_left[i] <- ci[1]
  median_right[i] <- ci[2]
}
median.df <- data.frame(age=age_values, 
                        educ=rep(educ_val_median, age_range), 
                        re74=rep(re74_val_median, age_range), 
                        re75=rep(re75_val_median, age_range),
                        lower=median_left, 
                        mean=median_mean,
                        upper=median_right)

# Hold educ, re74, and re75 at their 90% quantiles
simulated_ys_quantiles <- list()
educ_val = quantile(df$educ, 0.9)
re74_val = quantile(df$re74, 0.9)
re75_val = quantile(df$re75, 0.9)
for (i in 1:10000) {
  simulated_ys_quantiles[[i]] <- 
    sim_results@coef[i,1]*rep(1, age_range) + 
    sim_results@coef[i,2]*age_values + 
    sim_results@coef[i,3]*rep(educ_val, age_range)+
    sim_results@coef[i,4]*rep(re74_val, age_range)+
    sim_results@coef[i,5]*rep(re75_val, age_range)+
    sim_results@coef[i,6]*rep(educ_val*re74_val, age_range)+
    sim_results@coef[i,7]*rep(educ_val*re75_val, age_range)+
    sim_results@coef[i,8]*age_values*rep(re74_val, age_range)+
    sim_results@coef[i,9]*age_values*rep(re75_val, age_range)+
    sim_results@coef[i,10]*rep(re74_val*re75_val, age_range)+
    rnorm(age_range, 0, sim_results@sigma[i])
}
quantile.left <- vector()
quantile.right <- vector()
quantile.mean <- vector()

for (i in 1:age_range) {
  ith.unit <- sapply(simulated_ys_quantiles, function (x) x[i])
  ci <- quantile(ith.unit, c(0.025, 0.975))
  quantile.mean[i] <- mean(ith.unit)
  quantile.left[i] <- ci[1]
  quantile.right[i] <- ci[2]
}
ninety.quantile.df <- data.frame(age=age_values, 
                                 educ=rep(educ_val, age_range), 
                                 re74=rep(re74_val, age_range), 
                                 re75=rep(re75_val, age_range),
                                 lower=quantile.left,
                                 mean=quantile.mean,
                                 upper=quantile.right)
print("Table of point estimates with predictors held at their medians")
median.df
print("Table of point estimates with predictors held at their 90% quantiles")
ninety.quantile.df

require(plotrix)
plotCI(age_values, median_mean, ui=median_right, li=median_left,  
       cex.axis=0.8, cex.lab=0.8, cex.main=0.8, 
       main = 'Simulated Earnings in 1978 with 95% Confidence Intervals', 
       xlab = "Age", ylab='re78')
mtext('(Holding educ, re74, and re75 at their medians)', cex=0.8)

plotCI(age_values, quantile.mean, ui=quantile.right, li=quantile.left, 
       cex.axis=0.8, cex.lab=0.8, cex.main=0.8, 
       main = 'Simulated Earnings in 1978 with 95% Confidence Intervals', 
       xlab = "Age", ylab='re78')
mtext('(Holding educ, re74, and re75 at their 90% quantiles)', cex=0.8)

# Question 3
library(foreign)
df <- read.dta('Downloads/nsw.dta')
# Remove the data_id column
df$data_id <- NULL
lm.fit <- lm(re78~treat, data=df)

# Auxiliary function that returns the coefficient for treatment
boot.fn <- function(data, index) 
  return(coef(lm(re78~treat, data = data, subset = index))[2])
# Bootstrapping
boot.df <- vector()
for (i in 1:10000) {
  indexes <- sample(1:nrow(df), nrow(df), replace=T)
  boot.df <- c(boot.df, boot.fn(df, indexes))
}
conf.intervals <- quantile(boot.df, c(0.025, 0.975))
analytical.ci <- confint(lm.fit, level = 0.95)[2,]
data.frame(Bootstrapped_CI=conf.intervals, Analytical_CI=analytical.ci)
hist(boot.df, main = "Distribution of bootstrapped coefficients",col='blue',
     xlab = "Coefficients")

# Question 4 
# Function calculating R^2
rsq <- function (y, pred_y) {
  ss.res <- sum((y-pred_y)^2)
  ss.tot <- sum((y-rep(mean(y), length(y)))^2)
  return(1-(ss.res/ss.tot))
}
print("R-squared extracted from the model:")
summary(lm.fit)$r.squared
print("R-quared from the function")
rsq(df$re78, predict(lm.fit))

# Question 5
glm.fit <- glm(treat~age+education+black+hispanic+married+nodegree+re75,
               family=binomial,data=df)
glm.probs=predict(glm.fit,type="response")
control.index <- which(df$treat == 0)
library(ggplot2)
# Make a new column to identify the two groups
control.probs <- data.frame(Probability=glm.probs[control.index])
treat.probs <- data.frame(Probability=glm.probs[-control.index])
control.probs$Group <- 'Control'
treat.probs$Group <- 'Treatment'
# Combine the two dataframes into one.
plot.data <- rbind(control.probs, treat.probs)
# Histogram for control group
ggplot(plot.data, aes(Probability, fill = Group)) +
  geom_histogram(alpha = 0.5, position = 'identity', bins=15) +
  ggtitle("Distribution of estimated probabilities")
## Histogram solution without using ggplot
control.probs <- glm.probs[control.index]
treat.probs <- glm.probs[-control.index]
# Histogram for control group
hist(control.probs, main = "Distribution of estimated coefficients",
     col=rgb(0,0,1,1/2), xlab = "Estimated probabilities")
# Histogram for treatment group
hist(treat.probs,col=rgb(1,0,0,1/2), add=T)
legend("topright", legend=c("Control", "Treatment      "),
       col=c(rgb(0,0,1,1/2), rgb(1,0,0,1/2)), lwd = 10)

# Optional question
# Function to get the p-value of the regression
p.value <- function (model) {
  f <- summary(model)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  return(p)
}
alpha = 0.05
treat.effect <- vector()
sig.treat.effect <- vector()
for (i in 1:1000) {
  predictors_num <- sample(1:(length(df)-2),1)
  predictors <- sample(2:(length(df)-1), predictors_num)
  lm.fit <- lm(re78~., data=df[c(1,length(df),predictors)])
  predictors_val <- vector()
  for (j in predictors) {
    predictors_val <- c(predictors_val, median(df[,j]))
  }
  treatment <- sum(coef(lm.fit)*c(1,1,predictors_val))
  control <- sum(coef(lm.fit)*c(1,0,predictors_val))
  treat.effect[i] <- (treatment-control)
  if (p.value(lm.fit) < alpha){
    sig.treat.effect <- c(sig.treat.effect, (treatment-control))
  }
}
hist(treat.effect, main='Distribution of treatment effects', 
     xlab='Treatment effect')
hist(sig.treat.effect, xlab='Treatment effect', cex.main=0.9,
     main='Distribution of statistically significant treatment effects')
treat_effect <- data.frame(Treatment_effect=treat.effect)
sig_effect <- data.frame(Treatment_effect=sig.treat.effect)
treat_effect$Group <- 'All effects'
sig_effect$Group <- 'Statistically significant effects'
# Combine the two dataframes into one.
plot.data <- rbind(treat_effect, sig_effect)
# Histogram for control group
ggplot(plot.data, aes(Treatment_effect, fill = Group)) +
  geom_histogram(alpha = 0.5, position = 'identity', bins=15)+
  ggtitle("Distribution of treatment effects")