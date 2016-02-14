library(sandwich)
data("PublicSchools")
summary(PublicSchools)

#Removing school with missing data, execise caution with removing missing data
ps <- na.omit(PublicSchools)
ps$Income <- ps$Income / 10000

#Plot the data to examine if there influential observations
plot(Expenditure ~ Income, data = ps, ylim = c(230, 830))
ps_lm <- lm(Expenditure ~ Income, data = ps)
abline(ps_lm)

#############################################
#The basic tool for diagnosing regression models are residual plots
plot(ps_lm, which = 1:6)

#The first plot is to examine if there are any systematic variations in the data
#The second plot test the assumptions behind the statistical tests and confidence intervals assume the errors are i.i.d and normally distributed
#The third plot checks for homoskedasticity which can also impact statistical significance tests and also checks i.i.d. errors
#The fourth plot shows Cook's distance, a statistical measure of being an outlier


#############################################
#Deletion diagnostics is another tool to examine influential observations
influence.list <- influence.measures(ps_lm)
summary(influence.list)
influence.stat <- as.data.frame(influence.list[[1]])
influence.bol <- as.data.frame(influence.list[[2]])
