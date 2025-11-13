#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)
lapply(c("ggplot2", "stargazer"), pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read and look at data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/incumbents_subset.csv")
head(inc.sub)

# Question 1 
fit1 <- lm(voteshare ~ difflog, data = inc.sub)
fit1
stargazer(fit1)

png(file = "scatter_difflog_vs.pdf")
ggplot(inc.sub, aes(x = difflog, y = voteshare)) +
  geom_point() +
  geom_smooth(method = "lm") +  # regression line 
  labs(title = "Relationship between Incumbent Vote Share and Campaign Spending Gap",
       x = "Log of Campaign Spending Difference",
       y = "Incumbent Vote Share") +
    theme_minimal()
dev.off()

# save residuals 
resid_q1 <- fit1$residuals
resid_q1

# Question 2 
fit2 <- lm(presvote ~ difflog, data = inc.sub)
fit2
stargazer(fit2)

png(file = "scatter_difflog_pv.pdf")
ggplot(inc.sub, aes(x = difflog, y = presvote)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Relationship between Presidential Vote Share and Campaign Spending Gap",
       x = "Log of Campaign Spending Difference",
       y = "Presidential Vote Share") +
    theme_minimal()
dev.off()

# save residuals 
resid_q2 <- fit2$residuals
resid_q2

# Question 3 
fit3 <- lm(voteshare ~ presvote, data = inc.sub)
fit3
stargazer(fit3)

png(file = "scatter_pv_vs.pdf")
ggplot(inc.sub, aes(x = presvote, y = voteshare)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Relationship between Incumbent and Presidential Vote Share",
       x = "Presidential Vote Share",
       y = "Incumbent Vote Share") +
    theme_minimal()
dev.off()

# Question 4 
# make a residuals data frame
resid_df <- data.frame(resid_q1, resid_q2)
resid_df

fit4 <- lm(resid_q1 ~ resid_q2, data = resid_df)
fit4
stargazer(fit4)

png(file = "scatter_res2_res1.pdf")
ggplot(resid_df, aes(x = resid_q2, y = resid_q1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Relationship between Residuals 1 and Residuals 2",
       x = "Residuals 2",
       y = "Residuals 1") +
    theme_minimal()
dev.off()

# Question 5 
fit5 <- lm(voteshare ~ difflog + presvote, data = inc.sub)
fit5
stargazer(fit5)


