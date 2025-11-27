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
lapply(c("car", "stargazer", "ggplot2"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

install.packages(car)
library(car)
data(Prestige)
help(Prestige)

# Question 1
df = Prestige
df$professional <- ifelse(df$type == "prof", 1, 0)
df

pres_model = lm(prestige ~ income + professional +
				income:professional, data = df)
pres_model
stargazer(pres_model)

inc_increase = predict(pres_model,newdata=data.frame(professional = 1,income = 1000))
inc_baseline = predict(pres_model,newdata=data.frame(professional = 1,income = 0))

me_income = inc_increase - inc_baseline
me_income

prof_yes = predict(pres_model,newdata=data.frame(professional = 1,income = 6000))
prof_no = predict(pres_model,newdata=data.frame(professional = 0,income = 6000))

me_prof = prof_yes - prof_no
me_prof

# Question 2 
# h0: beta = 0
# hA: beta not = 0

# test statistic
t_stat1 = 0.042/0.016
t_stat1

# degrees of freedom
deg_fr = 131 - 2 - 1

# calculate the p-value
p_value1 = 2*pt(t_stat1, deg_fr, lower.tail = F)
p_value1

# h0: beta = 0
# hA: beta not = 0

# test statistic
t_stat2 = 0.042/0.013
t_stat2

# calculate the p-value
p_value2 = 2*pt(t_stat2, deg_fr, lower.tail = F)
p_value2

