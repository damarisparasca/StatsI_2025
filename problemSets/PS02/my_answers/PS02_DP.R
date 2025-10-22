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

# Problem 1 
# make a matrix with the observed frequencies
observedfs <- matrix(c(14, 7, 6, 7, 7, 1),  
  			  nrow = 2)

# add row and column names 
rownames(observedfs) <- c("Upper Class", "Lower Class")
colnames(observedfs) <- c("Not stopped", "Bribe requested", 
						"Stopped/given warning")

# add margins - total counts for each category 
con_table = addmargins(observedfs)
con_table

# get the grand total from the sum column
grand_total = con_table[3,4]

# calculate the expected frequencies 
# row total * column total / grand total 
ef1 = con_table[1,4] * con_table[3,1]/ grand_total
ef2 = con_table[2,4] * con_table[3,1]/ grand_total
ef3 = con_table[1,4] * con_table[3,2]/ grand_total
ef4 = con_table[2,4] * con_table[3,2]/ grand_total
ef5 = con_table[1,4] * con_table[3,3]/ grand_total
ef6 = con_table[2,4] * con_table[3,3]/ grand_total

# put the expected frequencies in a matrix formatted like the observed frequencies matrix 
expectedfs <- matrix(c(ef1, ef2, ef3, ef4, ef5, ef6),  
              nrow = 2)

rownames(expectedfs) <- c("Upper Class", "Lower Class")
colnames(expectedfs) <- c("Not stopped", "Bribe requested", 
						"Stopped/given warning")
expectedfs

# calculate the chi square statistic 
chi_square <- sum((observedfs - expectedfs)^2 / expectedfs)
chi_square

# calculate the degrees of freedom 
# df = (r - 1) * (c - 1)
df = (2-1)*(3 -1)

# calculate the p-value 
p = pchisq(chi_square, df, lower.tail = FALSE)
p

# conclusion: p-value is higher than the alpha value = 0.1 
# we cannot reject the null hypothesis 

# get the standardised residuals
result <- chisq.test(observedfs)
result$residuals

# Problem 2 
# read dataset and show the first few rows
df <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv", header=T)
head(df)

# fit the linear model
# outcome = water, explanatory  = reserved 
fit = lm(df$water ~ df$reserved)
summary(fit)

