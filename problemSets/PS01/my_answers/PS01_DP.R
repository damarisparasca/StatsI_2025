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

lapply(c(),  pkgTest, "ggplot2")

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# calculate the mean 
mean_y = mean(y)
mean_y

# calculate the standard deviation 
demeanedSum <- y - mean_y
squaredError <- demeanedSum ^ 2 
variance <- sum(squaredError)/(length(y)-1)
stdev = sqrt(variance)
stdev

# calculate the standard error 
sterror = stdev/sqrt(length(y))
sterror

# find the t-score for the desired confidence level 
t = qt(p = (1 - 0.9)/2 , df = 24, lower.tail = FALSE)
t 

# construct the confidence interval 
lowerlimit = mean_y - t * sterror
upperlimit = mean_y + t * sterror
lowerlimit
upperlimit

# hypothesis testing 
# step 1 - assumptions: data is quantitative, sampling method is random, sample size is smaller than 30 -> t-score instead of z-score 

# step 2- null hypothesis:  mean is lower or equal to 100 
# alternative hypothesis: mean is higher than 100 

# step 3 - calculate a test statistic
ts = (mean_y - 100)/sterror
ts 

# step 4 - calculate the p-value 
p = pt(ts, df = 24, lower.tail = FALSE)
p

# step 5 - conclusion 
# p-value is higher than alpha -> cannot reject the null hypothesis that the mean is lower or equal to 100 


#####################
# Problem 2
#####################
lapply(c("ggplot2"), pkgTest)
install.packages("ggpubr")
library(ggpubr)

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)
head(expenditure)

# plot the relationships between all the variables 
# add the correlation coefficient and hide the p-value
# save the plots as a png to add to the latex file 

png(file = "scatter_x1_y.pdf")
ggplot(expenditure, aes(x = X1, y = Y)) +
  geom_point() +
  labs(title = "Relationship between personal income and housing assistance expenditure",
                 x = "personal income (USD per capita)",
                 y = "expenditure on housing assistance (USD per capita)") +
                 stat_cor(aes(label = ..r.label..)) +
  theme_minimal()
dev.off()

png(file = "scatter_x2_y.png")
ggplot(expenditure, aes(x = X2, y = Y)) +
  geom_point() +
  labs(title = "Relationship between financial insecurity and housing assistance expenditure",
       x = "number of financially insecure residents (per 100,000)",
       y = "expenditure on housing assistance (USD per capita)") +
  stat_cor(aes(label = ..r.label..)) +
  theme_minimal()
dev.off()

png(file = "scatter_x3_y.png")
ggplot(expenditure, aes(x = X3, y = Y)) +
  geom_point() +
  labs(title = "Relationship between urban residency and expenditure on housing assistance (in state)",
       x = "number of urban residents (per 1000)",
       y = "expenditure on housing assistance (USD per capita)") +
  stat_cor(aes(label = ..r.label..)) +
  theme_minimal()
dev.off()

png(file = "scatter_x1_x2.png")
ggplot(expenditure, aes(x = X1, y = X2)) +
  geom_point() +
  labs(title = "Relationship between personal income and financial insecurity",
       x = "personal income (USD per capita)",
       y = "number of financially insecure residents (per 100,000)")+
  stat_cor(aes(label = ..r.label..)) +
  theme_minimal()
dev.off()

png(file = "scatter_x1_x3.png")
ggplot(expenditure, aes(x = X1, y = X3)) +
  geom_point() +
  labs(title = "Relationship between personal income and urban residency",
       x = "personal income (USD per capita)",
       y = "number of urban residents (per 1000)") +
  stat_cor(aes(label = ..r.label..)) +
  theme_minimal()
dev.off()

png(file = "scatter_x2_x3.png")
ggplot(expenditure, aes(x = X2, y = X3)) +
  geom_point() +
  labs(title = "Relationship between financial insecurity and urban residency",
       x = "number of financially insecure residents (per 100,000)",
       y = "number of urban residents (per 1000)") +
  stat_cor(aes(label = ..r.label..)) +
  theme_minimal()
dev.off()


# turn the Region into a factor to make the plot more readable
# now it shows the name of the region instead of 1,2,3,4
expenditure$Region <- factor(expenditure$Region,
  levels = c(1, 2, 3, 4),
  labels = c("Northeast", "North Central", "South", "West"))
  
# make a boxplot to compare the per capita housing expenditure in different regions 
png(file = "boxplot_reg_y.png")
ggplot(expenditure, aes(x=Region, y=Y, group=Region)) +
   geom_boxplot() +
    theme(
        legend.position="none",
        plot.title = element_text(size=11)) +
        labs(title = "Boxplot for housing assistance expenditure for each region",
                 x = "Region",
                 y = "expenditure on housing assistance (USD per capita)") + 
	theme_minimal()
dev.off()

# plot the relationship between x1, y and region
# each region is represented by a different shape and colour
png(file = "scatter_x1_y_reg.png")
ggplot(expenditure, aes(x = X1, y = Y, shape = factor(Region), color = factor(Region))) +
  geom_point(size = 3) +  # increase size to distinguish between the shapes 
  scale_color_manual(values = c("black", "red", "green", "purple")) +
  scale_shape_manual(values = c(21, 17, 18, 19)) +  # each value corresponds to a shape 
  labs(title = "Relationship between personal income and house assistance expenditure per region",
       x = "personal income (USD per capita)",
       y = "expenditure on housing assistance (USD per capita)",
       shape = "Region", 
       color = "Region") +
  theme_minimal()
dev.off()




