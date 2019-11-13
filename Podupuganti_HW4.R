
# --------------------- LIB ------
install.packages('EnvStats')
install.packages('mice')
install.packages('VIM')
install.packages('naniar')
install.packages('tidyverse', dependencies=TRUE, type="source")
library('mlbench')
library('car')
library('EnvStats')
library('mice')
library('ggplot2')
library('VIM')
library(tidyverse)
library(naniar)
library(caret)
library(reshape)

par(mar=c(1,1,1,1))

dev.off()
# ---------------------distributions and boxplots Before transformations
data("Glass") # there is no missing data
Glass <- Glass[!duplicated(Glass),] # removed duplicates
cols <- colnames(Glass)
par(mar = rep(2, 4)) # margins
par(mfrow=c(3,3)) 
for(col in seq(1, 9))
{
x <- Glass[,col]
hist(x,
     xlim=c(min(x),max(x)), probability=T, 
     col='lightblue',
     main=cols[col])
abline(v = mean(x), col = "blue", lwd = 2)
abline(v = median(x), col = "green", lwd = 2)
lines(density(x,bw=0.3),type="l", col='red', lwd=3)
#lines(density(log(x),bw=0.3),type="l", col='Green', lwd=3)
legend('topright', c("mean", "median"),
       col=c("blue", "green"), lty=1:1, bty = "n")
}

for(col in seq(1, 9))
{
  x <- Glass[,col]
  boxplot(x, main = cols[col])
}
#
#Ba, Mg, Fe think could benefit more from a skew transformation
# ---------------------distributions and boxplots after transformations using symbox
# for Mg
par(mfrow=c(1,1)) 
symbox(Glass$Mg, data=Glass, powers=c(3,2,1,0,-0.5,-1,-2), main = "Mg - parallel boxplots for different values of lambda")
par(mfrow=c(2,1)) 
hist(Glass$Mg)
hist(log(Glass$Mg))

# for Ba
par(mfrow=c(1,1)) 
symbox(Glass$Ba, data=Glass, powers=c(3,2,1,0,-0.5,-1,-2),main = "Ba - parallel boxplots for different values of lambda")
par(mfrow=c(2,1)) 
hist(Glass$Ba)
hist((Glass$Ba)^-2)

# for Fe
par(mfrow=c(1,1)) 
symbox(Glass$Fe, data=Glass, powers=c(3,2,1,0,-0.5,-1,-2),main = "Fe - parallel boxplots for different values of lambda")
par(mfrow=c(2,1)) 
hist(Glass$Fe)
hist((Glass$Fe)^3)
# ---------------------distributions and boxplots after transformations using boxcox


# for Mg
boxcox(Glass$Mg+0.001, optimize = TRUE,lambda=c(-3,3))
par(mfrow=c(2,1)) 
hist(Glass$Mg)
hist((Glass$Mg**3-1)/3)  

# for Ba
boxcox(Glass$Ba+0.001, optimize = TRUE,lambda=c(-3,3))
par(mfrow=c(2,1)) 
hist(Glass$Ba)
hist((Glass$Mg**0.1563629-1)/0.1563629)  


# for Fe
boxcox(Glass$Fe+0.001, optimize = TRUE,lambda=c(-3,3))
par(mfrow=c(2,1)) 
hist(Glass$Fe)
hist((Glass$Fe**0.5784652-1)/0.5784652)  

# --------------------Missing data -----------------
?msleep
data(msleep)
?msleep
msleep %>% select_if(is.numeric) %>% mutate_all(is.na) %>% summarise_all(mean)
par(mfrow=c(1,1))
a <- aggr(msleep)
summary(a)
#missings - containing the amount of missing or imputed values in each variable.
#combinations - the combinations of variables along with their frequencies and percentages.

x <- msleep[,-c(1:5)]
matrixplot(x,main = "matrixplot for missing values in msleep data ")# gives you the missing comination of data b/w the attributes. 
matrixplot(x,sortby = 3,main = "matrixplot for missing values in msleep (sortby sleep_cycle ) ")# gives you the missing comination of data b/w the attributes. 
marginplot(x[,c(2,3)], main = "marginplot b/w sleep_cycle and sleep_rem") # checking additional information on missing data b/w two attributes 
marginplot(x[,c(5,3)], main = "marginplot b/w sleep_cycle and brain wt") # checking additional information on missing data b/w two attributes 

md.pattern(x, plot = TRUE)
md.pairs(x)



vis_miss(x) #This plot provides a specific visualiation of the amount of missing data, showing in black the location of missing values, and also providing information on the overall percentage of missing values overall (in the legend), and in each variable
# here I can see data missing in black where I can see the squence, frequnecy of missing data, and also combinations,
gg_miss_upset(x) #An upset plot from the UpSetR package can be used to visualise the patterns of missingness, or rather the combinations of missingness across cases. To see combinations of missingness and intersections of missingness amongst variables, use the gg_miss_upset function:

#This tells us:

#Only 3 attrivutes has have missing values
#sleep_cycle has the most missing values
#There are some 14 cases where all three atributes have missing values together
#There are 8 cases where rem and cycle has missing values togather, similarly 11 cases for wt and cycle.

#------------ transformations:
completedata = na.omit(msleep[,-c(1:5)])
par(mfrow=c(3,2)) 
for (i in colnames(completedata))
{
  print(i)
hist((completedata[[i]]),main = i)
}
plot(completedata)

for (i in colnames(completedata))
{
  print(i)
  hist(log(completedata[[i]]),main =  paste('log',i) )
}
# lets see the correlation b/w sleeptotal with brain wt and body wt
par(mfrow=c(3,2)) 
cor((completedata))
cor(log(completedata))
plot(completedata$sleep_total, completedata$bodywt, pch = 16, cex = 1.3, col = "blue", main = "sleep total PLOTTED AGAINST BODY wt")
abline(lm(completedata$sleep_total ~ completedata$bodywt),col= 'green')
# after logorithm
plot(log(completedata$sleep_total), log(completedata$bodywt), pch = 16, cex = 1.3, col = "blue", main = "sleep total PLOTTED AGAINST BODY wt")
abline(lm(log(completedata$sleep_total) ~ log(completedata$bodywt)),col= 'green')


plot(completedata$sleep_total, completedata$brainwt, pch = 16, cex = 1.3, col = "blue", main = "sleep total PLOTTED AGAINST brain wt")
abline(lm(completedata$sleep_total ~ completedata$brainwt))
# after logorithm

plot(log(completedata$sleep_total), log(completedata$brainwt), pch = 16, cex = 1.3, col = "blue", main = "sleep total PLOTTED AGAINST brain wt")
abline(lm(log(completedata$sleep_total) ~ log(completedata$brainwt)), col= 'green')

plot(completedata$sleep_rem, completedata$brainwt, pch = 16, cex = 1.3, col = "blue", main = "sleep rem PLOTTED AGAINST brain wt")
abline(lm(completedata$sleep_rem ~ completedata$brainwt),col= 'green')
# after logorithm

plot(log(completedata$sleep_rem), log(completedata$brainwt), pch = 16, cex = 1.3, col = "blue", main = "sleep rem PLOTTED AGAINST brain wt")
abline(lm(log(completedata$sleep_rem) ~ log(completedata$brainwt)), col= 'green')

plot(completedata$sleep_rem, completedata$bodywt, pch = 16, cex = 1.3, col = "blue", main = "sleep rem PLOTTED AGAINST BODY wt")
abline(lm(completedata$sleep_rem ~ completedata$bodywt),col= 'green')
# after logorithm

plot(log(completedata$sleep_rem), log(completedata$bodywt), pch = 16, cex = 1.3, col = "blue", main = "sleep rem PLOTTED AGAINST BODY wt")
abline(lm(log(completedata$sleep_rem) ~ log(completedata$bodywt)), col= 'green')

#as far not getting any good linear relation lets convert hrs to mins and kg to grams and check b/w the cycle and brain wt 
par(mfrow=c(1,2)) 
plot(completedata$sleep_cycle*60, completedata$brainwt*1000, pch = 16, cex = 1.3, col = "blue", main = "sleep cycle PLOTTED AGAINST brain wt")
# after logorithm
plot(log(completedata$sleep_cycle*60), log(completedata$brainwt*1000), pch = 16, cex = 1.3, col = "blue", main = "log sleep cycle PLOTTED AGAINST log brain wt")

?msleep


#------------
impdata <- mice(msleep) # default package used here is pmm, with default no of imputations used is 5
#The seen observation were missing information for attributes. Above we can see what values were imputed for those observations in each of our 5 imputed datasets.
plot(impdata)
#Building and evaluating linear model on multiple imputed data
fit<-with(impdata,lm(sleep_total~sleep_cycle+sleep_rem))
summary(fit)

combFit <- pool(fit) 
summary(combFit)

# get full data from each imputed values
data1<- mice::complete(impdata,1)


summary(fullfit<-lm(data=na.omit(msleep),sleep_total~sleep_cycle+sleep_rem)) # with complete data


# with different imputation models
?mice
# add all want to compare with
methods <- c('norm','norm.predict')
for( m in methods)
{
  impdata<-mice(msleep,meth=m)
  print(impdata)
  fit<-with(impdata,lm(sleep_total~sleep_cycle+sleep_rem))
  print(summary(fit))
  combFit <- pool(fit) 
  print(summary(combFit))
  
}



# referenecs

#https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/
#https://web.maths.unsw.edu.au/~dwarton/missingDataLab.html
#https://stats.idre.ucla.edu/r/faq/how-do-i-perform-multiple-imputation-using-predictive-mean-matching-in-r/
#http://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/mi.html
#https://francish.netlify.com/post/multiple-imputation-in-r-with-regression-output/

# cleaning
rm(list = ls(all.names = TRUE))
gc()
