library(RColorBrewer)
library(MLmetrics)
library(forecast)
library(caret)
library(tidyr)

coun <- read.csv("266_country.csv")
rownames(coun) <- NULL
india <- read.csv("india.csv")
coun <- t(coun)
colnames(coun) <- coun[1,]
coun <- coun[-1,]
rownames(coun) <- NULL
year <- c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021")
coun <- cbind(year,coun)
coun<- coun[,c(-12,-11,-10,-9)]
write.csv(coun,"india_update.csv")
india_update <- read.csv("india_update.csv")
india_update <- india_update[,-1]
#read.csv(ind,file="C:/Users/Aashi/Desktop/MTH_project/266_country.csv")
#counting no. of Na in each rows
index <- numeric(length = 55)

j=1
for(i in 1:55){
  if(rowSums(is.na(india))[i]>=3){
    india_new = india[-i,]
  }}
india_new = subset(india,rowSums(is.na.data.frame(india))<3)
# GNI(atlas method) = final income in a year / population using Atlas methodology.
india_new = india_new[,c(-1,-3,-2,-5)]
#india_new = india_new[-24,]
#t_india <-t(india_new)
india_t <- setNames(data.frame(t(india_new[ , - 1])), india_new[ , 1])
#india_t = india_t[-1,]#removing series code
#cor(india_t$`Adolescent fertility rate (births per 1,000 women ages 15-19)`,india_t$`GDP growth (annual %)`)
#converting all character to int

j=1
for( j in 1:39){
  india_t[,j] = as.double(india_t[,j])

}

plot(india_t$`GDP (current US$)`,india_t$`Adolescent fertility rate (births per 1,000 women ages 15-19)`)
library(ggplot2)
ggplot(india_t, aes(`GDP (current US$)`,india_t[,10])) + geom_line()

cor.test(india_t$`Adolescent fertility rate (births per 1,000 women ages 15-19)`,india_t$`GDP (current US$)`)

library(psych)


#corPlot(data, cex = 1.2)
##library(WGCNA)
#corPlot(india_t, cex = 1.2)
#corAndPvalue(india_t, method='spearman')
#plot(cor(india_t, use = "complete.obs"))
india_t <- as.data.frame(india_t)

#corel = numeric(length = 43)
#for( i in 1:43){
 # if(sum(is.na(india_t[,i]))==0){
  #  corel[i] <- cor(india_t[,i],india_t[,10])
  #}
  #else {
   # corel[i] <- cor(na.omit(india_t[,i]),india_t[-which(is.na(india_t[,i])),10])
  #}
 
#}
#india_t = read.csv("india_globalvariable_data")
real_GDP <- numeric(length = 10)
real_GDP = india_t[,9]/india_t[,20]
india_t = cbind(real_GDP,india_t)
for( i in 1:40){
  plot(india_t[,10],india_t[,i], xlab = "GDP",ylab = colnames(india_t)[i]  ,pch = 16,col= rainbow(10),type="b")
  legend("topright", legend = c("2012","2013","2014","2015","2016","2017","2018","2019","2020","2021"),fill = rainbow(10),cex = 0.4)
}

  #india_t[,2] = as.double(india_t[,1])
plot(india_t[,10],india_t[,1])

india_t[is.na(india_t)]<- 0

library(randtests)
random <- numeric(length = 40)
for( i in 2:41){
  print(colnames(india_t[,i]))
 print(runs.test(india_t[,i],alternative = "left.sided",plot = TRUE,pvalue = ))
}

multi <- glm(india_t[,10] ~ .,data = india_t[,-10:-16])
other <- glm(india_t[,1] ~ india_t[,19]+india_t[,20]+india_t[,23]+india_t[,5],data = india_t)
other2 <- glm(india_t[,1]~ india_t[,8]+india_t[,27]+india_t[,17]++india_t[,36]+india_t[,39],data = india_t)

## india_update contain shortlisted variables

india_update <- read.csv("india_update.csv")

#india_update <- cbind(india_update,india_t[,10]) #adding GDP
#colnames(india_update)[10] <- "GDP"
#india_update <- india_update[,-4] # removing export of goods and services becoz net barter term index is similiar.
#plotting GDP with selected parameters;
for( i in 1:7){
  plot(india_update[,8],india_update[,i], xlab = "GDP",ylab = colnames(india_update)[i]  ,pch = 16,col= rainbow(10),type="b")
  legend("topright", legend = c("1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021"),fill = rainbow(10),cex = 0.4)
}
final <- lm(india_update[,9]~ india_update[,2]+india_update[,3]+india_update[,4]+india_update[,5],data = india_update)

#adding real_gdp
#india_update <- cbind(india_update,india_t[,1])
#colnames(india_update)[10] <- "Real_GDP"

#saving updated file
#write.csv(india_update,"india_update-Sheet1.csv")

t_gdp <- ts(india_update[,9],frequency = 1,start = 2012,end = 2021)
ddata <- decompose(t_gdp, "additive")
start(india_update[,9])
#ARIMA modelend
m_series <- auto.arima(india_update[,9])
    
 #If not already installed
library(vars)
library(astsa)
library(plotly)
GDP <- ts(india_update[,8],start = c(1998,1),end = c(2019,1),frequency = 1)
#plotting time series of GDP from 2009-2021
plot(GDP)
#trying to remove seasoning
c2 <- diff(data[,2], differences = 2)
c2_d2d12 <- diff(c2, lag = 10)
#PLOTTING TIME series graph for co-variants : x1....x7
data <- india_update[,2:7]
colnames(data) <- c("c1","c2","c3","c4","c5","c6")
par(mfrow=c(4, 2))
for( i in 2:7){
  obj_ts <- ts(india_update[,i],start = c(2000,1),end = c(2021,1))
  #cov_arima[i-1] <- Arima(data[,i],seasonal = FALSE,order = c(1,0,0),method="ML")
  plot.ts(obj_ts,xlab="years",ylab=i,col=i,plot.type = "multiple")
}
#VEry large Number
#making copy of only parameter

#data[,2]<- data[,2]/100000000
data[,c(1,2,3,4,5,6)] <- round(data[,c(1,2,3,4,5,6)],2)
mu <- sapply(data,mean)
sd <- sapply(data, sd)
# using VAR model
#making dimensions same for means and sd (13*7 matrix)
mu_mat <- matrix(rep(c(2.833818e+01 ,2.414240e+10, 2.984091e+01, 9.530591e+01 ,1.379318e+01,8.035000e+00),22),ncol = 6,nrow = 22,byrow = TRUE)

sd_mat <- matrix(rep(c(1.911458e+00,1.694725e+10 ,8.246780e+00, 8.592076e+00 ,6.320614e+00 ,4.788851e-01 ),22),ncol = 6,nrow = 22,byrow = TRUE)
data_update <- (data - mu_mat)/sd_mat
data <- data_update
data_update<- round(data_update,3)

modelvar <- VAR(data_update[,c(1,2,3,4,5,6)],p=1,type = "const" )
#dependency of other covariants over other is non-significants, we won'nt use VAR.
summary(modelvar)

indepen_test<- function(x1,x2,x3,x4){
  a<- abs(x1-x2)+abs(x3-x4)-abs(x1-x3)-abs(x2-x4)
  return(sign(a))
}


final <- matrix(data = NA,nrow =7315,ncol = 1)
combin_out <- read.csv("output_combination.csv")
mat_combin <- as.matrix(combin_out)
mat_combin <- rbind(c(1,2,3,4),mat_combin)
add <- matrix(data = NA)
inde <- matrix(data = NA)
#for(k in 1:6){
n = 2
  for(j in 1:7){
    for(i in 1:dim(mat_combin)[1]){
      index <- mat_combin[i,1:4]
      value1 <- indepen_test(data_update[index[[1]],n],data_update[index[[2]],n],data_update[index[[3]],n],data_update[index[[4]],n])
      value2 <- indepen_test(data_update[index[[1]],j],data_update[index[[2]],j],data_update[index[[3]],j],data_update[index[[4]],j])
      final[i]<- value1*value2
      #print(final[i])
    }
    print(c(n,j))
    print((sum(final)/(7315))) #22c4
  }
  


#applying polynomial Regression
set.seed(123)
K=22
degree<-7
index <- (time(c(1990:2050))-mean(time(c(1990:2050))))/sd(time(c(1990:2050)))

folds <- cut(seq(1,nrow(data_update)),breaks=2,labels=FALSE)
data_update <- cbind("x"=index[9:30],data_update)
india_update <- cbind("x"=c(index[9:30]),india_update)
# dependecy check :
# 1 -x,2,c4,c6,c3
# 2- x,c6,c3,c1,c5,c4(least)
# 3 - x,c2,c5,c6,c1*,c4* (least)
# 4 - x,1,2,c3*
# 5 - x,c3,c2,c6
# 6 -x,,c2,c3,c1,c5

# For time - (least)c5,c4,c6,c1,c3,c2(most dependent)
# to give all possible combinations of degree of poly

deci_base4 <- function(n,m){
  rem <- numeric(length = m)
  i=1
  while(n!=0){
    rem[i] <- as.integer(n%%4)
    n = as.integer(n/4)
    i=i+1}
  return(rev(rem))}

# Industrial value added(%GDP)
# 1 -x,2,c4,c6,c3(least)
# For time - (least)c5,c4,c6,c1,c3,c2(most dependent)

idx1 = 16
store <- matrix(data = NA,nrow = idx1,ncol = 2)
for(i in 1:idx1){
  store[i,] <- deci_base4(i-1,2)
}
K <- 22
mse1 = matrix(data=NA,ncol=2,nrow=idx1)
fit.test <- numeric(length = K)
shuff_data <-  data_update[sample(1:nrow(data_update)),]
for (j in 1:idx1){
  for(i in 1:2){
  #define training and testing data``
   
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- shuff_data[testIndexes, ]
  trainData <- shuff_data[-testIndexes, ]
  #use k-fold cv to evaluate models
  
    fit.train = lm(c1 ~ poly(x,store[j,1]+1)+poly(c4,store[j,2]+1),data = trainData)#+poly(c2,store[j,2]+1)+poly(c3,store[j,3]+1)+poly(c4,store[j,4]+1),data=trainData)
    fit.test= predict(fit.train, newdata=testData)
    #print(fit.test)
    mse1[j,i] = sqrt(mean((fit.test-testData[,2])^2 ))
    
  
  }
}

mmse_ind1 <- which(rowMeans(mse1)==min(rowMeans(mse1)))
deg1 = matrix(data = NA, ncol = 4, nrow = length(mmse_ind1))
#converting to decimal to septa
##deg1 <- matrix(data = NA,nrow = 6,ncol = )

deg1 <- deci_base4(mmse_ind1,2)
lm_final1 <- lm(c1~poly(x,4)+poly(c4,2),data = data_update)
model_fit_stats(lm_final1)
plot(data_update[,2], main="industrial value added",col = 4,ylim=c(-2,2), lty = 1)
lines(predict(lm_final1), col=2, lwd=2,type = "l", lty = 1)

  #fit <- lm(india_update[,3] ~ poly(x,4, raw=TRUE), india_update) 


#for Foreign direct investment
# 2- x,c6,c3,c1,c5,c4
# For time - (least)c5,c4,c6,c1,c3,c2(most dependent)
# -x,c1,c
deci_base7 <- function(n,m){
  rem <- numeric(length = m)
  i=1
  while(n!=0){
    rem[i] <- as.integer(n%%7)
    n = as.integer(n/7)
    i=i+1}
  return(rev(rem))}
indx2 = 64
store2 <- matrix(data = NA,nrow = indx2,ncol =3)
mse2 <- matrix(data = NA, nrow = indx2,ncol = 2)
for(i in 1:indx2){
  store2[i,] <- deci_base4(i-1,3)
}
for (j in 1:indx2){
  for(i in 1:2){
    #define training and testing data``
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- shuff_data[testIndexes, ]
    trainData <- shuff_data[-testIndexes, ]
    #use k-fold cv to evaluate models
    
    fit.train = lm(c2 ~ poly(x,store2[j,1]+1)+poly(c6,store2[j,2]+1)+poly(c3,store2[j,3]+1), data=trainData)
    fit.test= predict(fit.train, newdata=testData)
    #print(fit.test)
    #print(fit.test)
    mse2[j,i] = mean((fit.test-testData[,3])^2 )
    
  }
}
mse2 <- na.omit(mse2)
mmse_ind2 <- which(rowMeans(mse2)==min(rowMeans(mse2)))
#deg2 <- matrix(data = NA,nrow = 6,ncol = length(mmse_ind2))

deg2 <- deci_base4(mmse_ind2,3)

lm2_final <- lm(c2 ~ poly(x,1)+poly(c5,2), data=data_update)
model_fit_stats(lm2_final)
plot(data_update[,3], main="industrial value added",col = 4)
lines(predict(lm2_final), col=2, lwd=2)

tck <-  c(1:22)
axis(1, tck, labels=FALSE)
mtext(c(1998:2019), 1, 1, at=tck)

##for MechandiseTrade or c3
# 3 - x,c2,c5,c6,c1*,c4* (least)
# For time - (least)c5,c4,c6,c1,c3,c2(most dependent)

idx <- 64
store3 <- matrix(data = NA,nrow = idx,ncol =3)
mse3 <- matrix(data = NA, nrow = idx,ncol = 2)
for(i in 1:idx){
  store3[i,] <- deci_base4(i-1,3)
}
for (j in 1:idx){
  for(i in 1:2){
    #define training and testing data``
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- shuff_data[testIndexes, ]
    trainData <- shuff_data[-testIndexes, ]
    #use k-fold cv to evaluate models
    
    fit.train = lm(c3 ~ poly(x,store3[j,1]+1)+poly(c5,store3[j,2]+1)+poly(c6,store3[j,3]+1), data=trainData)
    fit.test= predict(fit.train, newdata=testData)
   # print(fit.test)
    #print(fit.test)
    mse3[j,i] = mean((fit.test-testData[,4])^2 )
    
  }
}
mse3 <- na.omit(mse3)
mmse_ind3 <- which(rowMeans(mse3)==min(rowMeans(mse3)))
#deg3 <- matrix(data = NA,nrow = 6,ncol = length(mmse_ind2))
deg3 <- deci_base4(mmse_ind3,3)
#final model for c3
lm3_final <- lm(c3~ poly(x,3)+poly(c5,1)+poly(c6,2), data=data_update )
model_fit_stats(lm3_final)
plot(data_update[,4], main="industrial value added",col = 4)
lines(predict(lm3_final), col=2, lwd=2)

#Net barter term -
# 4 - x,1,2,c3*
#For time - (least)c5,c4,c6,c1,c3,c2(most dependent)
idx <- 64
store4 <- matrix(data = NA,nrow = idx,ncol =3)
mse4 <- matrix(data = NA, nrow = idx,ncol = 2)
for(i in 1:idx){
  store4[i,] <- deci_base4(i-1,3)
}
for (j in 1:idx){
  for(i in 1:2){
    #define training and testing data``
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- shuff_data[testIndexes, ]
    trainData <- shuff_data[-testIndexes, ]
    #use k-fold cv to evaluate models
    
    fit.train = lm(c4 ~ poly(x,store4[j,1]+1)+poly(c3,store4[j,2]+1)+poly(c2,store4[j,3]+1), data=trainData)
    fit.test= predict(fit.train, newdata=testData)
    # print(fit.test)
    #print(fit.test)
    mse4[j,i] = mean((fit.test-testData[,5])^2 )
    
  }
}
mmse_ind4<- which(rowMeans(mse4)==min(rowMeans(mse4)))
#deg3 <- matrix(data = NA,nrow = 6,ncol = length(mmse_ind2))
deg4 <- deci_base4(mmse_ind4,3)

#final model for c4 ( total debt service)
lm4_final <- lm(c4~ poly(x,4)+poly(c3,1)+poly(c2,3), data=data_update )
model_fit_stats(lm4_final)

plot(data_update[,5], main="industrial value added",col = 4)
lines(predict(lm4_final),col=2,lwd=2)

# total debt services
# 5 - x,c3,c2,c6(least)
# For time - (least)c5,c6,c4,c3,c1,c2(most dependent)

idx <- 16
store5 <- matrix(data = NA,nrow = idx,ncol =2)
mse5 <- matrix(data = NA, nrow = idx,ncol = 2)
for(i in 1:idx){
  store5[i,] <- deci_base4(i-1,2)
}
for (j in 1:idx){
  for(i in 1:2){
    #define training and testing data``
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- shuff_data[testIndexes, ]
    trainData <- shuff_data[-testIndexes, ]
    #use k-fold cv to evaluate models
    
    fit.train = lm(c5 ~ poly(x,store5[j,1]+1)+poly(c2,store5[j,2]+1), data=trainData)
    fit.test= predict(fit.train, newdata=testData)
    # print(fit.test)
    #print(fit.test)
    mse5[j,i] = mean((fit.test-testData[,6])^2 )
    
  }
}
mmse_ind5<- which(rowMeans(mse5)==min(rowMeans(mse5)))
deg5 <- deci_base4(mmse_ind5[1],2)

#final model for c5 ( total debt service)
lm5_final <- lm(c5~poly(x,1)+poly(c2,3), data=data_update )
model_fit_stats(lm5_final)
plot(data_update[,6], main="industrial value added",col = 4)
lines(predict(lm5_final), col=2, lwd=2)

## 6th - Unemployment rate
# 6 -x,,c2,c3,c1,c5
# For time - (least)c5,c6,c4,c3,c1,c2(most dependent)

idx <- 64
store6 <- matrix(data = NA,nrow = idx,ncol =3)
mse6 <- matrix(data = NA, nrow = idx,ncol = 2)
for(i in 1:idx){
  store6[i,] <- deci_base4(i-1,3)
}
for (j in 1:idx){
  for(i in 1:2){
    #define training and testing data``
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- shuff_data[testIndexes, ]
    trainData <- shuff_data[-testIndexes, ]
    #use k-fold cv to evaluate models
    
    fit.train = lm(c6 ~ poly(x,store6[j,1]+1)+poly(c2,store6[j,2]+1)+poly(c3,store6[j,3]+1), data=trainData)
    fit.test= predict(fit.train, newdata=testData)
    # print(fit.test)
    #print(fit.test)
    mse6[j,i] = mean((fit.test-testData[,7])^2 )
    
  }
}
mmse_ind6<- which(rowMeans(mse6)==min(rowMeans(mse6)))
#deg3 <- matrix(data = NA,nrow = 6,ncol = length(mmse_ind2))
deg6 <- deci_base4(mmse_ind6,3)

#final model for c6 (Unemployment rate)
lm6_final <- lm(c6~poly(x,3)+poly(c2,1)+poly(c3,2), data=data_update )
model_fit_stats(lm6_final)
plot(data_update[,7], main="industrial value added",col = 4)
lines(predict(lm6_final), col=2, lwd=2)


# Now add Normalised_GDP)
norm_GDP <- (india_update[,9] - mean(india_update[,9]))/sd(india_update[,9])
data_update <- cbind(data_update,norm_GDP)
data_update <- round(data_update,3)



# As expected, our normalised GDP depends on all parameters + time series.
# (most)x,c2,c3,c1,c6,c4,c5(least)
# For time - (least)c5,c6,c4,c3,c1,c2(most dependent)
# Now considering all paramters, we will try to get best model :

idx <- 16384
num_para <- 7
gdp_store <- matrix(data = NA,nrow = idx,ncol =num_para)
gdp_mse <- matrix(data = NA, nrow = idx,ncol = 2)
for(i in 1:idx){
  gdp_store[i,] <- deci_base4(i-1,num_para)
}

newshuff_data <-  data_update[sample(1:nrow(data_update)),]

for (j in 1:idx){
  for(i in 1:2){
    #define training and testing data``
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- newshuff_data[testIndexes, ]
    trainData <- newshuff_data[-testIndexes, ]
    #use k-fold cv to evaluate models
    
    fit.train = lm(norm_GDP ~ poly(x,gdp_store[j,1]+1)+poly(c1,gdp_store[j,2]+1)+poly(c2,gdp_store[j,3]+1)+poly(c3,gdp_store[j,4]+1)+poly(c4,gdp_store[j,5]+1)+poly(c5,gdp_store[j,6]+1)+poly(c6,gdp_store[j,7]+1), data=trainData)
    fit.test= predict(fit.train, newdata=testData)
    # print(fit.test)
    #print(fit.test)
    gdp_mse[j,i] = mean((fit.test-testData[,8])^2 )
    
  }
}
gdp_mse <- na.omit(gdp_mse)
mmse_gdp<- which(rowMeans(gdp_mse)==min(rowMeans(gdp_mse)))
gdp_deg <- matrix(data = NA,ncol = 7,nrow  = length(mmse_gdp))
for(i in 1:length(mmse_gdp)){
  gdp_deg[i,] <- deci_base4(mmse_gdp[i],7)
}
#final model for c4 ( total debt service)
gdp_model <- lm(norm_GDP~poly(x,2)+poly(c1,1)+poly(c2,4)+poly(c3,2)+poly(c4,1)+poly(c5,1)+poly(c6,1), data=data_update )
model_fit_stats(gdp_model)
plot(c(data_update[,8],1.557,2.186), main="GDP",col = 4)
lines(c(predict(gdp_model),gdp_2020,gdp_2021), col=2, lwd=2)
test_data_2020 <- data.frame("x"=index[31],"c1"=(24.531 - mu[1])/sd[1],"c2"=(64362364994 - mu[2])/sd[2],"c3"=(24.351-mu[3])/sd[3],"c4"=(105.095-mu[4])/sd[4],"c5"=(15.191-mu[5])/sd[5],"c6"=(10.195-mu[6])/sd[6])
test_data_2021 <- data.frame("x"=index[32],"c1"=(26.071 - mu[1])/sd[1],"c2"=(44727277562.881 - mu[2])/sd[2],"c3"=(30.7436085101679-mu[3])/sd[3],"c4"=(96.625-mu[4])/sd[4],"c5"=(7.310-mu[5])/sd[5],"c6"=(7.713-mu[6])/sd[6])
test_data_1997 <- data.frame("x"=index[8],"c1"=(27.8370 - mu[1])/sd[1],"c2"=(3.577330e+09 - mu[2])/sd[2],"c3"=(1.838085e+01-mu[3])/sd[3],"c4"=(1.135922e+02-mu[4])/sd[4],"c5"=(2.630118e+01-mu[5])/sd[5],"c6"=(7.279-mu[6])/sd[6])
test_data_1996 <- data.frame("x"=index[7],"c1"=(27.9122703698412 - mu[1])/sd[1],"c2"=(2426057021.91092 - mu[2])/sd[2],"c3"=(18.082-mu[3])/sd[3],"c4"=(99.038-mu[4])/sd[4],"c5"=(27.615-mu[5])/sd[5],"c6"=(7.181-mu[6])/sd[6])
test_data_2022 <- data.frame("x"=index[33],"c1"=(25.620 - mu[1])/sd[1],"c2"=(49915506924.8119 - mu[2])/sd[2],"c3"=(34.7650739284701-mu[3])/sd[3],"c4"=mu[4],"c5"=mu[5],"c6"=(7.33-mu[6])/sd[6])

gdp_1996 <- predict(gdp_model,newdata = test_data_1996)
gdp_1996*s_gdp+m_gdp# actual - 392896860670.885

gdp_1997 <-  predict(gdp_model,newdata = test_data_1997)
gdp_1997*s_gdp+m_gdp # actual - 415867567334.185

gdp_2020 <- predict(gdp_model,newdata = test_data_2020)
gdp_2020*s_gdp+m_gdp # actual - 2671595389575.7

gdp_2021 <-  predict(gdp_model,newdata = test_data_2021)
gdp_2021*s_gdp+m_gdp # actual - 3150306834279.65

#we dont have all paramters available for 2022
gdp_2022 <-  predict(gdp_model,newdata = test_data_2022)
gdp_2022*s_gdp+m_gdp # actual - 3385089881935.39

pred_gdp <- s_gdp*predict(gdp_model) + m_gdp
plot(c(392896860670.885,415867567334.185,india_update[,9],2667687951796.5,3176295065497.24),col=4)
lines(c(gdp_1996*s_gdp+m_gdp,gdp_1997*s_gdp+m_gdp,predict(gdp_model)*s_gdp+m_gdp,gdp_2020*s_gdp+m_gdp,gdp_2021*s_gdp+m_gdp),col="red")
    