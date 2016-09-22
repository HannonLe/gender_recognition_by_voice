###### Gender Recognition by Voice #####
## Aug 27 2016

# Before moving on, PLEASE change the working directory to current project directory "XXX/Gender_Recognition_by_Voice"
setwd("D:/SelfLearning/Kaggle/Gender_Recognition_by_Voice")


voice <- read.csv("data/voice.csv",header=T)

head(voice)
str(voice)
nrow(voice)

## check. Data is clean.

## EDAV

pairs(voice[,1:5])

library(lattice)
library(RColorBrewer)
COL <- colorRampPalette(brewer.pal(11, "RdBu"))
myPanel <- function(x, y, z, ...) {
  panel.levelplot(x,y,z,...)
  panel.text(x,y,round(z,2),col='black',cex=0.8)
}
levelplot(cor(voice[,-21]),zlim=c(-1,1),scales=list(x=list(rot=90)),col.regions=COL,at=seq(-1,1,0.1),
          main="Correlation Matrix of Q1-Q28 responses",panel=myPanel)

### drop high correlation variables
library(dplyr)
voice <- select(voice,-one_of(c("centroid","IQR","dfrange")))

## divide data
set.seed(827)
intrain <- sort(sample(1:nrow(voice),round(0.75*nrow(voice),0),replace=F))

## logistic regression

fit <- glm(label~.,family="binomial",data=voice[intrain,])
summary(fit)
pred <- predict(fit,newdata=voice[-intrain,-18],type="response")
pred <- factor(pred >= 0.5,labels=c("female","male")) # final prediction

sum(pred == voice[-intrain,"label"])/length(pred) # overall prediction accuracy
1 - sum(pred == "female" & voice[-intrain,"label"] == "male")/sum(voice[-intrain,"label"]=="male") # male accuracy
1 - sum(pred == "male" & voice[-intrain,"label"] == "female")/sum(voice[-intrain,"label"]=="female") # female accuracy


