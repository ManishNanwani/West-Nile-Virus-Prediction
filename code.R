df<-read.csv("F:\\kaggle\\3. West Nile Virus\\train.csv")
spray<-read.csv("F:\\kaggle\\3. West Nile Virus\\spray.csv")
test<-read.csv("F:\\kaggle\\3. West Nile Virus\\test.csv")
head(spray)
head(df)
View(df)
View(spray)
View(test)
summary(df)
str(df)

df$Date<-as.Date(df$Date)
df$year<-as.numeric(strftime(df$Date,"%y"))
df$month<-as.numeric(strftime(df$Date,"%m"))

test$Date<-as.Date(test$Date)
test$year<-as.numeric(strftime(test$Date,"%y"))
test$month<-as.numeric(strftime(test$Date,"%m"))


spray$Date<-as.Date(spray$Date)
spray$year<-as.numeric(strftime(spray$Date,"%y"))
spray$month<-as.numeric(strftime(spray$Date,"%m"))


df1<-df[,c(4,8,9)]
df2<-spray[,c(3,4)]
df2$Block<-NA
View(d)
d<-rbind(df1,df2)

library(missForest)
d_imputed<-missForest(d)
block_imp<-d_imputed$ximp

spray$Block<-block_imp[10507:nrow(block_imp),1]
df$Sprayed<-vector(mode = "numeric",length = 10506)
i<-1
spray$Block<-as.numeric(spray$Block)
df$Block<-as.numeric(df$Block)


for(i in 1:length(df$Block))
{
  if(df$year[i]==11 | df$year[i]==13)
  {
   for(j in 1:length(spray$Block))
   {
     if(spray$Block[j]==df$Block[i] & df$year[i]== spray$year[j])
    {
      df$Sprayed[i]<-1
    }
   }
  }
}




df$AddressAccuracy<-factor(df$AddressAccuracy)
df$WnvPresent<-factor(df$WnvPresent)
df$Sprayed<-factor(df$Sprayed)

test$AddressAccuracy<-factor(test$AddressAccuracy)
test$Sprayed<-0
test$Sprayed<-factor(test$Sprayed)


spray$year<-factor(strftime(spray$Date,"%y"))
spray$month<-factor(strftime(spray$Date,"%m"))


View(df)
View(test)
colnames(train)
colnames(test)
train<-df[,-c(1,2,5,6,7,11)]
test<-test[,c(1,4,5,9,10,11,12,13,14)]

## To remove the extra level Unspecified Culex in Species column.
table(test$Species)
levels(test$Species)[levels(test$Species)=="UNSPECIFIED CULEX"]=NA
summary(test)
train_1<-train[,-6]
test_1<-test[,-1]

data<-rbind(train_1,test_1)
library(missForest)
d_imp=missForest(data)
test$Species=d_imp$ximp[10507:126799,"Species"]
summary(train)
library(devtools)
library(woe)
iv<-iv.mult(df[,-1],"WnvPresent",T)
iv.plot.summary(iv)
str(df)

iv<-iv.mult(train,"WnvPresent",T)
table(df$year,df$Sprayed)
View(df)
View(train)
summary(train)
model<-glm(WnvPresent~.,data = train,family = binomial(link = "logit"))
summary(model)
results<-predict(model,test,type="response")

x<-cbind(test$Id,results)
View(x)
colnames(x)<-c("id","WnvPresent")
write.csv(x,file="submission.csv", row.names = F)

