test <-read.csv("test.csv", sep = ";")
train <- read.csv("train.csv", sep = ";")

datos <- rbind(train,test)

class(FIFA_train$LW)

attributes(FIFA_test)

library(caret)

nearZeroVar(train)
colnames(train[,11])

levels(train[,"Month"])

cor(train)

library(caret)

dum<-dummyVars("~.", data = datos)
dum$facVars

datosdum<- data.frame(predict(dum, newdata = datos))

correlaciones<-cor(datosdum[,-22])

library(reshape)

cormelt <- melt(correlaciones)

#install.packages("ggplot2")
library(ggplot2)

ggplot(data=cormelt,aes(x=X1,y=X2,fill=value))+geom_tile()

cormelt[cormelt$value>=0.9&cormelt$value!=1,]

plot(datosdum$ExitRates,datos$BounceRates)

install.packages("dplyr")
library(dplyr)
graph<-train$Revenue %>% table() %>% barplot()

#####NO SE SI HACER ALGO DE CLASES IMBALANCEADAS###

trainID <- train$Id

trainDum <- datosdum[datosdum$Id==trainID,]
testDum <- datosdum[datosdum$Id!=trainID,]

#######################################################################
library(e1071)


#SVM
svm_fit=svm(Revenue~.-Id,data=trainDum)
summary(svm_fit)  #Resultado del modelo (significancia individual)

rang=list(cost=c(0.01,0.05,0.1,1,2,5),gamma=c(0.1,0.5,1,2))

set.seed(3)

#Tunning en dos parametros de calibracion
tune_svm=tune(svm,Revenue~-Id,data=trainDum,
              ranges=rang)

tune_svm$best.model

#Modelo Calibrado
svm_fit=svm(Revenue~.-Id,data=datosdum, cost=0.1,gamma=0.1,probability=T,kernel="radial")


#Ahora se predice en la muestra de prueba
predisvm=predict(svm_fit,testDum[,-22],probability=T,decision.values=F)
predisvm

plot(predisvm)

hist(predisvm)

quantile(predisvm, probs = 0.8)

resp <-rep(0, times=165)
resp[predisvm>=0.03]<-1
respuestas <- as.data.frame(cbind(test$Id,resp))

colnames(respuestas)<- c("Id", "Predicted")
rownames(respuestas) <- NULL

write.csv(x = respuestas, file = "respuestas.csv", sep = ",", row.names = F)

