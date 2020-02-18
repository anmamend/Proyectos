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

rang=list(cost=c(1,2,5,10),gamma=c(0.05,0.1))

#set.seed(3)

#Tunning en dos parametros de calibracion
class.weights=c('0'=1,'1'=2.5)
dd2=as.factor(datosdum$Revenue[1:900])
datosA=cbind(trainDum[,-c(1,22)],Revenue=dd2)


tune_svm=tune(svm,Revenue~.,data=datosA,kernel='radial',
              ranges=rang,class.weights=class.weights)

plot(tune_svm)
tune_svm$best.model



#Modelo Calibrado
auxi=rep(0,10)
flds <- createFolds(1:900, k = 10)

for (i in 1:10){
  dat_prue=datosA[unlist(flds[i],use.names=FALSE),]
  dat_entre=datosA[-unlist(flds[i],use.names=FALSE),]
  
svm_fit=svm(Revenue~.,data=dat_entre, cost=10,probability=T, gamma=0.05,kernel="radial",class.weights=class.weights)
predisvm=predict(svm_fit,dat_prue,probability=T,decision.values=F)
predicciones=attr(predisvm,'probabilities')[,2]
roc_boo=roc(dat_prue[,21],predicciones)
auxi[i]=auc(roc_boo)

}

totalsv=cbind(original_costo5=aux1, original_costo10=aux2,rela3a1_costo5=aux3,rela3a1_costo10=aux4 )

boxplot(totalsv)
colMeans(totalsv)
#Ahora se predice en la muestra de prueba
predisvm=predict(svm_fit,testDum[,-22],probability=T,decision.values=F)
predicciones=attr(predisvm,'probabilities')[,2]

plot(predisvm)

hist(predisvm)

quantile(predisvm, probs = 0.8)

resp <-rep(0, times=165)
resp[predisvm>=0.03]<-1
respuestas <- as.data.frame(cbind(test$Id,resp))

colnames(respuestas)<- c("Id", "Predicted")
rownames(respuestas) <- NULL

write.csv(x = respuestas, file = "respuestas.csv", sep = ",", row.names = F)

write.csv(x = predicciones, file = "predicciones.csv", sep = ",", row.names = F)


############################

#########ARBOLES CON BOOSTING###############

library(gbm)
#datos_nuevo=model.matrix(~.-1,data=datos_entrenamiento[,-c(1)])

shrink=c(0.1,0.3,0.5)
prof=3:5

mmm=matrix(rep(0,3*3),ncol=3)

finalmod=matrix(rep(0,3*3*10),ncol=10)
tot=1

require(caret)
library(pROC)
flds <- createFolds(1:900, k = 10)

c1=0
c2=0
for (k in shrink){ c1=c1+1
for (j in prof){c2=c2+1
auxi=rep(0,10)
for (i in 1:10){
  dat_prue=trainDum[unlist(flds[i],use.names=FALSE),-1]
  dat_entre=trainDum[-unlist(flds[i],use.names=FALSE),-1]
  boost.car=gbm(Revenue~.,data=dat_entre,distribution="bernoulli",
                n.trees=1000,interaction.depth=j,shrinkage=k)
  print(paste(i,j,k))
  
  pred_boo=predict(boost.car,dat_prue,type="response",n.trees=1000)
  roc_boo=roc(dat_prue[,21],pred_boo,n.trees=1000)
  auxi[i]=auc(roc_boo)
  finalmod[tot]=auc(roc_boo)
  tot=tot+1
}
mmm[c1,c2]=mean(auxi)}
c2=0
}

colnames(mmm)=c('par3','par4','par5')
rownames(mmm)=c('s_0.1','s_0.3','s_0.5')

boxplot(finalmod, ylim=c(0.94,1))

###Mejores valores c1=0.5, c2=3

res=gbm(Revenue~.,data=trainDum,distribution="bernoulli",
    n.trees=1000,interaction.depth=4,shrinkage=0.3)

pp=predict(res,testDum, n.trees=1000, type='response')

colnames(resultado_a)='pp'
sum(resultado_a-pp)
write.csv(x = pp, file = "predicciones.csv", sep = ",", row.names = F)

###########################3
