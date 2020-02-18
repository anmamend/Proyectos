FIFA_test <-read.csv("Test_FIFA.csv", sep = ";")
FIFA_train <- read.csv("Train_FIFA.csv", sep = ";")

class(FIFA_train$LW)

attributes(FIFA_test)

#install.packages("Hmisc")
library(Hmisc)

describe(FIFA_train)

?replace

#######Exploración########

plot(FIFA_train$Age, FIFA_train$Wage)
hist(FIFA_train$Age)
hist(FIFA_train$Wage)

#######Total Base#####

FIFA_TOTAL<-rbind(FIFA_train,FIFA_test)

###Reemplazos de las columnas 18 a 43
reemplazo <- data.frame(lapply(FIFA_TOTAL[,18:43], function(x){gsub("+2", "",x, fixed = TRUE, ignore.case = FALSE)}))
reemplazo <- lapply(reemplazo, function(x){gsub("\n","",x, fixed = T,ignore.case = F)})
reemplazo <- data.frame(reemplazo)
reemplazo <- lapply(reemplazo,function(x){as.integer(x)})
reemplazo <- data.frame(reemplazo)
reemplazo[reemplazo==1]<- NA

##############################
equipos <-levels(FIFA_train$Club)
library(dplyr)
equipos<-group_by(FIFA_train,Club)
resumen<-summarise(equipos,salarios=mean(Wage))
resumen <- resumen[order(resumen$salarios,decreasing = T),]
ranking <- sort(resumen$salarios,decreasing = T)
plot(ranking)

library(cluster)
kk <- kmeans(resumen[,2],4 )

wss <- rep(0,times=15)
for (i in 1:15) wss[i] <- sum(kmeans(resumen[,2],centers=i)$withinss)
#Within Sum of Squares para diferentes k
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")  #grafica de wss

km=kmeans(resumen[,2],3)

#plotting in 2 dimensions with PCA

#pcx=princomp(xxs)$scores[,1:2] #primeros dos componentes principales
plot(resumen[,2], col = km$cluster)  #separando clusters


#funcion el libreria cluster
clusplot(resumen[,2],km$cluster)  #la misma grafica
pagos <- cbind(resumen[,1], km$cluster)
colnames(pagos)<-c("Club","Codigo_Club")
res<-merge(FIFA_TOTAL,pagos,by.x="Club",by.y="Club")
res <- res[order(res$ID,decreasing = F),]
codigoclub <- as.factor(res$Codigo_Club)


releaseclause <- lapply(FIFA_TOTAL$Release.Clause, function(x){gsub("\u20ac", "", x, fixed = T )})
releaseclause <- lapply(releaseclause, function(x){gsub("K", "e3", x, fixed = T )})
releaseclause <- lapply(releaseclause, function(x){gsub(c("M"), "e6", x, fixed = T )})
releaseclause <- lapply(releaseclause,function(x){as.integer(x)})

prestamo<- lapply(FIFA_TOTAL$Loaned.From, function(x){
  if(x==""){0}else{1}})
prestamo <- as.character(prestamo)
prestamo <- as.factor(prestamo)

#install.packages("lubridate")
library(lubridate)

fecha <- as.character(FIFA_TOTAL$Joined)
fecha <- parse_date_time(fecha,orders = "bdy")

fecha <- as.numeric(fecha)
fecha[is.na(fecha)]<- max(fecha, na.rm = T)

#year(FIFA_train[1,"Contract.Valid.Until"])

fechasFinContrato <- as.character(FIFA_TOTAL$Contract.Valid.Until)
fechas2 <- year(parse_date_time(fechasFinContrato,orders = "bdy"))
fechasFinContrato <- as.numeric(fechasFinContrato)
datafecha=data.frame(cbind(fechas2,fechasFinContrato))
datafecha= datafecha %>% mutate(fecha_final_contrato=coalesce(fechas2,fechasFinContrato))



#####Cambios####

datosTrain <- FIFA_TOTAL
datosTrain[,18:43]<- reemplazo
datosTrain[,"Club"]<- codigoclub
datosTrain[,"Release.Clause"]<- as.numeric(releaseclause)
datosTrain[,"Loaned.From"]<- prestamo
datosTrain[,"Joined"]<- fecha
datosTrain[,"Contract.Valid.Until"]<- datafecha$fecha_final_contrato

levels(datosTrain$Work.Rate)
plot(datosTrain$Work.Rate, datosTrain$Wage, ylim=c(0, 25000))

work<-group_by(FIFA_train,Work.Rate)
resumen<-summarise(work,salarios=mean(Wage))
resumen <- resumen[order(resumen$salarios,decreasing = T),]
ranking <- sort(resumen$salarios,decreasing = T)
plot(ranking)

library(cluster)
wss <- rep(0,times=8)
for (i in 1:8) wss[i] <- sum(kmeans(resumen$salarios,centers=i)$withinss)
#Within Sum of Squares para diferentes k
plot(1:8, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")  #grafica de wss

km=kmeans(resumen[,2],3)

#plotting in 2 dimensions with PCA
plot(resumen[,2], col = km$cluster)  #separando clusters


#funcion el libreria cluster
clusplot(resumen[,2],km$cluster)  #la misma grafica
pagos <- cbind(resumen[,1], km$cluster)
colnames(pagos)<-c("Work.Rate","Codigo_Work.Rate")
res<-merge(FIFA_TOTAL,pagos,by.x="Work.Rate",by.y="Work.Rate")
res <- res[order(res$ID,decreasing = F),]
codigoWR<- as.factor(res$Codigo_Work.Rate)

datosTrain[, "Work.Rate"]<- codigoWR
levels(FIFA_train$Position)

####
posicion<-group_by(FIFA_train,Position)
resumen<-summarise(posicion,salarios=mean(Wage))
resumen <- resumen[order(resumen$salarios,decreasing = T),]
#resumen <- sort()
plot(resumen)


#Metodo JERARQUICO AGLOMERATIVO: enfoque de distancias

xx<- resumen$salarios
hc <- hclust(dist(xx), method="ave")  #con distancia promedio entre clusters
#hc <- hclust(dist(xx), method="single") #con distancia minima
#hc <- hclust(dist(xx), method="complete")  #con distancia maxima

#Graficando el Dendograma
plot(hc,hang=-1)

# cut tree into 5 clusters
rect.hclust(hc, k=4)  #Definir los k clusters
groups <- cutree(hc, k=4)  #Graficar los k clusters

posicion <- cbind(resumen$Position, groups)

groups
hc$labels
resumen$Position

plot(FIFA_train[,"Preferred.Foot"],FIFA_train[,"Wage"], ylim=c(0,25000))

#funcion el libreria cluster
pagos <- cbind(resumen[,1], groups)
colnames(pagos)<-c("Position","Codig_Position")
res<-merge(FIFA_TOTAL,pagos,by.x="Position",by.y="Position")
res <- res[order(res$ID,decreasing = F),]
codigoPosicion<- as.factor(res$Codig_Position)

###
nombresEstadisticas <- colnames(datosTrain[,18:43])
datosTrain <- datosTrain %>% mutate(hab_posi=pmax(LS,ST,RS,LW,LF,CF,RF,RW,LAM,RAM,CAM,LM,LCM,CM,RCM,RM,LWB,LDM,CDM,RDM,RWB,LB,LCB,CB,RCB,RB,na.rm=T))

plot(datosTrain$Overall, datosTrain$hab_posi)

cor(cbind(datosTrain$Overall,datosTrain$hab_posi),use="complete.obs")


datosTrainCompletos <- subset(datosTrain,select=-c(LS,ST,RS,LW,LF,CF,RF,RW,LAM,RAM,CAM,LM,LCM,CM,RCM,RM,LWB,LDM,CDM,RDM,RWB,LB,LCB,CB,RCB,RB))
datosTrainCompletos<-datosTrainCompletos %>% subset(select=-c(Skill.Moves,Body.Type,Position,Height,Weight))
datosTrainCompletos<- cbind(datosTrainCompletos,codigoPosicion)

correlaciones <- cor(datosTrainCompletos[,13:46], use = "complete.obs")

#install.packages("reshape")
library(reshape)

cormelt <- melt(correlaciones)

#install.packages("ggplot2")
library(ggplot2)

ggplot(data=cormelt,aes(x=X1,y=X2,fill=value))+geom_tile()

ff=filter(cormelt,value>0.9, value !=1)

ggplot(data=ff,aes(x=X1,y=X2,fill=value))+geom_tile()+theme(axis.text.x=element_text(angle=90))

length(unique(ff$X1))
length(unique(ff$X2))

estadisiticaGK <-datosTrainCompletos %>% subset(select=c(GKReflexes,GKDiving,GKHandling,GKKicking, GKPositioning))
estadisiticaGK <- rowMeans(estadisiticaGK)

estadisiticaControl <- rowMeans(datosTrainCompletos[,c("Dribbling","BallControl","ShortPassing")])
estadisticasVel <- rowMeans(datosTrainCompletos[,c("Acceleration","SprintSpeed")])
estadisticasDef <- rowMeans(datosTrainCompletos[,c("StandingTackle","Interceptions","Marking","SlidingTackle")])

estadisticasNuevas <- cbind(estadisiticaGK,estadisiticaControl,estadisticasVel,estadisticasDef)
datosTrainCompletos <- cbind(datosTrainCompletos, estadisticasNuevas)
datosTrainCompletos<-datosTrainCompletos %>% subset(select=-c(GKReflexes,GKDiving,GKKicking,GKHandling,
                                                              GKPositioning,Acceleration,SprintSpeed, Dribbling,BallControl,
                                                              ShortPassing,StandingTackle,Interceptions,Marking,SlidingTackle))

############datosTrainCompletos############

sum(is.na(datosTrain$Age))
na_count <-sapply(datosTrainCompletos, function(y) sum(length(which(is.na(y)))))

datosTrainCompletos[is.na(datosTrainCompletos$estadisiticaControl),1]
datosTrainCompletos[is.na(datosTrainCompletos$Weak.Foot),1]
datosTrainCompletos$hab_posi[is.na(datosTrainCompletos$estadisiticaControl)]


quantile(datosTrainCompletos$Wage, c(.05,.1),na.rm=T)
plot(datosTrainCompletos$codigoPosicion,datosTrainCompletos$estadisiticaControl)

esta <-sapply(datosTrainCompletos, function(y) sd(y,na.rm=T))
FIFA_train$Position

variablesx=data.frame(cbind(datosTrainCompletos$Overall,datosTrainCompletos$Potential, FIFA_TOTAL$Position))
variablesx$X3=as.factor(variablesx$X3)
revisar=c(13:32,37:40)
prueba=datosTrainCompletos

for (i in revisar){
  variabley=datosTrainCompletos[,i]
  fit=lm(variabley~.,data=variablesx,na.action=na.omit)
  
  reemp=predict(fit,variablesx[is.na(variabley),])
  
  
  prueba[is.na(variabley),i]=reemp
}
esta <-sapply(prueba, function(y) sum(length(which(is.na(y)))))

revisar2=c(33,35)
for (i in revisar2){
  variabley=datosTrainCompletos[,i]
  fit=lm(variabley~X1+X2,data=variablesx,na.action=na.omit)
  
  reemp=predict(fit,variablesx[is.na(variabley),])
  
  
  prueba[is.na(variabley),i]=reemp
}

plot(prueba$Contract.Valid.Until,prueba$Wage)
prueba$International.Reputation <-sapply(prueba$International.Reputation, function(y) {if(is.na(y)) {1} else{y}})
prueba$Weak.Foot <-sapply(prueba$Weak.Foot, function(y) {if(is.na(y)) {1} else{y}})
prueba$Contract.Valid.Until <-sapply(prueba$Contract.Valid.Until, function(y) {if(is.na(y)) {2018} else{y}})
esta <-sapply(prueba, function(y) sum(length(which(is.na(y)))))
esta

datos_entrenamiento<-prueba[1:8000,]
datos_prueba<-prueba[8001:10000,] 


###################Arranca la predicción################


###Datos atípicos######
# Se consideran que los datos atípicos son importantes y proveen información

#####Seleccion Exhaustiva de variables########

require(caret)
flds <- createFolds(1:8000, k = 10)
library(leaps)

##################3
reg_subset=regsubsets(Wage~.,datos_entrenamiento,nvmax=20,method="exhaustive")
reg_sub_summary=summary(reg_subset)
reg_sub_summary

reg_sub_summary$cp
plot(reg_sub_summary$cp)
variabes=which.min(reg_sub_summary$cp)
####Seleccion de 18 varianbles
p1=reg_sub_summary$which[variabes,][reg_sub_summary$which[variabes,]==TRUE]
p1=labels(p1)[2:variabes+1]
p1=paste(p1, collapse="+")

for (j in 1:9){
  p1=gsub(paste('',j, sep=''), '', p1)}
p1=gsub(paste('','Left', sep=''), '', p1)
p1=gsub(paste('','Right', sep=''), '', p1)
#######3
prom_reg=matrix(rep(0,50),ncol=10)
for(i in 1:10){
dat_prue=datos_entrenamiento[unlist(flds[i],use.names=FALSE),-1]
dat_entre=datos_entrenamiento[-unlist(flds[i],use.names=FALSE),-1]




lm1=lm(paste('Wage~',p1),data=datos_entrenamiento)
summary(lm1)
pred=predict(lm1,dat_prue[,-33])
summary(lm1)
mse=mean((dat_prue[,33]-pred)^2)
prom_reg[1,i]=mse


##########PCAR######
library(pls)

lm3=pcr(Wage~.,data=dat_entre,scale=T,validation="CV")
#plot(RMSEP(lm3)$val[1:60])
predpp=predict(lm3,dat_prue,ncomp=18)

msepp=mean((dat_prue$Wage)^2)
msepp
prom_reg[2,i]=msepp
print(i)

#########Partial Least Squares#######
library(pls)
lm4=plsr(Wage~.,data=dat_entre,scale=T,validation="CV")
#summary(lm4)
#plot(RMSEP(lm4)$val[1:60])
predpl=predict(lm4,dat_prue,ncomp=23)
msepl=mean((dat_prue$Wage-predpl)^2)
prom_reg[3,i]=msepl

######(LASSO)

library(glmnet)
X=model.matrix(Wage~.,dat_entre)[,-1]
Xtest=model.matrix(Wage~.,dat_prue)[,-1]

ytrain=dat_entre$Wage
ytest=dat_prue$Wage

cvmod1=cv.glmnet(X,ytrain,alpha=1)
cvmod1$lambda.min
plot(cvmod1)
mod_pen1=glmnet(X,ytrain,alpha=1,lambda=cvmod1$lambda.min)
#coef(mod_pen1)
predp1=predict(mod_pen1,Xtest)
msep1=mean((ytest-predp1)^2)
msep1
prom_reg[4,i]=msep1
######(Ridge)


cvmod=cv.glmnet(X,ytrain,alpha=0)
cvmod$lambda.min
plot(cvmod)

mod_pen2=glmnet(X,ytrain,alpha=0,lambda=cvmod$lambda.min)
#coef(mod_pen2)

predp2=predict(mod_pen2,Xtest)
msep2=mean((ytest-predp2)^2)
prom_reg[5,i]=msep2



}

rownames(prom_reg)=c('RL','PCA','PLS','LASSO','RIDGE')
boxplot(t(prom_reg))
boxplot(t(prom_reg)[,c(1,3:5)])
rowMeans(prom_reg)


########GAMS#############

#Generalized Additive Models GAMS
library(gam)
library(mgcv)
library(MASS)

p2=strsplit(p1,"\\+")
p2=unique(unlist(p2))

valor=paste(lapply(colnames(dat_entre[,-33]),function(x) {
  if ((class(dat_entre[1,x])=='factor')|| (x=='Weak.Foot')|| (x=='International.Reputation')|| (x=='Contract.Valid.Until')){x}
    else{paste('s(',x,')',sep='')}
}),collapse='+')
valor=as.formula(paste('Wage~',valor))


GAMS_r=matrix(rep(0,10),ncol=10)
for(i in 1:10){
  dat_prue=datos_entrenamiento[unlist(flds[i],use.names=FALSE),-1]
  dat_entre=datos_entrenamiento[-unlist(flds[i],use.names=FALSE),-1]

gams.fit <- gam(formula=valor,data=dat_entre)
print(i)
#summary(gams.fit)
#anova(gams.fit)
#gam.check(gams.fit)
#AIC(gams.fit)
pre=predict(gams.fit,dat_prue)

GAMS_r[1,i]=sum((dat_prue$Wage-pre)^2)
}

GAMS_r=GAMS_r/800
boxplot(t(GAMS_r))

#######REDES NEURONALES##########

xx=model.matrix(~.-1,data=datos_entrenamiento[,-c(1,34)])
yy=datos_entrenamiento[,34]
nngrid=expand.grid(.decay=c(0,0.01,.1,.5),.size=seq(1,10,by=2))  #valores para tune

ctrl=trainControl(method="cv", number=10)  #forma de hacer estimacion de test MSE

nettune=train(xx,yy,
              method="nnet" , tuneGrid=nngrid,
              maxit=5000,trControl=ctrl, trace=F,linout=T,
              MaxNWts= 451,metric="RMSE"
)

nettune
plot(nettune)


library(nnet)
redneuronal=matrix(rep(0,10), ncol=10)

for (i in 1:10){
  dat_prue=datos_entrenamiento[unlist(flds[i],use.names=FALSE),-1]
  dat_entre=datos_entrenamiento[-unlist(flds[i],use.names=FALSE),-1]
xx=model.matrix(~.-1,data=dat_entre[,-33])
yy=dat_entre[,33]
xxp=model.matrix(~.-1,data=dat_prue[,-c(33)])
yyp=dat_prue[,33]


nnfit=nnet(xx,yy,size=9,decay=0.5,linout=T, na.action=na.omit)

redneuronal[1,i]=mean((predict(nnfit,xxp)-yyp)^2)

}

boxplot(t(redneuronal))

#########ARBOLES CON BOOSTING###############

library(gbm)
#datos_nuevo=model.matrix(~.-1,data=datos_entrenamiento[,-c(1)])

shrink=c(0.1,0.3,0.5)
prof=3:5

mmm=matrix(rep(0,3*3),ncol=3)
c1=0
c2=0
for (k in shrink){ c1=c1+1
  for (j in prof){c2=c2+1
    auxi=rep(0,10)
    for (i in 1:10){
dat_prue=datos_entrenamiento[unlist(flds[i],use.names=FALSE),-1]
dat_entre=datos_entrenamiento[-unlist(flds[i],use.names=FALSE),-1]
boost.car=gbm(Wage~.,data=dat_entre,distribution="gaussian",
              n.trees=1000,interaction.depth=j,shrinkage=k)
print(paste(i,j,k))

pred_boo=predict(boost.car,dat_prue,type="response",n.trees=1000)
auxi[i]=sum((pred_boo-dat_prue[,33])^2)
    }
    mmm[c1,c2]=mean(auxi)}
  c2=0
}

load('CVArboles.R')
arboles=mmm/8000
colnames(arboles)=c('pr3','pr4','pr5')
rownames(arboles)=c('s0.1','s0.3','s0.5')
save(mmm,file='CVArboles.R')

boost.car=gbm(Wage~.,data=dat_entre[,-c(1)],
              n.trees=1000,interaction.depth=10,shrinkage=0.1)


############SUPPORT VECTOR REGRESION######

#Support Vector Regression

library(e1071)
#xx=model.matrix(~.-1,data=prueba[,-c(1,34)])
#ppp=princomp(xx)
#dent=cbind(ppp$scores[1:8000,1:10],Wage=prueba[1:8000,34])
svmmatrix=rep(0,10)
for (i in 1:10){
  dat_prue=datos_entrenamiento[unlist(flds[i],use.names=FALSE),-1]
  dat_entre=datos_entrenamiento[-unlist(flds[i],use.names=FALSE),-1]
svm_reg=svm(Wage~.,data=dat_entre, kernel='radial')
predisvm=predict(svm_reg,dat_prue)
print(i)
svmmatrix[i]=mean((predisvm-dat_prue[,33])^2)
}
boxplot(svmmatrix)

###El MSE da muy alto, no vale la pena hacer CV t tunning, tarda mucho y el MSE en
#los datos de entrenamiento es muy alto

#tc <- tune.control(cross = 1)
#rang=list(cost=c(0.01),gamma=c(0.1))

#set.seed(3)

#Tunning en dos parametros de calibracion
#tune_svm_reg=tune(svm,Wage~.,data=datos_entrenamiento,kernel="radial",
#                  ranges=rang, tubecontrol=tc)

###############Mejor Modelo###############3

rowMeans(prom_reg)
  mean(redneuronal)
arboles
mean(svmmatrix)

#####Entreno Boosting 0.1 y 3#####

boost.car=gbm(Wage~.,data=datos_entrenamiento[,-c(1)],distribution="gaussian",
              n.trees=1000,interaction.depth=3,shrinkage=0.1)
 
pred_boo=predict(boost.car,datos_prueba,type="response",n.trees=1000)
View(pred_boo)
write.table(pred_boo,'C:/Users/anmam/Documentos sin sincronizar/resultado2.csv',sep=';')

#####Regresion LASSO#######
library(glmnet)
X1=model.matrix(ID~.-1,prueba[,-34])[,-c(1)]
X=X1[1:8000,]
XP=X1[8001:10000,]
ytrain=datos_entrenamiento$Wage


cvmod1=cv.glmnet(X,ytrain,alpha=1)
cvmod1$lambda.min
plot(cvmod1)
mod_pen1=glmnet(X,ytrain,alpha=1,lambda=cvmod1$lambda.min)

coef(mod_pen1)

predp1=predict(mod_pen1,XP)
pred2=as.matrix(lapply(predp1,function(x){if (x<1000){1000} else{x}}))

write.table(pred2,'C:/Users/anmam/Documentos sin sincronizar/resultadoLASSO.csv',sep=';')


###########Intento con Poisson Regresion############33

datos_entrenamiento[,c(3,4,5,34,35,36)]

xtotal=cbind(X,Wage=ytrain)

boost.car=gbm(Wage/1000~.,data=data.frame(xtotal[1:7200,c(1:5,37:40,44)]),distribution="poisson",
              n.trees=2000,interaction.depth=3,shrinkage=0.07)

pred_boo=predict(boost.car,data.frame(xtotal[7201:8000,c(1:5,37:40,44)]),type="response",n.trees=2000)


mean((pred_boo*1000-as.matrix(xtotal[7201:8000,44]))^2)/2.5


#####3Intento conSVM##########

svm_reg=svm(Wage~.,data=datos_entrenamiento[,-1], kernel='radial')
predisvm=predict(svm_reg,datos_prueba[,-c(1,34)])

mean((as.matrix(predisvm)-as.matrix(pruebayres))^2)


View(pred_boo)
write.table(pred_boo,'C:/Users/anmam/Documentos sin sincronizar/resultado3.csv',sep=';')

plot()

