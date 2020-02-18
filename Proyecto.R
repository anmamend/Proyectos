data <- read.csv("E:/Henry D Saenz L/Documents/Uniandes/MEL/Proyecto/AB_NYC_2019.csv/AB_NYC_2019.csv")
data=AB_NYC_2019
head(data)
#Las variables id, name host id , host name , last review se eliminan del modelo, 
#ademas de la informacion de los barrios ya que esta contenida en las coordenadas
data<- data[,c(7:12,14:16,5)]
plot(data$price)
summary(data$price)
quantil=quantile(data$price,0.95)

summary(data)



####Remover errores en los datos, tomar hasta el percentil 95 de las variables precio, minimum nights, reviews, revies_per_month, calculated host_listing_count
q95precio=quantile(data$price,0.95,na.rm=T)
q95mn=quantile(data$minimum_nights,0.95,na.rm=T)
q95nr=quantile(data$number_of_reviews,0.95,na.rm=T)
q95rpm=quantile(data$reviews_per_month,0.95,na.rm=T)
q95chlc=quantile(data$calculated_host_listings_count,0.95,na.rm=T)
library(dplyr)
data=filter(data,price<q95precio,minimum_nights<q95mn,number_of_reviews<q95nr,reviews_per_month<q95rpm,calculated_host_listings_count<q95chlc)
summary(data)



#Los valores con NA en variables como price, neighbourhood_group y neighbourhood donde no se puede asumir el NA como 0
#se eliminan de la bd

data<-data[complete.cases(data[ , c(1:4)]),]

#Se actualizan los valores de NA en columnas numericas por cero 

data$minimum_nights[is.na(data$minimum_nights)] <- 0

data$number_of_reviews[is.na(data$number_of_reviews)] <- 0

data$reviews_per_month[is.na(data$reviews_per_month)] <- 0

data$calculated_host_listings_count[is.na(data$calculated_host_listings_count)] <- 0

data$availability_365[is.na(data$availability_365)] <- 0

#Se definen las variables Dummies

data$room_type=as.factor(data$room_type)

library(dummies)

TiposHabdbaux <-dummy(data$room_type, sep = "_")

#Eliminar un nivel

#Base Shared Room
TiposHabdbaux <-TiposHabdbaux[,1:2]


###Variable dummi neighborhoud
Barrio<-dummy(data$neighbourhood_group, sep = "_")
#Base Staten Island
Barrio<-Barrio[,1:4]


#Consolidar base de datos inicial

data <- cbind(data[,c(1,2,4:9)],TiposHabdbaux, Barrio)

#Inter
EntireXLat=data$latitude*data$`room_type_Entire home/apt`

EntireXLon=data$longitude*data$`room_type_Entire home/apt`

EntireXMinNigts=data$minimum_nights*data$`room_type_Entire home/apt`

EntireXNumReviews=data$number_of_reviews*data$`room_type_Entire home/apt`

EntireXReviMonth=data$reviews_per_month*data$`room_type_Entire home/apt`

EntireXCHLC=data$calculated_host_listings_count*data$`room_type_Entire home/apt`

EntireXAva=data$availability_365*data$`room_type_Entire home/apt`

#----------------
PrivateRXLat=data$latitude*data$`room_type_Private room`

PrivateRXLon=data$longitude*data$`room_type_Private room`

PrivateRXMinNigts=data$minimum_nights*data$`room_type_Private room`

PrivateRXNumReviews=data$number_of_reviews*data$`room_type_Private room`

PrivateRXReviMonth=data$reviews_per_month*data$`room_type_Private room`

PrivateRXCHLC=data$calculated_host_listings_count*data$`room_type_Private room`

PrivateRXAva=data$availability_365*data$`room_type_Private room`

## Interacciones por Barrio (Sector de NY)

interaccionMinnights=Barrio*data$minimum_nights
colnames(interaccionMinnights)=c('Bronx_minnights','Brooklyn_minnights','Manhattan_minnights','Queens_minnights')
interaccionNumReviews=Barrio*data$number_of_reviews
colnames(interaccionNumReviews)=c('Bronx_numreviews','Brooklyn_numreviews','Manhattan_num_reviews','Queens_numreviews')
interaccionReviMonth=Barrio*data$reviews_per_month
colnames(interaccionReviMonth)=c('Bronx_reviewpermonth','Brooklyn_reviewpermonth','Manhattan_reviewpermonth','Queens_reviewpermonth')
interaccionCHLC=Barrio*data$calculated_host_listings_count
colnames(interaccionCHLC)=c('Bronx_CHLC','Brooklyn_CHLC','Manhattan_CHLC','Queens_CHLC')
interaccionAva=Barrio*data$availability_365
colnames(interaccionAva)=c('Bronx_Ava','Brooklyn_Ava','Manhattan_Ava','Queens_Ava')

#####No se va a incluir latitud, ni longitud, ya que no hay una evidencia que indique que 
#estas variables sean ordenadas, es decir que al aumentar la latitud/longitud aumente o disminuya el precio.


fit<-lm(data$price~data$minimum_nights+data$number_of_reviews+data$reviews_per_month+data$calculated_host_listings_count+data$availability_365+data$`room_type_Entire home/apt`+
          data$`room_type_Private room`+EntireXMinNigts+EntireXNumReviews+EntireXReviMonth+EntireXCHLC+EntireXAva+PrivateRXMinNigts+PrivateRXNumReviews+PrivateRXReviMonth
        +PrivateRXCHLC+PrivateRXAva+data$neighbourhood_group_Manhattan+data$neighbourhood_group_Bronx+
          data$neighbourhood_group_Brooklyn+data$neighbourhood_group_Queens+interaccionMinnights+interaccionNumReviews+
          interaccionReviMonth+interaccionCHLC+interaccionAva,data = data)

summary(fit)

#No se si se pueda sacar de una las variables no significativas  o toca verificar los supuestos primero yo diria que si

#Verificar supuestos del modelo-------------------

######Datos atípicos e influyentes en el modelo#########

library(olsrr)
ols_plot_dffits(fit)

ols_plot_cooksd_chart(fit)

cooksd <- cooks.distance(fit)
data2=cbind(price=data$price,minimum_nights=data$minimum_nights,number_of_reviews=data$number_of_reviews,reviews_per_month=data$reviews_per_month,calculated_host_listings_count=data$calculated_host_listings_count,availability_365=data$availability_365,
            `room_type_Entire home/apt`=data$`room_type_Entire home/apt`,`room_type_Private room`=data$`room_type_Private room`,EntireXMinNigts,EntireXNumReviews,EntireXReviMonth,EntireXCHLC,EntireXAva,PrivateRXMinNigts,
            PrivateRXNumReviews,PrivateRXReviMonth,PrivateRXCHLC,PrivateRXAva,Barrio,interaccionMinnights,interaccionNumReviews,
              interaccionReviMonth,interaccionCHLC,interaccionAva)
influential <- as.numeric(names(cooksd)[(cooksd > (0.1/dim(data)[1]))])

data2 <- data2[-influential, ]

fit2<-lm(price~.,data=data.frame(data2))
summary(fit2)
ols_plot_cooksd_chart(fit2)
###Se elimina la mayor parte de datos influyentes (outliers), para que el problema 
#sea un problema lineal

#Heteroscedasticidad
resi=fit2$residuals
data2=data.frame(data2)
plot(data2$price,resi,type="b",lwd=2)

plot(resi)
abline(0,0)

library(lmtest)
bptest(price~.,data=data2)


###El modelo es heteroscedastico

#Correlacion de errores No es un problema temporal, no tiene sentido mirar autocorrelación

library(lmtest)
dwtest(fit)

#Realizar pruebas para eliminar variables.

#Feasible Weighted LS
resi2=resi^2

aux=lm(log(resi2)~.-price,data2)
summary(aux)

wg=1/exp(predict(aux))

fit3=lm(price~.,data=data2,weights=wg)
summary(fit3)
resi3=fit3$residuals


Omega=diag(c(exp(predict(aux))))
L=diag(resi3^2*wg^2)

X=model.matrix(price~.,data=data2,weights=wg)
V_fwls=vcov(fit3)

qqnorm(fit3$residuals)
qqline(fit3$residuals)

#Se asume normalidad por el gráfico

#V_fwls=solve(t(X)%*%solve(Omega)%*%X)%*%t(X)%*%L%*%X%*%solve(t(X)%*%solve(Omega)%*%X)


##################
library(leaps)
rrg=regsubsets(price~., data=data2, weights=wg,method="exhaustive", nvmax=42)
s_sum=summary(rrg)
plot(s_sum$cp[8:42])
which.min(s_sum$cp)
s_sum$outmat[37,]
#El mejor CP de Mallows se logra con todas las variables


#Se usa la prueba de Wald
#Las interacciones de tipo de habitación son significativas?.
A=matrix(c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), ncol=42, byrow=T)
c=rep(0,10)

EP=t(A%*%fit3$coefficients-c)%*%A%*%V_fwls%*%t(A)%*%(A%*%fit3$coefficients-c)
EP
pivalor=1-pchisq(EP,10)
pivalor

#Existe al menos una interacción que si es significativa

#Las interacciones de barrio son significativas?.

A=matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1), ncol=42, byrow=T)
c=rep(0,20)

EP=t(A%*%fit3$coefficients-c)%*%A%*%V_fwls%*%t(A)%*%(A%*%fit3$coefficients-c)
EP
pivalor=1-pchisq(EP,20)
pivalor
#Existe al menos una interacción que si es significativa

###El número de reviews es una variable significativa en el modelo?

A=matrix(c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0), ncol=42,byrow=T)
c=rep(0,7)
EP=t(A%*%fit3$coefficients-c)%*%A%*%V_fwls%*%t(A)%*%(A%*%fit3$coefficients-c)
EP
pivalor=1-pchisq(EP,7)
pivalor

# El número de reviews no es una variables significativa en el modelo

#### El Tipo de habiación influye en el precio?

A=matrix(c(0,0,0,0,0,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
         ncol=42, byrow=T)
c=rep(0,2)
EP=t(A%*%fit3$coefficients-c)%*%A%*%V_fwls%*%t(A)%*%(A%*%fit3$coefficients-c)
EP
pivalor=1-pchisq(EP,2)
pivalor

#El tipo de habitación si influye en el modelo

#####El Barrio influye en los resultados?

A=matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1),
  ncol=42, byrow=T)
c=rep(0,4)
EP=t(A%*%fit3$coefficients-c)%*%A%*%V_fwls%*%t(A)%*%(A%*%fit3$coefficients-c)
EP
pivalor=1-pchisq(EP,4)
pivalor

#El barrio si influye en el modelo


###Las interacciones de los barrios son significativas?

A=matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1),
         ncol=42, byrow=T)
c=rep(0,20)
EP=t(A%*%fit3$coefficients-c)%*%A%*%V_fwls%*%t(A)%*%(A%*%fit3$coefficients-c)
EP
pivalor=1-pchisq(EP,20)
pivalor

#Las interacciones de los barrios si son significativas.

###Cuesta lo mismo dormir en una habitación privada en Queens que en Brooklin?

A=matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,-1,0,1,0,-1,0,1,0,-1,0,1,0,-1,0,1,0,-1,0,1,0,-1),
ncol=42, byrow=T)
c=rep(0,1)
EP=t(A%*%fit3$coefficients-c)%*%A%*%V_fwls%*%t(A)%*%(A%*%fit3$coefficients-c)
EP
pivalor=1-pchisq(EP,1)
pivalor

#Es diferente del precio de una habitación en Queens al de una habitación en Brooklyn.

################################

#Se considera el modelo completo sin la variable numero de reviews ya que se indicaba como no significativa en el modelo
#y para facilitar la interpretabilidad

data3=data2[,-c(3,10,15,27:30)]
fit4<-lm(price~.,data=data3)
resi4=fit4$residuals^2

aux=lm(log(resi4)~.-price,data3)
summary(aux)

wg=1/exp(predict(aux))

fit5=lm(price~.,data=data3,weights=wg)

summary(fit5)



#Multicolinealidad

corr<-car::vif(fit5)

print(corr)

# Se aplicara ridge hasta disminuir la multicolinealidad y poder darle una interpretación al resultado.
summary(data3[,2:5])

fridge=lmridge(price~.,data=data3,weights=wg, K=0.1)

car::vif(fridge)
