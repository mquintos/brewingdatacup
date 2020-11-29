#CÓDIGO de Manuel Quintos y Analy Moreno (DataCup2020) - Team DataTiger
#############################################################################PARTE I
library(tidyr)
library(tidyverse)
library(sqldf)
library(corrplot)

active_promos = read.csv('active_promos.csv', header = TRUE);
clients = read.csv('clients_attributes.csv', header = TRUE);
executed_promos = read.csv('executed_promos.csv', header = TRUE);
sales = read.csv('sales.csv', header = TRUE);
test = read.csv('test.csv', header = TRUE);

dim(active_promos)
summary(active_promos)
str(active_promos)

##dando formato fecha a las columnas de fecha:
active_promos$Fecha_Desde <- as.Date(active_promos$Fecha_Desde)
active_promos$Fecha_Hasta <- as.Date(active_promos$Fecha_Hasta)

#############################################################################
##comprobaci?n de fecha para cruce de datos:
active_promos$mesDesde <- as.numeric(format(active_promos$Fecha_Desde,'%m'))
active_promos$mesHasta <- as.numeric(format(active_promos$Fecha_Hasta,'%m'))

active_promos$a?oDesde <- as.numeric(format(active_promos$Fecha_Desde,'%Y'))
active_promos$a?oHasta <- as.numeric(format(active_promos$Fecha_Hasta,'%Y'))
##Comprobaci?n mes si la promoci?n fue en el mismo a?o desde-hasta
active_promos$mmm <- ""
active_promos$mmm <- active_promos$mesHasta - active_promos$mesDesde
abc <- sqldf("select CodigoDC, Marca, Cupo, mesDesde, mesHasta from active_promos where mmm <> 0 ")
head(abc)

##Comprobaci?n a?o si la promoci?n fue en el mismo a?o desde-hasta
active_promos$aaa <- ""
active_promos$aaa <- active_promos$a?oHasta - active_promos$a?oDesde
pqr <- sqldf("select CodigoDC, Marca, Cupo, mesDesde, mesHasta from active_promos where aaa <> 0 ")
head(pqr)
############################################################################


##########################Creamos una sola tabla que consolida###############################
##Creamos llaves en cada tabla para unirlos en una base de datos. La tabla sobre la cual se 
## llenar? informaci?n es active_promos:

##tabla active_promos: creamos una llave, concatenando campos, para facilitar el cruce con la tabla 
##executed_promo:
active_promos <- within(active_promos, llave1 <- paste(CodigoDC,Cliente, Marca, Cupo,sep='-'))#con executed_promos
active_promos <- within(active_promos, llave2 <- paste(Cliente, Marca, Cupo,sep='-'))#con sales
##con la tabla clientes, la llave ser? el campo "cliente"

##tabla executed_promos: creamos una llave1 para facilitar el cruce con la tabla active_promo:
executed_promos <- within(executed_promos, llave1 <- paste(CodigoDC,Cliente, Marca, Cupo,sep='-'))

##tabla sales: creamos una llave2 para facilitar el cruce con la tabla active_promo:
sales <- within(sales, llave2 <- paste(Cliente, Marca, Cupo,sep='-'))

####################creamos una tabla auxiliar######################
##a la tabla auxiliar le agregamos informaci?n de la tabla clientes:
active_promos_aux <- left_join(active_promos,clients, 
                               by=c("Cliente"))

##a la tabla auxiliar le agregamos informaci?n de la tabla sales:
#active_promos_aux <- left_join(active_promos_aux,sales, 
#                               by=c("Cliente","Marca","Cupo","llave2")) 

####en reemplazo del paso anterior, calculamos por cliente/marca/cupo:
sales_aux <- sqldf("select llave2, Cliente, Marca, Cupo, sum(Nr) as sumNr,
                    sum(Dcto) as sumDcto, sum(Hl) as sumHl
                    from sales group by Cliente, Marca, Cupo")
write.csv(sales_aux,file="sales_aux.csv")#guarda salex_aux

####ahora juntamos salex_aux con active_promos por cliente/marca/cupo (llave2)
active_promos_aux <- left_join(active_promos_aux,sales_aux, 
                               by=c("llave2","Cliente","Marca","Cupo"))

## para identificar a aquellos clientes que si usaron su cupon de promoci?n: 
active_promos_aux <- left_join(active_promos_aux,executed_promos, 
                               by=c("CodigoDC","Cliente","Marca","Cupo"))  

##cambiamos los valores NA por cero:
active_promos_aux[is.na(active_promos_aux)] <- 0

##Creamos una columna adicional para ingresar 0 si no se encuentra en BD executed_promos
##y 1 si el cliente si ejecut? su promoci?n:
active_promos_aux$Executed <-""
active_promos_aux[active_promos_aux$llave1.y!=0,]$Executed=1
active_promos_aux[active_promos_aux$llave1.y==0,]$Executed=0

table(active_promos_aux$Executed)#comprobamos que toda la columna tenga valor 0 o 1.
table(active_promos_aux$Executed)/length(active_promos_aux$Executed)

##guardamos toda la base de datos en uno sola matriz###############3
write.csv(active_promos_aux,file="mmaestro.csv")

str(active_promos_aux)
active_promos_aux$Executed <- as.numeric(active_promos_aux$Executed)
########################################################################################
###############################Creamos dummy para distinguir marca/cupo#################
##Creamos dummy01 para Marca=29 y Cupo=9
active_promos_aux$MarcaCupo01 <-""
active_promos_aux[active_promos_aux$Marca!=29,]$MarcaCupo01=0
active_promos_aux[active_promos_aux$Marca==29,]$MarcaCupo01=1
##Creamos dummy02 para Marca=40 y Cupo=16
active_promos_aux$MarcaCupo02 <-""
active_promos_aux[active_promos_aux$Marca!=40,]$MarcaCupo02=0
active_promos_aux[active_promos_aux$Marca==40,]$MarcaCupo02=1
##Creamos dummy02 para Marca=39 y Cupo=20
active_promos_aux$MarcaCupo03 <-""
active_promos_aux[active_promos_aux$Marca!=39,]$MarcaCupo03=0
active_promos_aux[active_promos_aux$Marca==39,]$MarcaCupo03=1
table(active_promos_aux$MarcaCupo03)
#######################################################################################
##################### calculando antiguedad del cliente con la empresa#################
today="2020-12-30"
##creamos la columna Antiguedad:
active_promos_aux$AntiguedadM<-difftime(today, active_promos_aux$FechaAltaCliente, units='days')/30
active_promos_aux$AntiguedadM <- as.numeric(active_promos_aux$AntiguedadM)
hist(active_promos_aux$AntiguedadM)
##creamos columna donde la antiguedad se clasifica en 4 niveles:
active_promos_aux$AntiguedadRang<-cut(active_promos_aux$AntiguedadM,breaks=4)
table(active_promos_aux$AntiguedadRang)
##creando variables dummie para AntiguedadRang #######(De acuerdo a lo analizado en los modelos)
##Creamos dummie02 para AntiguedadRang = (52.3,82.9]
active_promos_aux$AntiguedadRangD02 <-""
active_promos_aux[active_promos_aux$AntiguedadRang!='(52.3,82.9]',]$AntiguedadRangD02=0
active_promos_aux[active_promos_aux$AntiguedadRang=='(52.3,82.9]',]$AntiguedadRangD02=1

##Creamos dummie03 para AntiguedadRang = (82.9,113]
active_promos_aux$AntiguedadRangD03 <-""
active_promos_aux[active_promos_aux$AntiguedadRang!='(82.9,113]',]$AntiguedadRangD03=0
active_promos_aux[active_promos_aux$AntiguedadRang=='(82.9,113]',]$AntiguedadRangD03=1

##Creamos dummie04 para AntiguedadRang = (113,144]
active_promos_aux$AntiguedadRangD04 <-""
active_promos_aux[active_promos_aux$AntiguedadRang!='(82.9,113]',]$AntiguedadRangD04=0
active_promos_aux[active_promos_aux$AntiguedadRang=='(82.9,113]',]$AntiguedadRangD04=1

########################################################################################
########################### estratificando el "estrato" del cliente de 6 estratos a 3 estratos##
numEstratos<- sqldf("select distinct Estrato from active_promos_aux")
numEstratos #encontramos 6 estratos
##Nuevo Estratos: Estratos 1 y 2 = newEstrato 1, Estratos 3 y 4 = newEstrato 2, Estratos 5 y 6 = newEstrato 3,
active_promos_aux$newEstrato <-""
active_promos_aux[active_promos_aux$Estrato==1,]$newEstrato=1
active_promos_aux[active_promos_aux$Estrato==2,]$newEstrato=1
active_promos_aux[active_promos_aux$Estrato==3,]$newEstrato=2
active_promos_aux[active_promos_aux$Estrato==4,]$newEstrato=2
active_promos_aux[active_promos_aux$Estrato==5,]$newEstrato=3
active_promos_aux[active_promos_aux$Estrato==6,]$newEstrato=3
table(active_promos_aux$newEstrato)

##creando variables dummie para NewEstrato #######(De acuerdo a lo analizado en los modelos)
##Creamos dummie02 para newEstrato = 2
active_promos_aux$newEstratoD02 <-""
active_promos_aux[active_promos_aux$newEstrato!=2,]$newEstratoD02=0
active_promos_aux[active_promos_aux$newEstrato==2,]$newEstratoD02=1
##Creamos dummie02 para newEstrato = 3
active_promos_aux$newEstratoD03 <-""
active_promos_aux[active_promos_aux$newEstrato!=3,]$newEstratoD03=0
active_promos_aux[active_promos_aux$newEstrato==3,]$newEstratoD03=1
table(active_promos_aux$newEstratoD03)
#########################################################################################

##################calculamos net revenue mensual##############################
active_promos_aux$RevenueMensual <- active_promos_aux$sumNr/as.numeric(active_promos_aux$AntiguedadM)
##############################################################################
##################Calculamos descuento por hectolitro consumido###############
active_promos_aux$Dcto_Hl <- abs(active_promos_aux$sumDcto)/active_promos_aux$sumHl
#uuu <- sqldf("select COUNT(Dcto_Hl) from active_promos_aux where Dcto_Hl >=0")
#uuu
##cambiamos los valores NA por cero:
active_promos_aux[is.na(active_promos_aux)] <- 0
##############################################################################
##############################################################################
##########Creamos la tabla Test para Modelo1##################################
test_aux <- left_join(test,clients, 
                      by=c("Cliente"))
##elimino las variables que no quiero:
test_aux$Region <-NULL
test_aux$Gerencia <-NULL
test_aux$SubCanal <-NULL
head(test_aux)

####Dumie Marca-Cupo
##Creamos dummy02 para Marca=40 y Cupo=16
test_aux$MarcaCupo02 <-""
test_aux[test_aux$Marca!=40,]$MarcaCupo02=0
test_aux[test_aux$Marca==40,]$MarcaCupo02=1
##Creamos dummy02 para Marca=39 y Cupo=20
test_aux$MarcaCupo03 <-""
test_aux[test_aux$Marca!=39,]$MarcaCupo03=0
test_aux[test_aux$Marca==39,]$MarcaCupo03=1
table(test_aux$MarcaCupo03)

####Dumie NewEstrato
##Nuevo Estratos: Estratos 1 y 2 = newEstrato 1, Estratos 3 y 4 = newEstrato 2, Estratos 5 y 6 = newEstrato 3,
test_aux$newEstrato <-""
test_aux[test_aux$Estrato==1,]$newEstrato=1
test_aux[test_aux$Estrato==2,]$newEstrato=1
test_aux[test_aux$Estrato==3,]$newEstrato=2
test_aux[test_aux$Estrato==4,]$newEstrato=2
test_aux[test_aux$Estrato==5,]$newEstrato=3
test_aux[test_aux$Estrato==6,]$newEstrato=3
table(test_aux$newEstrato)

##creando variables dummie para NewEstrato #######(De acuerdo a lo analizado en los modelos)
##Creamos dummie02 para newEstrato = 2
test_aux$newEstratoD02 <-""
test_aux[test_aux$newEstrato!=2,]$newEstratoD02=0
test_aux[test_aux$newEstrato==2,]$newEstratoD02=1
##Creamos dummie02 para newEstrato = 3
test_aux$newEstratoD03 <-""
test_aux[test_aux$newEstrato!=3,]$newEstratoD03=0
test_aux[test_aux$newEstrato==3,]$newEstratoD03=1
table(test_aux$newEstratoD03)
#############################################################################################
####Dumie Marca-Cupo calculando antiguedad del cliente con la empresa#######################
today="2020-12-30"
##creamos la columna Antiguedad:
test_aux$AntiguedadM<-difftime(today, test_aux$FechaAltaCliente, units='days')/30
test_aux$AntiguedadM <- as.numeric(test_aux$AntiguedadM)
hist(test_aux$AntiguedadM)
##creamos columna donde la antiguedad se clasifica en 4 niveles:
test_aux$AntiguedadRang<-cut(test_aux$AntiguedadM,breaks=4)
table(test_aux$AntiguedadRang)
##creando variables dummie para AntiguedadRang #######(De acuerdo a lo analizado en los modelos)
##Creamos dummie02 para AntiguedadRang = (52.3,82.9]
test_aux$AntiguedadRangD02 <-""
test_aux[test_aux$AntiguedadRang!='(52.3,82.9]',]$AntiguedadRangD02=0
test_aux[test_aux$AntiguedadRang=='(52.3,82.9]',]$AntiguedadRangD02=1

##Creamos dummie03 para AntiguedadRang = (82.9,113]
test_aux$AntiguedadRangD03 <-""
test_aux[test_aux$AntiguedadRang!='(82.9,113]',]$AntiguedadRangD03=0
test_aux[test_aux$AntiguedadRang=='(82.9,113]',]$AntiguedadRangD03=1

##Creamos dummie04 para AntiguedadRang = (113,144]
test_aux$AntiguedadRangD04 <-""
test_aux[test_aux$AntiguedadRang!='(82.9,113]',]$AntiguedadRangD04=0
test_aux[test_aux$AntiguedadRang=='(82.9,113]',]$AntiguedadRangD04=1

###Agregamos a tex_aux net Revenue para calcular RevenueMensual
####ahora juntamos salex_aux con test_aux por cliente/marca/cupo
test_aux <- left_join(test_aux,sales_aux, 
                      by=c("Cliente","Marca","Cupo"))
###cambiamos los valores NA por cero:
test_aux[is.na(test_aux)] <- 0
##Calculamos Variable RevenueMensual
test_aux$RevenueMensual <- test_aux$sumNr/test_aux$AntiguedadM

##elimino las variables que no quiero:
test_aux$FechaAltaCliente <-NULL
test_aux$Estrato <-NULL
test_aux$newEstrato <-NULL
test_aux$AntiguedadM <-NULL
test_aux$AntiguedadRang <-NULL
test_aux$llave2 <-NULL
test_aux$sumNr <-NULL
test_aux$sumDcto <-NULL
test_aux$sumHl <-NULL
head(test_aux)

########################################################################################
##guardamos toda la base de datos en uno sola matriz###############3
write.csv(test_aux,file="test_aux.csv")

#############correlaciones###################################################
## Observaremos la correlaci?n existente entre todas las variables cuantitativas
Mcor <- cor(active_promos_aux[, c(16,17,18,24)])
corrplot(Mcor, method = "ellipse")


#############################################################################################
#######Auxiliar de an?lisis##################################################################
vari01 <- sqldf("select Mes,sum(Nr)/1000000 from sales group by Mes order by Mes ")
head(vari01)

plot(active_promos_aux$sumDcto, active_promos_aux$sumNr)
diz <- sqldf("select distinct CodigoDC,Marca,Cupo,Cliente from active_promos_aux")
dim(diz)[1]
dim(active_promos_aux)[1]

exez <- sqldf("select distinct CodigoDC,Marca,Cupo,Cliente from executed_promos")
dim(exez)[1]

##Numero de  clientes a los que les llego la promocion:
clientes_Act <- sqldf("select distinct(Cliente) from active_promos_aux")
dim(clientes_Act)
head(clientes_act)


##Numero de marcas
clientes_Ejec <- sqldf("select distinct(cliente) from executed_promos")
Marca_Act
table(active_promos$Marca)

##Numero de cupon
cupon_Act <- sqldf("select count(distinct(Cupo)) from active_promos")
cupon_Act
table(active_promos$Cupo)

##Numero Marcas por cupon
marca_cupon_Act <- sqldf("select distinct Marca,Cupo from active_promos")
marca_cupon_Act

## 
cc<- sqldf("select Cupo from active_promos where Marca =29 group by Cupo")
cc

##Dado que hemos identificado que se tiene 3 marcas y cada marca tiene un cup?n de promoci?n,
##pasamos a dividir a todos los clientes activados por marca con su propia promoci?n:

modeloM1C1 <- filter(active_promos_aux, Marca.x == 29)
head(modeloM1C1)
dim(modeloM1C1)

modeloM2C2 <- filter(active_promos_aux, Marca.x == 40)
head(modeloM2C2)
dim(modeloM2C2)

modeloM3C3 <- filter(active_promos_aux, Marca.x == 39)
head(modeloM3C3)
dim(modeloM3C3)

##Comprobamos que las nuevas tablas contienen a 
aa <- dim(modeloM1C1)+dim(modeloM2C2)+dim(modeloM3C3)
aa

##########################################################PARTE II
library(readxl)
library(openxlsx)
library(imputeTS)
library(sqldf)
###################################CARGA DE DATOS###########
Test <- read.csv("test.csv") #carga output
Test<-data.frame(Test)

active_promo <- read.csv("active_promos.csv") #carga active
active_promo<-data.frame(active_promo)

sales<- read.csv("sales.csv") #carga sales
sales<-data.frame(sales)

clienAtrib<- read.csv("clients_attributes.csv") #carga atributos cliente
clienAtrib<-data.frame(clienAtrib)

expromo<- read.csv("executed_promos.csv") #carga ejec promo
expromo<-data.frame(expromo)

mmaestro<- read.csv("mmaestro.csv") #carga 
mmaestro<-data.frame(mmaestro)

test_aux<- read.csv("test_aux.csv") #carga 
test_aux<-data.frame(test_aux)

#############################################################################
library(gmodels)

antiguedad=mmaestro$FechaAltaCliente


##############################################################################



#########Active_Promo##########
head(active_promo)
head(expromo)

active_promo$Fecha_Desde=as.Date(active_promo$Fecha_Desde)
active_promo$Fecha_Hasta=as.Date(active_promo$Fecha_Hasta)
str(active_promo)
###############################

head(clienAtrib)
clienAtrib$FechaAltaCliente=as.Date(clienAtrib$FechaAltaCliente)
str(clienAtrib)

min(clienAtrib$FechaAltaCliente)

max(active_promo$Fecha_Hasta)
min(active_promo$Fecha_Hasta)


today="2020-12-30"

table(clienAtrib$Estrato)
table(clienAtrib$TipoPoblacion)

clienAtrib$Antiguedad=###

#Preparación de los datos
mmaestro$FechaAltaCliente=as.Date(mmaestro$FechaAltaCliente)
  
antiguedad<-difftime(today, clienAtrib$FechaAltaCliente, units='days')
hist(as.numeric(antiguedad))

cortes<-cut(c(1:100),breaks=4)

str(cortes)
table(cortes)






###Fusión de tablas##################
promoUnicos<-sqldf("select distinct CodigoDC,Cliente,Marca,Cupo from expromo")
dim(promoUnicos)
dim(expromo)
remove(promoUnicos)
######################################

###########################
library(gmodels)
install.packages("gmodels")
#Marcacupo01
#Marcacupo02
#Marcacupo03
mmaestro$TipoPoblacion=mmaestro$TipoPoblacion-1   #VolviendoDummy
sapply(mmaestro, function(x) sum(is.na(x)))
mmaestro$Dcto_Hl
mmaestro[is.na(mmaestro$Dcto_Hl),]$Dcto_Hl <- 0   #Limpieza NAs

set.seed(1234)
indice<-sample(1:nrow(mmaestro),round(0.7*nrow(mmaestro)))
Train<-mmaestro[indice,]
datatest<-mmaestro[-indice,]

######################################
Modelo1 <- glm(executed ~ factor(EF) +factor(MarcaCupo02)+factor(MarcaCupo03)+factor(TipoPoblacion)+factor(newEstrato)+RevenueMensual, data = Train, family = "binomial")
summary(Modelo1)
#alias(Modelo1) 
##################
Modelo2 <- glm(executed ~ factor(EF) +factor(MarcaCupo02)+factor(MarcaCupo03)+factor(TipoPoblacion)+factor(newEstrato)+RevenueMensual+factor(AntiguedadRang), data = Train, family = "binomial")
summary(Modelo2)
##################
Modelo3 <- glm(executed ~ factor(EF) +factor(MarcaCupo02)+factor(MarcaCupo03)+factor(newEstrato)+RevenueMensual+factor(AntiguedadRang), data = Train, family = "binomial")
summary(Modelo3)

##################
Modelo4 <- glm(executed ~ factor(EF) +factor(MarcaCupo02)+factor(TipoPoblacion)+factor(newEstrato)+RevenueMensual+factor(AntiguedadRang), data = Train, family = "binomial")
summary(Modelo4)

c(AIC(Modelo1),AIC(Modelo2),AIC(Modelo3),AIC(Modelo4))
##########################################
library(MASS)
executed ~ 

ms1 = glm(executed~1,family=binomial(link=logit),data=Train)
ms2 = stepAIC(ms1,scope=list(upper=~factor(EF) +factor(MarcaCupo02)+factor(MarcaCupo03)+factor(TipoPoblacion)+factor(newEstrato)+RevenueMensual+factor(AntiguedadRang),lower=~1))
ms3 = stepAIC(ms1,
              scope=list(upper=~Sexo*Edad*pareja*AbusoAlcohol*ConsCocaina*Padresbipolares*TiempoEnf*NumRec*NumHospit*csui
                         ,lower=~1))






###################curva roc
library(ROCR)
Resultados<-datatest$executed
prediccion<-predict(Modelo1,newdata=datatest,type="response")
f=floor(prediccion+0.5)
table(Resultados,f)

pred1<-prediction(prediccion,datatest$executed)
perf1<-performance(pred1,measure = "tpr",x.measure = "fpr")
plot(perf1,xlab="1-Especificidad",ylab="Sensitividad",cex=0.5,cex.axis=0.3,cex.lab=1)
abline(a=0,b=1,lty=2)

###################curva roc2
library(ROCR)
Resultados<-datatest$executed
prediccion2<-predict(Modelo2,newdata=datatest,type="response")
f=floor(prediccion2+0.5)
table(Resultados,f)

pred2<-prediction(prediccion2,datatest$executed)
perf2<-performance(pred2,measure = "tpr",x.measure = "fpr")
plot(perf2,xlab="1-Especificidad",ylab="Sensitividad",cex=0.5,cex.axis=0.3,cex.lab=1)


plot(perf2,xlab="1-Especificidad",ylab="Sensitividad",cex=0.5,cex.axis=0.3,cex.lab=1,col=1)
plot(perf1,xlab="1-Especificidad",ylab="Sensitividad",cex=0.5,cex.axis=0.3,cex.lab=1,add=T,col=2)
abline(a=0,b=1,lty=2)


#probabilities
prediccionx<-predict(Modelo2,newdata=datatest,type="response")

###################
# sapply(Train, function(x) sum(is.na(x)))
# head(datos)
# table(datos$Marca)
# table(datos$Cupo)
# clientes<-sqldf("select count(distinct Cliente) from datos")
# active_promos.csv
# table(mmaestro$MarcaCupo03)


###############################################################
#SELECCIÓN DE DATOS DE ENTRENAMIENTO Y DATOS APRENDIZAJE
ID<-test_aux[,c(1,2,3)]  #cliente,marca,cupo
#test_aux$TipoPoblacion=test_aux$TipoPoblacion-1   #VolviendoDummy
covars1<-test_aux[,c("EF","MarcaCupo02","MarcaCupo03","TipoPoblacion","newEstratoD02","newEstratoD03","RevenueMensual")] 
covars2<-test_aux[,c("EF","MarcaCupo02","MarcaCupo03","TipoPoblacion","newEstratoD02","newEstratoD03","RevenueMensual","AntiguedadRangD02","AntiguedadRangD03","AntiguedadRangD04")] 
covars3<-test_aux[,c("EF","MarcaCupo02","MarcaCupo03","newEstratoD02","newEstratoD03","RevenueMensual","AntiguedadRangD02","AntiguedadRangD03","AntiguedadRangD04")] 
covars4<-test_aux[,c("EF","MarcaCupo02","TipoPoblacion","newEstratoD02","newEstratoD03","RevenueMensual","AntiguedadRangD02","AntiguedadRangD03","AntiguedadRangD04")] 

#limpiar tipopoblacion
sapply(test_aux, function(x) sum(is.na(x)))

ExePromo<-function(model,ID,covars){
  N = dim(covars)[1]
  X = as.matrix(cbind(rep(1,N),covars))
  Y = X%*%coef(model)
  prob<-exp(Y)/(1+exp(Y))
  ExePromo<-cbind(ID,prob)
  return(ExePromo)
}

Carga1<-ExePromo(Modelo1,ID,covars1)
Carga2<-ExePromo(Modelo2,ID,covars2)
Carga3<-ExePromo(Modelo3,ID,covars3)
Carga4<-ExePromo(Modelo4,ID,covars4)
Cargafinal<-ExePromo(Modelo2,ID,covars2)
#####################################
head(Carga1)
colnames(Carga1)[4]="Ejecuto_Promo"
colnames(Carga2)[4]="Ejecuto_Promo"
colnames(Carga3)[4]="Ejecuto_Promo"
colnames(Carga4)[4]="Ejecuto_Promo"
write.csv(Carga1,file="Carga1.csv",row.names = F)
write.csv(Carga2,file="Carga2.csv",row.names = F)
write.csv(Carga3,file="Carga3.csv",row.names = F)
write.csv(Carga4,file="Carga4.csv",row.names = F)
write.csv(Cargafinal,file="Completo.csv",row.names = F)







