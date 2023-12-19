rm(list=ls())





MT_D <- read.csv('C:/Users/laksh/Downloads/metadata.csv')
TP_D<-read.csv("C:/Users/laksh/Downloads/MaxTemp.csv")




#columns of the variablles having missing data with the number of missing data
colSums(is.na(MT_D))

colSums(is.na(TP_D))





#induvidually displaying only the clumns that have missing data with the no.of missing data in each columns

which(colSums(is.na(MT_D))>0)
which(colSums(is.na(TP_D))>0)




library(VIM)


MISS_MT_D <- aggr(MT_D, numbers=TRUE,col=c('navyblue','red'), sortVars=TRUE, cex.axis=.7, labels=names(df),gap=3, ylab=c("Histogram of missing data","Pattern"))





MISS_TP_D <- aggr(TP_D, numbers=TRUE,col=c('navyblue','red'), sortVars=TRUE,  cex.axis=.7, labels=names(df),gap=3, ylab=c("Histogram of missing data","Pattern"))







library(mice)
imp_MT_D <-  mice(MT_D, method="cart")
MT_D<- complete(imp_MT_D)



imp_TP_D <-  mice(TP_D, method="cart")
TP_D<- complete(imp_TP_D)




Na_Data<-merge(MT_D,TP_D)
aggr_plot <- aggr(Na_Data, col=c('navyblue','red'), sortVars=TRUE,numbers=TRUE,  labels=names(data), gap=3,cex.axis=.7,  ylab=c("Histogram of missing data","Pattern"))





#install packages
library(geoR)
library(tidyverse)
library(dplyr)
library(tidyr)
library(magrittr)
library(dlm)
library(naniar)
library(forecast)



TP_D<- TP_D %>% gather(value = Temperature,key = Location,  -Date)





MTP_mrg<- merge(by='Location',TP_D,MT_D)
summary(MTP_mrg)
head(MTP_mrg)


boxplot(MTP_mrg$Temperature)




summary(MTP_mrg$Temperature)
max(MTP_mrg$Temperature)




L_mx <- MTP_mrg  %>%  filter(Location == "London")
boxplot(main="LONDON",L_mx$Temperature)




M_mx <- MTP_mrg  %>%  filter(Location == "Marham")
boxplot(main="Marham",M_mx$Temperature)



L_mx <- MTP_mrg  %>%  filter(Location == "Lyneham")
boxplot(main="Lyneham",L_mx$Temperature)




MTP_DT <- MTP_mrg[which(MTP_mrg$Date %in% c("20200912")),]

head(n=20,MTP_DT)
summary(MTP_DT)





GD<- as.geodata(MTP_mrg,data.col = 6,coords.col = 2:3)
plot(GD)
summary(GD)

CD <- dup.coords(GD)






set.seed(123)
GD2 <- jitterDupCoords(GD,min=0.1,max=0.5)
plot(GD2)
summary(GD2)







GD3<- as.geodata(data.col = 6,coords.col = 2:3,MTP_DT)
plot(GD3)
summary(GD3)

cord <- dup.coords(GD3)



GD4 <- jitterDupCoords(GD3,min=0.01,max=0.5)
summary(GD4)
plot(GD4)




is_GD<-variog4(GD2,option="bin")
plot(is_GD)



plot(omnidirectionl=TRUE,is_GD)



is_GD2<-variog4(option = "bin",trend = '1st',GD2)
plot(is_GD2)



plot(omnidirectional=TRUE,is_GD2)



is_GD3<-variog4(option="bin",trend="2nd",GD2)
plot(is_GD3)



plot(omnidirectional=TRUE,is_GD3)









vro_GD<-variog(option='bin',GD2)
plot(main="vro_GD",vro_GD)



vro_GD2<-variog( trend = '2nd',option='bin',GD2)
plot(main="vro_GD2",vro_GD2)




is_DFT<- variofit(vro_GD,fix.nugget = FALSE,nugget = 0,simul.number = NULL)
summary(is_DFT)




is_DFT2<-variofit(vro_GD2,fix.nugget = FALSE,nugget = 0,simul.number = NULL)
summary(is_DFT2)




is_vro<- variofit(fix.kappa=TRUE , kappa = 1.5,vro_GD )
summary(is_vro)





par(mar=c(6,6,4,4))
plot( pch = 15,vro_GD)
lines(col="blue", lty = 4,is_vro)
lines(col="red",lty=3,is_DFT2)




is_gd<- expand.grid(seq(-12,8,by=.1), seq(40, 60,by=.1))
pd_is <- krige.conv(loc=is_gd,GD4,  krige=krige.control(obj.model=is_vro))
image (col = viridis::viridis(100),pd_is,  zlim = c(0,max(c(pd_is$predict))),
       coords.data = GD4[1]$coords,ylab = 'y', xlab = 'x', main = 'THE Mean of prediction ' )
image ( values = pd_is$krige.var, col = heat.colors(100)[100:1],pd_is,
        zlim = c(0,max(c(pd_is$krige.var))), coords.data = GD4[1]$coords,
        ylab = 'y', xlab = 'x',  main = 'Variance of prediction',)




#X-validation
x_gd <- xvalid(model = is_vro,GD4)
par(  mar = c(3,3,2,2),
      mfcol = c(5,3))
plot(x_gd)



#Bayesian Estimation
gd_xMT<- as.matrix(expand.grid(seq(-11,5,l=50),seq(50,70,l=50) ))
kg_B <- krige.bayes( loc=gd_xMT,geodata = GD4,
                     model= model.control(kappa=0.5,cov.m="matern" ),
                     prior = prior.control(phi.prior="reciprocal",phi.discrete=seq(0, 1, l=51)))



par(mar=c(4,4,2,2))
plot( type="h", col=c("black", "green"), tausq.rel = FALSE,kg_B)




par( mar=c(4,4,2,2),mfrow=c(1,3))
hist(kg_B)




image( col = viridis::viridis(100), coords.data=kg_B[1]$coords, ylab = 'y',xlab = 'x',main=" MEaN",kg_B)





image( val="variance",coords.data=kg_B[1]$coords, ylab = 'y', xlab = 'x',main="BaYES",kg_B)




image(kg_B, number.col=1, val= "simulation", col = viridis::viridis(100),
      coords.data=kg_B[1]$coords,
      ylab = 'y',xlab = 'x',main=" SIMULaTION 1")




image(kg_B, number.col=1, val= "simulation", coords.data=kg_B[1]$coords,col = viridis::viridis(100),ylab = 'y', xlab = 'x',
      main="Simulation 1" )




image(kg_B, number.col=2, val= "simulation",coords.data=kg_B[1]$coords,col = viridis::viridis(100),
      
      main="BaYES 2", xlab = 'x', ylab = 'y')







par(pch = 15,mar=c(4,4,2,2)); plot(variog(GD4))
lines( lty = 2,is_vro)
lines(lty=1,is_DFT2)
lines(kg_B,col = 'blue', lwd=2,  lty=1,summ = mean)
lines(kg_B, col = 'red',lwd=2 , lty=2,summ = median)
lines(kg_B,lwd=2 ,lty=3, post = "parameters",  col ='black',summary = "mode")






tm_st<- MTP_mrg %>% filter(Location == "Dun_Fell") %>% select(c(Date,Temperature))



ts_st<-as.ts(tm_st$Temperature)




plot(ts_st)



snl_1<- ts(diff( ts_st,1))
par(mar=c(5,3,3,3)); plot(snl_1)



snl_tm<- ts(diff(snl_1,1))
par(mar=c(5,3,3,3)); plot(snl_tm,main="SNL_TM")



#fitting auto.arma to find the model
atar<- auto.arima(snl_tm, max.d = 0, max.q = 4,max.p = 4
                  , seasonal = FALSE)
summary(atar)



atar_pd<- predict( n.ahead = 6,atar)
atar_pd$se
atar_pd$pred
atar_pp<-atar_pd$pred












summary(atar)

tsdiag(atar)
AIC(atar)
print(atar)




atar_ar_1<-arima(snl_tm,order = c(1,1,2))

tsdiag(atar_ar_1)

print(atar_ar_1)
summary(atar_ar_1)
AIC(atar_ar_1)




atar_ar_2<-arima(snl_tm,order = c(2,1,1))

tsdiag(atar_ar_2)

print(atar_ar_2)
summary(atar_ar_2)
AIC(atar_ar_2)







atar_pd_1<- predict( n.ahead = 6,atar_ar_1)
atar_pd_1$se
atar_pd_1$pred
atar_pp_1<-atar_pd_1$pred



atar_pd_2<- predict( n.ahead = 6,atar_ar_2)
atar_pd_2$se
atar_pd_2$pred
atar_pp_2<-atar_pd_1$pred


#forecasting analysis plots using arimam model on each data and predicts



ts.plot(snl_tm, pred$pred, lty = c(1,3), col=c(5,2))




library(forecast)
pd_plot <- forecast (atar, h = 7)
plot(pd_plot)






par(mfrow = c(1,2))
fit_atar1 = Arima(snl_tm,order = c(4,1,1),include.drift = T)
fut_atar1 = forecast(snl_tm, h = 50)

fit_atar2 = Arima(snl_tm,order = c(4,1,1), include.drift = F)
fut_atar2 = forecast(fit2, h = 50)
plot(fut_atar1)
plot(fut_atar2)


summary(fit_atar1)
AIC(fit_atar1)


summary(fit_atar2)
AIC(fit_atar2)




checkresiduals(fit_atar1)


checkresiduals(fit_atar2)




autoplot(forecast(fit_atar1))
autoplot(forecast(fit_atar2))






require(tseries)
require(forecast)
require(astsa)

ts.plot(snl_1, atar_pd_1$pred, lty = c(1,3), col=c(5,2))
pred_temp<-snl_tm[length(snl_tm)]
for(i in 1:length(pred$pred)){
  gtemp_pred[i+1]<-pred_temp[i]+pred$pred[i]
}
plot(c(snl_tm,gtemp_pred),type="l")










tm_pmt<- MTP_mrg %>% filter(Location == "Lyneham") %>% select(c(Date,Temperature))
ts_pmt<- as.ts(tm_pmt$Temperature)                  
atar_ar_pmt<-arima(order = c(1,1,2),ts_pmt)
tsdiag(atar_ar_pmt)



tm_mcb<- MTP_mrg %>% filter(Location == "London") %>% select(c(Date,Temperature))
ts_mcb<- as.ts(tm_mcb$Temperature)
atar_ar_mcb<-arima(order = c(1,1,2),ts_mcb)
tsdiag(atar_ar_mcb)





tm_mrm<- MTP_mrg %>% filter(Location == "Marham") 
ts_mrm<- as.ts(tm_mrm$Temperature)
atar_ar_sfd<-arima(order = c(1,1,2),ts_mrm)
tsdiag(atar_ar_sfd)


`

library(dlm)



atar_dms<- dlmModSeas(10)
summary(atar_dms)




Atar_DLM<- dlmModSeas(10)
summary(Atar_DLM)



plot(ts_st)




tune_DLM<- log(ts_st) # log model
plot(tune_DLM)




pol_DLM<- dlmModPoly(order = 1) + dlmModSeas(frequency = 10)
pol_DLM$GG; DLM_mod_pol$W; DLM_mod_pol$FF



BF_DLM<- function(x) {
  diag(W(pol_DLM))[1:2] <- exp(x[1:2])
  V(pol_DLM) <- exp(x[3])
  return(pol_DLM)
}



fit_DLM<- dlmMLE(ts_st, parm = rep(0, 3), build =BF_DLM)
fit_DLM$par



fit_2_DLM<- BF_DLM(fit_DLM$par)
V(fit_2_DLM)
W(fit_2_DLM)





smo_DLM <- dlmSmooth(ts_st, mod = fit_2_DLM)
summary(smo_DLM)





dim(smo_DLM$s)



bind_DLM<- cbind(ts_st
                 , dropFirst(smo_DLM$s[,c(1,2)]))
colnames(bind_DLM) <- c("Date", "B", "C")
plot(bind_DLM, type = 'o', main = "OVERTUNNING")








Filt_DLM<- dlmFilter(ts_st$Temperature, mod = fit_2_DLM)
summary(Filt_DLM)



dim(DLM_Filt$m)




DLM_for<- DLMForecast(DLM_Filt, nAhead = 6)
summary(DLM_for)




dim(DLM_for$a)






dim(DLM_for$f)
par(mar=c(4,2,2,2));tsdiag(DLM_Filt
                           