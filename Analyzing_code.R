## Name: Bruce Zhu
## College: UCLA

data=read.csv('Beijing_ori.csv')

#transform_data

data_trans=function(data){
  years=c(2010:2014)
  months=c(1:12)
  row=0
  df=data.frame(year=integer(),month=integer(),indicator=integer(),pm=double())
  for (i in years){
    data_temp=data[which(data$year==i),]
    for(j in months){
      row=row+1
      data_temp2=data_temp[which(data_temp$month==j),]
      first_q=data_temp2[which(data_temp$day<=8),]
      second_q=data_temp2[which(data_temp$day<=15&data_temp$day>8),]
      third_q=data_temp2[which(data_temp$day>15&data_temp$day<=23),]
      fourth_q=data_temp2[which(data_temp$day>23),]
      
      first_q_mean=mean(first_q$pm2.5,na.rm=TRUE)
      second_q_mean=mean(second_q$pm2.5,na.rm=TRUE)
      third_q_mean=mean(third_q$pm2.5,na.rm=TRUE)
      fourth_q_mean=mean(fourth_q$pm2.5,na.rm=TRUE)
      
      first_vec=c(i,j,4,first_q_mean)
      second_vec=c(i,j,12,second_q_mean)
      third_vec=c(i,j,19,third_q_mean)
      fourth_vec=c(i,j,26,fourth_q_mean)
      
      df[row,]=first_vec
      row=row+1
      df[row,]=second_vec
      row=row+1
      df[row,]=third_vec
      row=row+1
      df[row,]=fourth_vec
      }
  }
  return (df)
}
transform_data=data_trans(data)
plot(transform_data$pm,type='l',main='Beijing PM2.5 (2010-2014)')

write.csv(transform_data,file='Beijing_pm.csv')


#########Transform data to time series##########
library(zoo)
library(xts)
library(lubridate)

data_orig=read.csv('Beijing_ori.csv')
data_proc=read.csv('Beijing_pm.csv')

data_proc$date<-as.Date(with(data_proc,paste(year,month,indicator,sep='-')),"%Y-%m-%d")
data_ts=subset(data_proc, select=c('date','pm'))

ts=xts(x=data_ts[,-1],order.by = data_ts$date)

plot(data_orig$pm2.5,type='l',main='PM2.5 Concentration of Original Data', ylab='PM2.5 Concentration',xlab='time index (every hour)',col='blue')
plot(data_proc$pm,type='l',main='PM2.5 Concentration of Processed Data', ylab='PM2.5 Concentration',xlab='time index (every quarter month)',lwd=2,col='red')

plot(ts,type='l',main='PM2.5 Concentration of Processed Data', ylab='PM2.5 Concentration',xlab='time index (every half month)',lwd=2,col='red')
######Regression#########
#data ts is time series data
library(astsa)
fit=lm(ts~time(ts),na.action = NULL)
plot(resid(fit),main='Detrended',type='l')
abline(fit,col='red',lwd=3)

#################try two years###################
setwd('/Users/brucezhu')
library(zoo)
library(xts)
library(lubridate)
data=read.csv('Beijing_ori.csv')
data1314=data[which(data$year==2014|data$year==2013),]
data1314$date=as.Date(with(data1314,paste(year,month,day,sep='-')),"%Y-%m-%d")
result=aggregate(data1314$pm2.5,list(data1314$date),mean,na.rm=TRUE)

ts=xts(x=result[,-1], order.by =result$Group.1)
ts=diff(ts,1)[-1]

fit=lm(ts~time(ts),na.action=NULL) #regression soi on time 
plot(time(ts),resid(fit),main='Detrended',xlab='Year',ylab='Detrend Analyzing data',type='l') 
abline(fit,col='red',lwd=3)
summary(fit)

plot(ts,type='l',main='PM2.5 Concentration of Processed Data', ylab='PM2.5',xlab='time index (every half month)',lwd=2,col='red')

############see average the pm concentration each month in 2013 and 2014#########
data_month=aggregate(data1314$pm2.5,list(data1314$day,data1314$month),mean,na.rm=TRUE)
library(ggplot2)
data_month$Group.2<-factor(data_month$Group.2,labels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))
p10=ggplot(data_month,aes(x=data_month$Group.2,y=data_month$x))+geom_boxplot(colour ="#1F3552", fill = "#4271AE",size = 1)+scale_x_discrete(name = "Month")+scale_y_continuous(name = "PM 2.5 Concentration")+ggtitle("Boxplot of PM2.5 concentration by month (average values in 2013 and 2014)")+theme_bw()+theme(panel.grid.major = element_line(colour = "#d3d3d3"),
                                                                                                                                                                                                                                                                                          panel.grid.minor = element_blank(),
                                                                                                                                                                                                                                                                                          panel.border = element_blank(),
                                                                                                                                                                                                                                                                                          panel.background = element_blank(),                                                                                                                                                                                                                                                                                       plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
                                                                                                                                                                                                                                                                                          text=element_text(family = "Tahoma"),
                                                                                                                                                                                                                                                                                          axis.title = element_text(face="bold"),
                                                                                                                                                                                                                                                                                          axis.text.x = element_text(colour="black", size = 11),
                                                                                                                                                                                                                                                                                          axis.text.y = element_text(colour="black", size = 9),
                                                                                                                                                                                                                                                                                          axis.line = element_line(size=0.5, colour = "black"))
#######TS Analysis###############
library(astsa)

acf(ts,main='ACF of the analyzing data',lag.max = 20)
pacf(ts,main='PACF of the analyzing data',lag.max=20) #prefer AR(2)

ts_raw=mvspec(ts,log='no',main='Raw Periodogram of the Analyzing Data')
par(mfrow=c(2,1))
ts_ave = mvspec(ts, kernel('daniell',4), log='no',main='Non-parametric Estimation of Periodogram')
ts_para=spec.ar(ts,log='no', main='Parametric Estimation of Periodogram AR(15)')
par(mfrow=c(1,1))
#####detrend#######
#fit=lm(ts~time(ts),na.action=NULL) #regression soi on time 
#plot(time(ts),resid(fit),main='Detrended',xlab='Year',ylab='Detrend Analyzing data',type='l') 
#abline(fit,col='red',lwd=3)
#summary(fit)


gridtest=function(ts){
  df=data.frame(AIC=double(),BIC=double(),stringsAsFactors=FALSE)
  dfname=data.frame(ARIMA=character(),stringsAsFactors=FALSE)
  row=1
  for (a in c(0:2)){
    for(b in c(0:1)){
      for(c in c(0:2)){
        if (a==0 & c==0){
        }
        else{
        fit=sarima(ts,a,b,c)
        name=as.character(paste0('(',a,',',b,',',c,')'))
        vec=c(fit$AIC,fit$BIC)
        vec2=c(name)
        df[row,]=vec
        dfname[row,]=vec2
        row=row+1
        }
      }
    }
  }
  output=cbind(dfname,df)
  return(output)
}

result=gridtest(ts)

modelAIC=result[which.min(result$AIC),]
modelBIC=result[which.min(result$BIC),]
modelAIC
modelBIC
sarima(ts,1,0,1)

pred_data=predict(Arima(ts,order=c(2,0,1)),12)

library(forecast)
fit=Arima(ts,order=c(1,0,1))
forecast(fit,5)
plot(forecast(fit,12),xlim=c(550,740),ylim=c(-50,400),main='Forecast PM2.5 in Beijing based on ARIMA(1,0,1) model',xlab='Time Period',ylab='PM 2.5.Concentration')

###for diff data###
plot(forecast(fit,12),xlim=c(550,740),ylim=c(-300,300),main='Forecast PM2.5 in Beijing based on ARIMA(2,0,1) model',xlab='Time Period',ylab='PM 2.5.Concentration')

