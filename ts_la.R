library(ggplot2)
library(tseries)
library(forecast)

setwd('L:/Datathon')

raw_inv = read.csv('InventoryMeasure_County_Public.csv')
raw_pri = read.csv('County_MedianRentalPrice_AllHomes.csv')
la_inv = raw_inv[raw_inv$RegionName=='Los Angeles', 7:75]
la_pri = raw_pri[raw_pri$RegionName=='Los Angeles',43:111]

la_df = data.frame(t(rbind(la_inv, la_pri)))
time = as.POSIXct(paste(substring(rownames(la_df),2), '.01', sep=''), format='%Y.%m.%d')
la_df = cbind(la_df, time)
colnames(la_df) = c('Inventory', 'MedianPrice', 'time')

ggplot(data = la_df, aes(x=time) ) + geom_line(aes(y=Inventory, color = 'Inventory'),group = 1) +
  geom_line(aes(y=MedianPrice*2,  color = 'MedianPrice'),group = 1)+
  labs(x='Time') + 
  scale_y_continuous(sec.axis = sec_axis(~./2, name = "Median Price"))+
  labs(title='Rental inventory and median price in Los Angeles')+
  labs(colour="Lines")+
  scale_x_datetime(date_breaks = "year")+
  theme(legend.position = c(0.8, 0.9))







filter_with_padding = function(x,the.filter,iter=1)
{
  q = (length(the.filter)-1)/2
  n = length(x)
  w = stats::filter(c(rep(x[1],q),x,rep(x[n],q)),the.filter)[(q+1):(q+n)]
  if(iter > 1) for(i in 2:iter) w = filter(c(rep(w[1],q),w,rep(w[n],q)),the.filter)[(q+1):(q+n)]
  return(w)
}

m_hat = filter_with_padding(la_inv,c(1/24,rep(1/12,11),1/24))
pre_seasonal = unlist(la_inv)-m_hat
qplot(time,pre_seasonal, main = 'Seasonal component of Los Angeles rental inventory')

seasonal = rowMeans(matrix(pre_seasonal[1:60],nrow=12)) - mean( rowMeans(matrix(pre_seasonal[1:60],nrow=12)))

qplot(1:12, seasonal, main='Estimated Seasonal component within one year', xlab = 'Month', ylab='Relative change') + scale_x_continuous(breaks=1:12)


deseasoned = unlist(la_inv) - c(rep(seasonal,5), seasonal[1:9])


qplot(time,deseasoned, main = 'De-seasoned Los Angeles rental inventory on Zillow') + scale_x_datetime(date_breaks = "year") + stat_smooth(method='lm')

reg_model = lm(deseasoned~time)
summary(reg_model)

plot.ACFest <- function(ts, main=NULL, n.lags=40)
{
  ts.acf <- acf(ts, lag.max=n.lags, plot=FALSE)
  n.ts <- length(ts)
  xs <- 1:n.lags
  ys <- ts.acf$acf[2:(n.lags+1)]
  plot(xs,ys,typ="h",xlab="h  (lag)",ylab="ACF",ylim=c(-1,1),col="blue",main=main)
  points(xs,ys,col="red",cex=0.5)
  xs <- 1:n.lags
  xs[1] <- xs[1] - 0.25
  xs[n.lags] <- xs[n.lags] + 0.25
  lines(xs,1.96*sqrt(n.ts-xs)/n.ts,col="magenta",lty="dashed")
  lines(xs,-1.96*sqrt(n.ts-xs)/n.ts,col="magenta",lty="dashed")
  abline(h=0,lty="dashed")
  CI.hw <- 1.96/sqrt(n.ts)
  lines(c(0.75,n.lags+0.25),rep(CI.hw,2),col="blue",lty="dashed")
  lines(c(0.75,n.lags+0.25),rep(-CI.hw,2),col="blue",lty="dashed")
  return(ts.acf$acf)
}

phi <- plot.ACFest(resid(reg_model), expression(paste("Sample ACF for {", 'Los Angeles rental inventory time series',"}")))[2]

pacf(resid(reg_model), main=paste("Sample PACF for {", 'Los Angeles rental inventory time series',"}"))

arma_model = arma(resid(reg_model), c(4,1))

qplot(time, resid(arma_model))

plot.ACFest(resid(arma_model)[5:69])
pacf(resid(arma_model)[5:69], main=paste("Sample ACF for {", 'Los Angeles rental inventory time series',"}"))


raw_data = read.csv('L:/Datathon/Datathon Materials/calendar.csv')
raw_data$date = as.POSIXct(raw_data$date, format = '%Y-%m-%d')
listings = read.csv('L:/Datathon/Datathon Materials/listings.csv')

head(raw_data)
str(raw_data)

summary(listings$city)

la_listings = listings$id[listings$city=='los angeles']
la_data = raw_data[raw_data$listing_id %in% la_listings,]
summary(la_data$date)
la_data$date = format(as.Date(la_data$date), "%Y-%m")
la_data = la_data[la_data$available=='t',]
la_data$price = as.double(gsub(substring(la_data$price,2), pattern = ',', replacement = ''))
la_price = la_data %>% group_by(date) %>% summarise(Meanprice = mean(price, na.rm=TRUE))
la_price = data.frame(la_price)
la_price$Meanprice = la_price$Meanprice 
la_price$date = as.POSIXct(paste(la_price$date, '-01', sep=''), format='%Y-%m-%d')

ggplot(data = la_price, aes(x=date) ) + geom_line(aes(y=Meanprice, color = 'Mean daily price'),group = 1) +
  geom_line(aes(y=deseasoned[37:49]/70,  color = 'Deseansoned rental inventory'),group = 1)+
  scale_y_continuous(sec.axis = sec_axis(~.*70, name = "Rental inventory"))+
  labs(title='Rental inventory and mean Airbnb price in Los Angeles')+
  labs(colour="Lines")+
  scale_x_datetime(date_breaks = "3 month")+
  theme(legend.position = c(0.15, 0.85))



