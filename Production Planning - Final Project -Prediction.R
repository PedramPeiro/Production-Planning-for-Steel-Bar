defaultW <- getOption("warn")
options(warn = -1)

raw_data = 	data.frame(
  month = c(1:6), 
  cumulative_sales = c( 1544160,3769895,2127270,2739475,2591490,5585350)
  )

# multiplying the sales with 1.05 to denote the demand
raw_data['demand'] = raw_data['cumulative_sales']*1.05

df = raw_data[c('month','demand')]
attach(df)
# visualizing total demand for size 18 and 20
library(ggplot2)
ggplot(data=raw_data, aes(x=month, y=demand, group=1))+ 
  geom_line() +
  geom_point()+
  labs(x="month", y = "Demand")

df_pred = data.frame(month=7:12,SMA=c(0)*6 , WMA=c(0)*6 , SES=c(0)*6 ,SLR=c(0)*6)
#-------------------------------------------------------------------
# Simple Moving Average 
library(forecast)
library(TTR)

print('RMSE for SMA Prediction:')
for (i in 1:(nrow(df)-1)){
  prediction = SMA(demand , n=i)
  prediction = forecast( prediction, h=6)
  print(paste('k = ',i , 'neighbours is: ',(accuracy(prediction)[2]),collapse=''))
}
prediction = SMA(demand , n=4)
df_pred[1:6,"SMA"] = forecast(prediction,h=6)[2]
prediction = data.frame(prediction)
plot(month, demand, type="o", col="blue", pch="o", lty=1 )
points(5:6, y=prediction[4:5,'prediction'], col="red", pch="*")
lines(5:6, y=prediction[4:5,'prediction'], col="red",lty=2)


autoplot( forecast( SMA(demand , n=4),h=6 ))
#-----------------------------------------------------------------------
# Weighted Moving Average
prediction=WMA(demand, n = 4 , c(0.1,0.1,0.3,0.5))
df_pred[1:6,"WMA"] = forecast(prediction,h=6)[2]
prediction = data.frame(prediction)
plot(month, demand, type="o", col="blue", pch="o", lty=1 )
points(5:6, y=prediction[4:5,1], col="red", pch="*")
lines(5:6, y=prediction[4:5,1], col="red",lty=2)

prediction=forecast(prediction[,1],h=6)
autoplot(prediction)

paste0('RMSE Error for WMA with weights=(0.5,0.3,0.1,0.1): ', accuracy(prediction)[2])


#-----------------------------------------------------------------------
# Simple Exponential Smoothing

print('RMSE for SES Prediction:')
for (alpha in seq(0.1,0.9,0.2)){
  prediction = ses(demand, h=6,alpha = alpha,lambda = NULL)
  prediction = forecast(prediction, h=6)
  print(paste('for alpha = ',alpha , 'is: ',(accuracy(prediction)[2]),collapse=''))
}
prediction = ses(demand, h=6,alpha = alpha,lambda = NULL)
df_pred[1:6,"SES"] = forecast(prediction,h=6)[2]


autoplot( forecast( ses(demand, h=6,alpha = 0.5,lambda = NULL),h=6 ))
#------------------------------------------------------------------------
# Simple Linear Regression

prediction=lm(demand~month,data=df) # Demand=a*Period+b
prediction
summary(prediction)
# plot(prediction)


b= unname(prediction$coefficients[1]) # Intercept
a= unname(prediction$coefficients[2]) # Period 

plot(y=demand,x=month,type="o",col="red",xlim=c(1,6),pch='o')
lines(y=a*month+b,x=month,col ="blue" , lty=3,pch='+')


paste0('RMSE for Linear Regression Prediction: ', sqrt(mean(((a*month+b)-demand)^2)))

print('Demand prediction for periods 7 till 12 are:')
for (i in 7:12){
  df_pred[i-6,'SLR']=a*i+b
}
print(df_pred['SLR'])


#---------------------------------------------------------------------
# ٍError Analysis of different forecase methods

pred1 = forecast(SMA(demand , n=4), h=6)
error_pred1 = accuracy(pred1)[c(2,3,5)]

pred2 = forecast(WMA(demand, n = 4 , c(0.1,0.1,0.3,0.5)) , h=6)
error_pred2 = accuracy(pred2)[c(2,3,5)]

prediction = ses(demand, h=6,alpha = alpha,lambda = NULL)
pred3 = forecast(prediction , h=6)
error_pred3 = accuracy(pred3)[c(2,3,5)]

model_lr = lm(demand~month,data=df)
library(modelr)
df_errors = data.frame(
  RMSE = rmse(model_lr, data = list(demand)),
  MAE = mae(model_lr, data = list(demand)),
  MAPE = mape(model_lr ,data=list(demand)),
  row.names=c('LR')
)
df_errors[c('SMA','WMA','SES'),] = c(error_pred1,error_pred2,error_pred3 ) 

library(creditmodel)
weights = entropy_weight(df_errors,pos_vars=c(),neg_vars=c('RMSE','MAE','MAPE'))
print("weights of each criterion calculated with Entropy Method:")
print(weights)

library(topsis)
print("Topsis results")
print(topsis(decision=data.matrix(df_errors) , weights=weights$Weight , impacts=c("-" , "-" , "-")))
