# Load and check the wanted dataset
data("AirPassengers")

#print the head of the data
print(head(AirPassengers,6))

#printing the length of the time series
is.ts(AirPassengers)
print(length(AirPassengers))

#Checking the frequency then plotting the data
frequency(AirPassengers)
plot.ts(AirPassengers)

#Next we do a basic decomposition
decomposition <- decompose(AirPassengers, type = "multiplicative")
plot(decomposition)

#We test with some basic transformations can we obtain more stationary data?
dif_log_daatta_s <- ts(diff(log(AirPassengers)),12)
plot.ts(dif_log_daatta_s)

#Autocorrelation plot
ac_graph <- acf(dif_log_daatta_s)
ac_graph

#Next we install the forecast library if needed (this may take a while)
if (!('forecast' %in% installed.packages())){
  install.packages('forecast', dependencies =TRUE)
} else {
  print("Package forecast already installed")
}
library(forecast)

# Trying automatically determining the optimal lambda for BoxCox
lamb <- BoxCox.lambda(AirPassengers)
plot(diff(BoxCox(AirPassengers, lambda = lamb)))

#Split the data
cut_off <- round(length(AirPassengers)*0.9)
train_data <- head(AirPassengers, cut_off)
test_data <- tail(AirPassengers, length(AirPassengers)- cut_off)
print(length(train_data))
print(length(test_data))

#Make an automatic forecast for the test set
frcst_model_a <- auto.arima(train_data, D=1, lambda = lamb)
frcst_a <- forecast(frcst_model_a,h=length(test_data))
frcst_model_a

#get accuracy and plot the forecast and also the true values
library(ggplot2)
accuracy(frcst_a, x = test_data)
p <- autoplot(frcst_a, include = 20, ylab="passangers", xlab = "year")  
p
p + geom_line(aes(x=as.numeric(time(test_data)), y = as.numeric(test_data)), col ="red") 
p 
