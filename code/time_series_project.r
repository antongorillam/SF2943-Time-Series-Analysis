library(aTSA)
library(plotly)
library(itsmr)

data_train <- read.csv("../data/GlobalTemperatures.csv", header=TRUE)[1201:2401, 1:2]
data_test <- read.csv("../data/GlobalTemperatures.csv", header=TRUE)[2401:3001, 1:2]
plot_ly(
  x = as.Date(data_train$"dt"), 
  y = data_train$"LandAverageTemperature", 
  type = 'scatter', 
  mode = 'lines', 
  name = 'Sample plot')%>%
  layout(
  title = 'Temperature over time',
  plot_bgcolor='#e5ecf6',  
  xaxis = list(  
    title = 'Time',
    zerolinecolor = '#ffff',  
    zerolinewidth = 2,  
    gridcolor = 'ffff'),  
  yaxis = list(   
    title = 'Temperature',
    zerolinecolor = '#ffff',  
    zerolinewidth = 2,  
    gridcolor = 'ffff'),
  showlegend = TRUE, width = 1500)

season_comp <- season(data_train$"LandAverageTemperature", 12)
trend_comp <- trend(data_train$"LandAverageTemperature", 1)
residual_temp <- data_train$"LandAverageTemperature" - season_comp - trend_comp 

plot_ly(x = as.Date(data_train$"dt"), 
        y = season_comp, 
        type = 'scatter', 
        mode = 'lines', 
        name = 'Sample plot')#%>% 
layout(title = 'Temperature over time',
       plot_bgcolor='#e5ecf6',  
       xaxis = list(  
         title = 'Time',
         zerolinecolor = '#ffff',  
         zerolinewidth = 2,  
         gridcolor = 'ffff'),  
       yaxis = list(  
         title = 'Temperature',
         zerolinecolor = '#ffff',  
         zerolinewidth = 2,  
         gridcolor = 'ffff'),
       showlegend = TRUE, width = 1500)

residual_temp <- data_train$"LandAverageTemperature" - season_comp
residual_temp <- diff(residual_temp, differences=1)
adf.test(residual_temp)
acf(residual_temp)
pacf(residual_temp)

ar_model = burg(residual_temp, 11)
ma_model = hannan(residual_temp, p=0, q=2)
arma = autofit(residual_temp, p=0:10, q=0:5)
