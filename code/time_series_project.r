library(aTSA)
library(plotly)
data <- read.csv("~/KTH/year4/SF2943-Time-Series-Analysis/data/GlobalTemperatures.csv", header=TRUE)[1201:3192, 1:3]
# library(itsmr)
print(data)
plot_ly(
  x = as.Date(data$"dt"), 
  y = data$"LandAverageTemperature", 
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

season_comp <- season(data$"LandAverageTemperature", 12)
trend_comp <- trend(data$"LandAverageTemperature", 1)
residual_temp <- data$"LandAverageTemperature" - season_comp - trend_comp 

plot_ly(x = as.Date(data$"dt"), 
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

residual_temp_test <- data$"LandAverageTemperature" - season_comp
residual_temp <- diff(residual_temp, differences=1)
residual_temp_test <- diff(residual_temp_test, differences=1)
adf.test(residual_temp)
acf(residual_temp)
acf(residual_temp_test)
pacf(residual_temp)
pacf(residual_temp_test)