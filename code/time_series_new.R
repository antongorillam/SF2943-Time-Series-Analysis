library(aTSA)
library(plotly)
library(itsmr)

data_train <- read.csv("GlobalTemperatures.csv", header=TRUE)[1201:2401, 1:2]
data_test <- read.csv("GlobalTemperatures.csv", header=TRUE)[2401:3001, 1:2]
fig <-plot_ly(x = as.Date(data_train$"dt"), 
              y = data_train$"LandAverageTemperature", 
              type = 'scatter', 
              mode = 'lines', 
              name = 'Sample plot')
fig <- fig %>% layout(title = 'Original time-series',
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
fig

season_comp <- season(data_train$"LandAverageTemperature", 12)

fig <-plot_ly(x = as.Date(data_train$"dt"), 
              y = season_comp, 
              type = 'scatter', 
              mode = 'lines', 
              name = 'Sample plot')
fig <- fig %>% layout(title = 'Seasonal component',
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
fig

trend_comp <- trend(data_train$"LandAverageTemperature", 1)

fig <-plot_ly(x = as.Date(data_train$"dt"), 
              y = trend_comp, 
              type = 'scatter', 
              mode = 'lines', 
              name = 'Sample plot')
fig <- fig %>% layout(title = 'Trend component',
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
fig

residual_temp <- data_train$"LandAverageTemperature" - season_comp
residual_temp <- diff(residual_temp, differences=1)

fig <-plot_ly(x = as.Date(data_train$"dt"[-1]), 
              y = residual_temp, 
              type = 'scatter', 
              mode = 'lines', 
              name = 'Sample plot')
fig <- fig %>% layout(title = 'Residuals',
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
fig

adf.test(residual_temp)
acf(residual_temp)
pacf(residual_temp)

ar_model = burg(residual_temp, 11)
ma_model = arma(residual_temp, p=0, q=2)
arma_model = autofit(residual_temp, p=0:10, q=0:10)
# arma_model = autofit(residual_temp, p=3, q=5)
f = forecast(data_train$"LandAverageTemperature", c("season",12,"diff",1), arma_model, 601)

fig <- plot_ly(
  x = as.Date(data_test$"dt"), 
  y = data_test$"LandAverageTemperature", 
  type = 'scatter', 
  mode = 'lines', 
  name = 'Residuals')%>%
  layout(
    title = 'Residuals',
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
fig
fig %>% add_trace(y = ~f$pred, type = 'scatter', 
                                    mode = 'lines',
                                    name = 'Predictions')


season_comp_test <- season(data_test$"LandAverageTemperature", 12)
residual_temp_test <- data_test$"LandAverageTemperature" - season_comp_test
residual_temp_test <- diff(residual_temp_test, differences=1)
f = forecast(residual_temp, NULL, arma_model, 601)


fig <- plot_ly(
  x = as.Date(data_test$"dt")[-1], 
  y = residual_temp_test, 
  type = 'scatter', 
  mode = 'lines', 
  name = 'Residuals')%>%
  layout(
    title = 'Residuals',
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
fig
fig %>% add_trace(y = ~f$pred[-1], type = 'scatter', 
                  mode = 'lines',
                  name = 'Predictions')
