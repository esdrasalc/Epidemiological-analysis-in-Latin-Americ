setwd("~/Tesis/Databases")

#Datos
datos=openxlsx::read.xlsx("Series de tiempo nuevos.xlsx",detectDates=TRUE) %>%
  filter(Fecha<="2021-12-31")

datos$Pais

#Data wrangling 
glimpse(datos)

#subset the location only Covid and column i..Date, and New.Cases and change data type

covid1 <- datos %>% 
  subset(Pais == "Uruguay") %>% 
  select(Fecha, Casos)
 
head(covid1)

#Missing values
colSums(is.na(covid1))

# Check range `Date` the data
range(covid1$Fecha)

#create object ts 
startW <- as.numeric(strftime(head(covid1$Fecha, 1), format = "%W"))
startD <- as.numeric(strftime(head(covid1$Fecha, 1) + 1, format =" %w")) 
covid_ts<-ts(covid1$Casos, frequency = 7, start = c(startW, startD))

covid_dc <- decompose(covid_ts, type = "multiplicative")

covid_dc %>% autoplot() +
labs(title="Time series descomposition",
	x="Weeks") +
scale_y_continuous(labels = scales::comma) +
theme(plot.title=element_text(hjust=0.5, size=14, face="bold"),
	plot.caption=element_text(size=10, face="italic"))

ggsave("Decomposición Uruguay.png")

#Test
test <- covid_ts %>% tail(7) #get 7 last days 

train <- covid_ts %>% head(-7) #get the rest data

#modeling holt
covid_holt <- HoltWinters(x = train,
                              gamma = F)
covid_holt

#Modeling auto arima
adf.test(train)

diff(train, lag = 1) %>% adf.test() #Stationer data

#ACF
bacf <- acf(diff(train), plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))

p<-ggplot(data=bacfdf, mapping=aes(x=lag, y=acf)) +
	geom_bar(stat = "identity", position = "identity") +
		labs(title="Uruguay",
		     x="Lags",
		     y="ACF",
caption="Fuente: Autor") +
theme(plot.title = element_text(hjust=0.5, size=14),
	plot.caption=element_text(size=10, face="italic")) 


covid_arima1 <- Arima(y = train, order = c(1,1,1))
covid_arima2 <- Arima(y = train, order = c(2,1,1))
covid_arima3 <- Arima(y = train, order = c(3,1,1))

##AIC from each model
covid_arima1$aic
covid_arima2$aic
covid_arima3$aic


accuracy(covid_arima1)
accuracy(covid_arima2)
accuracy(covid_arima3)

covid_arima_auto <- auto.arima(y = train) #Auro arima model

##Compares
covid_arima3$aic
covid_arima_auto$aic

accuracy(covid_arima3)
accuracy(covid_arima_auto)


#Forecasting
# forecast
forecast_holt <- forecast(covid_holt, h=7)
forecast_auto <- forecast(covid_arima_auto, h=7)
forecast_arima3 <- forecast(covid_arima3, h=7)

# visualization

a <- autoplot(forecast_holt, series = "Holt", fcol = "red") +
  autolayer(covid_ts, series = "Actual", color = "black") + 
  labs(title="Forecast with HoltWinters",
	subtitle = "New COVID-19 cases in Uruguay from February 2020 to December 2021",
       y = "New Cases",
	 x = "Weeks") +
scale_y_continuous(labels = scales::comma) +
  theme(plot.title = element_text(size=13, hjust=0.5, face="bold"),
  panel.background=element_rect(fill="grey95"),
  plot.caption=element_text(size=10, face="italic"))

b <- autoplot(forecast_auto, series = "Auto Arima", fcol = "green") +
  autolayer(covid_ts, series = "Actual", color = "black") +
  labs(title="Forecast with ARIMA(1,1,2)(1,0,1)",
	subtitle = "New COVID-19 cases in Uruguay from February 2020 to December 2021",
       y = "New Cases",
	 x = "Weeks") +
scale_y_continuous(labels = scales::comma) +
  theme(plot.title = element_text(size=13, hjust=0.5, face="bold"),
  panel.background=element_rect(fill="grey95"),
  plot.caption=element_text(size=10, face="italic"))

c <- autoplot(forecast_arima3, series = "Arima3", fcol = "blue") +
  autolayer(covid_ts, series = "Actual", color = "black") +
  labs(title="Forecast with ARIMA(3,1,1)",
	subtitle = "New COVID-19 cases in Uruguay from February 2020 to December 2021",
       y = "New Cases",
	 x = "Weeks")+
scale_y_continuous(labels = scales::comma) +
  theme(plot.title = element_text(size=13, hjust=0.5, face="bold"),
  panel.background=element_rect(fill="grey95"),
  plot.caption=element_text(size=10, face="italic")) 


gridExtra::grid.arrange(a,b,c)

#Evaluation Model
accuracy(forecast_holt, test)

accuracy(forecast_auto, test)

accuracy(forecast_arima3, test)

#Assumption check
shapiro.test(forecast_auto$residuals)

shapiro.test(forecast_holt$residuals)

shapiro.test(forecast_arima3$residuals)

#Autocorrelation
Box.test(forecast_holt$residuals, type = "Ljung-Box")

Box.test(forecast_auto$residuals, type = "Ljung-Box")

Box.test(forecast_arima3$residuals, type = "Ljung-Box")

#Add
#forecasting the value of new.cases in January by using Holt method.
January_f <- forecast(HoltWinters(x = covid_ts, gamma=F), h = 31)

#visualize by table
january_df <- as.data.frame(January_f) %>% 
  mutate(date = seq(from = ymd("2022-01-01"), to = ymd("2022-01-31"), by = "day"))
rmarkdown::paged_table(january_df)

#visualize by plot
covid_ts %>% autoplot() +
autolayer(January_f) +
theme(plot.title=element_text(hjust=0.5, size=14, face="bold"),
plot.caption=element_text(size=15, face="italic")) +
labs(y="Cases", x="Weeks") 

ggsave("forecast.png", width = 15, height = 15, units = c("cm"), dpi = 300)

#
gridExtra::grid.arrange(d,e,f,g,h,i,j,k,l,m,n,o,p, 
nrow= 3)

ggsave("ACF.png")

