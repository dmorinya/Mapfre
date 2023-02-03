library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(CausalImpact)
library(MMWRweek)
library(forecast)
library(tseries)

mapfre_ts <- read_csv(unzip("Data/MAPFRE_Weekly_data.zip", "MAPFRE_Weekly_data.csv"))
colnames(mapfre_ts) <- c("Year", "WeekNum", "Code_Real", "Code_Esp", "Code_Group", "Id", "Province", "Sex", "Age", "Acts")
totals <- read_xlsx("Data/S67_IAL_CUBO_CARTERA_HISTORICA_SALUD_FAMILIAS.xlsx")
colnames(totals) <- c("Date", "No.Fun", "Fun", "Dental", "Reimb", "Ind", "Total")
totals$Date <- paste0("01/", totals$Date)
totals$Date <- as.Date(totals$Date, "%d/%m/%Y")
totals <- totals[totals$Date >= "2019-01-01" &
                 totals$Date <= "2022-01-01", ]

df <- approx(x=totals$Date,y=totals$Total, xout=seq(as.Date("2019/1/1"), as.Date("2022/01/07"), "weeks"))
totals <- data.frame(Date=df$x, Total=df$y)

global <- mapfre_ts %>% group_by(Year, WeekNum) %>%
          summarise(Acts=sum(Acts))

global$Date <- seq(as.Date("2019/1/1"), as.Date("2022/01/07"), "weeks")
global <- global[-158, ]
totals <- totals[-158, ]

### DESCRIPTIVE
ref <- sum(global$Acts[global$Year==2019])
y2020 <- sum(global$Acts[global$Year==2020])
y2021 <- sum(global$Acts[global$Year==2021])

(y2020-ref)/ref*100
(y2021-ref)/ref*100

### ARIMA modelling
y <- global$Acts[global$Date<="2021-06-01"]

### Stationarity test
kpss.test(y, null="Trend") ### Not trend stationary (p-value = 0.01)

x <- totals$Total[totals$Date<="2021-06-01"]
fit2 <- auto.arima(y, xreg=x)

pred <- predict(fit2, n.ahead=30, newxreg=data.frame(x=rep(607852.9, 30)), interval="prediction")

rmse <- sqrt(mean((global$Acts[global$Date>"2021-06-01"]-pred$pred)^2)) ### 46989.39
mape <- mean(abs((global$Acts[global$Date>"2021-06-01"]-pred$pred)/global$Acts[global$Date>"2021-06-01"])) * 100 ### 17.23%

lower <- pred$pred - qnorm(0.975)*pred$se
upper <- pred$pred + qnorm(0.975)*pred$se

png("Papers/Paper 4 (BSTS)/Results/arima_global_prediction.png", width=1280)
ggplot(global, aes(x=Date, y=Acts)) +
  geom_line() + ylim(10000, 450000) + xlab("")+geom_point()+scale_x_date(date_labels = "%m-%Y", date_breaks = "3 months") + ggtitle("GLOBAL") + theme(plot.title = element_text(hjust = 0.5))+geom_line(y = c(rep(NA, 127), pred$pred), color = "red")+
  geom_ribbon(aes(ymin=c(rep(NA, 127), lower),ymax=c(rep(NA, 127), upper)), fill="blue", alpha=0.2)
dev.off()