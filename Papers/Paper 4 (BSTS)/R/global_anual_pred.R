library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(CausalImpact)
library(MMWRweek)

mapfre_ts <- read.table("Data/MAPFRE_Weekly_data.csv", header=T, sep=",") # nolint
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

y <- global$Acts[global$Date<="2021-06-01"]
x <- totals$Total[totals$Date<="2021-06-01"]
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)

### Predictions
pred <- predict(bsts.model, newdata=data.frame(x=rep(607852.9, 30)), burn = 100)

rmse <- sqrt(mean((global$Acts[global$Date>"2021-06-01"]-pred$mean)^2)) ### 37982.85
mape <- mean(abs((global$Acts[global$Date>"2021-06-01"]-pred$mean)/global$Acts[global$Date>"2021-06-01"])) * 100 ### 14.21%

png("Papers/Paper 4 (BSTS)/Results/bsts_global_prediction.png", width=1280)
ggplot(global, aes(x=Date, y=Acts)) +
  geom_line() + ylim(10000, 450000) + xlab("")+geom_point()+scale_x_date(date_labels = "%m-%Y", date_breaks = "3 months") + ggtitle("GLOBAL") + theme(plot.title = element_text(hjust = 0.5))+geom_line(y = c(rep(NA, 127), pred$mean), color = "red")+
  geom_ribbon(aes(ymin=c(rep(NA, 127), pred$interval[1,]),ymax=c(rep(NA, 127), pred$interval[2,])), fill="blue", alpha=0.2)
dev.off()