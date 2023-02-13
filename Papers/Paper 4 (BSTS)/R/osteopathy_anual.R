library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(CausalImpact)
library(MMWRweek)

mapfre_ts <- read_csv(unzip("Data/MAPFRE_Weekly_data.zip", "MAPFRE_Weekly_data.csv"))
totals <- read_xlsx("Data/S67_IAL_CUBO_CARTERA_HISTORICA_SALUD_FAMILIAS.xlsx")
colnames(mapfre_ts) <- c("Year", "WeekNum", "Code_Real", "Code_Esp", "Code_Group", "Id", "Province", "Sex", "Age", "Acts")
colnames(totals) <- c("Date", "No.Fun", "Fun", "Dental", "Reimb", "Ind", "Total")
totals$Date <- paste0("01/", totals$Date)
totals$Date <- as.Date(totals$Date, "%d/%m/%Y")
totals <- totals[totals$Date >= "2019-01-01" &
                   totals$Date <= "2022-01-01", ]

df <- approx(x=totals$Date,y=totals$Total, xout=seq(as.Date("2019/1/1"), as.Date("2022/01/07"), "weeks"))
totals <- data.frame(Date=df$x, Total=df$y)

totals <- totals[-158, ]

osteopathy <- mapfre_ts[mapfre_ts$Code_Real==54 & mapfre_ts$Code_Esp==54 & mapfre_ts$Code_Group=="10100" & mapfre_ts$Id== 1, ]

osteopathy <- osteopathy %>% group_by(Year, WeekNum) %>% summarise(Acts=sum(Acts)) ### Remove duplicates

### La darrera setmana de 2021 té uns valors molt baixos en tots els casos, no semblen correctes
osteopathy <- osteopathy[-158, ]

osteopathy$Date <- totals$Date

### 2019 vs 2020
y <- osteopathy$Acts[year(osteopathy$Date)<=2020 ]
x <- totals$Total[year(totals$Date)<=2020 ]
post.period <- c(which(year(osteopathy$Date)==2020)[1], which(year(osteopathy$Date)==2020)[length(which(year(osteopathy$Date)==2020))])
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Results/osteopathy_2019_2020.png")
plot(impact)
dev.off()
summary(impact)

### 2019 vs 2021
y <- osteopathy$Acts[year(osteopathy$Date)!=2020]
x <- totals$Total[year(totals$Date)!=2020]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Results/osteopathy_2019_2021.png")
plot(impact)
dev.off()
summary(impact)