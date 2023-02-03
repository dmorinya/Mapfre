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

### TOTAL
### 2019 vs 2020
y <- global$Acts[year(global$Date)<=2020 ]
x <- totals$Total[year(totals$Date)<=2020 ]
post.period <- c(which(year(global$Date)==2020)[1], which(year(global$Date)==2020)[length(which(year(global$Date)==2020))])
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Results/global_2019_2020.png")
  plot(impact)
dev.off()
summary(impact)

### 2019 vs 2021
y <- global$Acts[year(global$Date)!=2020]
x <- totals$Total[year(totals$Date)!=2020]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Results/global_2019_2021.png")
  plot(impact)
dev.off()
summary(impact)

rm(global)

### ONLY FEMALES
females <- mapfre_ts %>% filter(Sex==1) %>% group_by(Year, WeekNum) %>% 
          summarise(Acts=sum(Acts))

females$Date <- seq(as.Date("2019/1/1"), as.Date("2022/01/07"), "weeks")
females <- females[-158, ]
### 2019 vs 2020
y <- females$Acts[year(females$Date)<=2020 ]
x <- totals$Total[year(totals$Date)<=2020 ]
post.period <- c(which(year(females$Date)==2020)[1], which(year(females$Date)==2020)[length(which(year(females$Date)==2020))])
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Results/females_2019_2020.png")
  plot(impact)
dev.off()
summary(impact)

### 2019 vs 2021
y <- females$Acts[year(females$Date)!=2020]
x <- totals$Total[year(totals$Date)!=2020]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Results/females_2019_2021.png")
  plot(impact)
dev.off()
summary(impact)

### MALES (Sex=2)

### OVER 60
over60 <- mapfre_ts %>% filter(Age=="(59, 200]") %>% group_by(Year, WeekNum) %>% 
          summarise(Acts=sum(Acts))

over60$Date <- seq(as.Date("2019/1/1"), as.Date("2022/01/07"), "weeks")
over60 <- over60[-158, ]
### 2019 vs 2020
y <- over60$Acts[year(over60$Date)<=2020 ]
x <- totals$Total[year(totals$Date)<=2020 ]
post.period <- c(which(year(over60$Date)==2020)[1], which(year(over60$Date)==2020)[length(which(year(over60$Date)==2020))])
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Results/over60_2019_2020.png")
  plot(impact)
dev.off()
summary(impact)

### 2019 vs 2021
y <- over60$Acts[year(over60$Date)!=2020]
x <- totals$Total[year(totals$Date)!=2020]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Results/over60_2019_2021.png")
  plot(impact)
dev.off()
summary(impact)

### MADRID
madrid <- mapfre_ts %>% filter(Province==28) %>% group_by(Year, WeekNum) %>% 
          summarise(Acts=sum(Acts))

madrid$Date <- seq(as.Date("2019/1/1"), as.Date("2022/01/07"), "weeks")
madrid <- madrid[-158, ]
### 2019 vs 2020
y <- madrid$Acts[year(madrid$Date)<=2020 ]
x <- totals$Total[year(totals$Date)<=2020 ]
post.period <- c(which(year(madrid$Date)==2020)[1], which(year(madrid$Date)==2020)[length(which(year(madrid$Date)==2020))])
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Results/madrid_2019_2020.png")
  plot(impact)
dev.off()
summary(impact)

### 2019 vs 2021
y <- madrid$Acts[year(madrid$Date)!=2020]
x <- totals$Total[year(totals$Date)!=2020]
post.period <- c(54,105)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA
ss <- AddStaticIntercept(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 52)
bsts.model <- bsts(y~x, ss, niter = 50000)
impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
png("Results/madrid_2019_2021.png")
  plot(impact)
dev.off()
summary(impact)

### BARCELONA (Province=08)

### VALENCIA (Province=46)