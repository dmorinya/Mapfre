library(matrixStats)
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(MMWRweek)

cases <- read_xls("Data/cases.xls")
cases <- cases[, c(1,2,3)]
cases$fecha <- as.Date(cases$fecha, format="%Y-%m-%d")
colnames(cases) <- c("CCAA", "Date", "cases")

#### Group by week
cases$Week <- MMWRweek(cases$Date)[,2]
cases$Year <- year(cases$Date)
cases$Year[cases$Week==53 & cases$Year==2021] <- 2020
cases$Year[cases$Week==52 & cases$Year==2022] <- 2021
cases2 <- cases %>%
  group_by(Year, Week) %>%
  summarise(cases2=sum(cases))
cases2 <- cases2[order(cases2$Year, cases2$Week), ]
cases <- cases2
colnames(cases) <- c("Year", "Week", "cases")

pob <- read_xls("Data/poblacio.xls")
cases$incid <- cases$cases/sum(pob$Pob)*100000
cases <- cases[cases$Week>8 | cases$Year>2020, ]
#cases <- cases[cases$CCAA!="Ceuta" & cases$CCAA!="Melilla", ]
cases <- cases[order(cases$Year, cases$Week), ]

AN <- read.table("Results/global_incidence.csv", header=T, sep=",")/100000*sum(pob$Pob)
q2.5 <- colQuantiles(as.matrix(AN), probs=c(0.025))
med <- colMedians(as.matrix(AN))
q97.5 <- colQuantiles(as.matrix(AN), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week
resum$Value <- "Estimated"
resum$med[107:212] <- cases$cases
resum$Value[107:212] <- "Registered"
resum$q2.5[107:212] <- resum$med[107:212]
resum$q97.5[107:212] <- resum$med[107:212]
resum$med2 <- c(resum$med[1:106], rep(NA, 106))
resum$Year <- cases$Year
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_AN <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Global")+
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  labs(col = "Value")

### Global values
est <- sum(resum$med[resum$Value=="Estimated"])
reg <- sum(resum$med[resum$Value=="Registered"])
reg/est*100
