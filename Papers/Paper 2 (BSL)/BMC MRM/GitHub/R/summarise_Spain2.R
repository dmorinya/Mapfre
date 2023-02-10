library(matrixStats)
library(gdata)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(MMWRweek)

setwd("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/Paper 2 (BSL)/BMC MRM/GitHub/")
cases <- read.xls("Data/cases.xls")
cases <- cases[, c(1,2,3)]
cases$fecha <- as.Date(cases$fecha, format="%Y-%m-%d")
colnames(cases) <- c("CCAA", "Date", "cases")

#### Group by week
cases$Week <- MMWRweek(cases$Date)[,2]
cases$Year <- year(cases$Date)
cases$Year[cases$Week==53 & cases$Year==2021] <- 2020
cases$Year[cases$Week==52 & cases$Year==2022] <- 2021
cases2 <- cases %>%
  group_by(CCAA, Year, Week) %>%
  summarise(cases2=sum(cases))
cases2 <- cases2[order(cases2$Year, cases2$Week), ]
cases <- cases2
colnames(cases) <- c("CCAA2", "Year", "Week", "cases")
cases$CCAA <- NA
cases$CCAA[cases$CCAA2=="AN"] <- "Andalucía"
cases$CCAA[cases$CCAA2=="AR"] <- "Aragón"
cases$CCAA[cases$CCAA2=="AS"] <- "Principado de Asturias"
cases$CCAA[cases$CCAA2=="CB"] <- "Cantabria"
cases$CCAA[cases$CCAA2=="CE"] <- "Ceuta"
cases$CCAA[cases$CCAA2=="CL"] <- "Castilla y León"
cases$CCAA[cases$CCAA2=="CM"] <- "Castilla - La Mancha"
cases$CCAA[cases$CCAA2=="CN"] <- "Canarias"
cases$CCAA[cases$CCAA2=="CT"] <- "Cataluña"
cases$CCAA[cases$CCAA2=="EX"] <- "Extremadura"
cases$CCAA[cases$CCAA2=="GA"] <- "Galicia"
cases$CCAA[cases$CCAA2=="IB"] <- "Islas Baleares"
cases$CCAA[cases$CCAA2=="MC"] <- "Región de Murcia"
cases$CCAA[cases$CCAA2=="MD"] <- "Madrid"
cases$CCAA[cases$CCAA2=="ML"] <- "Melilla"
cases$CCAA[cases$CCAA2=="NC"] <- "Comunidad Foral de Navarra"
cases$CCAA[cases$CCAA2=="PV"] <- "País Vasco"
cases$CCAA[cases$CCAA2=="RI"] <- "La Rioja"
cases$CCAA[cases$CCAA2=="VC"] <- "Comunidad Valenciana"
cases$cod_ine[cases$CCAA2=="AN"] <- 1
cases$cod_ine[cases$CCAA2=="AR"] <- 2
cases$cod_ine[cases$CCAA2=="AS"] <- 3
cases$cod_ine[cases$CCAA2=="CB"] <- 6
cases$cod_ine[cases$CCAA2=="CE"] <- 18
cases$cod_ine[cases$CCAA2=="CL"] <- 7
cases$cod_ine[cases$CCAA2=="CM"] <- 8
cases$cod_ine[cases$CCAA2=="CN"] <- 5
cases$cod_ine[cases$CCAA2=="CT"] <- 9
cases$cod_ine[cases$CCAA2=="EX"] <- 11
cases$cod_ine[cases$CCAA2=="GA"] <- 12
cases$cod_ine[cases$CCAA2=="IB"] <- 4
cases$cod_ine[cases$CCAA2=="MC"] <- 14
cases$cod_ine[cases$CCAA2=="MD"] <- 13
cases$cod_ine[cases$CCAA2=="ML"] <- 19
cases$cod_ine[cases$CCAA2=="NC"] <- 15
cases$cod_ine[cases$CCAA2=="PV"] <- 16
cases$cod_ine[cases$CCAA2=="RI"] <- 17
cases$cod_ine[cases$CCAA2=="VC"] <- 10
cases$CCAA2 <- NULL

#cases <- cases[cases$Year < 2022, ]

pob <- read.xls("Data/poblacio.xls", encoding="latin1")
cases <- merge(cases, pob, by=c("CCAA"))
cases$incid <- cases$cases/cases$Pob*100000
cases <- cases[cases$Week>8 | cases$Year>2020, ]
#cases <- cases[cases$CCAA!="Ceuta" & cases$CCAA!="Melilla", ]
cases <- cases[order(cases$CCAA, cases$Year, cases$Week), ]

AN <- read.table("Results/Andalucía_incidence3.csv", header=T, sep=",")/100000*8414240
q2.5 <- colQuantiles(as.matrix(AN), probs=c(0.025))
med <- colMedians(as.matrix(AN))
q97.5 <- colQuantiles(as.matrix(AN), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Andalucía"]
resum$Value <- "Estimated"
resum$med[107:212] <- cases$cases[cases$CCAA=="Andalucía"]
resum$Value[107:212] <- "Registered"
resum$q2.5[107:212] <- resum$med[107:212]
resum$q97.5[107:212] <- resum$med[107:212]
resum$med2 <- c(resum$med[1:106], rep(NA, 106))
resum$Year <- cases$Year[cases$CCAA=="Andalucía"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_AN <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Andalucía")+
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  labs(col = "Value")
error_AN <- data.frame(Week=cases$Week[cases$CCAA=="Andalucía"], Year=cases$Year[cases$CCAA=="Andalucía"],
                       reg=cases$cases[cases$CCAA=="Andalucía"], reg_est=(1-0.957977+0.957977*0.452245)*colMedians(as.matrix(AN)))
rmse_AN <- sqrt(mean((error_AN$reg-error_AN$reg_est)^2))
mape_AN <- mean(abs((error_AN$reg-error_AN$reg_est)/error_AN$reg)) * 100

AR <- read.table("Results/Aragón_incidence3.csv", header=T, sep=",")/100000*1319291
q2.5 <- colQuantiles(as.matrix(AR), probs=c(0.025))
med <- colMedians(as.matrix(AR))
q97.5 <- colQuantiles(as.matrix(AR), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Aragón"]
resum$Value <- "Estimated"
resum$med[107:212] <- cases$cases[cases$CCAA=="Aragón"]
resum$Value[107:212] <- "Registered"
resum$q2.5[107:212] <- resum$med[107:212]
resum$q97.5[107:212] <- resum$med[107:212]
resum$med2 <- c(resum$med[1:106], rep(NA, 106))
resum$Year <- cases$Year[cases$CCAA=="Aragón"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_AR <- resum %>% ggplot(aes(x=Date, y=med, group=Value, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Aragón") +
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")
error_AR <- data.frame(Week=cases$Week[cases$CCAA=="Aragón"], Year=cases$Year[cases$CCAA=="Aragón"],
                       reg=cases$cases[cases$CCAA=="Aragón"], reg_est=(1-0.9798+0.9798*0.2962)*colMedians(as.matrix(AR)))
rmse_AR <- sqrt(mean((error_AR$reg-error_AR$reg_est)^2))
mape_AR <- mean(abs((error_AR$reg-error_AR$reg_est)/error_AR$reg)) * 100

CN <- read.table("Results/Canarias_incidence3.csv", header=T, sep=",")/100000*2153389
q2.5 <- colQuantiles(as.matrix(CN), probs=c(0.025))
med <- colMedians(as.matrix(CN))
q97.5 <- colQuantiles(as.matrix(CN), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Canarias"]
resum$Value <- "Estimated"
resum$med[107:212] <- cases$cases[cases$CCAA=="Canarias"]
resum$Value[107:212] <- "Registered"
resum$q2.5[107:212] <- resum$med[107:212]
resum$q97.5[107:212] <- resum$med[107:212]
resum$med2 <- c(resum$med[1:106], rep(NA, 106))
resum$Year <- cases$Year[cases$CCAA=="Canarias"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_CN <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Canarias") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")
error_CN <- data.frame(Week=cases$Week[cases$CCAA=="Canarias"], Year=cases$Year[cases$CCAA=="Canarias"],
                       reg=cases$cases[cases$CCAA=="Canarias"], reg_est=(1-0.9760+0.9760*0.3242)*colMedians(as.matrix(CN)))
rmse_CN <- sqrt(mean((error_CN$reg-error_CN$reg_est)^2))
mape_CN <- mean(abs((error_CN$reg-error_CN$reg_est)/error_CN$reg)) * 100

CB <- read.table("Results/Cantabria_incidence3.csv", header=T, sep=",")/100000*581078
q2.5 <- colQuantiles(as.matrix(CB), probs=c(0.025))
med <- colMedians(as.matrix(CB))
q97.5 <- colQuantiles(as.matrix(CB), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Cantabria"]
resum$Value <- "Estimated"
resum$med[107:212] <- cases$cases[cases$CCAA=="Cantabria"]
resum$Value[107:212] <- "Registered"
resum$q2.5[107:212] <- resum$med[107:212]
resum$q97.5[107:212] <- resum$med[107:212]
resum$med2 <- c(resum$med[1:106], rep(NA, 106))
resum$Year <- cases$Year[cases$CCAA=="Cantabria"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_CB <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Cantabria") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")
error_CB <- data.frame(Week=cases$Week[cases$CCAA=="Cantabria"], Year=cases$Year[cases$CCAA=="Cantabria"],
                       reg=cases$cases[cases$CCAA=="Cantabria"], reg_est=(1-0.9710+0.9710*0.31497)*colMedians(as.matrix(CB)))
rmse_CB <- sqrt(mean((error_CB$reg-error_CB$reg_est)^2))
mape_CB <- mean(abs((error_CB$reg-error_CB$reg_est)/error_CB$reg)) * 100

CM <- read.table("Results/Castilla - La Mancha_incidence3.csv", header=T, sep=",")/100000*2032863
q2.5 <- colQuantiles(as.matrix(CM), probs=c(0.025))
med <- colMedians(as.matrix(CM))
q97.5 <- colQuantiles(as.matrix(CM), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Castilla - La Mancha"]
resum$Value <- "Estimated"
resum$med[107:212] <- cases$cases[cases$CCAA=="Castilla - La Mancha"]
resum$Value[107:212] <- "Registered"
resum$q2.5[107:212] <- resum$med[107:212]
resum$q97.5[107:212] <- resum$med[107:212]
resum$med2 <- c(resum$med[1:106], rep(NA, 106))
resum$Year <- cases$Year[cases$CCAA=="Castilla - La Mancha"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_CM <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Castilla - La Mancha") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")
error_CM <- data.frame(Week=cases$Week[cases$CCAA=="Castilla - La Mancha"], Year=cases$Year[cases$CCAA=="Castilla - La Mancha"],
                       reg=cases$cases[cases$CCAA=="Castilla - La Mancha"], reg_est=(1-0.963280+0.963280*0.357228)*colMedians(as.matrix(CM)))
rmse_CM <- sqrt(mean((error_CM$reg-error_CM$reg_est)^2))
mape_CM <- mean(abs((error_CM$reg-error_CM$reg_est)/error_CM$reg)) * 100

CL <- read.table("Results/Castilla y León_incidence3.csv", header=T, sep=",")/100000*2399548
q2.5 <- colQuantiles(as.matrix(CL), probs=c(0.025))
med <- colMedians(as.matrix(CL))
q97.5 <- colQuantiles(as.matrix(CL), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Castilla y León"]
resum$Value <- "Estimated"
resum$med[107:212] <- cases$cases[cases$CCAA=="Castilla y León"]
resum$Value[107:212] <- "Registered"
resum$q2.5[107:212] <- resum$med[107:212]
resum$q97.5[107:212] <- resum$med[107:212]
resum$med2 <- c(resum$med[1:106], rep(NA, 106))
resum$Year <- cases$Year[cases$CCAA=="Castilla y León"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_CL <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Castilla y León") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")
error_CL <- data.frame(Week=cases$Week[cases$CCAA=="Castilla y León"], Year=cases$Year[cases$CCAA=="Castilla y León"],
                       reg=cases$cases[cases$CCAA=="Castilla y León"], reg_est=(1-0.9789+0.9789*0.3770)*colMedians(as.matrix(CL)))
rmse_CL <- sqrt(mean((error_CL$reg-error_CL$reg_est)^2))
mape_CL <- mean(abs((error_CL$reg-error_CL$reg_est)/error_CL$reg)) * 100

CT <- read.table("Results/Cataluña_incidence3.csv", header=T, sep=",")/100000*7675217
q2.5 <- colQuantiles(as.matrix(CT), probs=c(0.025))
med <- colMedians(as.matrix(CT))
q97.5 <- colQuantiles(as.matrix(CT), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Cataluña"]
resum$Value <- "Estimated"
resum$med[107:212] <- cases$cases[cases$CCAA=="Cataluña"]
resum$Value[107:212] <- "Registered"
resum$q2.5[107:212] <- resum$med[107:212]
resum$q97.5[107:212] <- resum$med[107:212]
resum$med2 <- c(resum$med[1:106], rep(NA, 106))
resum$Year <- cases$Year[cases$CCAA=="Cataluña"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_CT <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Catalunya") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")
error_CT <- data.frame(Week=cases$Week[cases$CCAA=="Cataluña"], Year=cases$Year[cases$CCAA=="Cataluña"],
                       reg=cases$cases[cases$CCAA=="Cataluña"], reg_est=(1-0.9820+0.9820*0.3545)*colMedians(as.matrix(CT)))
rmse_CT <- sqrt(mean((error_CT$reg-error_CT$reg_est)^2))
mape_CT <- mean(abs((error_CT$reg-error_CT$reg_est)/error_CT$reg)) * 100

CE <- read.table("Results/Ceuta_incidence3.csv", header=T, sep=",")/100000*84777
q2.5 <- colQuantiles(as.matrix(CE), probs=c(0.025))
med <- colMedians(as.matrix(CE))
q97.5 <- colQuantiles(as.matrix(CE), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Ceuta"]
resum$Value <- "Estimated"
resum$med[107:212] <- cases$cases[cases$CCAA=="Ceuta"]
resum$Value[107:212] <- "Registered"
resum$q2.5[107:212] <- resum$med[107:212]
resum$q97.5[107:212] <- resum$med[107:212]
resum$med2 <- c(resum$med[1:106], rep(NA, 106))
resum$Year <- cases$Year[cases$CCAA=="Ceuta"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_CE <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Ceuta") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")
error_CE <- data.frame(Week=cases$Week[cases$CCAA=="Ceuta"], Year=cases$Year[cases$CCAA=="Ceuta"],
                       reg=cases$cases[cases$CCAA=="Ceuta"], reg_est=(1-0.97088+0.97088*0.30232)*colMedians(as.matrix(CE)))
rmse_CE <- sqrt(mean((error_CE$reg-error_CE$reg_est)^2))
mape_CE <- mean(abs((error_CE$reg-error_CE$reg_est)/error_CE$reg)) * 100

NC <- read.table("Results/Comunidad Foral de Navarra_incidence3.csv", header=T, sep=",")/100000*654214
q2.5 <- colQuantiles(as.matrix(NC), probs=c(0.025))
med <- colMedians(as.matrix(NC))
q97.5 <- colQuantiles(as.matrix(NC), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Comunidad Foral de Navarra"]
resum$Value <- "Estimated"
resum$med[107:212] <- cases$cases[cases$CCAA=="Comunidad Foral de Navarra"]
resum$Value[107:212] <- "Registered"
resum$q2.5[107:212] <- resum$med[107:212]
resum$q97.5[107:212] <- resum$med[107:212]
resum$med2 <- c(resum$med[1:106], rep(NA, 106))
resum$Year <- cases$Year[cases$CCAA=="Comunidad Foral de Navarra"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_NC <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Nafarroa") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")
error_NC <- data.frame(Week=cases$Week[cases$CCAA=="Comunidad Foral de Navarra"], Year=cases$Year[cases$CCAA=="Comunidad Foral de Navarra"],
                       reg=cases$cases[cases$CCAA=="Comunidad Foral de Navarra"], reg_est=(1-0.9836+0.9836*0.3118)*colMedians(as.matrix(NC)))
rmse_NC <- sqrt(mean((error_NC$reg-error_NC$reg_est)^2))
mape_NC <- mean(abs((error_NC$reg-error_NC$reg_est)/error_NC$reg)) * 100

VC <- read.table("Results/Comunidad Valenciana_incidence3.csv", header=T, sep=",")/100000*5003769
q2.5 <- colQuantiles(as.matrix(VC), probs=c(0.025))
med <- colMedians(as.matrix(VC))
q97.5 <- colQuantiles(as.matrix(VC), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Comunidad Valenciana"]
resum$Value <- "Estimated"
resum$med[107:212] <- cases$cases[cases$CCAA=="Comunidad Valenciana"]
resum$Value[107:212] <- "Registered"
resum$q2.5[107:212] <- resum$med[107:212]
resum$q97.5[107:212] <- resum$med[107:212]
resum$med2 <- c(resum$med[1:106], rep(NA, 106))
resum$Year <- cases$Year[cases$CCAA=="Comunidad Valenciana"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_VC <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("País Valencià") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")
error_VC <- data.frame(Week=cases$Week[cases$CCAA=="Comunidad Valenciana"], Year=cases$Year[cases$CCAA=="Comunidad Valenciana"],
                       reg=cases$cases[cases$CCAA=="Comunidad Valenciana"], reg_est=(1-0.9860+0.9860*0.3843)*colMedians(as.matrix(VC)))
rmse_VC <- sqrt(mean((error_VC$reg-error_VC$reg_est)^2))
mape_VC <- mean(abs((error_VC$reg-error_VC$reg_est)/error_VC$reg)) * 100

EX <- read.table("Results/Extremadura_incidence3.csv", header=T, sep=",")/100000*1067710
q2.5 <- colQuantiles(as.matrix(EX), probs=c(0.025))
med <- colMedians(as.matrix(EX))
q97.5 <- colQuantiles(as.matrix(EX), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Extremadura"]
resum$Value <- "Estimated"
resum$med[107:212] <- cases$cases[cases$CCAA=="Extremadura"]
resum$Value[107:212] <- "Registered"
resum$q2.5[107:212] <- resum$med[107:212]
resum$q97.5[107:212] <- resum$med[107:212]
resum$med2 <- c(resum$med[1:106], rep(NA, 106))
resum$Year <- cases$Year[cases$CCAA=="Extremadura"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_EX <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Extremadura") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")
error_EX <- data.frame(Week=cases$Week[cases$CCAA=="Extremadura"], Year=cases$Year[cases$CCAA=="Extremadura"],
                       reg=cases$cases[cases$CCAA=="Extremadura"], reg_est=(1-0.896262+0.896262*0.502994)*colMedians(as.matrix(EX)))
rmse_EX <- sqrt(mean((error_EX$reg-error_EX$reg_est)^2))
mape_EX <- mean(abs((error_EX$reg-error_EX$reg_est)/error_EX$reg)) * 100

GA <- read.table("Results/Galicia_incidence3.csv", header=T, sep=",")/100000*2699499
q2.5 <- colQuantiles(as.matrix(GA), probs=c(0.025))
med <- colMedians(as.matrix(GA))
q97.5 <- colQuantiles(as.matrix(GA), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Galicia"]
resum$Value <- "Estimated"
resum$med[107:212] <- cases$cases[cases$CCAA=="Galicia"]
resum$Value[107:212] <- "Registered"
resum$q2.5[107:212] <- resum$med[107:212]
resum$q97.5[107:212] <- resum$med[107:212]
resum$med2 <- c(resum$med[1:106], rep(NA, 106))
resum$Year <- cases$Year[cases$CCAA=="Galicia"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_GA <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Galiza") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")
error_GA <- data.frame(Week=cases$Week[cases$CCAA=="Galicia"], Year=cases$Year[cases$CCAA=="Galicia"],
                       reg=cases$cases[cases$CCAA=="Galicia"], reg_est=(1-0.9847+0.9847*0.3296)*colMedians(as.matrix(GA)))
rmse_GA <- sqrt(mean((error_GA$reg-error_GA$reg_est)^2))
mape_GA <- mean(abs((error_GA$reg-error_GA$reg_est)/error_GA$reg)) * 100

IB <- read.table("Results/Islas Baleares_incidence3.csv", header=T, sep=",")/100000*1149460
q2.5 <- colQuantiles(as.matrix(IB), probs=c(0.025))
med <- colMedians(as.matrix(IB))
q97.5 <- colQuantiles(as.matrix(IB), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Islas Baleares"]
resum$Value <- "Estimated"
resum$med[107:212] <- cases$cases[cases$CCAA=="Islas Baleares"]
resum$Value[107:212] <- "Registered"
resum$q2.5[107:212] <- resum$med[107:212]
resum$q97.5[107:212] <- resum$med[107:212]
resum$med2 <- c(resum$med[1:106], rep(NA, 106))
resum$Year <- cases$Year[cases$CCAA=="Islas Baleares"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_IB <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Illes Balears") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")
error_IB <- data.frame(Week=cases$Week[cases$CCAA=="Islas Baleares"], Year=cases$Year[cases$CCAA=="Islas Baleares"],
                       reg=cases$cases[cases$CCAA=="Islas Baleares"], reg_est=(1-0.9641+0.9641*0.3899)*colMedians(as.matrix(IB)))
rmse_IB <- sqrt(mean((error_IB$reg-error_IB$reg_est)^2))
mape_IB <- mean(abs((error_IB$reg-error_IB$reg_est)/error_IB$reg)) * 100

RI <- read.table("Results/La Rioja_incidence3.csv", header=T, sep=",")/100000*316798
q2.5 <- colQuantiles(as.matrix(RI), probs=c(0.025))
med <- colMedians(as.matrix(RI))
q97.5 <- colQuantiles(as.matrix(RI), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="La Rioja"]
resum$Value <- "Estimated"
resum$med[107:212] <- cases$cases[cases$CCAA=="La Rioja"]
resum$Value[107:212] <- "Registered"
resum$q2.5[107:212] <- resum$med[107:212]
resum$q97.5[107:212] <- resum$med[107:212]
resum$med2 <- c(resum$med[1:106], rep(NA, 106))
resum$Year <- cases$Year[cases$CCAA=="La Rioja"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_RI <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("La Rioja") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")
error_RI <- data.frame(Week=cases$Week[cases$CCAA=="La Rioja"], Year=cases$Year[cases$CCAA=="La Rioja"],
                       reg=cases$cases[cases$CCAA=="La Rioja"], reg_est=(1-0.9774+0.9774*0.3236)*colMedians(as.matrix(RI)))
rmse_RI <- sqrt(mean((error_RI$reg-error_RI$reg_est)^2))
mape_RI <- mean(abs((error_RI$reg-error_RI$reg_est)/error_RI$reg)) * 100

MD <- read.table("Results/Madrid_incidence3.csv", header=T, sep=",")/100000*6663394
q2.5 <- colQuantiles(as.matrix(MD), probs=c(0.025))
med <- colMedians(as.matrix(MD))
q97.5 <- colQuantiles(as.matrix(MD), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Madrid"]
resum$Value <- "Estimated"
resum$med[107:212] <- cases$cases[cases$CCAA=="Madrid"]
resum$Value[107:212] <- "Registered"
resum$q2.5[107:212] <- resum$med[107:212]
resum$q97.5[107:212] <- resum$med[107:212]
resum$med2 <- c(resum$med[1:106], rep(NA, 106))
resum$Year <- cases$Year[cases$CCAA=="Madrid"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_MD <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Madrid") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")
error_MD <- data.frame(Week=cases$Week[cases$CCAA=="Madrid"], Year=cases$Year[cases$CCAA=="Madrid"],
                       reg=cases$cases[cases$CCAA=="Madrid"], reg_est=(1-0.9752+0.9752*0.3971)*colMedians(as.matrix(MD)))
rmse_MD <- sqrt(mean((error_MD$reg-error_MD$reg_est)^2))
mape_MD <- mean(abs((error_MD$reg-error_MD$reg_est)/error_MD$reg)) * 100

ME <- read.table("Results/Melilla_incidence3.csv", header=T, sep=",")/100000*86487
q2.5 <- colQuantiles(as.matrix(ME), probs=c(0.025))
med <- colMedians(as.matrix(ME))
q97.5 <- colQuantiles(as.matrix(ME), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Melilla"]
resum$Value <- "Estimated"
resum$med[107:212] <- cases$cases[cases$CCAA=="Melilla"]
resum$Value[107:212] <- "Registered"
resum$q2.5[107:212] <- resum$med[107:212]
resum$q97.5[107:212] <- resum$med[107:212]
resum$med2 <- c(resum$med[1:106], rep(NA, 106))
resum$Year <- cases$Year[cases$CCAA=="Melilla"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_ME <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Melilla") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")
error_ME <- data.frame(Week=cases$Week[cases$CCAA=="Melilla"], Year=cases$Year[cases$CCAA=="Melilla"],
                       reg=cases$cases[cases$CCAA=="Melilla"], reg_est=(1-0.9728+0.9728*0.3466)*colMedians(as.matrix(ME)))
rmse_ME <- sqrt(mean((error_ME$reg-error_ME$reg_est)^2))
mape_ME <- mean(abs((error_ME$reg-error_ME$reg_est)/error_ME$reg)) * 100

PV <- read.table("Results/País Vasco_incidence3.csv", header=T, sep=",")/100000*2207776
q2.5 <- colQuantiles(as.matrix(PV), probs=c(0.025))
med <- colMedians(as.matrix(PV))
q97.5 <- colQuantiles(as.matrix(PV), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="País Vasco"]
resum$Value <- "Estimated"
resum$med[107:212] <- cases$cases[cases$CCAA=="País Vasco"]
resum$Value[107:212] <- "Registered"
resum$q2.5[107:212] <- resum$med[107:212]
resum$q97.5[107:212] <- resum$med[107:212]
resum$med2 <- c(resum$med[1:106], rep(NA, 106))
resum$Year <- cases$Year[cases$CCAA=="País Vasco"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_PV <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Euskadi") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")
error_PV <- data.frame(Week=cases$Week[cases$CCAA=="País Vasco"], Year=cases$Year[cases$CCAA=="País Vasco"],
                       reg=cases$cases[cases$CCAA=="País Vasco"], reg_est=0.29616*colMedians(as.matrix(PV)))
rmse_PV <- sqrt(mean((error_PV$reg-error_PV$reg_est)^2))
mape_PV <- mean(abs((error_PV$reg-error_PV$reg_est)/error_PV$reg)) * 100

AS <- read.table("Results/Principado de Asturias_incidence3.csv", header=T, sep=",")/100000*1022800
q2.5 <- colQuantiles(as.matrix(AS), probs=c(0.025))
med <- colMedians(as.matrix(AS))
q97.5 <- colQuantiles(as.matrix(AS), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Principado de Asturias"]
resum$Value <- "Estimated"
resum$med[107:212] <- cases$cases[cases$CCAA=="Principado de Asturias"]
resum$Value[107:212] <- "Registered"
resum$q2.5[107:212] <- resum$med[107:212]
resum$q97.5[107:212] <- resum$med[107:212]
resum$med2 <- c(resum$med[1:106], rep(NA, 106))
resum$Year <- cases$Year[cases$CCAA=="Principado de Asturias"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_AS <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Asturies") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")
error_AS <- data.frame(Week=cases$Week[cases$CCAA=="Principado de Asturias"], Year=cases$Year[cases$CCAA=="Principado de Asturias"],
                       reg=cases$cases[cases$CCAA=="Principado de Asturias"], reg_est=(1-0.9830+0.9830*0.3454)*colMedians(as.matrix(AS)))
rmse_AS <- sqrt(mean((error_AS$reg-error_AS$reg_est)^2))
mape_AS <- mean(abs((error_AS$reg-error_AS$reg_est)/error_AS$reg)) * 100

MC <- read.table("Results/Región de Murcia_incidence3.csv", header=T, sep=",")/100000*1493898
q2.5 <- colQuantiles(as.matrix(MC), probs=c(0.025))
med <- colMedians(as.matrix(MC))
q97.5 <- colQuantiles(as.matrix(MC), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Región de Murcia"]
resum$Value <- "Estimated"
resum$med[107:212] <- cases$cases[cases$CCAA=="Región de Murcia"]
resum$Value[107:212] <- "Registered"
resum$q2.5[107:212] <- resum$med[107:212]
resum$q97.5[107:212] <- resum$med[107:212]
resum$med2 <- c(resum$med[1:106], rep(NA, 106))
resum$Year <- cases$Year[cases$CCAA=="Región de Murcia"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_MC <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Región de Murcia") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")
error_MC <- data.frame(Week=cases$Week[cases$CCAA=="Región de Murcia"], Year=cases$Year[cases$CCAA=="Región de Murcia"],
                       reg=cases$cases[cases$CCAA=="Región de Murcia"], reg_est=(1-0.96680+0.96680*0.42987)*colMedians(as.matrix(MC)))
rmse_MC <- sqrt(mean((error_MC$reg-error_MC$reg_est)^2))
mape_MC <- mean(abs((error_MC$reg-error_MC$reg_est)/error_MC$reg)) * 100

### Figure 1
ggarrange(graph_AN, graph_AR, graph_CN, graph_CB, graph_CE,
            graph_CM, graph_CL, graph_CT, graph_NC,
            graph_VC, graph_EX, graph_GA, graph_IB,
            graph_RI, graph_MD, graph_PV, graph_ME,
            graph_AS, graph_MC,
            ncol = 4, nrow = 5)

### Global graph (Figure 2)
cases <- cases %>% group_by(Year, Week) %>% summarise(med=sum(cases))
ESP <- AN+AR+CN+CB+CM+CL+CT+NC+VC+EX+GA+IB+RI+MD+PV+AS+MC+ME+CE

q2.5 <- colQuantiles(as.matrix(ESP), probs=c(0.025))
med <- colMedians(as.matrix(ESP))
q97.5 <- colQuantiles(as.matrix(ESP), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week
resum$Value <- "Estimated"
resum$med[107:212] <- cases$med
resum$Value[107:212] <- "Registered"
resum$q2.5[107:212] <- resum$med[107:212]
resum$q97.5[107:212] <- resum$med[107:212]
resum$med2 <- c(resum$med[1:106], rep(NA, 106))
resum$Year <- cases$Year
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)

ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+theme(plot.title = element_text(hjust = 0.5), 
                                                                                                            axis.text.x = element_text(hjust = 1))+
  labs(col = "Value")+ggtitle("Covid-19 cases in Spain")

### Global values
est <- sum(resum$med[resum$Value=="Estimated"])
reg <- sum(resum$med[resum$Value=="Registered"])
reg/est*100
