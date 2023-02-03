library(matrixStats)
library(gdata)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(MMWRweek)

cases <- read.csv("https://cnecovid.isciii.es/covid19/resources/casos_diag_ccaadecl.csv")
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

pob <- read.xls("Data/poblacio.xls", encoding="latin1")
cases <- merge(cases, pob, by=c("CCAA"))
cases$incid <- cases$cases/cases$Pob*100000
cases <- cases[cases$Week>8 | cases$Year>2020, ]
cases <- cases[order(cases$CCAA, cases$Year, cases$Week), ]

AN <- read.table("Presentation/Results/Andalucía_incidence3.csv", header=T, sep=",")/100000*8414240
q2.5 <- colQuantiles(as.matrix(AN), probs=c(0.025))
med <- colMedians(as.matrix(AN))
q97.5 <- colQuantiles(as.matrix(AN), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Andalucía"]
resum$Value <- "Estimated"
resum$med[(dim(resum)[1]/2+1):dim(resum)[1]] <- cases$cases[cases$CCAA=="Andalucía"]
resum$Value[(dim(resum)[1]/2+1):dim(resum)[1]] <- "Registered"
resum$q2.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$q97.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$med2 <- c(resum$med[1:(dim(resum)[1]/2)], rep(NA, dim(resum)[1]/2))
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

AR <- read.table("Presentation/Results/Aragón_incidence3.csv", header=T, sep=",")/100000*1319291
q2.5 <- colQuantiles(as.matrix(AR), probs=c(0.025))
med <- colMedians(as.matrix(AR))
q97.5 <- colQuantiles(as.matrix(AR), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Aragón"]
resum$Value <- "Estimated"
resum$med[(dim(resum)[1]/2+1):dim(resum)[1]] <- cases$cases[cases$CCAA=="Aragón"]
resum$Value[(dim(resum)[1]/2+1):dim(resum)[1]] <- "Registered"
resum$q2.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$q97.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$med2 <- c(resum$med[1:(dim(resum)[1]/2)], rep(NA, dim(resum)[1]/2))
resum$Year <- cases$Year[cases$CCAA=="Aragón"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_AR <- resum %>% ggplot(aes(x=Date, y=med, group=Value, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Aragón") +
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")

CN <- read.table("Presentation/Results/Canarias_incidence3.csv", header=T, sep=",")/100000*2153389
q2.5 <- colQuantiles(as.matrix(CN), probs=c(0.025))
med <- colMedians(as.matrix(CN))
q97.5 <- colQuantiles(as.matrix(CN), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Canarias"]
resum$Value <- "Estimated"
resum$med[(dim(resum)[1]/2+1):dim(resum)[1]] <- cases$cases[cases$CCAA=="Canarias"]
resum$Value[(dim(resum)[1]/2+1):dim(resum)[1]] <- "Registered"
resum$q2.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$q97.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$med2 <- c(resum$med[1:(dim(resum)[1]/2)], rep(NA, dim(resum)[1]/2))
resum$Year <- cases$Year[cases$CCAA=="Canarias"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_CN <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Canarias") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")

CB <- read.table("Presentation/Results/Cantabria_incidence3.csv", header=T, sep=",")/100000*581078
q2.5 <- colQuantiles(as.matrix(CB), probs=c(0.025))
med <- colMedians(as.matrix(CB))
q97.5 <- colQuantiles(as.matrix(CB), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Cantabria"]
resum$Value <- "Estimated"
resum$med[(dim(resum)[1]/2+1):dim(resum)[1]] <- cases$cases[cases$CCAA=="Cantabria"]
resum$Value[(dim(resum)[1]/2+1):dim(resum)[1]] <- "Registered"
resum$q2.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$q97.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$med2 <- c(resum$med[1:(dim(resum)[1]/2)], rep(NA, dim(resum)[1]/2))
resum$Year <- cases$Year[cases$CCAA=="Cantabria"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_CB <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Cantabria") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")

CM <- read.table("Presentation/Results/Castilla - La Mancha_incidence3.csv", header=T, sep=",")/100000*2032863
q2.5 <- colQuantiles(as.matrix(CM), probs=c(0.025))
med <- colMedians(as.matrix(CM))
q97.5 <- colQuantiles(as.matrix(CM), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Castilla - La Mancha"]
resum$Value <- "Estimated"
resum$med[(dim(resum)[1]/2+1):dim(resum)[1]] <- cases$cases[cases$CCAA=="Castilla - La Mancha"]
resum$Value[(dim(resum)[1]/2+1):dim(resum)[1]] <- "Registered"
resum$q2.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$q97.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$med2 <- c(resum$med[1:(dim(resum)[1]/2)], rep(NA, dim(resum)[1]/2))
resum$Year <- cases$Year[cases$CCAA=="Castilla - La Mancha"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_CM <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Castilla - La Mancha") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")

CL <- read.table("Presentation/Results/Castilla y León_incidence3.csv", header=T, sep=",")/100000*2399548
q2.5 <- colQuantiles(as.matrix(CL), probs=c(0.025))
med <- colMedians(as.matrix(CL))
q97.5 <- colQuantiles(as.matrix(CL), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Castilla y León"]
resum$Value <- "Estimated"
resum$med[(dim(resum)[1]/2+1):dim(resum)[1]] <- cases$cases[cases$CCAA=="Castilla y León"]
resum$Value[(dim(resum)[1]/2+1):dim(resum)[1]] <- "Registered"
resum$q2.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$q97.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$med2 <- c(resum$med[1:(dim(resum)[1]/2)], rep(NA, dim(resum)[1]/2))
resum$Year <- cases$Year[cases$CCAA=="Castilla y León"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_CL <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Castilla y León") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")

CT <- read.table("Presentation/Results/Cataluña_incidence3.csv", header=T, sep=",")/100000*7675217
q2.5 <- colQuantiles(as.matrix(CT), probs=c(0.025))
med <- colMedians(as.matrix(CT))
q97.5 <- colQuantiles(as.matrix(CT), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Cataluña"]
resum$Value <- "Estimated"
resum$med[(dim(resum)[1]/2+1):dim(resum)[1]] <- cases$cases[cases$CCAA=="Cataluña"]
resum$Value[(dim(resum)[1]/2+1):dim(resum)[1]] <- "Registered"
resum$q2.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$q97.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$med2 <- c(resum$med[1:(dim(resum)[1]/2)], rep(NA, dim(resum)[1]/2))
resum$Year <- cases$Year[cases$CCAA=="Cataluña"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_CT <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Catalunya") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")

CE <- read.table("Presentation/Results/Ceuta_incidence3.csv", header=T, sep=",")/100000*84777
q2.5 <- colQuantiles(as.matrix(CE), probs=c(0.025))
med <- colMedians(as.matrix(CE))
q97.5 <- colQuantiles(as.matrix(CE), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Ceuta"]
resum$Value <- "Estimated"
resum$med[(dim(resum)[1]/2+1):dim(resum)[1]] <- cases$cases[cases$CCAA=="Ceuta"]
resum$Value[(dim(resum)[1]/2+1):dim(resum)[1]] <- "Registered"
resum$q2.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$q97.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$med2 <- c(resum$med[1:(dim(resum)[1]/2)], rep(NA, dim(resum)[1]/2))
resum$Year <- cases$Year[cases$CCAA=="Ceuta"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_CE <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Ceuta") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")

NC <- read.table("Presentation/Results/Comunidad Foral de Navarra_incidence3.csv", header=T, sep=",")/100000*654214
q2.5 <- colQuantiles(as.matrix(NC), probs=c(0.025))
med <- colMedians(as.matrix(NC))
q97.5 <- colQuantiles(as.matrix(NC), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Comunidad Foral de Navarra"]
resum$Value <- "Estimated"
resum$med[(dim(resum)[1]/2+1):dim(resum)[1]] <- cases$cases[cases$CCAA=="Comunidad Foral de Navarra"]
resum$Value[(dim(resum)[1]/2+1):dim(resum)[1]] <- "Registered"
resum$q2.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$q97.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$med2 <- c(resum$med[1:(dim(resum)[1]/2)], rep(NA, dim(resum)[1]/2))
resum$Year <- cases$Year[cases$CCAA=="Comunidad Foral de Navarra"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_NC <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Nafarroa") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")

VC <- read.table("Presentation/Results/Comunidad Valenciana_incidence3.csv", header=T, sep=",")/100000*5003769
q2.5 <- colQuantiles(as.matrix(VC), probs=c(0.025))
med <- colMedians(as.matrix(VC))
q97.5 <- colQuantiles(as.matrix(VC), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Comunidad Valenciana"]
resum$Value <- "Estimated"
resum$med[(dim(resum)[1]/2+1):dim(resum)[1]] <- cases$cases[cases$CCAA=="Comunidad Valenciana"]
resum$Value[(dim(resum)[1]/2+1):dim(resum)[1]] <- "Registered"
resum$q2.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$q97.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$med2 <- c(resum$med[1:(dim(resum)[1]/2)], rep(NA, dim(resum)[1]/2))
resum$Year <- cases$Year[cases$CCAA=="Comunidad Valenciana"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_VC <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("País Valencià") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")

EX <- read.table("Presentation/Results/Extremadura_incidence3.csv", header=T, sep=",")/100000*1067710
q2.5 <- colQuantiles(as.matrix(EX), probs=c(0.025))
med <- colMedians(as.matrix(EX))
q97.5 <- colQuantiles(as.matrix(EX), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Extremadura"]
resum$Value <- "Estimated"
resum$med[(dim(resum)[1]/2+1):dim(resum)[1]] <- cases$cases[cases$CCAA=="Extremadura"]
resum$Value[(dim(resum)[1]/2+1):dim(resum)[1]] <- "Registered"
resum$q2.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$q97.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$med2 <- c(resum$med[1:(dim(resum)[1]/2)], rep(NA, dim(resum)[1]/2))
resum$Year <- cases$Year[cases$CCAA=="Extremadura"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_EX <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Extremadura") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")

GA <- read.table("Presentation/Results/Galicia_incidence3.csv", header=T, sep=",")/100000*2699499
q2.5 <- colQuantiles(as.matrix(GA), probs=c(0.025))
med <- colMedians(as.matrix(GA))
q97.5 <- colQuantiles(as.matrix(GA), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Galicia"]
resum$Value <- "Estimated"
resum$med[(dim(resum)[1]/2+1):dim(resum)[1]] <- cases$cases[cases$CCAA=="Galicia"]
resum$Value[(dim(resum)[1]/2+1):dim(resum)[1]] <- "Registered"
resum$q2.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$q97.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$med2 <- c(resum$med[1:(dim(resum)[1]/2)], rep(NA, dim(resum)[1]/2))
resum$Year <- cases$Year[cases$CCAA=="Galicia"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_GA <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Galiza") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")

IB <- read.table("Presentation/Results/Islas Baleares_incidence3.csv", header=T, sep=",")/100000*1149460
q2.5 <- colQuantiles(as.matrix(IB), probs=c(0.025))
med <- colMedians(as.matrix(IB))
q97.5 <- colQuantiles(as.matrix(IB), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Islas Baleares"]
resum$Value <- "Estimated"
resum$med[(dim(resum)[1]/2+1):dim(resum)[1]] <- cases$cases[cases$CCAA=="Islas Baleares"]
resum$Value[(dim(resum)[1]/2+1):dim(resum)[1]] <- "Registered"
resum$q2.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$q97.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$med2 <- c(resum$med[1:(dim(resum)[1]/2)], rep(NA, dim(resum)[1]/2))
resum$Year <- cases$Year[cases$CCAA=="Islas Baleares"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_IB <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Illes Balears") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")

RI <- read.table("Presentation/Results/La Rioja_incidence3.csv", header=T, sep=",")/100000*316798
q2.5 <- colQuantiles(as.matrix(RI), probs=c(0.025))
med <- colMedians(as.matrix(RI))
q97.5 <- colQuantiles(as.matrix(RI), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="La Rioja"]
resum$Value <- "Estimated"
resum$med[(dim(resum)[1]/2+1):dim(resum)[1]] <- cases$cases[cases$CCAA=="La Rioja"]
resum$Value[(dim(resum)[1]/2+1):dim(resum)[1]] <- "Registered"
resum$q2.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$q97.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$med2 <- c(resum$med[1:(dim(resum)[1]/2)], rep(NA, dim(resum)[1]/2))
resum$Year <- cases$Year[cases$CCAA=="La Rioja"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_RI <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("La Rioja") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")

MD <- read.table("Presentation/Results/Madrid_incidence3.csv", header=T, sep=",")/100000*6663394
q2.5 <- colQuantiles(as.matrix(MD), probs=c(0.025))
med <- colMedians(as.matrix(MD))
q97.5 <- colQuantiles(as.matrix(MD), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Madrid"]
resum$Value <- "Estimated"
resum$med[(dim(resum)[1]/2+1):dim(resum)[1]] <- cases$cases[cases$CCAA=="Madrid"]
resum$Value[(dim(resum)[1]/2+1):dim(resum)[1]] <- "Registered"
resum$q2.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$q97.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$med2 <- c(resum$med[1:(dim(resum)[1]/2)], rep(NA, dim(resum)[1]/2))
resum$Year <- cases$Year[cases$CCAA=="Madrid"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_MD <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Madrid") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")

ME <- read.table("Presentation/Results/Melilla_incidence3.csv", header=T, sep=",")/100000*86487
q2.5 <- colQuantiles(as.matrix(ME), probs=c(0.025))
med <- colMedians(as.matrix(ME))
q97.5 <- colQuantiles(as.matrix(ME), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Melilla"]
resum$Value <- "Estimated"
resum$med[(dim(resum)[1]/2+1):dim(resum)[1]] <- cases$cases[cases$CCAA=="Melilla"]
resum$Value[(dim(resum)[1]/2+1):dim(resum)[1]] <- "Registered"
resum$q2.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$q97.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$med2 <- c(resum$med[1:(dim(resum)[1]/2)], rep(NA, dim(resum)[1]/2))
resum$Year <- cases$Year[cases$CCAA=="Melilla"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_ME <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Melilla") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")

PV <- read.table("Presentation/Results/País Vasco_incidence3.csv", header=T, sep=",")/100000*2207776
q2.5 <- colQuantiles(as.matrix(PV), probs=c(0.025))
med <- colMedians(as.matrix(PV))
q97.5 <- colQuantiles(as.matrix(PV), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="País Vasco"]
resum$Value <- "Estimated"
resum$med[(dim(resum)[1]/2+1):dim(resum)[1]] <- cases$cases[cases$CCAA=="País Vasco"]
resum$Value[(dim(resum)[1]/2+1):dim(resum)[1]] <- "Registered"
resum$q2.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$q97.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$med2 <- c(resum$med[1:(dim(resum)[1]/2)], rep(NA, dim(resum)[1]/2))
resum$Year <- cases$Year[cases$CCAA=="País Vasco"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_PV <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Euskadi") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")

AS <- read.table("Presentation/Results/Principado de Asturias_incidence3.csv", header=T, sep=",")/100000*1022800
q2.5 <- colQuantiles(as.matrix(AS), probs=c(0.025))
med <- colMedians(as.matrix(AS))
q97.5 <- colQuantiles(as.matrix(AS), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Principado de Asturias"]
resum$Value <- "Estimated"
resum$med[(dim(resum)[1]/2+1):dim(resum)[1]] <- cases$cases[cases$CCAA=="Principado de Asturias"]
resum$Value[(dim(resum)[1]/2+1):dim(resum)[1]] <- "Registered"
resum$q2.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$q97.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$med2 <- c(resum$med[1:(dim(resum)[1]/2)], rep(NA, dim(resum)[1]/2))
resum$Year <- cases$Year[cases$CCAA=="Principado de Asturias"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_AS <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Asturies") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")

MC <- read.table("Presentation/Results/Región de Murcia_incidence3.csv", header=T, sep=",")/100000*1493898
q2.5 <- colQuantiles(as.matrix(MC), probs=c(0.025))
med <- colMedians(as.matrix(MC))
q97.5 <- colQuantiles(as.matrix(MC), probs=c(0.975))
resum <- data.frame(q2.5=c(q2.5, rep(NA, length(q2.5))), med=c(med, rep(NA, length(med))),
                    q97.5=c(q97.5, rep(NA, length(q97.5))))
resum$Week <- cases$Week[cases$CCAA=="Región de Murcia"]
resum$Value <- "Estimated"
resum$med[(dim(resum)[1]/2+1):dim(resum)[1]] <- cases$cases[cases$CCAA=="Región de Murcia"]
resum$Value[(dim(resum)[1]/2+1):dim(resum)[1]] <- "Registered"
resum$q2.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$q97.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$med2 <- c(resum$med[1:(dim(resum)[1]/2)], rep(NA, dim(resum)[1]/2))
resum$Year <- cases$Year[cases$CCAA=="Región de Murcia"]
resum$Date <- MMWRweek2Date(resum$Year, resum$Week)
round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)
graph_MC <- ggplot(data=resum, aes(x=Date, y=med, col=Value)) +
  geom_line()+xlab("Date")+ylab("New cases")+geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "lightgrey", show.legend=F)+
  labs(col = "Value")+ggtitle("Región de Murcia") + 
  geom_line(data=resum[!is.na(resum$med2),], aes(x=Date, y=med2), color = "red", linetype = "dotted")+
  theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"), axis.text.x = element_text(hjust = 1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+  labs(col = "Value")

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
resum$med[(dim(resum)[1]/2+1):dim(resum)[1]] <- cases$med
resum$Value[(dim(resum)[1]/2+1):dim(resum)[1]] <- "Registered"
resum$q2.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$q97.5[(dim(resum)[1]/2+1):dim(resum)[1]] <- resum$med[(dim(resum)[1]/2+1):dim(resum)[1]]
resum$med2 <- c(resum$med[1:(dim(resum)[1]/2)], rep(NA, dim(resum)[1]/2))
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
