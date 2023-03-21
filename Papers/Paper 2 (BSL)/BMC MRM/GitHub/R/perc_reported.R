library(matrixStats)
library(readxl)
library(MMWRweek)
library(lubridate)
library(ggplot2)
library(tidyverse)
setwd("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/Paper 2 (BSL)/BMC MRM/GitHub/")
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

pob <- read_xls("Data/poblacio.xls")
cases <- merge(cases, pob, by=c("CCAA"))
cases$incid <- cases$cases/cases$Pob*100000
cases <- cases[cases$Week>8 | cases$Year>2020, ]
cases <- cases[order(cases$CCAA, cases$Year, cases$Week), ]
cases$Date <- MMWRweek2Date(cases$Year, cases$Week)

perc_reported <- vector()
AN <- read.table("Results/Andalucía_incidence3.csv", header=T, sep=",")/100000*8414240
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
perc_reported[1] <- round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)

AR <- read.table("Results/Aragón_incidence3.csv", header=T, sep=",")/100000*1319291
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
perc_reported[2] <- round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)

CN <- read.table("Results/Canarias_incidence3.csv", header=T, sep=",")/100000*2153389
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
perc_reported[3] <- round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)

CB <- read.table("Results/Cantabria_incidence3.csv", header=T, sep=",")/100000*581078
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
perc_reported[4] <- round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)

CM <- read.table("Results/Castilla - La Mancha_incidence3.csv", header=T, sep=",")/100000*2032863
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
perc_reported[5] <- round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)

CL <- read.table("Results/Castilla y León_incidence3.csv", header=T, sep=",")/100000*2399548
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
perc_reported[6] <- round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)

CT <- read.table("Results/Cataluña_incidence3.csv", header=T, sep=",")/100000*7675217
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
perc_reported[7] <- round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)

CE <- read.table("Results/Ceuta_incidence3.csv", header=T, sep=",")/100000*84777
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
perc_reported[8] <- round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)

NC <- read.table("Results/Comunidad Foral de Navarra_incidence3.csv", header=T, sep=",")/100000*654214
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
perc_reported[9] <- round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)

VC <- read.table("Results/Comunidad Valenciana_incidence3.csv", header=T, sep=",")/100000*5003769
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
perc_reported[10] <- round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)

EX <- read.table("Results/Extremadura_incidence3.csv", header=T, sep=",")/100000*1067710
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
perc_reported[11] <- round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)

GA <- read.table("Results/Galicia_incidence3.csv", header=T, sep=",")/100000*2699499
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
perc_reported[12] <- round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)

IB <- read.table("Results/Islas Baleares_incidence3.csv", header=T, sep=",")/100000*1149460
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
perc_reported[13] <- round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)

RI <- read.table("Results/La Rioja_incidence3.csv", header=T, sep=",")/100000*316798
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
perc_reported[14] <- round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)

MD <- read.table("Results/Madrid_incidence3.csv", header=T, sep=",")/100000*6663394
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
perc_reported[15] <- round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)

ME <- read.table("Results/Melilla_incidence3.csv", header=T, sep=",")/100000*86487
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
perc_reported[16] <- round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)

PV <- read.table("Results/País Vasco_incidence3.csv", header=T, sep=",")/100000*2207776
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
perc_reported[17] <- round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)

AS <- read.table("Results/Principado de Asturias_incidence3.csv", header=T, sep=",")/100000*1022800
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
perc_reported[18] <- round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)

MC <- read.table("Results/Región de Murcia_incidence3.csv", header=T, sep=",")/100000*1493898
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
perc_reported[19] <- round(sum(resum$med[resum$Value=="Registered"])/sum(resum$med[resum$Value=="Estimated"])*100)

data_plot <- data.frame(CCAA=c("Andalucía", "Aragón", "Canarias", "Cantabria", "Castilla - La Mancha",
                                "Castilla y León", "Catalunya", "Ceuta", "Nafarroa", "País Valencià",
                                "Extremadura", "Galiza", "Illes Balears", "La Rioja", "Madrid", "Melilla",
                                "Euskadi", "Asturies", "Región de Murcia"), Value=perc_reported)

data_plot %>% ggplot(aes(x=CCAA, y=perc_reported)) + ylim(c(40, 70)) +
  geom_point()+xlab("Region")+ylab("Percentage of reported cases") + theme(axis.text.x = element_text(angle = 25))