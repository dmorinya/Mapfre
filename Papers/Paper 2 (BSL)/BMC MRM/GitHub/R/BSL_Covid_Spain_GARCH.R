setwd("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/Paper 2 (BSL)/BMC MRM/GitHub/")
sink(file="Results/output.txt", append=TRUE)
library(BSL)
library(ggplot2)
library(parallel)
library(doParallel)
library(readxl)
library(ggpubr)
library(dplyr)
library(lubridate)
library(forecast)
library(MMWRweek)
library(mgcv)
library(robustbase)
library(mixtools)
library(tseries)
library(quantspec)

source("R/Estep.R")
source("R/Mstep.R")
source("R/EM.R")

ncores <- detectCores()
cl <- makeCluster(ncores-2)
registerDoParallel(cl)

logPrior <- function(theta)
{
  log(theta[1] > 0 & theta[2] > 0 & theta[2] < 1 & theta[5] > 0 & theta[5] < 1 &
        theta[3] > 0 & theta[4] > 0 & theta[4] < 2 & theta[6] > 0 &
        theta[7] > 0 & theta[7] < 1)
}

sim <- function(theta, T, x1, x2)
{
  library(TSA)
  library(quantspec)
  mu <- vector()
  error <- vector()
  error[1] <- 0
  for (i in 1:T)
  {
    mu[i] <- (exp(log(exp(theta[8]+theta[9]*x1[i]+theta[10]*x2[i])*exp(theta[11]*i))-
                    log(exp(theta[8]+theta[9]*x1[i]+theta[10]*x2[i])+(exp(theta[11]*i)-1)))-1)
  }
  error <- ARCH1(T, theta[3], theta[4])
  x <- theta[1]+arima.sim(model=list(order=c(1,0,0), ar=theta[2]), 
                          rand.gen=function(n, ...) rnorm(n, mean=mu, sd=theta[6]), n=T)+error
  z  <- rbinom(T, 1, theta[5])
  q <- theta[7]
  y  <- x*(1-z)+q*z*x
  return(y)
}

st <- function(z){ 
  s1=mean(z); s2=sd(z); s3=acf(z,plot=F)$acf[2] 
  s4=acf(z,plot=F)$acf[3]; s5=acf(z,plot=F)$acf[4]
  c(s1,s2,s3,s4,s5)}

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
cases$CCAA[cases$CCAA2=="AN"] <- "Andaluc??a"
cases$CCAA[cases$CCAA2=="AR"] <- "Arag??n"
cases$CCAA[cases$CCAA2=="AS"] <- "Principado de Asturias"
cases$CCAA[cases$CCAA2=="CB"] <- "Cantabria"
cases$CCAA[cases$CCAA2=="CE"] <- "Ceuta"
cases$CCAA[cases$CCAA2=="CL"] <- "Castilla y Le??n"
cases$CCAA[cases$CCAA2=="CM"] <- "Castilla - La Mancha"
cases$CCAA[cases$CCAA2=="CN"] <- "Canarias"
cases$CCAA[cases$CCAA2=="CT"] <- "Catalu??a"
cases$CCAA[cases$CCAA2=="EX"] <- "Extremadura"
cases$CCAA[cases$CCAA2=="GA"] <- "Galicia"
cases$CCAA[cases$CCAA2=="IB"] <- "Islas Baleares"
cases$CCAA[cases$CCAA2=="MC"] <- "Regi??n de Murcia"
cases$CCAA[cases$CCAA2=="MD"] <- "Madrid"
cases$CCAA[cases$CCAA2=="ML"] <- "Melilla"
cases$CCAA[cases$CCAA2=="NC"] <- "Comunidad Foral de Navarra"
cases$CCAA[cases$CCAA2=="PV"] <- "Pa??s Vasco"
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

cases$Date <- MMWRweek2Date(cases$Year, cases$Week)
pob <- read_xls("Data/poblacio.xls")
cases <- merge(cases, pob, by=c("CCAA"))
cases$incid <- cases$cases/cases$Pob*100000
cases <- cases[cases$Week>8 | cases$Year>2020, ]
cases <- cases[order(cases$CCAA, cases$Year, cases$Week), ]
#### Add dummy for mandatory confinement
cases$Conf <- 0
cases$Conf[cases$Year==2020 & cases$Week >= 11 & cases$Week <= 18] <- 1

#### Add dummy for vaccination (value 1 the day after a 50% of the population has 1 dose at least)
cases$Vacc <- 0
cases$Vacc[cases$Date >= "2021-06-22"] <- 1

#### The model has to be defined and ran for every CCAA (with the same initial values as defined below)
for (i in 1:length(table(cases$CCAA)))
{
  CA <- names(table(cases$CCAA))[i]
  print(CA)
  addTaskCallback(function(...) {set.seed(123);TRUE})
  init <- normalmixEM(cases$incid[cases$CCAA==CA], maxit=10000)
  q_init <- init$mu[init$mu==min(init$mu)]/init$mu[init$mu==max(init$mu)]
  init_sigma <- init$sigma[init$sigma==max(init$sigma)]
  ind <- ifelse(init$posterior[, init$mu==min(init$mu)]<0.5, 0, 1)
  x <- ifelse(ind==1, cases$incid[cases$CCAA==CA]/q_init, cases$incid[cases$CCAA==CA])
  pr <- tryCatch(arima(x, order=c(1,0,0)))
  init_ar1  <- tryCatch((garch(!is.na(pr$resid), order=c(0, 1))$coef))
  while(is(init_ar1, "warning") | is(pr, "warning"))
  {
    addTaskCallback(function(...) {set.seed(123);TRUE})
    init <- normalmixEM(cases$incid[cases$CCAA==CA], maxit=10000)
    q_init <- init$mu[init$mu==min(init$mu)]/init$mu[init$mu==max(init$mu)]
    init_sigma <- init$sigma[init$sigma==max(init$sigma)]
    ind <- ifelse(init$posterior[, init$mu==min(init$mu)]<0.5, 0, 1)
    x <- ifelse(ind==1, cases$incid[cases$CCAA==CA]/q_init, cases$incid[cases$CCAA==CA])
    pr <- arima(x, order=c(1,0,0))
    init_ar1  <- tryCatch((garch(!is.na(pr$resid), order=c(0, 1))$coef))
  }
  init_vals <- c(mean(x), pr$coef[1], init_ar1, init$lambda[init$mu==min(init$mu)],
                 init_sigma, 0.9, 7.2, 0.3, 0.3, 0.3)
  library(quantspec)
  model <- newModel(fnSim = sim, fnSum = st,
                    simArgs = list(T = length(cases$Conf[cases$CCAA==CA]), x1=cases$Conf[cases$CCAA==CA],
                    x2=cases$Vacc[cases$CCAA==CA]), theta0 = init_vals,
                    fnLogPrior = logPrior, thetaNames=c(expression(phi[0]), expression(alpha[1]), "intercept", 
                                                        expression(gamma[1]), expression(omega),  expression(sigma),
                                                        "q", "m", expression(beta[0]), expression(beta[1]), expression(beta[2])))
  resultCovid <- bsl(y = cases$incid[cases$CCAA==CA], n = 500, M = 5000, model = model, # M = 50000
                     diag(c(.01,.005,.005,.005,.005,.005,.005,.005,.005,.01,.01)),
                     method = 'BSL', parallel=TRUE, verbose = TRUE)
  
  ### Parameter estimates and 95% credible intervals
  print(show(resultCovid))
  
  ### Parameter estimates distribution
  est_distr <- paste0("Results/est_distr_", CA, ".pdf")
  pdf(est_distr, width=8.5, height=6.5)
  plot(resultCovid, which = 2, thin = 30,
       options.density = list(color = 'blue4', fill = 'blue', alpha = 0.5),
       options.theme = list(panel.background = element_rect(fill = 'lightgrey'),
                            plot.margin = grid::unit(rep(0.05, 4), "npc")))
  dev.off()
  
  ### Hidden process reconstruction
  x1 <- cases$Conf[cases$CCAA==CA]
  x2 <- cases$Vacc[cases$CCAA==CA]
  
  posterior_probs <- lapply(resultCovid@theta[, 7], FUN=function(x){EM(cases$incid[cases$CCAA==CA],2,1e-16,x)})
  ur_indicator    <- lapply(posterior_probs, FUN=function(x){ifelse(x$gamma[, 1] > x$gamma[, 2], 1, 0)})
  q <- resultCovid@theta[, 7]
  rec_values <- list()
  for (j in 1:length(ur_indicator))
  {
    den <- q[j]
    rec_values[[j]] <- ifelse(ur_indicator[[j]]==0, cases$incid[cases$CCAA==CA], 
                              cases$incid[cases$CCAA==CA]/den)
  }
  
  pr <- as.data.frame(do.call(rbind, rec_values))
  
  ### Save the data
  name_data <- paste0("Results/", CA, "_incidence3.csv")
  write.csv(pr, name_data, row.names=FALSE)
}

sink()