library(WriteXLS)
library(BSL)
library(ggplot2)
library(doParallel)
library(readxl)
library(tseries)
library(quantspec)
source("R/Estep.R")
source("R/Mstep.R")
source("R/EM.R")

ncores <- detectCores()
cl <- makeCluster(ncores)
registerDoParallel(cl)

logPrior <- function(theta)
{
  log(theta[1] > 0 & theta[2] > 0 & theta[2] < 1 & theta[5] > 0 & theta[5] < 1 &
        theta[3] > 0 & theta[4] > 0 & theta[7] > 0 &
        theta[6] > 0 & theta[6] < 1)
}

sim <- function(theta, T)
{
  library(TSA)
  library(quantspec)
  mu <- vector()
  error <- vector()
  error[1] <- 0
  for (i in 1:T)
  {
    mu[i] <- (exp(log(exp(theta[8])*exp(theta[9]*i))-log(exp(theta[8])+(exp(theta[9]*i)-1)))-1)
  }
  error <- ARCH1(T, theta[3], theta[4])
  x <- theta[1]+arima.sim(model=list(order=c(1,0,0), ar=theta[2]), 
                          rand.gen=function(n, ...) rnorm(n, mean=mu, sd=theta[7]), n=T)+error
  #x  <- garch.sim(alpha=c(theta[1], theta[2]), beta=c(theta[3]), n=T, rnd=function(n, ...) rnorm(n, mean=mu, sd=theta[8]))
  z  <- rbinom(T, 1, theta[5])
  y  <- x*(1-z)+theta[6]*z*x
  return(y)
}

st <- function(z){ 
  s1=mean(z); s2=sd(z); s3=acf(z,plot=F)$acf[2] 
  s4=acf(z,plot=F)$acf[3]; s5=acf(z,plot=F)$acf[4]
  c(s1,s2,s3,s4,s5)}

intercept <- round(seq(0.1, 0.9, 0.2), 1); q <- round(seq(0.1, 0.9, 0.2), 1); 
w <- round(seq(0.1, 0.9, 0.2), 1); phi0 <- 5; se <- 1;
alpha1 <- round(seq(0.1, 0.9, 0.2), 1); ar1 <- round(seq(0.1, 0.9, 0.2), 1)
m <- 0.2; beta <- 0.4
res <- data.frame(expand.grid(phi0=phi0, ar1=ar1, intercept=intercept, alpha1=alpha1, 
                              w=w, q=q, se=se))

genEsts <- function(i)
{
  library(mixtools)
  library(BSL)
  library(TSA)
  library(tseries)
  print(paste0("Simulation step ", i, " out of ", dim(res)[1]))
  theta=c(res[i, 1], res[i, 2], res[i, 3], res[i, 4], res[i, 5], res[i, 6], res[i, 7])
  yy=sim(c(theta[1], theta[2], theta[3], theta[4], theta[5], theta[6], theta[7], m, beta), 1000)
  ### Initial values
  init <- normalmixEM(yy, maxit=10000)
  q_init <- init$mu[init$mu==min(init$mu)]/init$mu[init$mu==max(init$mu)]
  init_sigma <- init$sigma[init$sigma==max(init$sigma)]
  ind <- ifelse(init$posterior[, init$mu==min(init$mu)]<0.5, 0, 1)
  x <- ifelse(ind==1, yy/q_init, yy)
  pr <- arima(x, order=c(1,0,0))
  pr <- tryCatch(arima(x, order=c(1,0,0)))
  init_ar1  <- tryCatch((garch(!is.na(pr$resid), order=c(0, 1))$coef))
  while(is(init_ar1, "warning") | is(pr, "warning"))
  {
    addTaskCallback(function(...) {set.seed(123);TRUE})
    init <- normalmixEM(yy, maxit=10000)
    q_init <- init$mu[init$mu==min(init$mu)]/init$mu[init$mu==max(init$mu)]
    init_sigma <- init$sigma[init$sigma==max(init$sigma)]
    ind <- ifelse(init$posterior[, init$mu==min(init$mu)]<0.5, 0, 1)
    x <- ifelse(ind==1, yy/q_init, yy)
    pr <- arima(x, order=c(1,0,0))
    pr <- tryCatch(arima(x, order=c(1,0,0)))
    init_ar1  <- tryCatch((garch(!is.na(pr$resid), order=c(0, 1))$coef))
  }
  init_vals <- c(mean(x), pr$coef[1], init_ar1, init$lambda[init$mu==min(init$mu)],
                 q_init, init_sigma, 0.2, 0.4)
  library(quantspec)
  model <- newModel(fnSim = sim, fnSum = st,
                    simArgs = list(T = 100), 
                    theta0 = init_vals,
                    fnLogPrior = logPrior, thetaNames=c(expression(phi[0]), "ar1", "intercept", expression(alpha[1]),
                                                        expression(omega), "q", expression(sigma), "m", expression(beta)))
  
  resultAr1 <- try(bsl(y = yy, n = 500, M = 500, model = model,
                   covRandWalk = diag(c(.1^2,.05^2,.05^2,.05^2,.005^2,.005^2,.05^2,.01^2,.01^2)),
                   method = 'BSL', parallel=FALSE, verbose = FALSE))
  while(class(resultAr1)=="try-error")
  {
    resultAr1 <- try(bsl(y = yy, n = 500, M = 500, model = model,
                         covRandWalk = diag(c(.1^2,.05^2,.05^2,.05^2,.005^2,.005^2,.05^2,.01^2,.01^2)),
                         method = 'BSL', parallel=FALSE, verbose = FALSE))
  }
  # Keep the results
  phi0_p50 <- median(resultAr1@theta[, 1])
  phi0_p2.5 <- quantile(resultAr1@theta[, 1], 0.025)
  phi0_p97.5 <- quantile(resultAr1@theta[, 1], 0.975)
  phi0_mean <- mean(resultAr1@theta[, 1])
  phi0_sd   <- sd(resultAr1@theta[, 1])
  ar1_p50  <- median(resultAr1@theta[, 2])
  ar1_p2.5 <- quantile(resultAr1@theta[, 2], 0.025)
  ar1_p97.5 <- quantile(resultAr1@theta[, 2], 0.975)
  ar1_mean <- mean(resultAr1@theta[, 2])
  ar1_sd   <- sd(resultAr1@theta[, 2])
  intercept_p50  <- median(resultAr1@theta[, 3])
  intercept_p2.5 <- quantile(resultAr1@theta[, 3], 0.025)
  intercept_p97.5 <- quantile(resultAr1@theta[, 3], 0.975)
  intercept_mean <- mean(resultAr1@theta[, 3])
  intercept_sd   <- sd(resultAr1@theta[, 3])
  alpha1_p50  <- median(resultAr1@theta[, 4])
  alpha1_p2.5 <- quantile(resultAr1@theta[, 4], 0.025)
  alpha1_p97.5 <- quantile(resultAr1@theta[, 4], 0.975)
  alpha1_mean <- mean(resultAr1@theta[, 4])
  alpha1_sd   <- sd(resultAr1@theta[, 4])
  w_p50    <- median(resultAr1@theta[, 5])
  w_p2.5 <- quantile(resultAr1@theta[, 5], 0.025)
  w_p97.5 <- quantile(resultAr1@theta[, 5], 0.975)
  w_mean <- mean(resultAr1@theta[, 5])
  w_sd   <- sd(resultAr1@theta[, 5])
  q_p50    <- median(resultAr1@theta[, 6])
  q_p2.5 <- quantile(resultAr1@theta[, 6], 0.025)
  q_p97.5 <- quantile(resultAr1@theta[, 6], 0.975)
  q_mean <- mean(resultAr1@theta[, 6])
  q_sd   <- sd(resultAr1@theta[, 6])
  se_p50   <- median(resultAr1@theta[, 7])
  se_p2.5 <- quantile(resultAr1@theta[, 7], 0.025)
  se_p97.5 <- quantile(resultAr1@theta[, 7], 0.975)
  se_mean <- mean(resultAr1@theta[, 7])
  se_sd   <- sd(resultAr1@theta[, 7])
  m_p50   <- median(resultAr1@theta[, 8])
  m_p2.5 <- quantile(resultAr1@theta[, 8], 0.025)
  m_p97.5 <- quantile(resultAr1@theta[, 8], 0.975)
  m_mean <- mean(resultAr1@theta[, 8])
  m_sd   <- sd(resultAr1@theta[, 8])
  beta_p50   <- median(resultAr1@theta[, 9])
  beta_p2.5 <- quantile(resultAr1@theta[, 9], 0.025)
  beta_p97.5 <- quantile(resultAr1@theta[, 9], 0.975)
  beta_mean <- mean(resultAr1@theta[, 9])
  beta_sd   <- sd(resultAr1@theta[, 9])
  return(c(res$phi0[i], res$ar1[i], res$intercept[i], res$alpha1[i], res$w[i], res$q[i], res$se[i], 
           phi0_p50, phi0_p2.5, phi0_p97.5, phi0_mean, phi0_sd,
           ar1_p50, ar1_p2.5, ar1_p97.5, ar1_mean, ar1_sd,
           intercept_p50, intercept_p2.5, intercept_p97.5, intercept_mean, intercept_sd,
           alpha1_p50, alpha1_p2.5, alpha1_p97.5, alpha1_mean, alpha1_sd,
           w_p50, w_p2.5, w_p97.5, w_mean, w_sd, 
           q_p50, q_p2.5, q_p97.5, q_mean, q_sd, 
           se_p50, se_p2.5, se_p97.5, se_mean, se_sd,
           m_p50, m_p2.5, m_p97.5, m_mean, m_sd,
           beta_p50, beta_p2.5, beta_p97.5, beta_mean, beta_sd))
}

dat.fin <- foreach(k=1:100, .combine=rbind) %do% genEsts(k)
stopCluster(cl)
colnames(dat.fin) <- c("phi0", "ar1", "intercept", "alpha1", "w", "q", "se", 
                       "phi0_p50", "phi0_p2.5", "phi0_p97.5", "phi0_mean", "phi0_sd", 
                       "ar1_p50", "ar1_p2.5", "ar1_p97.5", "ar1_mean", "ar1_sd",
                       "intercept_p50", "intercept_p2.5", "intercept_p97.5", "intercept_mean", "intercept_sd",
                       "alpha1_p50", "alpha1_p2.5", "alpha1_p97.5", "alpha1_mean", "alpha1_sd",
                       "w_p50", "w_p2.5", "w_p97.5", "w_mean", "w_sd", 
                       "q_p50", "q_p2.5", "q_p97.5", "q_mean", "q_sd", 
                       "se_p50", "se_p2.5", "se_p97.5", "se_mean", "se_sd",
                       "m_p50", "m_p2.5", "m_p97.5", "m_mean", "m_sd",
                       "beta_p50", "beta_p2.5", "beta_p97.5", "beta_mean", "beta_sd")

### Excel exportation
WriteXLS(as.data.frame(dat.fin), "Results/sim_GARCH01_1.xls")
