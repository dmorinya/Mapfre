### Summarise simulation study results
library(readxl)
garch01 <- read_xls("Seminaris/Seminari 2 (IWSM)/Results/sim_GARCH01_5.xls")
garch01$m <- 0.2
garch01$beta <- 0.4

### GARCH(0, 1)
### Bias
garch01$phi0_bias   <- garch01$phi0-garch01$phi0_mean
garch01$ar1_bias    <- garch01$ar1-garch01$ar1_mean
garch01$inter_bias  <- garch01$intercept-garch01$intercept_mean
garch01$alpha1_bias <- garch01$alpha1-garch01$alpha1_mean
garch01$q_bias      <- garch01$q-garch01$q_mean
garch01$w_bias      <- garch01$w-garch01$w_mean
garch01$m_bias      <- garch01$m-garch01$m_mean
garch01$beta_bias   <- garch01$beta-garch01$beta_mean
garch01$se_bias     <- garch01$se-garch01$se_mean

round(mean(garch01$phi0_bias), 3)
round(mean(garch01$ar1_bias), 3)
round(mean(garch01$inter_bias), 3)
round(mean(garch01$alpha1_bias), 3)
round(mean(garch01$q_bias), 3)
round(mean(garch01$w_bias), 3)
round(mean(garch01$m_bias), 3)
round(mean(garch01$beta_bias), 3)
round(mean(garch01$se_bias), 3)

### AIL
garch01$phi0_AIL   <- garch01$phi0_p97.5-garch01$phi0_p2.5
garch01$ar1_AIL    <- garch01$ar1_p97.5-garch01$ar1_p2.5
garch01$inter_AIL  <- garch01$intercept_p97.5-garch01$intercept_p2.5
garch01$alpha1_AIL <- garch01$alpha1_p97.5-garch01$alpha1_p2.5
garch01$q_AIL      <- garch01$q_p97.5-garch01$q_p2.5
garch01$w_AIL      <- garch01$w_p97.5-garch01$w_p2.5
garch01$m_AIL      <- garch01$m_p97.5-garch01$m_p2.5
garch01$beta_AIL   <- garch01$beta_p97.5-garch01$beta_p2.5
garch01$se_AIL     <- garch01$se_p97.5-garch01$se_p2.5

round(mean(garch01$phi0_AIL, na.rm=T), 3)
round(mean(garch01$ar1_AIL, na.rm=T), 3)
round(mean(garch01$inter_AIL, na.rm=T), 3)
round(mean(garch01$alpha1_AIL, na.rm=T), 3)
round(mean(garch01$q_AIL, na.rm=T), 3)
round(mean(garch01$w_AIL, na.rm=T), 3)
round(mean(garch01$m_AIL, na.rm=T), 3)
round(mean(garch01$beta_AIL, na.rm=T), 3)
round(mean(garch01$se_AIL, na.rm=T), 3)

### Coverage
garch01$phi0_coverage   <- ifelse(garch01$phi0_p2.5<=garch01$phi0 & garch01$phi0_p97.5>=garch01$phi0, 1, 0)
garch01$ar1_coverage    <- ifelse(garch01$ar1_p2.5<=garch01$ar1 & garch01$ar1_p97.5>=garch01$ar1, 1, 0)
garch01$inter_coverage  <- ifelse(garch01$intercept_p2.5<=garch01$intercept & garch01$intercept_p97.5>=garch01$intercept, 1, 0)
garch01$alpha1_coverage <- ifelse(garch01$alpha1_p2.5<=garch01$alpha1 & garch01$alpha1_p97.5>=garch01$alpha1, 1, 0)
garch01$q_coverage      <- ifelse(garch01$q_p2.5<=garch01$q & garch01$q_p97.5>=garch01$q, 1, 0)
garch01$w_coverage      <- ifelse(garch01$w_p2.5<=garch01$w & garch01$w_p97.5>=garch01$w, 1, 0)
garch01$m_coverage      <- ifelse(garch01$m_p2.5<=garch01$m & garch01$m_p97.5>=garch01$m, 1, 0)
garch01$beta_coverage   <- ifelse(garch01$beta_p2.5<=garch01$beta & garch01$beta_p97.5>=garch01$beta, 1, 0)
garch01$sigma_coverage  <- ifelse(garch01$se_p2.5<=garch01$se & garch01$se_p97.5>=garch01$se, 1, 0)
round(table(garch01$phi0_coverage)[2]/length(garch01$phi0_coverage)*100, 2)
round(table(garch01$ar1_coverage)[2]/length(garch01$ar1_coverage)*100, 2)
round(table(garch01$inter_coverage)[2]/length(garch01$inter_coverage)*100, 2)
round(table(garch01$alpha1_coverage)[2]/length(garch01$alpha1_coverage)*100, 2)
round(table(garch01$q_coverage)[2]/length(garch01$q_coverage)*100, 2)
round(table(garch01$w_coverage)[2]/length(garch01$w_coverage)*100, 2)
round(table(garch01$m_coverage)[2]/length(garch01$m_coverage)*100, 2)
round(table(garch01$beta_coverage)[2]/length(garch01$beta_coverage)*100, 2)
round(table(garch01$sigma_coverage)[2]/length(garch01$sigma_coverage)*100, 2)
