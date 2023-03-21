### UNDERREPORTING

### Case 1: w=0.7 and moderate underreporting (phi1=0.2; phi2=0.3) 
set.seed(1234)
setwd("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/Paper 3 (model)")
load("Results/Sim/case1.RData")

estimates <- data.frame(alpha=rep(NA, 600), lambda=rep(NA, 600), 
                        omega=rep(NA, 600), phi1=rep(NA, 600), phi2=rep(NA, 600))
for (i in 1801:2400)
{
  if (length(result[[i]])==6)
  {
    estimates[i-1800,] <- result[[i]]$estimate
  }else{
    estimates[i-1800,] <- rep(NA, 5)
  }
}

estimates <- estimates[!is.na(estimates$alpha), ]

idx <- sample(1:dim(estimates)[1], 500)
estimates <- estimates[idx, ]

lower <- data.frame(alpha=rep(NA, 600), lambda=rep(NA, 600), 
                        omega=rep(NA, 600), phi1=rep(NA, 600), phi2=rep(NA, 600))
for (i in 1801:2400)
{
  if (length(result[[i]])==6)
  {
    lower[i-1800,] <- result[[i]]$estimate-qnorm(0.975)*sqrt(diag(solve(result[[i]]$hessian)))
  }else{
    lower[i-1800,] <- rep(NA, 5)
  }
}

lower <- lower[!is.na(lower$alpha), ]
lower <- lower[idx, ]

upper <- data.frame(alpha=rep(NA, 600), lambda=rep(NA, 600), 
                        omega=rep(NA, 600), phi1=rep(NA, 600), phi2=rep(NA, 600))
for (i in 1801:2400)
{
  if (length(result[[i]])==6)
  {
    upper[i-1800,] <- result[[i]]$estimate+qnorm(0.975)*sqrt(diag(solve(result[[i]]$hessian)))
  }else{
    upper[i-1800,] <- rep(NA, 5)
  }
}

upper <- upper[!is.na(upper$alpha), ]
upper <- upper[idx, ]

### Mean estimated value
colMeans(estimates)

### Bias
pars <- c(0.5, 3.0, 0.7, 0.2, 0.3)
colMeans(estimates)-pars

### Coverage
coverage <- data.frame(alpha=rep(NA, 500), lambda=rep(NA, 500), 
                        omega=rep(NA, 500), phi1=rep(NA, 500), phi2=rep(NA, 500))

for (i in 1:dim(coverage)[1])
{
  for (j in 1:dim(coverage)[2])
  {
    coverage[i, j] <- 0
    if(pars[j]>=lower[i, j] & pars[j]<=upper[i, j]) coverage[i, j] <- 1
  }
}

colSums(coverage)/500*100

### Case 2: w=0.3 and moderate underreporting (phi1=0.2; phi2=0.3) 
set.seed(1234)
setwd("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/Paper 3 (model)")
load("Results/Sim/case2.RData")

estimates <- data.frame(alpha=rep(NA, 600), lambda=rep(NA, 600), 
                        omega=rep(NA, 600), phi1=rep(NA, 600), phi2=rep(NA, 600))
for (i in 1801:2400)
{
  if (length(result[[i]])==6)
  {
    estimates[i-1800,] <- result[[i]]$estimate
  }else{
    estimates[i-1800,] <- rep(NA, 5)
  }
}

estimates <- estimates[!is.na(estimates$alpha), ]

idx <- sample(1:dim(estimates)[1], 500)
estimates <- estimates[idx, ]

lower <- data.frame(alpha=rep(NA, 600), lambda=rep(NA, 600), 
                        omega=rep(NA, 600), phi1=rep(NA, 600), phi2=rep(NA, 600))
for (i in 1801:2400)
{
  if (length(result[[i]])==6)
  {
    lower[i-1800,] <- result[[i]]$estimate-qnorm(0.975)*sqrt(diag(solve(result[[i]]$hessian)))
  }else{
    lower[i-1800,] <- rep(NA, 5)
  }
}

lower <- lower[!is.na(lower$alpha), ]
lower <- lower[idx, ]

upper <- data.frame(alpha=rep(NA, 600), lambda=rep(NA, 600), 
                        omega=rep(NA, 600), phi1=rep(NA, 600), phi2=rep(NA, 600))
for (i in 1801:2400)
{
  if (length(result[[i]])==6)
  {
    upper[i-1800,] <- result[[i]]$estimate+qnorm(0.975)*sqrt(diag(solve(result[[i]]$hessian)))
  }else{
    upper[i-1800,] <- rep(NA, 5)
  }
}

upper <- upper[!is.na(upper$alpha), ]
upper <- upper[idx, ]

### Mean estimated value
colMeans(estimates)

### Bias
pars <- c(0.5, 3.0, 0.3, 0.2, 0.3)
colMeans(estimates)-pars

### Coverage
coverage <- data.frame(alpha=rep(NA, 500), lambda=rep(NA, 500), 
                        omega=rep(NA, 500), phi1=rep(NA, 500), phi2=rep(NA, 500))

for (i in 1:dim(coverage)[1])
{
  for (j in 1:dim(coverage)[2])
  {
    coverage[i, j] <- 0
    if(pars[j]>=lower[i, j] & pars[j]<=upper[i, j]) coverage[i, j] <- 1
  }
}

colSums(coverage)/500*100

### Case 3: w=0.7 and severe underreporting (phi1=0.1; phi2=0.1) 
set.seed(1234)
setwd("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/Paper 3 (model)")
load("Results/Sim/case3.RData")

estimates <- data.frame(alpha=rep(NA, 600), lambda=rep(NA, 600), 
                        omega=rep(NA, 600), phi1=rep(NA, 600), phi2=rep(NA, 600))
for (i in 1801:2400)
{
  if (length(result[[i]])==6)
  {
    estimates[i-1800,] <- result[[i]]$estimate
  }else{
    estimates[i-1800,] <- rep(NA, 5)
  }
}

estimates <- estimates[!is.na(estimates$alpha), ]

idx <- sample(1:dim(estimates)[1], 500)
estimates <- estimates[idx, ]

lower <- data.frame(alpha=rep(NA, 600), lambda=rep(NA, 600), 
                        omega=rep(NA, 600), phi1=rep(NA, 600), phi2=rep(NA, 600))
for (i in 1801:2400)
{
  if (length(result[[i]])==6)
  {
    lower[i-1800,] <- result[[i]]$estimate-qnorm(0.975)*sqrt(diag(solve(result[[i]]$hessian)))
  }else{
    lower[i-1800,] <- rep(NA, 5)
  }
}

lower <- lower[!is.na(lower$alpha), ]
lower <- lower[idx, ]

upper <- data.frame(alpha=rep(NA, 600), lambda=rep(NA, 600), 
                        omega=rep(NA, 600), phi1=rep(NA, 600), phi2=rep(NA, 600))
for (i in 1801:2400)
{
  if (length(result[[i]])==6)
  {
    upper[i-1800,] <- result[[i]]$estimate+qnorm(0.975)*sqrt(diag(solve(result[[i]]$hessian)))
  }else{
    upper[i-1800,] <- rep(NA, 5)
  }
}

upper <- upper[!is.na(upper$alpha), ]
upper <- upper[idx, ]

### Mean estimated value
colMeans(estimates)

### Bias
pars <- c(0.5, 3.0, 0.7, 0.1, 0.1)
colMeans(estimates)-pars

### Coverage
coverage <- data.frame(alpha=rep(NA, 500), lambda=rep(NA, 500), 
                        omega=rep(NA, 500), phi1=rep(NA, 500), phi2=rep(NA, 500))

for (i in 1:dim(coverage)[1])
{
  for (j in 1:dim(coverage)[2])
  {
    coverage[i, j] <- 0
    if(pars[j]>=lower[i, j] & pars[j]<=upper[i, j]) coverage[i, j] <- 1
  }
}

colSums(coverage)/500*100

### Case 4: w=0.3 and severe underreporting (phi1=0.1; phi2=0.1) 
set.seed(1234)
setwd("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/Paper 3 (model)")
load("Results/Sim/case4.RData")

estimates <- data.frame(alpha=rep(NA, 600), lambda=rep(NA, 600), 
                        omega=rep(NA, 600), phi1=rep(NA, 600), phi2=rep(NA, 600))
for (i in 1801:2400)
{
  if (length(result[[i]])==6)
  {
    estimates[i-1800,] <- result[[i]]$estimate
  }else{
    estimates[i-1800,] <- rep(NA, 5)
  }
}

estimates <- estimates[!is.na(estimates$alpha), ]

idx <- sample(1:dim(estimates)[1], 500)
estimates <- estimates[idx, ]

lower <- data.frame(alpha=rep(NA, 600), lambda=rep(NA, 600), 
                        omega=rep(NA, 600), phi1=rep(NA, 600), phi2=rep(NA, 600))
for (i in 1801:2400)
{
  if (length(result[[i]])==6)
  {
    lower[i-1800,] <- result[[i]]$estimate-qnorm(0.975)*sqrt(diag(solve(result[[i]]$hessian)))
  }else{
    lower[i-1800,] <- rep(NA, 5)
  }
}

lower <- lower[!is.na(lower$alpha), ]
lower <- lower[idx, ]

upper <- data.frame(alpha=rep(NA, 600), lambda=rep(NA, 600), 
                        omega=rep(NA, 600), phi1=rep(NA, 600), phi2=rep(NA, 600))
for (i in 1801:2400)
{
  if (length(result[[i]])==6)
  {
    upper[i-1800,] <- result[[i]]$estimate+qnorm(0.975)*sqrt(diag(solve(result[[i]]$hessian)))
  }else{
    upper[i-1800,] <- rep(NA, 5)
  }
}

upper <- upper[!is.na(upper$alpha), ]
upper <- upper[idx, ]

### Mean estimated value
colMeans(estimates)

### Bias
pars <- c(0.5, 3.0, 0.3, 0.1, 0.1)
colMeans(estimates)-pars

### Coverage
coverage <- data.frame(alpha=rep(NA, 500), lambda=rep(NA, 500), 
                        omega=rep(NA, 500), phi1=rep(NA, 500), phi2=rep(NA, 500))

for (i in 1:dim(coverage)[1])
{
  for (j in 1:dim(coverage)[2])
  {
    coverage[i, j] <- 0
    if(pars[j]>=lower[i, j] & pars[j]<=upper[i, j]) coverage[i, j] <- 1
  }
}

colSums(coverage)/500*100

### OVERREPORTING
### Case 1: w=0.7 and moderate overreporting (phi1=0.2; phi2=0.7) 
set.seed(1234)
setwd("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/Paper 3 (model)")
load("Results/Sim/case1_OVER.RData")

estimates <- data.frame(alpha=rep(NA, 520), lambda=rep(NA, 520), 
                        omega=rep(NA, 520), phi1=rep(NA, 520), phi2=rep(NA, 520))
for (i in 1561:2080)
{
  if (length(result[[i]])==6)
  {
    estimates[i-1560,] <- result[[i]]$estimate
  }else{
    estimates[i-1560,] <- rep(NA, 5)
  }
}

estimates <- estimates[!is.na(estimates$alpha), ]

idx <- sample(1:dim(estimates)[1], 500)
estimates <- estimates[idx, ]

lower <- data.frame(alpha=rep(NA, 520), lambda=rep(NA, 520), 
                        omega=rep(NA, 520), phi1=rep(NA, 520), phi2=rep(NA, 520))
for (i in 1561:2080)
{
  if (length(result[[i]])==6)
  {
    lower[i-1560,] <- result[[i]]$estimate-qnorm(0.975)*sqrt(diag(solve(result[[i]]$hessian)))
  }else{
    lower[i-1560,] <- rep(NA, 5)
  }
}

lower <- lower[!is.na(lower$alpha), ]
lower <- lower[idx, ]

upper <- data.frame(alpha=rep(NA, 520), lambda=rep(NA, 520), 
                        omega=rep(NA, 520), phi1=rep(NA, 520), phi2=rep(NA, 520))
for (i in 1561:2080)
{
  if (length(result[[i]])==6)
  {
    upper[i-1560,] <- result[[i]]$estimate+qnorm(0.975)*sqrt(diag(solve(result[[i]]$hessian)))
  }else{
    upper[i-1560,] <- rep(NA, 5)
  }
}

upper <- upper[!is.na(upper$alpha), ]
upper <- upper[idx, ]

### Mean estimated value
colMeans(estimates)

### Bias
pars <- c(0.5, 3, 0.7, 0.2, 0.7)
colMeans(estimates)-pars

### Coverage
coverage <- data.frame(alpha=rep(NA, 500), lambda=rep(NA, 500), 
                        omega=rep(NA, 500), phi1=rep(NA, 500), phi2=rep(NA, 500))

for (i in 1:dim(coverage)[1])
{
  for (j in 1:dim(coverage)[2])
  {
    coverage[i, j] <- 0
    if(pars[j]>=lower[i, j] & pars[j]<=upper[i, j]) coverage[i, j] <- 1
  }
}

colSums(coverage)/500*100

### Case 2: w=0.3 and moderate overreporting (phi1=0.2; phi2=0.7) 
set.seed(1234)
setwd("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/Paper 3 (model)")
load("Results/Sim/case2_OVER.RData")

estimates <- data.frame(alpha=rep(NA, 600), lambda=rep(NA, 600), 
                        omega=rep(NA, 600), phi1=rep(NA, 600), phi2=rep(NA, 600))
for (i in 1801:2400)
{
  if (length(result[[i]])==6)
  {
    estimates[i-1800,] <- result[[i]]$estimate
  }else{
    estimates[i-1800,] <- rep(NA, 5)
  }
}

estimates <- estimates[!is.na(estimates$alpha), ]

idx <- sample(1:dim(estimates)[1], 500)
estimates <- estimates[idx, ]

lower <- data.frame(alpha=rep(NA, 600), lambda=rep(NA, 600), 
                        omega=rep(NA, 600), phi1=rep(NA, 600), phi2=rep(NA, 600))
for (i in 1801:2400)
{
  if (length(result[[i]])==6)
  {
    lower[i-1800,] <- result[[i]]$estimate-qnorm(0.975)*sqrt(diag(solve(result[[i]]$hessian)))
  }else{
    lower[i-1800,] <- rep(NA, 5)
  }
}

lower <- lower[!is.na(lower$alpha), ]
lower <- lower[idx, ]

upper <- data.frame(alpha=rep(NA, 600), lambda=rep(NA, 600), 
                        omega=rep(NA, 600), phi1=rep(NA, 600), phi2=rep(NA, 600))
for (i in 1801:2400)
{
  if (length(result[[i]])==6)
  {
    upper[i-1800,] <- result[[i]]$estimate+qnorm(0.975)*sqrt(diag(solve(result[[i]]$hessian)))
  }else{
    upper[i-1800,] <- rep(NA, 5)
  }
}

upper <- upper[!is.na(upper$alpha), ]
upper <- upper[idx, ]

### Mean estimated value
colMeans(estimates)

### Bias
pars <- c(0.5, 3, 0.3, 0.2, 0.7)
colMeans(estimates)-pars

### Coverage
coverage <- data.frame(alpha=rep(NA, 500), lambda=rep(NA, 500), 
                        omega=rep(NA, 500), phi1=rep(NA, 500), phi2=rep(NA, 500))

for (i in 1:dim(coverage)[1])
{
  for (j in 1:dim(coverage)[2])
  {
    coverage[i, j] <- 0
    if(pars[j]>=lower[i, j] & pars[j]<=upper[i, j]) coverage[i, j] <- 1
  }
}

colSums(coverage)/500*100

### Case 3: w=0.7 and severe overreporting (phi1=0.3; phi2=0.5) 
set.seed(1234)
setwd("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/Paper 3 (model)")
load("Results/Sim/case3_OVER.RData")

estimates <- data.frame(alpha=rep(NA, 600), lambda=rep(NA, 600), 
                        omega=rep(NA, 600), phi1=rep(NA, 600), phi2=rep(NA, 600))
for (i in 1801:2400)
{
  if (length(result[[i]])==6)
  {
    estimates[i-1800,] <- result[[i]]$estimate
  }else{
    estimates[i-1800,] <- rep(NA, 5)
  }
}

estimates <- estimates[!is.na(estimates$alpha), ]

idx <- sample(1:dim(estimates)[1], 500)
estimates <- estimates[idx, ]

lower <- data.frame(alpha=rep(NA, 600), lambda=rep(NA, 600), 
                        omega=rep(NA, 600), phi1=rep(NA, 600), phi2=rep(NA, 600))
for (i in 1801:2400)
{
  if (length(result[[i]])==6)
  {
    lower[i-1800,] <- result[[i]]$estimate-qnorm(0.975)*sqrt(diag(solve(result[[i]]$hessian)))
  }else{
    lower[i-1800,] <- rep(NA, 5)
  }
}

lower <- lower[!is.na(lower$alpha), ]
lower <- lower[idx, ]

upper <- data.frame(alpha=rep(NA, 600), lambda=rep(NA, 600), 
                        omega=rep(NA, 600), phi1=rep(NA, 600), phi2=rep(NA, 600))
for (i in 1801:2400)
{
  if (length(result[[i]])==6)
  {
    upper[i-1800,] <- result[[i]]$estimate+qnorm(0.975)*sqrt(diag(solve(result[[i]]$hessian)))
  }else{
    upper[i-1800,] <- rep(NA, 5)
  }
}

upper <- upper[!is.na(upper$alpha), ]
upper <- upper[idx, ]

### Mean estimated value
colMeans(estimates)

### Bias
pars <- c(0.5, 3, 0.7, 0.3, 0.5 )
colMeans(estimates)-pars

### Coverage
coverage <- data.frame(alpha=rep(NA, 500), lambda=rep(NA, 500), 
                        omega=rep(NA, 500), phi1=rep(NA, 500), phi2=rep(NA, 500))

for (i in 1:dim(coverage)[1])
{
  for (j in 1:dim(coverage)[2])
  {
    coverage[i, j] <- 0
    if(pars[j]>=lower[i, j] & pars[j]<=upper[i, j]) coverage[i, j] <- 1
  }
}

colSums(coverage)/500*100

### Case 4: w=0.3 and severe overreporting (phi1=0.3; phi2=0.5) 
set.seed(1234)
setwd("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre/Papers/Paper 3 (model)")
load("Results/Sim/case4_OVER.RData")

estimates <- data.frame(alpha=rep(NA, 600), lambda=rep(NA, 600), 
                        omega=rep(NA, 600), phi1=rep(NA, 600), phi2=rep(NA, 600))
for (i in 1801:2400)
{
  if (length(result[[i]])==6)
  {
    estimates[i-1800,] <- result[[i]]$estimate
  }else{
    estimates[i-1800,] <- rep(NA, 5)
  }
}

estimates <- estimates[!is.na(estimates$alpha), ]

idx <- sample(1:dim(estimates)[1], 500)
estimates <- estimates[idx, ]

lower <- data.frame(alpha=rep(NA, 600), lambda=rep(NA, 600), 
                        omega=rep(NA, 600), phi1=rep(NA, 600), phi2=rep(NA, 600))
for (i in 1801:2400)
{
  if (length(result[[i]])==6)
  {
    lower[i-1800,] <- result[[i]]$estimate-qnorm(0.975)*sqrt(diag(solve(result[[i]]$hessian)))
  }else{
    lower[i-1800,] <- rep(NA, 5)
  }
}

lower <- lower[!is.na(lower$alpha), ]
lower <- lower[idx, ]

upper <- data.frame(alpha=rep(NA, 600), lambda=rep(NA, 600), 
                        omega=rep(NA, 600), phi1=rep(NA, 600), phi2=rep(NA, 600))
for (i in 1801:2400)
{
  if (length(result[[i]])==6)
  {
    upper[i-1800,] <- result[[i]]$estimate+qnorm(0.975)*sqrt(diag(solve(result[[i]]$hessian)))
  }else{
    upper[i-1800,] <- rep(NA, 5)
  }
}

upper <- upper[!is.na(upper$alpha), ]
upper <- upper[idx, ]

### Mean estimated value
colMeans(estimates)

### Bias
pars <- c(0.5, 3, 0.3, 0.3, 0.5 )
colMeans(estimates)-pars

### Coverage
coverage <- data.frame(alpha=rep(NA, 500), lambda=rep(NA, 500), 
                        omega=rep(NA, 500), phi1=rep(NA, 500), phi2=rep(NA, 500))

for (i in 1:dim(coverage)[1])
{
  for (j in 1:dim(coverage)[2])
  {
    coverage[i, j] <- 0
    if(pars[j]>=lower[i, j] & pars[j]<=upper[i, j]) coverage[i, j] <- 1
  }
}

colSums(coverage)/500*100