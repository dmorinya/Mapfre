setwd("/home/dmorina/Insync/dmorina@ub.edu/OneDrive Biz/Projectes/2022/0052022. MAPFRE/Mapfre")
load("Results/Sim/case1_1_OVER.RData")
result1 <- result
load("Results/Sim/case1_2_OVER.RData")
result2 <- result
load("Results/Sim/case1_3_OVER.RData")
result3 <- result
load("Results/Sim/case1_4_OVER.RData")
result4 <- result
load("Results/Sim/case1_5_OVER.RData")
result5 <- result
load("Results/Sim/case1_6_OVER.RData")
result6 <- result

result <- rbind(result1, result2, result3, result4, result5, result6)

save(list="result", file="Results/Sim/case1_OVER.RData")
