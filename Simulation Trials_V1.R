library("party", quietly = TRUE)
library("randomForest", quietly = TRUE)
library("permimp")
library("randomForestExplainer")
library("simstudy")
library("data.table")

x1 = c(rnorm(500, 0,1.5), rnorm(500,3,1.5))
x2 = c(rnorm(500,0,1.5), rnorm(500,3,1.5))
x3 = c(rnorm(500,0,1.5), rnorm(500,3,1.5))
x4 = x3 + c(rnorm(1000,0,0.5))
x5 = c(rnorm(500,0,1.5), rnorm(500,3,1.5))
x6 = x5 + c(rnorm(1000,0,1))
x7 = x6 + c(rnorm(1000,0,1))
x8 = rnorm(1000)
x9 = rnorm(1000)
x10 = x9 + rnorm(1000)
class= data.frame(factor(rep(1:2, each=500)))
class
sim <- data.frame(class,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)

#class$factor.rep.1.2..each...500.. <- as.numeric(class$factor.rep.1.2..each...500..)
#x = data.table(class)

#cor_data <- addCorData(x, idname = "factor.rep.1.2..each...500..", mu = c(0, 0), sigma = c(2, 0.2), rho = -0.2,
#                 corstr = "cs", cnames = c("a0", "a1"))

sim_rf <- randomForest(factor.rep.1.2..each...500.. ~ ., data = cor_data, mtry = 2, replace = FALSE, nodesize = 7, keep.forest = TRUE, keep.inbag = TRUE,
                        importance = T, localImp = T)
system.time(sim_cpi <- permimp(sim_rf, conditional = TRUE, progressBar = FALSE, thresholdDiagnostics = TRUE, threshold = 0.9))
1
system.time(sim_pi <- permimp(sim_rf, progressBar = FALSE, pre1.0.0 = TRUE))
1
plot(sim_cpi, horizontal = TRUE, interval = "quantile")
plot(sim_pi, horizontal = TRUE, interval = "quantile")
varImpPlot(sim_rf, type = 1)
varImpPlot(sim_rf, type = 2)

system.time(sim_mdi <- importance(sim_rf, type = 2))
system.time(sim_mda <- importance(sim_rf, type = 1))