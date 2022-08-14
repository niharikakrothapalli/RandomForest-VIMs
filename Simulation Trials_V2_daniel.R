library("party", quietly = TRUE)
library("randomForest", quietly = TRUE)
library("permimp")
library("randomForestExplainer")
library("simstudy")
library("data.table")

x1 = c(rnorm(500, 0,2), rnorm(500,3,2))
x2 = c(rnorm(500,0,2), rnorm(500,3,2))
x3 = c(rnorm(500,0,2.1), rnorm(500,3,2.1))
x4 = x3 + c(rnorm(1000,0,0.2))
x5 = c(rnorm(500,0,2.1), rnorm(500,3,2.1))
x6 = x5 + c(rnorm(1000,0,1.5))
x7 = c(rnorm(500,0,2.1), rnorm(500,3,2.1))
x8 = x7 ** 2 + c(rnorm(1000,0,0.2))
x9 = x4 + x6 + x8
x10 = rnorm(1000)
class= factor(rep(1:2, each=500))
sim <- data.frame(class,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)

write.csv(sim,'sim_trial1.csv')

#class$factor.rep.1.2..each...500.. <- as.numeric(class$factor.rep.1.2..each...500..)
#x = data.table(class)

#cor_data <- addCorData(x, idname = "factor.rep.1.2..each...500..", mu = c(0, 0), sigma = c(2, 0.2), rho = -0.2,
#                 corstr = "cs", cnames = c("a0", "a1"))

sim_rf <- randomForest(class ~ ., data = sim, replace = FALSE, keep.forest = TRUE, keep.inbag = TRUE,
                        importance = T, localImp = T)
system.time(sim_cpi <- permimp(sim_rf, conditional = TRUE, progressBar = FALSE, thresholdDiagnostics = TRUE, threshold = 0.9))
1
system.time(sim_pi <- permimp(sim_rf, progressBar = FALSE, pre1.0.0 = TRUE))
1
plot(sim_cpi, horizontal = TRUE, interval = "quantile", nVar = 9)
plot(sim_pi, horizontal = TRUE, interval = "quantile")
varImpPlot(sim_rf, type = 1)
varImpPlot(sim_rf, type = 2)

system.time(sim_mdi <- importance(sim_rf, type = 2))
system.time(sim_mda <- importance(sim_rf, type = 1))

system.time(impt_frame <- measure_importance(sim_rf))
plot_min_depth_distribution(impt_frame) #THIS MIGHT NOT WORK
plot_multi_way_importance(impt_frame, no_of_labels = 6)
plot_importance_ggpairs(impt_frame)