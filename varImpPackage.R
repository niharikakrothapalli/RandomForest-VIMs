library(ggplot2)
library(party)
library(varImp)

sim_trial1 <- read.csv("C:/Users/Pratiksha Sharma/Downloads/sim_trial1.csv")

sim_trial1$class = factor(sim_trial1$class, levels = unique(sim_trial1$class))

mod1 = cforest(class ~ ., data = sim_trial1)

system.time(res1 <- varImp(mod1, conditional = FALSE))

system.time(res2 <- varImp(mod1, conditional = TRUE))

system.time(res3 <- varImpACC(mod1, conditional = FALSE))

system.time(res4 <- varImpACC(mod1, conditional = TRUE))

system.time(res5 <- varImpAUC(mod1, conditional = FALSE))

system.time(res6 <- varImpAUC(mod1, conditional = TRUE))

res1 = as.data.frame(res1)
res1['factors'] = row.names(res1)
res1 = res1[order(-res1$res1),]

res2 = as.data.frame(res2)
res2['factors'] = row.names(res2)
res2 = res2[order(-res2$res2),]

res3 = as.data.frame(res3)
res3['factors'] = row.names(res3)
res3 = res3[order(-res3$res3),]

res4 = as.data.frame(res4)
res4['factors'] = row.names(res4)
res4 = res4[order(-res4$res4),]

res5 = as.data.frame(res5)
res5['factors'] = row.names(res5)
res5 = res5[order(-res5$res5),]

res6 = as.data.frame(res6)
res6['factors'] = row.names(res6)
res6 = res6[order(-res6$res6),]

res1$factors = factor(res1$factors, levels = unique(res1$factors))
res2$factors = factor(res2$factors, levels = unique(res2$factors))
res3$factors = factor(res3$factors, levels = unique(res3$factors))
res4$factors = factor(res4$factors, levels = unique(res4$factors))
res5$factors = factor(res5$factors, levels = unique(res5$factors))
res6$factors = factor(res6$factors, levels = unique(res6$factors))

ggplot(res1, aes(x = factors, y = res1)) + geom_bar(stat="identity")+ggtitle("Conditional = FALSE") + xlab("Factors") + ylab("Variable Importances")

ggplot(res2, aes(x = factors, y = res2)) + geom_bar(stat="identity")+ggtitle("Conditional = TRUE") + xlab("Factors") + ylab("Variable Importances")

ggplot(res3, aes(x = factors, y = res3)) + geom_bar(stat="identity")+ggtitle("Conditional = FALSE") + xlab("Factors") + ylab("Variable Importances")

ggplot(res4, aes(x = factors, y = res4)) + geom_bar(stat="identity")+ggtitle("Conditional = TRUE") + xlab("Factors") + ylab("Variable Importances")

ggplot(res5, aes(x = factors, y = res5)) + geom_bar(stat="identity")+ggtitle("Conditional = FALSE") + xlab("Factors") + ylab("Variable Importances")

ggplot(res6, aes(x = factors, y = res6)) + geom_bar(stat="identity")+ggtitle("Conditional = TRUE") + xlab("Factors") + ylab("Variable Importances")
