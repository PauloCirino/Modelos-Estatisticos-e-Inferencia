

Data <- read.csv("./EngSis_weight_estXmeasure.csv",
                  sep = ",",
                  header = TRUE);
Data <- cbind(Data,
              Dif = Data$Measured.weight.kg - Data$Estimated.weight.kg )

##Box Plot
par(mfrow = c(1, 1))
boxplot(Data$Measured.weight.kg, Data$Estimated.weight.kg)

##Distribuicao de Densidade
par(mfrow = c(1, 1))
plot(density(Data$Dif),
     main = "Densidade Graduacao em Engenharia de Sistemas", xlab = NULL)
polygon(density(Data$Dif ), col="darkcyan", border = "red")

require("car")
par(mfrow = c(1, 1))
qqPlot(Data$Dif,
       pch=16,
       cex=1.5,
       las=1)

t.test(Data$Dif,
       alternative = "greater")

power.t.test(n = dim(Data)[1],
             delta = 0.6,
             sd = sd(Data$Dif),
             sig.level = 0.05,
             alternative = "one.sided",
             type = "one.sample")


power.t.test(n = dim(Data)[1],
             sd = sd(Data$Dif),
             sig.level = 0.05,
             alternative = "one.sided",
             type = "one.sample",
             power = 0.80)

power.t.test(delta = 0.6,
             sd = sd(Data$Dif),
             sig.level = 0.05,
             alternative = "one.sided",
             type = "one.sample",
             power = 0.80)

plot(x = Data$Measured.weight.kg, y=(Data$Dif)) 
