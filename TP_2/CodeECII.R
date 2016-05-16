##Estudo de caso 02 - Marcus e Paulo

##Leitura dos Dados
Data <- read.csv("Data.csv")

##Separacao das Posicoes de cada curso
posGrad <- (Data$Course == "EngSis")
posPosGrad <- !(posGrad)  

#Calculo BMI
Data <- cbind(Data,
              BMI = (Data$Weight.kg) / (Data$Height.cm/100)^2 )

##Box Plot
par(mfrow = c(1, 1))
boxplot(BMI ~ Course, data = Data)

##Distribuicao de Densidade
par(mfrow = c(2, 1))
plot(density(Data$BMI [posGrad] ),
     main = "Densidade Graduacao em Engenharia de Sistemas", xlab = NULL)
polygon(density(Data$BMI [posGrad]), col="darkcyan", border = "red")

plot(density(Data$BMI [posPosGrad]),
     main = "Densidade Pos Graduacao em Engenharia Eletrica", xlab = NULL)
polygon(density(Data$BMI [posPosGrad]), col="darkcyan", border = "red")

##F-test
testeF<-var.test(Data$BMI [posGrad], Data$BMI [posPosGrad], conf.level = 1 - 0.05)
print(testeF)

#Teste de hipótese
testeT<-with(Data,
     t.test(BMI~Course,
            alternative = "two.sided",
            mu = 0,
            conf.level = 0.95,
            var.equal = TRUE))
print(testeT)

#Potência de teste
require("pwr");
potenciaTest <- pwr.t2n.test(n1 = 13,
                             n2 = 28,
                             d = 1.26,
                             sig.level = 0.05,
                             alternative = "two.sided");
print( paste("Potencia do teste", potenciaTest$power, sep = " = "))

#Normalidade dos resíduos
means <- aggregate(BMI ~ Course, data = Data, mean)
Means <- numeric()
Means[posGrad] <- means$BMI[1]
Means[posPosGrad] <- means$BMI[2]
Data <- cbind(Data, Means)
resid <- Data$BMI - Data$Means      

require("car")
par(mfrow = c(1, 1))
qqPlot(resid,
       pch=16,
       cex=1.5,
       las=1)

#Independência dos resíduos
require(lmtest)
independencia<-with(Data,
     dwtest(BMI~Course))

plot(resid,
     pch=16,
     cex=1.5,
     type="b",
     las=1)
print(independencia)