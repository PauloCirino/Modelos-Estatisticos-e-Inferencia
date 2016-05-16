####install.packages("ExpDE") 
require(ExpDE)

callFunction <- function(recpars, mutpars, popsize){
    
    #Random Seed
    set.seed(sample(100000,1))
    
    #Operators set by the professor
    selpars <- list(name = "selection_standard");
    stopcrit <- list(names = "stop_maxeval", maxevals = 60000, maxiter = 1000);
    probpars <- list(name = "sphere", xmin = -seq(1,20), xmax = 20 + 5 * seq(5, 24));
    
    #Results Vector 
    results <- numeric();
    
    obsResult <- ExpDE(popsize, mutpars, recpars, selpars, stopcrit, probpars);
    
    obsResult
}

callFunctionWithOperators <- function(numCalls){
    
    recpars <- list(list(name = "recombination_arith"),
                    list(name = "recombination_bin", cr = 0.7),
                    list(name = "recombination_blxAlphaBeta", alpha = 0.4, beta = 0.4),
                    list(name = "recombination_eigen", othername = "recombination_bin", cr = 0.9));
    
    mutpars <- list(list(name = "mutation_rand", f = 4), 
                    list(name = "mutation_best", f = 3),
                    list(name = "mutation_rand", f = 4),
                    list(name = "mutation_best", f = 2.8));
    popsize <- c( 300, 300, 230, 85 );
    
    Operators <- rep(c(1,2,3,4), numCalls)[sample(4*numCalls)]
    Results <- data.frame()
    for( callNum in 1:length(Operators))
    {
        op <- Operators[callNum]
        Results[callNum, 1] <- paste("Op", op, sep = " ")
        Results[callNum, 2] <- callFunction(recpars[[op]],
                                            mutpars[[op]],
                                            popsize[op])$Fbest
    }
    
    colnames(Results) <- c("Operator", "Result")
    Results$Operator <- as.factor(Results$Operator)
    Results
    
}

###DefiniÁ„o das Premissas do Teste
alpha <- 0.05
beta <- 0.15
potencia <- 1 - beta
levels <- 4
sigma <- 10
delta <- 2.5


#Formual 01

##Definição do N
tau <- c(-delta/2, 
         delta/2, 
         rep(0, levels-2)) # define tau vector
vartau <- var(tau)
n <- power.anova.test(groups = 4, 
                 between.var = vartau, 
                 within.var = sigma^2, 
                 sig.level = alpha, 
                 power = 1-beta)$n


##Coleta de Dados
OpTime <- callFunctionWithOperators(n)
#load("Data.rdata")

###An·lise ExploratÛria dos Dados
boxplot(Result~Operator,
        data = OpTime,
        main = "ComparaÁ„o entre AlgorÌtimos",
        xlab = "Operador",
        ylab = "Resultado",
        pch  = 16,
        col  = "gray")


# Computational modeling and analysis
model <- aov(Result~Operator, 
             data = OpTime)
summary.aov(model)


# Check normality
library(car)
shapiro.test(model$residuals)
qqPlot(model$residuals, 
       pch = 16, 
       lwd = 3, 
       cex = 2, 
       las = 1)

# Check homoscedasticity
fligner.test(Result~Operator, 
             data = OpTime)

plot(x    = model$fitted.values,
     y    = model$residuals,
     cex  = 2,
     las  = 1,
     pch  = 16,
     xlab = "Fitted values",
     ylab = "Residuals")
grid(NULL,NULL, lwd=2, col = "#44444422")


# Check independence
durbinWatsonTest(model)
plot(x    = seq_along(model$residuals),
     y    = model$residuals,
     type = "l",
     las  = 1,
     lwd  = 2,
     lty  = 1,
     xlab = "Residual order",
     ylab = "Residual value")
points(x    = seq_along(model$residuals),
       y    = model$residuals,
       type = "p",
       cex  = 2,
       pch  = 16,
       col  = as.numeric(OpTime[, 1]))
grid(NA,NULL, lwd=2, col = "#44444422")


library(multcomp)

# Situation 1: all vs. all
OpTime_tukey <- glht(model, 
                     linfct = mcp(Operator = "Tukey"))
OpTime_tukey_CI <- confint(OpTime_tukey, 
                           level = 0.95)
OpTime_tukey_CI
plot(OpTime_tukey_CI, 
     xlab       = "FOOO",
     sub        = "- Comparação Entre Algortítimos -",
     cex.axis   = 1.2,
     cex        = 2)

## Multiple comparisons
require(agricolae)
MDS_Fisher <- LSD.test(model,"Operator",console=TRUE)
plot(MDS_Fisher$groups$trt,MDS_Fisher$groups$means,
     main="MÈdia Amostral dos Operadores",
     xlab="Operadores",
     ylab="MÈdias")


###ANOVA Test
vartau <- var(tau)
power.anova.test(groups = 4, 
                 n = 395,
                 between.var = vartau, 
                 within.var = sigma^2, 
                 sig.level = alpha)

