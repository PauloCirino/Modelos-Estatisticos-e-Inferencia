#####################################################################################
#####################################################################################
#####################################################################################
##  Alunos:
##          Marcus Vinicius Barros
##          Paulo Cirino Ribeiro Neto
#####################################################################################
#####################################################################################
#####################################################################################


##Leitura dos Dados
FileName = "Dados.csv";
FileName = paste(getwd(),FileName,sep="/");
Dados = read.csv(FileName, header = TRUE, sep=",");

## Calculo IMC
Massa = Dados[,1];
Altura = Dados[,2];
IMC = (Massa) / (Altura*Altura);

## Definicao de Variaveis
alpha = 0.01;
expectedMean <- 26.3

#Teste T
t2Sided = t.test(x = IMC, 
       alternative= "two.sided",
       mu = expectedMean,
       conf.level = 1-alpha);
print(t2Sided)
print("Hipotese nula é regeitada.")


#Valores Criticos
a <- qt(1-0.005,12);
b <- qt(0.005,12);

print( paste("Os Valores Criticos sao :",a,"e",b,sep=" ")  )


estimateVar <- function(Data)
{
  Mean <- mean (Data);
  Var <- sapply(Data, function(x) (x-Mean)**2 );
  Var <- sum(Var)/(length(Var)-1);
  Var
}

Var <- estimateVar(IMC);
desvioPadraoEstimado <- sqrt(Var);
effectSize <- ( mean(IMC) - expectedMean) / sqrt(Var)

print( paste("Desvio Padrao estimado = ", desvioPadraoEstimado,sep=" "))
print(paste("Tamanho de efeito", effectSize, sep=" "))

h <- hist(IMC,breaks = 7, col = "dark red")
xfit<-seq(min(IMC),max(IMC),length=100) 
yfit<-dnorm(xfit,mean=mean(IMC),sd=sd(IMC)) 
yfit <- yfit*diff(h$mids[1:2])*length(IMC) 
lines(xfit, yfit, col="blue")

# Prova de que os dados não sao normais
d <- density(x = IMC) 
plot(d, main = "Funcao de Densidade", col = "red")
polygon(d, col="darkcyan", border = "red")


