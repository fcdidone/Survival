##' Kaplan-meier é uma maneira de estimar a curva de sobrevivência
##' você pode desenhar ela para diferentes fatores.
##' Pro: pouca suposições sobre o formato da curva porque é um método não 
##' paramétrico.
##' Cons: Não é feito para incluir variáves continuas. Eg.: Idade.
##'  Quantifica se existe diferença no risco de morte entre os tratamentos,
##' mas não diz aonde esta a diferença;
##' Outros métodos dizem, mas fazem mais suposição sobre o formato
##' 


##' Arrumar os dados, tu pode so selecionar todos e colar.

setwd("c:/coursera/pati") 
read.csv("c.csv",header = T,sep = ";")->c
install.packages("dplyr")
library(dplyr)
mutate(c,death = 20 - Total, D = paste(Date,Time)) -> c1
gsub("\\.","",c1$D) -> c1$D 
strptime(c1$D, "%d-%b %H:%M") -> c1$D
c1[,c(-3,-4)] -> c1

split(c1,list(c1$Replicate,c1$Plant.species)) -> c20

l <- function(x){
        d1<- c(0)
        length(x$D) -> x1
        if(x1==0)
        {0} else 
        { for (i in 1:x1){
                d1 <- c(d1,x$D[i+1] - x$D[i])                
        }
        d1     
        }
}

sapply(c20,l) -> g1
c() -> x1
for(i in 1:84) {
        length(unlist(g1[i],use.names = F)) -> x2
        if(x2 > 1 ) {
                unlist(g1[i],use.names = F) -> x3
                cumsum(x3) -> x3
                x3 <- x3[!is.na(x3)]
                c(x1,x3)->x1               
        }  
}

cbind(c1,x1) -> c2

c(0,c2$death[2:826]-c2$death[1:825]) -> l1
cbind(c2,l1) -> c2
c2$l1[which(c2$l1 == -20)] <- 0
which(c2$l1 < -0) -> l2
(c2$death[l2 -1] + c2$l1[which(c2$l1 < -0)]) -> c2$death[l2 -1]

c(0,c2$death[2:826]-c2$death[1:825]) -> l3
cbind(c2,l3) -> c2
c2$l3[which(c2$l3 == -20)] <- 0
which(c2$l3 < -0) -> l4
(c2$death[l4 -1] + c2$l3[which(c2$l3 < -0)]) -> c2$death[l4 -1]

c(0,c2$death[2:826]-c2$death[1:825]) -> l5
cbind(c2,l5) -> c2
c2$l5[which(c2$l5 == -20)] <- 0
which(c2$l5 < -0) -> l6
(c2$death[l6 -1] + c2$l5[which(c2$l5 < -0)]) -> c2$death[l6 -1]

c(0,c2$death[2:826]-c2$death[1:825]) -> l7
cbind(c2,l7) -> c2
c2$l7[which(c2$l7 == -20)] <- 0
which(c2$l7 < -0) -> l8
(c2$death[l8 -1] + c2$l7[which(c2$l7 < -0)]) -> c2$death[l8 -1]

c(0,c2$death[2:826]-c2$death[1:825]) -> l9
cbind(c2,l9) -> c2
c2$l9[which(c2$l9 == -20)] <- 0
c2$death[596] <- 19;c2$death[676:677] <- 18;c2$death[698] <- 9
c2$death[716:723] <- 15; c2$death[741:743] <- 15

c(0,c2$death[2:826]-c2$death[1:825]) -> l10
cbind(c2,l10) -> c2
c2$l10[which(c2$l10 == -20)] <- 0
c2[,c(1,2,3,4,5,6,12)] -> c3

a1 <- data.frame()
for(i in 1:826)
if(c2$l10[i]>0) {        
        rep(c2$x1[i],c2$l10[i]) -> time
        rep(c2$Plant.species[i],c2$l10[i]) -> plant
        data.frame(plant,time) -> a2
        rbind(a2,a1) -> a1      
}




##' Pacote survival


install.packages("survival")
library(survival)

##' Dados sobre a curva
survfit(Surv(a1$time)~a1$plant) -> fit
summary(fit)

##' Plotar a curva


plot(survfit(Surv(a1$time)~a1$plant))



##' Salvar os gráficos
##' plot1 --> Com intervalo de confiança
##' plot2 --> sen intervalo de confiança

as.character(rev(unique(a1$plant))) -> lg 

bmp("plot1.bmp",pointsize = 20,width = 800,height = 800)

plot(fit,col=c(1:12),xlab="time(hours)",ylab="Survival Probability",lwd=1,conf.int=T)
legend(225,1,lg,col=c(1:12),lwd=0.7)

dev.off()

bmp("plot2.bmp",pointsize = 20,width = 800,height = 800)

plot(fit,col=c(1:12),xlab="time(hours)",ylab="Survival Probability",lwd=1)
legend(225,1,lg,col=c(1:12),lwd=0.7)

dev.off()

##' Testes estatísticos

survdiff(Surv(a1$time)~a1$plant,rho=0)
survdiff(Surv(a1$time)~a1$plant,rho=1)
survdiff(Surv(a1$time)~a1$plant,rho=1.5)

##' 3 teste e todos os testes dizem que o H0 é igual e o h1 tem diferença entre
##' as curvas

##' Rho= 0 log-rank teste (mais peso em valores maiores de tempo)
##' Rho= 1 é teste wilcoxon generelalizado (mais peso em valores menores de tempo)
##' Rho= 1.5 é o Tarome-Ware teste (fica entre o log-rank teste e o wilcoxon)
##' Logrank - O método mais popular compara o risco entre os grupos. É aplicavél
##' se a função de risco não se curza. Ex.: Risco proporcional
##' Função de risco que se cruzam sugerem a presença de interção entre o tempo
##' de vida e os grupos
##' 
##' Resultados diferentes de escolha de peso geralmente levam a conclusão similares
##' A melhor escolha é aquela com maior poder
##' Poder ter uma razão clinica para escolher um peso em particular
##' O peso escolhido deve ser decidido antes. Não pode ser um método para
##' escolher o menor valor de p.
##' 
##' 