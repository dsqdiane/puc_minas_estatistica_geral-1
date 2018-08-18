print "hello john"
print("hello john")
setwd("~/")
setwd("C:/Users/jgodoi1/rwork")
library(readxl)
install.packages("tidyverse")
library(readxl)
Nota_Alunos <- read_excel("Nota de Alunos - Parte 1.xlsx")
view(Nota_Alunos)
view(Nota_Alunos)
View(Nota_Alunos)
View(Nota_Alunos,"Nota de Alunos")
View(Nota_Alunos)
#----- Tabela de frequencia para genero-----#
freq_genero <- table(Nota_Alunos$Genero)
freq_genero
prop_genero <- prop.table(freq_genero)
prop_genero
perc_genero <- round(prop_genero*100,digits = 2)
perc_genero
coluna_freq <- c(freq_genero,sum(freq_genero))
coluna_freq
coluna_perc <- c(perc_genero,sum(perc_genero))
perc_genero
coluna_perc
names(coluna_freq)[length(coluna_freq)] <- "Total"
coluna_freq
coluna_perc
tabela_freq <- cbind(coluna_freq,coluna_perc)
tabela_freq
#----- tabela de frequência para conceito ------#
freq_conceito <- table(Nota_Alunos$Conceito)
freq_conceito
prop_conceito <- prop.table(freq_conceito)
prop_conceito[]
prop_conceito
perc_conceito <- round(prop_conceito*100,digits = 2)
perc_conceito
coluna_freq <- c(freq_conceito,sum(freq_conceito))
coluna_freq
coluna_perc <- c(perc_conceito,sum(perc_conceito))
coluna_perc
names(coluna_freq)[length(coluna_freq)] <- "Total"
coluna_freq
table_freq <- cbind(coluna_freq,coluna_perc)
table_freq
#----- Tabela de frequencia para nota final -----#
intervalos <- cut(Nota_Alunos$Nota_Final,breaks = 0:10, right = F)
intervalos
freq_notas <- table(intervalos)
freq_notas
prop_notas <- prop.table(freq_notas)
perc_notas <- round(prop_notas*100, digits = 2)
coluna_freq <- c(freq_notas,sum(freq_notas))
coluna_perc <- c(perc_notas,sum(perc_notas))
names(coluna_freq)[length(coluna_freq)]<- "Total"
tabela_freq <- cbind(coluna_freq,coluna_perc)
tabela_freq
#--------Gráfico de Pizza -------#
rotulos <- paste(perc_genero,"%",sep = "")
rotulos
pie(freq_genero,main="Gráfico de Pizza: Genêro dos Alunos",labels = rotulos, col=rainbow(7))
legend(1,1,names(freq_genero),col=rainbow(7),pch=15)
#----- Gráfico de barras ou Colunas -----#
barplot(freq_conceito)
barplot(freq_conceito, horiz = T)
freq_cruzada <- table(Nota_Alunos$Genero, Nota_Alunos$Conceito)
freq_cruzada
barplot(freq_cruzada, beside = T, main = "Conceito vs Genero", ylab = "Numero de Alunos", col = c("darkblue", "red"))
legend(1,30, rownames(freq_cruzada), col = c("darkblue", "red"), pch=16)
legend(1,30, rownames(freq_cruzada), col = c("darkblue", "red"), pch=17)
hist(Nota_Alunos$Nota_Final,breaks = 0:10,right=F,col="pink",xlab = "Notas",ylab = "Frequência",main="Distribuição de Notas")
plot
plot?
;
?plot
plot(Nota_Alunos$Prova_1,type='l',xlab = "ID Aluno",ylab = "Nota")
lines(Nota_Alunos$Prova_2,col="yellow")
lines(Nota_Alunos$Prova_3,col="orange")
#----- Gráfico de Caixa -----#
boxplot(Nota_Alunos$Nota_Final ~ Nota_Alunos$Disciplina, main = "Nota final por diciplina", xlab = "Diciplina", col = c("purple", "brown"))
savehistory("C:/Users/jgodoi1/rwork/history_console_code.r")
