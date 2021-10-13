#----------------------------------------------------------------------------------------------------------

#                                         BASE DE DADOS DO ENADE 2017

#----------------------------------------------------------------------------------------------------------

###########################################################################################################

#----------------------------------------------------------------------------------------------------------

#                                              ALGORITMO APRIORI

#----------------------------------------------------------------------------------------------------------


#CARREGANDO/INSTALANDO PACOTES
#install.packages("reader")
library(readr)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("plyr", dependencies= TRUE)
if(sessionInfo()['basePkgs']=="dplyr" | sessionInfo()['otherPkgs']=="dplyr"){
  detach(package:dplyr, unload=TRUE)
}

library(plyr)

#install.packages("arules", dependencies=TRUE)
library(arules)

library(grid)

#install.packages("graph")
#install.packages("Rgraphviz")
#install.packages("Rtools")

#install.packages("arulesViz", dependencies = TRUE)
library(arulesViz)
require("arulesViz")

#CARREGANDO A BASE DE DADOS PARA O R

# --------------- base Pré-Processada:
BaseApriori <- read.csv("BaseProc_E2017.csv")

BaseApriori_2 <- BaseApriori
#view(BaseApriori_2)

# --------------- classificando as notas Gerais do ENADE:

j <- 12
for (i in 1: length(BaseApriori_2[,1])) {
  for (j in (12:15)) {
    if(BaseApriori_2[i,j] <= 30){
      BaseApriori_2[i,j] <- "Ruim"
    } else if (BaseApriori_2[i,j] <= 50){
      BaseApriori_2[i,j] <- "Médio"
    }else{
      BaseApriori_2[i,j] <- "Bom"
    }
  }
}


#view(BaseApriori_2)

# --------------- transformando variáveis numericas em character:
BaseApriori_3 <- BaseApriori_2
BaseApriori_3 <- BaseApriori_3 %>% select(-1,-6, -(13:15))
#
#view(BaseApriori_3)

BaseApriori_3$CO_CATEGAD <- as.character(BaseApriori_3$CO_CATEGAD)
BaseApriori_3$CO_GRUPO <- as.character(BaseApriori_3$CO_GRUPO)
BaseApriori_3$CO_MODALIDADE <- as.character(BaseApriori_3$CO_MODALIDADE)
BaseApriori_3$CO_UF_CURSO <- as.character(BaseApriori_3$CO_UF_CURSO)
#BaseApriori_3$CO_REGIAO_CURSO <- as.character(BaseApriori_3$CO_REGIAO_CURSO)
BaseApriori_3$NU_IDADE <- as.character(BaseApriori_3$NU_IDADE)
BaseApriori_3$TP_SEXO <- as.character(BaseApriori_3$TP_SEXO)
BaseApriori_3$ANO_FIM_EM <- as.character(BaseApriori_3$ANO_FIM_EM)
BaseApriori_3$ANO_IN_GRAD <- as.character(BaseApriori_3$ANO_IN_GRAD)
BaseApriori_3$CO_TURNO_GRADUACAO <- as.character(BaseApriori_3$CO_TURNO_GRADUACAO)
#BaseApriori_3$NT_GER <- as.character(BaseApriori_3$NT_GER)
#BaseApriori_3$NT_FG <- as.character(BaseApriori_3$NT_FG)
#BaseApriori_3$NT_OBJ_FG <- as.character(BaseApriori_3$NT_OBJ_FG)
#BaseApriori_3$NT_DIS_FG <- as.character(BaseApriori_3$NT_DIS_FG)
#view(BaseApriori_3)

# --------------- Aplicando o algoritmo:
rules <- apriori(BaseApriori_3, parameter = list(supp = 0.1, conf = 0.9))
#inspect(rules)

BaseRules <- as(rules, "data.frame")
view(BaseRules)

# --------------- Testes Gráficos:
plot(BaseRules)
inspect(head(rules, n = 5, by = "lift"))
inspect(head(rules, n = 5, by = "confidence"))
inspect(head(rules, n = 5, by = "support"))

p <- plot(rules, jitter = 0)
head(quality(rules))
plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, method = "two-key plot")
sel <- plot(rules, measure=c("support", "lift"), shading = "confidence",interactive = TRUE)

#p2 <- plot(rules, method = "matrix")
subrules2 <- head(rules, n = 10, by = "lift")
plot(subrules2, method = "graph")
#saveAsGraph(head(rules, n = 1000, by = "lift"), file = "rules.graphml")
plot(subrules2, method = "paracoord")
plot(rules, method = "grouped", control = list(k = 50))

# --------------- Salvando as regras em um arquivo csv:
write.csv(BaseRules, "RegrasOrig.csv")
