#----------------------------------------------------------------------------------------------------------

#                                         BASE DE DADOS DO ENADE 2017

#----------------------------------------------------------------------------------------------------------

###########################################################################################################

#----------------------------------------------------------------------------------------------------------

#                                        SELEÇÃO DAS MELHORES REGRAS

#----------------------------------------------------------------------------------------------------------


#CARREGANDO/INSTALANDO PACOTES
library(tidyverse)
library(readr)
#install.packages("stringr")
library(stringr)
require(stringr)
#install.packages("hashmap")
library(hashmap)
require(hashmap)

#CARREGANDO A BASE DE DADOS PARA O R

# --------------- Base de regras geradas pelo aprior:
Regras <- read.csv("RegrasOrig.csv", stringsAsFactors = FALSE)
View(Regras)

# --------------- algoritmo de poda "NÃO OTIMIZADO O(n²)":
Poda_Regras <- function(arq_regras){
  #separando a coluna de regras em duas
  Aux <- arq_regras
  Entrada_Aux <- Aux %>% separate(rules, into = c('antecedente', 'consequente'), sep = ' => ')
  #Poda por cobertura
  Saida <- Aux[0,]
   for (i in 1 : length(Entrada_Aux[,1])) {
     elem <- gsub(".*[{]([^.]+)[}].*", "\\1", Entrada_Aux[i,2])
     flag <- 0
     for (j in 1 : length(Entrada_Aux[,1])) {
       if((i != j) && (grepl(elem, Entrada_Aux[j,2]) == TRUE) && (Entrada_Aux[i,3] %in% Entrada_Aux[j,3]) == TRUE){
         flag <- 1
         Entrada_Aux <- Entrada_Aux[-c(j),]
       }
     }
     if (flag == 1){
      Saida <- rbind(Saida, Entrada_Aux[i,])
     }
   }
  print(length(Saida[,1]))
  #eliminando os paradoxo de Simpson
   for (i in 1 : length(Saida[,1])) {
    flag <- 0
    if(i > length(Saida[,1])){
      break
     }
    for (j in 1 : length(Saida[,1])) {
      if(j > length(Saida[,1])){
        break
      }
      if((all(Saida[i,2] == Saida[j,3])) && (all(Saida[i,3] == Saida[j,2]))){
        Saida <- Saida[-c(j),]
        flag <- 1
      }
    }
   if (flag == 1){
     Saida <- Saida[-c(i),]
   }
   }
  print(length(Saida[,1]))
  return(Saida)
}

# --------------- aplicando a função de poda nas base de regras:
Reducao_Regras <- Poda_Regras(Regras)
view(Reducao_Regras)
#length(Regras[,1])
#length(Reducao_Regras[,1])
write.csv(Reducao_Regras, "Regras_Poda.csv")
