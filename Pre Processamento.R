#----------------------------------------------------------------------------------------------------------

#                                         BASE DE DADOS DO ENADE 2017

#----------------------------------------------------------------------------------------------------------

###########################################################################################################

#----------------------------------------------------------------------------------------------------------

#                                         PRE-PROCESSAMENTO

#----------------------------------------------------------------------------------------------------------


#CARREGANDO/INSTALANDO PACOTES

library(tidyverse)
library(readr)

#CARREGANDO A BASE DE DADOS PARA O R

# --------------- base original oficial:
DadosEnade2017 <- read.csv("dados.csv")
#view(DadosEnade2017)


#SELECIONANDO AS VARIÁVEIS NECESSARIAS

Enade2017_Selecao <- DadosEnade2017 %>% select(-(1:2),-4, -6, -8, -(16:33), -(36:44), -(49:69), -(72:76), -(78:150))
#view(Enade2017_Selecao)

#LIMPEZA DA BASE

# --------------- eliminando candidatos ausentes:
Enade2017_Limpo <- Enade2017_Selecao %>% filter(TP_PRES != 222 & TP_PR_GER != 222)
#view(Enade2017_Limpo)

# --------------- mantendo apenas candidatos presentes com provas válidas:
Enade2017_Limpo2 <- Enade2017_Limpo %>% filter(TP_PRES == 555 & TP_PR_GER == 555)
#view(Enade2017_Limpo2)

# --------------- selecionando uma regiao do pais
Enade2017_Regiao <- Enade2017_Limpo2 %>% filter(CO_REGIAO_CURSO == 4) #Região Sul
#view(Enade2017_Regiao)

# --------------- selecionando por curso: Todos os cursos, menos as Engenharias
Enade2017_curso <- Enade2017_Regiao %>% filter(CO_GRUPO == 21 | 
                                                 CO_GRUPO == 701 | 
                                                 CO_GRUPO == 702 |
                                                 CO_GRUPO == 903 |
                                                 CO_GRUPO == 904 |
                                                 CO_GRUPO == 905 |
                                                 CO_GRUPO == 906 |
                                                 CO_GRUPO == 1401 |
                                                 CO_GRUPO == 1402 |
                                                 CO_GRUPO == 1501 |
                                                 CO_GRUPO == 1502 |
                                                 CO_GRUPO == 1602 |
                                                 CO_GRUPO == 1602 |
                                                 CO_GRUPO == 2001 |
                                                 CO_GRUPO == 1402 |
                                                 CO_GRUPO == 2401 |
                                                 CO_GRUPO == 2402 |
                                                 CO_GRUPO == 2501 |
                                                 CO_GRUPO == 3001 |
                                                 CO_GRUPO == 3002 |
                                                 CO_GRUPO == 3201 |
                                                 CO_GRUPO == 3202 |
                                                 CO_GRUPO == 3502) 
#view(Enade2017_curso)                                                 

# --------------- eliminhando as colunas de presenca
Enade2017_Oficial <- Enade2017_curso %>% select(-11, -12)
#view(Enade2017_Oficial)

# --------------- eliminhando campos com NA
Enade2017_Oficial <- Enade2017_Oficial[!(Enade2017_Oficial$QE_I02 == ""), ]

ApagarLinhas <- c()

for (i in 1: length(Enade2017_Oficial[,1])) {
  for (j in 1: length(Enade2017_Oficial[1,])){
    if((Enade2017_Oficial[i,j] == "" | is.na(Enade2017_Oficial[i,j]))){
      ApagarLinhas <- c(ApagarLinhas,i)
      break
    }
  }
}

ApagarLinhas
length(ApagarLinhas)
ApagarLinhas2 <- sort(ApagarLinhas, decreasing = TRUE)
ApagarLinhas2

Enade2017_Oficial_2 <- Enade2017_Oficial

for (k in ApagarLinhas2) {
  #Enade2017_Oficial_2 <- removeRows(i, Enade2017_Oficial)
  Enade2017_Oficial_2 <- Enade2017_Oficial_2[-k, , drop = FALSE]
}


# --------------- Salvando a base pre processada em arquivo csv
write.csv(Enade2017_Oficial_2, "BaseProc_E2017.csv")

