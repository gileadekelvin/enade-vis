import_code_raca <- function(){
    library(tidyverse)
    
    cor_raca <- data.frame(codigo = c("A", "B", "C", "D", "E", "F"), 
                           cor_raca = c("Branca", "Preta", "Amarela", "Parda", "Indígena", "Não quero declarar"), stringsAsFactors = FALSE)
    
    return(cor_raca)
}

import_code_renda <- function(){
    library(tidyverse)
    
    renda <- data.frame(codigo = c("A", "B", "C", "D", "E", "F", "G"), 
                        renda_valor = c("até R$ 1.405,50", "R$ 1.405,51 a R$ 2.811,00", "R$ 2.811,01 a R$ 4.216,50", "R$ 4.216,51 a R$ 5.622,00", 
                                  "R$ 5. 622,01 a R$ 9.370,00", "R$ 9.370,01 a R$ 28.110,00", "mais de R$ 28.110,00"), 
                        renda = c("Baixíssima", "Baixa", "Média-baixa", "Média", "Média-alta", "Alta", "Altíssima"),
                        renda_num = c(1, 2, 3, 4, 5, 6, 7),
                        stringsAsFactors = FALSE)
    return(renda)
}

import_code_cotas <- function(){
    library(tidyverse)
    
    cotas <- data.frame(codigo = c("A", "B", "C", "D", "E", "F"), 
                        cota = c("Não", "étnico-racial", "renda", "escola pública", "mais de uma cota", "outra cota"), stringsAsFactors = FALSE)

    return(cotas)
}

import_code_ensino_medio <- function(){
    library(tidyverse)
    
    ensino_medio <- data.frame(codigo = c("A", "B", "C", "D", "E", "F"),
                               ensino_medio = c("Todo escola pública", "Todo escola particular", "Todo exterior", "Maior parte escola pública",
                                                "Maior parte escola particular", "Parte no Brasil e parte no exterior"), stringsAsFactors = FALSE)
    
    return(ensino_medio)
}

import_code_motivo <- function(){
    library(tidyverse)
    
    motivo <- data.frame(codigo = c("A", "B", "C", "D", "E", "F", "G", "H"),
                               motivo = c("Inserção no Mercado de Trabalho", "Influência familiar", "Valorização Profissional", "Prestígio Social",
                                                "Vocação", "Oferecido EAD", "Baixa concorrência", "Outro"), stringsAsFactors = FALSE)
    
    return(motivo)
}

import_dados_ufcg_computacao <- function() {
    library(tidyverse)
    library(here)
    
    raw <- read.csv(here("data/enade_2017_ufcg.csv"), stringsAsFactors = FALSE)
    
    cor_raca <- import_code_raca()
    renda <- import_code_renda()
    cotas <- import_code_cotas()
    ensino_medio <- import_code_ensino_medio()
    motivo <- import_code_motivo()
    
    dados_ufcg_computacao <- raw %>%
        select(CO_UF_CURSO, CO_IES, CO_CURSO, QE_I02, QE_I08, QE_I15, QE_I17, QE_I25) %>% 
        
        left_join(cor_raca, by = c("QE_I02" = "codigo")) %>% 
        left_join(renda, by = c("QE_I08" = "codigo")) %>% 
        left_join(cotas, by = c("QE_I15" = "codigo")) %>% 
        left_join(ensino_medio, by = c("QE_I17" = "codigo")) %>% 
        left_join(motivo, by = c("QE_I25" = "codigo")) %>% 
        select(UF = CO_UF_CURSO, cod_Inst = CO_IES, cod_Curso = CO_CURSO, cor_raca, renda_num, renda_valor, renda, cota, ensino_medio, motivo) %>% 
        unite(id_renda_motivo, renda, motivo, remove = FALSE) %>% 
        # Código de Computação na UFCG é 13446
        filter(cod_Curso == 13446)
        
    return(dados_ufcg_computacao)
}

write_data <- function(){
    library(tidyverse)
    
    import_dados_ufcg_computacao() %>% write.csv(here("data/dados_ufcg_computacao.csv"), row.names = FALSE)
}

    