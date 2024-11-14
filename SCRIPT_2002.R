# PACOTES ---------------------------------------------------------

  # Pacotes
  
  install.packages("PNADcIBGE")
  install.packages("survey")
  install.packages("convey")
  install.packages("magrittr")
  install.packages("gtsummary")
  install.packages("xfun")
  install.packages("xlsx")
  install.packages("broom")
  install.packages("knitr")
  install.packages("scales")
  install.packages("flextable")
  install.packages("rmarkdown")
  install.packages("webshot2")
  install.packages("gtExtras")
  install.packages("stringr")
  install.packages("devtools")
  install.packages("readr")
  
  # Carregar pacotes/dependências necessárias
  
  library(PNADcIBGE)
  library(survey)
  library(convey)
  library(magrittr)
  library(AER)
  library(stats)
  library(tidyverse)
  library(dplyr)
  library(gtsummary)
  library(xfun)
  library(xlsx)
  library(gt)
  library(ggplot2)
  library(broom)
  library(knitr)
  library(scales)
  library(flextable)
  library(rmarkdown)
  library(webshot2)
  library(gtExtras)
  library(stringr)
  library(lodown)
  library(readr)

# --------------------------------- 2002 ----------------------------------
# IMPORTAR DADOS --------------------------------------------------------

  # Variaveis do Modelo
  
  # Renda = V4718
  # Cor ou Raça = V0404
  # Sexo = V0302
  # Idade = V8005
  # Anos de escolaridade = V4703
  
  # Variaveis Instrumentais
  
  # Unidade da federação = UF  
  # Horas efetivamente trabalhadas = V9058
  # Codigo da Ocupação = V9906
  # Carteira Assinada = V4706

  # Importar dados
  
  # Coletar microdados de 2002
  
  Imagens <- "C:/Users/Arthur/Documents/PROJETO R/Imagens"
  setwd(Imagens)
  
  pnad2002 <- read_fwf(file = "PES2002.txt", fwf_cols(UF = c(5, 6),
                                                      Cor = c(33, 33), 
                                                      Sexo = c(18, 18), 
                                                      Idade = c(27, 29), 
                                                      Ano_Esc = c(666,667),
                                                      Renda = c(688, 699),
                                                      Carg_Hor = c(355, 356),
                                                      Cod_Ocup = c(137, 140),
                                                      Cart_Assi = c(670, 671)))
  
# TRATAMENTO DA BASE DE DADOS ------------------------------------------------------
  
  # Ajusta o tipos de das variaveis
  
    pnad2002$UF <- as.character(pnad2002$UF)
    pnad2002$Cor <- as.numeric(pnad2002$Cor)
    pnad2002$Sexo <- as.numeric(pnad2002$Sexo)
    pnad2002$Idade <- as.numeric(pnad2002$Idade)   
    pnad2002$Ano_Esc <- as.numeric(pnad2002$Ano_Esc)    
    pnad2002$Renda <- as.double(pnad2002$Renda)
    pnad2002$Carg_Hor <- as.numeric(pnad2002$Carg_Hor)
    pnad2002$Cod_Ocup <- as.numeric(pnad2002$Cod_Ocup)
    pnad2002$Cart_Assi <- as.character(pnad2002$Cart_Assi)    
    
    # 1 - Padronização da Variavel Renda por 40 horas
    
      # 1.1 - Filtrar Carga Horaria "0" e Anos de Estudo = Não determinados e sem declaração
      
        pnad2002 <- pnad2002 %>%
          filter(Carg_Hor != "0")
        
        pnad2002 <- pnad2002 %>%
          filter(Ano_Esc != 17)
    
    # 1.2 - RendaE = (Renda/Carga)*40
    
      pnad2002 <- pnad2002 %>%
        rowwise() %>%
        mutate(RendaE = (Renda / Carg_Hor) * 40)
      pnad2002$RendaE <- as.integer(pnad2002$RendaE)  
    
    # 2 - Filtrar os individuos que são de BA e SP
      
      UF_BA_2002 <- 29
      UF_SP_2002 <- 35
      
      pnad2002 <- pnad2002 %>%
        filter(UF == UF_BA_2002 | UF == UF_SP_2002)
    
    # 3 - Filtrar individuos que não são Pretos, Pardos ou Brancos
    
      Cor_Preta_2002 <- 4
      Cor_Branca_2002 <- 2
      Cor_Parda_2002 <- 8
      
      pnad2002 <- pnad2002 %>%
        filter(Cor == Cor_Preta_2002 | Cor == Cor_Branca_2002 | Cor == Cor_Parda_2002)
    
    # 4 - Filtrar Rendas NA    
    
      pnad2002 <- pnad2002 %>%
        filter(RendaE != 0)
    
    # 5 - Categorias ou/e grupos de trabalhadores
    
      # Classe: DG = Diregentes e gerentes; NA = Outros
      # Registro: ECR = 1 = Sim; ESR = 2 = Não
      # Primeiro_Quintil: PQ = Sim;  NA = Não
    
        # Codigos de Colunas
    
          # 5.1 - Classe
    
            # DG = Diregentes e gerentes; NA = Outros
    
              pnad2002 <- pnad2002 %>%
                rowwise() %>%
                mutate(Classe = case_when(Cod_Ocup <= 1320 & Cod_Ocup >= 1111 ~ 'DG'))
    
          # 5.2 - Registro
              
            # 01 = Empregado com carteira = 1
            # 02 = Militar = 1
            # 03 = Funcionário público estatutário = 1
            # 04 = Outros Empregados sem carteira = 2
            # 05 = Empregados sem declaração de carteira = NA
            # 06 = Trabalhador doméstico com carteira = 1
            # 07 = Trabalhador doméstico sem carteira = 2
            # 08 = Trabalhador doméstico sem declaração de carteira = NA
            # 09 = Conta-própria = 2
            # 10 = Empregador = 1
            # 11 = Trabalhador na produção para o próprio consumo = 2
            # 12 = Trabalhador na construção para o próprio uso = 2
            # 13 = Não remunerado = 2
            # 14 = Sem declaração = NA
              
            # ECR = 1 = Sim; ESR = 2 = Não
    
              pnad2002$Cart_Assi <- as.character(pnad2002$Cart_Assi)
              
              pnad2002 <- pnad2002 %>%
                rowwise() %>%
                mutate(Registro = case_when(Cart_Assi == '01' ~ 'ECR',
                                            Cart_Assi == '02' ~ 'ECR',
                                            Cart_Assi == '03' ~ 'ECR',
                                            Cart_Assi == '04' ~ 'ESR',
                                            Cart_Assi == '05' ~ NA,
                                            Cart_Assi == '06' ~ 'ECR',
                                            Cart_Assi == '07' ~ 'ESR',
                                            Cart_Assi == '08' ~ NA,
                                            Cart_Assi == '09' ~ 'ESR',
                                            Cart_Assi == '10' ~ 'ECR',
                                            Cart_Assi == '11' ~ 'ESR',
                                            Cart_Assi == '12' ~ 'ESR',
                                            Cart_Assi == '13' ~ 'ESR',
                                            Cart_Assi == '14' ~ NA))
              
              pnad2002 <- pnad2002 %>%
                rowwise() %>%
                mutate(ECR = case_when(Registro == 'ECR' ~ 'ECR'), 
                       ESR = case_when(Registro == 'ESR' ~ 'ESR'))
    
          # 5.3 - Primeiro Quintil
    
            # PQ = Sim;  NA = Não
    
              # 5.3.1 - Atribuir parametro de primeiro quintil por Estado
    
                pnad2002_BA <- pnad2002 %>%
                  filter(UF == UF_BA_2002)
                quantile(pnad2002_BA$RendaE, 
                         probs = 0.2)
                PQ_RendaE_BA_2002 <- quantile(pnad2002_BA$RendaE, 
                                              probs = 0.2)
                
                pnad2002_SP <- pnad2002 %>%
                  filter(UF == UF_SP_2002)
                quantile(pnad2002_SP$RendaE, 
                         probs = 0.2)
                PQ_RendaE_SP_2002 <- quantile(pnad2002_SP$RendaE, 
                                              probs = 0.2)
    
              # 5.3.2 - Criar a nova Variavel por Estado
    
                pnad2002 <- pnad2002 %>%
                  rowwise() %>%
                  mutate(Prim_Quintil = case_when(RendaE <= PQ_RendaE_SP_2002 & 
                                                    UF == UF_SP_2002 ~ 'PQ', 
                                                  RendaE <= PQ_RendaE_BA_2002 &
                                                    UF == UF_BA_2002 ~ 'PQ'))

# ANÁLISE EXPLORATÓRIA DOS DADOS ------------------------------------------
                
                # Análise de dados
                
                # 1 - Dados Populacionais    
                
                # Agrupando: Parda = 4 e Negra = 2
                
                Negros <- 1
                Brancos <- 0
                Homem <- 0
                Mulher <- 1
                
                pnad2002 <- pnad2002 %>%
                  rowwise %>%
                  mutate(Cor = case_when(Cor == Cor_Parda_2002 ~ Negros, 
                                         Cor == Cor_Preta_2002 ~ Negros, 
                                         Cor == Cor_Branca_2002 ~ Brancos),
                         Sexo = case_when(Sexo == 4 ~ Mulher,
                                          Sexo == 2 ~ Homem))
                
                # Mudança de Diretório
                
                Imagens <- "C:/Users/Arthur/Documents/PROJETO R/Imagens"
                setwd(Imagens)
                
                # TABELA 1
                
                # Transformações para tabela
                
                pnad2002 <- pnad2002 %>%
                  rowwise %>%
                  mutate(UF_l = case_when(UF == UF_BA_2002 ~ "Bahia", UF == UF_SP_2002 ~ "São Paulo"),
                         Cor_l = case_when(Cor == Negros ~ "0_Negros", Cor == Brancos ~ "1_Brancos",),
                         Classe_l = case_when(Classe == "DG" ~ "Dirigentes e Gerentes"),
                         ECR_l = case_when(ECR == "ECR" ~ "Empregados Com Registro"),
                         ESR_l = case_when(ESR == "ESR" ~ "Empregados Sem Registro"),
                         Prim_Quintil_l = case_when(Prim_Quintil == "PQ" ~ "Primeiro Quintil"),
                         Sexo_l = case_when(Sexo == Mulher ~ "Mulher", Sexo == Homem ~ "Homem"),
                         UF2_l = case_when(UF == UF_BA_2002 ~ "População", UF == UF_SP_2002 ~ "População"))
                
                # Configurações da tabela        
                
                #theme_gtsummary_reset(set_theme = FALSE, font_size = 20)
                theme_gtsummary_language(language = "pt", big.mark = ".", decimal.mark = ",")
                
                POP_2002 <- pnad2002 %>%
                  select(UF_l,
                         Cor_l,
                         Classe_l,
                         ECR_l,
                         ESR_l,
                         Prim_Quintil_l,
                         UF2_l) %>%
                  tbl_strata(
                    strata = UF_l,
                    .tbl_fun =
                      ~.x %>%
                      tbl_summary(by = Cor_l,
                                  sort = list(everything() ~ "alphanumeric"),
                                  label = list(Classe_l ~ NULL, 
                                               ECR_l ~ NULL, 
                                               ESR_l ~ NULL, 
                                               Prim_Quintil_l ~ NULL),
                                  missing = "no", 
                                  percent = "row",
                                  statistic = list(all_categorical() ~ "{p}"),
                                  digits = list(Classe_l ~ c(1, 1), 
                                                ECR_l ~ c(1, 1), 
                                                ESR_l ~ c(1, 1), 
                                                Prim_Quintil_l ~ c(1, 1),
                                                UF2_l ~ c(1,1)))) %>%
                  modify_header(label = "Grupos",
                                stat_1_1 = "Negros",
                                stat_2_1 = "Brancos",
                                stat_1_2 = "Negros",
                                stat_2_2 = "Brancos") %>%
                  modify_table_body(
                    ~ .x %>% 
                      dplyr::filter(!(variable %in% "Classe_l" & row_type %in% "label"),
                                    !(variable %in% "ECR_l" & row_type %in% "label"),
                                    !(variable %in% "ESR_l" & row_type %in% "label"),
                                    !(variable %in% "Prim_Quintil_l" & row_type %in% "label"),
                                    !(variable %in% "UF2_l" & row_type %in% "label"))) %>%
                  modify_spanning_header(c(stat_1_1,
                                           stat_2_1) ~ "Bahia", 
                                         c(stat_1_2, 
                                           stat_2_2) ~ "São Paulo") %>%
                  modify_footnote(update = everything() ~ NA) %>%
                  as_gt() %>%
                  gt::tab_options(table.font.names = "Arial Narrow",
                                  heading.border.bottom.color = "black",
                                  column_labels.border.top.color = "black",
                                  column_labels.border.bottom.color = "black",
                                  table_body.border.bottom.color = "black",
                                  table_body.border.top.color = "black",
                                  table_body.hlines.style = "white",
                                  table.font.color = "black",
                                  table.border.top.color = "black",
                                  table.border.bottom.color = "black")
                
                # Visualizar a tabela
                POP_2002
                
                # Salvar a tabela
                gtsave(POP_2002, filename = "POP_2002.png")
                
                # TABELA 2
                
                # Transformações para tabela
                
                pnad2002 <- pnad2002 %>%
                  rowwise %>%
                  mutate(Classe_R = case_when(Classe == "DG" ~ RendaE),
                         ECR_R = case_when(ECR == "ECR" ~ RendaE),
                         ESR_R = case_when(ESR == "ESR" ~ RendaE),
                         Prim_Quintil_R = case_when(Prim_Quintil == "PQ" ~ RendaE))
                
                # Configurações da tabela
                
                RMEAN_2002 <- pnad2002 %>%
                  select(UF_l,
                         Cor_l,
                         Sexo_l,
                         Classe_R,
                         ECR_R,
                         ESR_R,
                         Prim_Quintil_R) %>%
                  tbl_strata(strata = c(UF_l, Cor_l),
                             .tbl_fun =
                               ~.x %>%
                               tbl_summary(by = Sexo_l,
                                           missing = "no",
                                           statistic = all_continuous() ~ "{mean}",
                                           label = list(Classe_R ~ "Diregentes e Gerentes", 
                                                        ECR_R ~ "Empregados Com Registro", 
                                                        ESR_R ~ "Empregados Sem Registro", 
                                                        Prim_Quintil_R ~ "Primeiro Quintil"),
                                           digits = list(Classe_R ~ c(2), 
                                                         ECR_R ~ c(2), 
                                                         ESR_R ~ c(2), 
                                                         Prim_Quintil_R ~ c(2)))) %>%
                  modify_header(label = "Renda Média",
                                stat_1_1 = "Homem",
                                stat_2_1 = "Mulher",
                                stat_1_2 = "Homem",
                                stat_2_2 = "Mulher",
                                stat_1_3 = "Homem",
                                stat_2_3 = "Mulher",
                                stat_1_4 = "Homem",
                                stat_2_4 = "Mulher") %>%
                  modify_spanning_header(c(stat_1_1, 
                                           stat_2_1) ~ "Negros",
                                         c(stat_1_2, 
                                           stat_2_2) ~ "Brancos",
                                         c(stat_1_3, 
                                           stat_2_3) ~ "Negros", 
                                         c(stat_1_4, 
                                           stat_2_4) ~ "Brancos") %>%
                  modify_footnote(update = everything() ~ NA) %>%
                  as_gt() %>%
                  gt::tab_options(table.font.names = "Arial Narrow",
                                  heading.border.bottom.color = "black",
                                  column_labels.border.top.color = "black",
                                  column_labels.border.bottom.color = "black",
                                  table_body.border.bottom.color = "black",
                                  table_body.border.top.color = "black",
                                  table_body.hlines.style = "white",
                                  table.font.color = "black",
                                  table.border.top.color = "black",
                                  table.border.bottom.color = "black") %>%
                  tab_spanner(label = "Bahia", columns = c(stat_1_1, 
                                                           stat_2_1, 
                                                           stat_1_2, 
                                                           stat_2_2)) %>%
                  tab_spanner(label = "São Paulo", columns = c(stat_1_3, 
                                                               stat_2_3,
                                                               stat_1_4, 
                                                               stat_2_4))
                
                
                # Visualizar a tabela
                RMEAN_2002
                
                # Salvar a tabela
                gtsave(RMEAN_2002, filename = "RMEAN_2002.png")
                
                # TABELA 3
                
                # Configurações da tabela
                
                RMEDIAN_2002 <- pnad2002 %>%
                  select(UF_l,
                         Cor_l,
                         Sexo_l,
                         Classe_R,
                         ECR_R,
                         ESR_R,
                         Prim_Quintil_R) %>%
                  tbl_strata(strata = c(UF_l, Cor_l),
                             ~.x %>%
                               tbl_summary(by = Sexo_l,
                                           missing = "no",
                                           statistic = all_continuous() ~ "{median}",
                                           label = list(Classe_R ~ "Diregentes e Gerentes", 
                                                        ECR_R ~ "Empregados Com Registro", 
                                                        ESR_R ~ "Empregados Sem Registro", 
                                                        Prim_Quintil_R ~ "Primeiro Quintil"),
                                           digits = list(Classe_R ~ c(2), 
                                                         ECR_R ~ c(2), 
                                                         ESR_R ~ c(2), 
                                                         Prim_Quintil_R ~ c(2)))) %>%
                  modify_header(label = "Renda Mediana",
                                stat_1_1 = "Homem",
                                stat_2_1 = "Mulher",
                                stat_1_2 = "Homem",
                                stat_2_2 = "Mulher",
                                stat_1_3 = "Homem",
                                stat_2_3 = "Mulher",
                                stat_1_4 = "Homem",
                                stat_2_4 = "Mulher") %>%
                  modify_spanning_header(c(stat_1_1, 
                                           stat_2_1) ~ "Negros",
                                         c(stat_1_2, 
                                           stat_2_2) ~ "Brancos",
                                         c(stat_1_3, 
                                           stat_2_3) ~ "Negros", 
                                         c(stat_1_4, 
                                           stat_2_4) ~ "Brancos") %>%
                  modify_footnote(update = everything() ~ NA) %>%
                  as_gt() %>%
                  gt::tab_options(table.font.names = "Arial Narrow",
                                  heading.border.bottom.color = "black",
                                  column_labels.border.top.color = "black",
                                  column_labels.border.bottom.color = "black",
                                  table_body.border.bottom.color = "black",
                                  table_body.border.top.color = "black",
                                  table_body.hlines.style = "white",
                                  table.font.color = "black",
                                  table.border.top.color = "black",
                                  table.border.bottom.color = "black") %>%
                  tab_spanner(label = "Bahia", columns = c(stat_1_1, 
                                                           stat_2_1, 
                                                           stat_1_2, 
                                                           stat_2_2)) %>%
                  tab_spanner(label = "São Paulo", columns = c(stat_1_3, 
                                                               stat_2_3,
                                                               stat_1_4, 
                                                               stat_2_4))
                
                # Visualizar a tabela
                RMEDIAN_2002
                
                # Salvar a tabela
                gtsave(RMEDIAN_2002, filename = "RMEDIAN_2002.png")
                
# MODELAGEM ---------------------------------------------------------------
      
      # PARAMETROS BINARIOS DE RENDA ESPECIFICADA
      
      UM <- 1
      ZERO <- 0
      
      # VETOR DE RENDA ESPECFICADA
      
      Y <- as_data_frame(seq(from = 100, to = 10000, by = 100))
      names(Y) = c("Renda")
      
      # ADIÇAO DOS VETORES PARAMETRIZADOS PELA RENDA ESPECIFICADA
      
      TBL_ENS_2002 <- pnad2002 %>%
        select(UF, Cor, Sexo, Idade, Ano_Esc, Classe, ECR, ESR, Prim_Quintil, RendaE) %>%
        mutate(Renda_1 = case_when(RendaE > Y$Renda[1] ~ UM, RendaE <= Y$Renda[1] ~ ZERO),
               Renda_2 = case_when(RendaE > Y$Renda[2] ~ UM, RendaE <= Y$Renda[2] ~ ZERO),
               Renda_3 = case_when(RendaE > Y$Renda[3] ~ UM, RendaE <= Y$Renda[3] ~ ZERO),
               Renda_4 = case_when(RendaE > Y$Renda[4] ~ UM, RendaE <= Y$Renda[4] ~ ZERO),
               Renda_5 = case_when(RendaE > Y$Renda[5] ~ UM, RendaE <= Y$Renda[5] ~ ZERO),
               Renda_6 = case_when(RendaE > Y$Renda[6] ~ UM, RendaE <= Y$Renda[6] ~ ZERO),
               Renda_7 = case_when(RendaE > Y$Renda[7] ~ UM, RendaE <= Y$Renda[7] ~ ZERO),
               Renda_8 = case_when(RendaE > Y$Renda[8] ~ UM, RendaE <= Y$Renda[8] ~ ZERO),
               Renda_9 = case_when(RendaE > Y$Renda[9] ~ UM, RendaE <= Y$Renda[9] ~ ZERO),
               Renda_10 = case_when(RendaE > Y$Renda[10] ~ UM, RendaE <= Y$Renda[10] ~ ZERO),
               Renda_11 = case_when(RendaE > Y$Renda[11] ~ UM, RendaE <= Y$Renda[11] ~ ZERO),
               Renda_12 = case_when(RendaE > Y$Renda[12] ~ UM, RendaE <= Y$Renda[12] ~ ZERO),
               Renda_13 = case_when(RendaE > Y$Renda[13] ~ UM, RendaE <= Y$Renda[13] ~ ZERO),
               Renda_14 = case_when(RendaE > Y$Renda[14] ~ UM, RendaE <= Y$Renda[14] ~ ZERO),
               Renda_15 = case_when(RendaE > Y$Renda[15] ~ UM, RendaE <= Y$Renda[15] ~ ZERO),
               Renda_16 = case_when(RendaE > Y$Renda[16] ~ UM, RendaE <= Y$Renda[16] ~ ZERO),
               Renda_17 = case_when(RendaE > Y$Renda[17] ~ UM, RendaE <= Y$Renda[17] ~ ZERO),
               Renda_18 = case_when(RendaE > Y$Renda[18] ~ UM, RendaE <= Y$Renda[18] ~ ZERO),
               Renda_19 = case_when(RendaE > Y$Renda[19] ~ UM, RendaE <= Y$Renda[19] ~ ZERO),
               Renda_20 = case_when(RendaE > Y$Renda[20] ~ UM, RendaE <= Y$Renda[20] ~ ZERO),
               Renda_21 = case_when(RendaE > Y$Renda[21] ~ UM, RendaE <= Y$Renda[21] ~ ZERO),
               Renda_22 = case_when(RendaE > Y$Renda[22] ~ UM, RendaE <= Y$Renda[22] ~ ZERO),
               Renda_23 = case_when(RendaE > Y$Renda[23] ~ UM, RendaE <= Y$Renda[23] ~ ZERO),
               Renda_24 = case_when(RendaE > Y$Renda[24] ~ UM, RendaE <= Y$Renda[24] ~ ZERO),
               Renda_25 = case_when(RendaE > Y$Renda[25] ~ UM, RendaE <= Y$Renda[25] ~ ZERO),
               Renda_26 = case_when(RendaE > Y$Renda[26] ~ UM, RendaE <= Y$Renda[26] ~ ZERO),
               Renda_27 = case_when(RendaE > Y$Renda[27] ~ UM, RendaE <= Y$Renda[27] ~ ZERO),
               Renda_28 = case_when(RendaE > Y$Renda[28] ~ UM, RendaE <= Y$Renda[28] ~ ZERO),
               Renda_29 = case_when(RendaE > Y$Renda[29] ~ UM, RendaE <= Y$Renda[29] ~ ZERO),
               Renda_30 = case_when(RendaE > Y$Renda[30] ~ UM, RendaE <= Y$Renda[30] ~ ZERO),
               Renda_31 = case_when(RendaE > Y$Renda[31] ~ UM, RendaE <= Y$Renda[31] ~ ZERO),
               Renda_32 = case_when(RendaE > Y$Renda[32] ~ UM, RendaE <= Y$Renda[32] ~ ZERO),
               Renda_33 = case_when(RendaE > Y$Renda[33] ~ UM, RendaE <= Y$Renda[33] ~ ZERO),
               Renda_34 = case_when(RendaE > Y$Renda[34] ~ UM, RendaE <= Y$Renda[34] ~ ZERO),
               Renda_35 = case_when(RendaE > Y$Renda[35] ~ UM, RendaE <= Y$Renda[35] ~ ZERO),
               Renda_36 = case_when(RendaE > Y$Renda[36] ~ UM, RendaE <= Y$Renda[36] ~ ZERO),
               Renda_37 = case_when(RendaE > Y$Renda[37] ~ UM, RendaE <= Y$Renda[37] ~ ZERO),
               Renda_38 = case_when(RendaE > Y$Renda[38] ~ UM, RendaE <= Y$Renda[38] ~ ZERO),
               Renda_39 = case_when(RendaE > Y$Renda[39] ~ UM, RendaE <= Y$Renda[39] ~ ZERO),
               Renda_40 = case_when(RendaE > Y$Renda[40] ~ UM, RendaE <= Y$Renda[40] ~ ZERO),
               Renda_41 = case_when(RendaE > Y$Renda[41] ~ UM, RendaE <= Y$Renda[41] ~ ZERO),
               Renda_42 = case_when(RendaE > Y$Renda[42] ~ UM, RendaE <= Y$Renda[42] ~ ZERO),
               Renda_43 = case_when(RendaE > Y$Renda[43] ~ UM, RendaE <= Y$Renda[43] ~ ZERO),
               Renda_44 = case_when(RendaE > Y$Renda[44] ~ UM, RendaE <= Y$Renda[44] ~ ZERO),
               Renda_45 = case_when(RendaE > Y$Renda[45] ~ UM, RendaE <= Y$Renda[45] ~ ZERO),
               Renda_46 = case_when(RendaE > Y$Renda[46] ~ UM, RendaE <= Y$Renda[46] ~ ZERO),
               Renda_47 = case_when(RendaE > Y$Renda[47] ~ UM, RendaE <= Y$Renda[47] ~ ZERO),
               Renda_48 = case_when(RendaE > Y$Renda[48] ~ UM, RendaE <= Y$Renda[48] ~ ZERO),
               Renda_49 = case_when(RendaE > Y$Renda[49] ~ UM, RendaE <= Y$Renda[49] ~ ZERO),
               Renda_50 = case_when(RendaE > Y$Renda[50] ~ UM, RendaE <= Y$Renda[50] ~ ZERO),
               Renda_51 = case_when(RendaE > Y$Renda[51] ~ UM, RendaE <= Y$Renda[51] ~ ZERO),
               Renda_52 = case_when(RendaE > Y$Renda[52] ~ UM, RendaE <= Y$Renda[52] ~ ZERO),
               Renda_53 = case_when(RendaE > Y$Renda[53] ~ UM, RendaE <= Y$Renda[53] ~ ZERO),
               Renda_54 = case_when(RendaE > Y$Renda[54] ~ UM, RendaE <= Y$Renda[54] ~ ZERO),
               Renda_55 = case_when(RendaE > Y$Renda[55] ~ UM, RendaE <= Y$Renda[55] ~ ZERO),
               Renda_56 = case_when(RendaE > Y$Renda[56] ~ UM, RendaE <= Y$Renda[56] ~ ZERO),
               Renda_57 = case_when(RendaE > Y$Renda[57] ~ UM, RendaE <= Y$Renda[57] ~ ZERO),
               Renda_58 = case_when(RendaE > Y$Renda[58] ~ UM, RendaE <= Y$Renda[58] ~ ZERO),
               Renda_59 = case_when(RendaE > Y$Renda[59] ~ UM, RendaE <= Y$Renda[59] ~ ZERO),
               Renda_60 = case_when(RendaE > Y$Renda[60] ~ UM, RendaE <= Y$Renda[60] ~ ZERO),
               Renda_61 = case_when(RendaE > Y$Renda[61] ~ UM, RendaE <= Y$Renda[61] ~ ZERO),
               Renda_62 = case_when(RendaE > Y$Renda[62] ~ UM, RendaE <= Y$Renda[62] ~ ZERO),
               Renda_63 = case_when(RendaE > Y$Renda[63] ~ UM, RendaE <= Y$Renda[63] ~ ZERO),
               Renda_64 = case_when(RendaE > Y$Renda[64] ~ UM, RendaE <= Y$Renda[64] ~ ZERO),
               Renda_65 = case_when(RendaE > Y$Renda[65] ~ UM, RendaE <= Y$Renda[65] ~ ZERO),
               Renda_66 = case_when(RendaE > Y$Renda[66] ~ UM, RendaE <= Y$Renda[66] ~ ZERO),
               Renda_67 = case_when(RendaE > Y$Renda[67] ~ UM, RendaE <= Y$Renda[67] ~ ZERO),
               Renda_68 = case_when(RendaE > Y$Renda[68] ~ UM, RendaE <= Y$Renda[68] ~ ZERO),
               Renda_69 = case_when(RendaE > Y$Renda[69] ~ UM, RendaE <= Y$Renda[69] ~ ZERO),
               Renda_70 = case_when(RendaE > Y$Renda[70] ~ UM, RendaE <= Y$Renda[70] ~ ZERO),
               Renda_71 = case_when(RendaE > Y$Renda[71] ~ UM, RendaE <= Y$Renda[71] ~ ZERO),
               Renda_72 = case_when(RendaE > Y$Renda[72] ~ UM, RendaE <= Y$Renda[72] ~ ZERO),
               Renda_73 = case_when(RendaE > Y$Renda[73] ~ UM, RendaE <= Y$Renda[73] ~ ZERO),
               Renda_74 = case_when(RendaE > Y$Renda[74] ~ UM, RendaE <= Y$Renda[74] ~ ZERO),
               Renda_75 = case_when(RendaE > Y$Renda[75] ~ UM, RendaE <= Y$Renda[75] ~ ZERO),
               Renda_76 = case_when(RendaE > Y$Renda[76] ~ UM, RendaE <= Y$Renda[76] ~ ZERO),
               Renda_77 = case_when(RendaE > Y$Renda[77] ~ UM, RendaE <= Y$Renda[77] ~ ZERO),
               Renda_78 = case_when(RendaE > Y$Renda[78] ~ UM, RendaE <= Y$Renda[78] ~ ZERO),
               Renda_79 = case_when(RendaE > Y$Renda[79] ~ UM, RendaE <= Y$Renda[79] ~ ZERO),
               Renda_80 = case_when(RendaE > Y$Renda[80] ~ UM, RendaE <= Y$Renda[80] ~ ZERO),
               Renda_81 = case_when(RendaE > Y$Renda[81] ~ UM, RendaE <= Y$Renda[81] ~ ZERO),
               Renda_82 = case_when(RendaE > Y$Renda[82] ~ UM, RendaE <= Y$Renda[82] ~ ZERO),
               Renda_83 = case_when(RendaE > Y$Renda[83] ~ UM, RendaE <= Y$Renda[83] ~ ZERO),
               Renda_84 = case_when(RendaE > Y$Renda[84] ~ UM, RendaE <= Y$Renda[84] ~ ZERO),
               Renda_85 = case_when(RendaE > Y$Renda[85] ~ UM, RendaE <= Y$Renda[85] ~ ZERO),
               Renda_86 = case_when(RendaE > Y$Renda[86] ~ UM, RendaE <= Y$Renda[86] ~ ZERO),
               Renda_87 = case_when(RendaE > Y$Renda[87] ~ UM, RendaE <= Y$Renda[87] ~ ZERO),
               Renda_88 = case_when(RendaE > Y$Renda[88] ~ UM, RendaE <= Y$Renda[88] ~ ZERO),
               Renda_89 = case_when(RendaE > Y$Renda[89] ~ UM, RendaE <= Y$Renda[89] ~ ZERO),
               Renda_90 = case_when(RendaE > Y$Renda[90] ~ UM, RendaE <= Y$Renda[90] ~ ZERO),
               Renda_91 = case_when(RendaE > Y$Renda[91] ~ UM, RendaE <= Y$Renda[91] ~ ZERO),
               Renda_92 = case_when(RendaE > Y$Renda[92] ~ UM, RendaE <= Y$Renda[92] ~ ZERO),
               Renda_93 = case_when(RendaE > Y$Renda[93] ~ UM, RendaE <= Y$Renda[93] ~ ZERO),
               Renda_94 = case_when(RendaE > Y$Renda[94] ~ UM, RendaE <= Y$Renda[94] ~ ZERO),
               Renda_95 = case_when(RendaE > Y$Renda[95] ~ UM, RendaE <= Y$Renda[95] ~ ZERO),
               Renda_96 = case_when(RendaE > Y$Renda[96] ~ UM, RendaE <= Y$Renda[96] ~ ZERO),
               Renda_97 = case_when(RendaE > Y$Renda[97] ~ UM, RendaE <= Y$Renda[97] ~ ZERO),
               Renda_98 = case_when(RendaE > Y$Renda[98] ~ UM, RendaE <= Y$Renda[98] ~ ZERO),
               Renda_99 = case_when(RendaE > Y$Renda[99] ~ UM, RendaE <= Y$Renda[99] ~ ZERO),
               Renda_100 = case_when(RendaE > Y$Renda[100] ~ UM, RendaE <= Y$Renda[100] ~ ZERO)
        )

# # 1 - Primeiro Quintil - SP -------------------------------------------------
      
      # Filtro na Base a UF e a Categoria
      TBL_ENS_01_2002 <- TBL_ENS_2002 %>%
        filter(UF == UF_SP_2002, Prim_Quintil == "PQ")
      
      # Estimar os Coeficientes dos Modelos
      COEF_01_2002 <- as_tibble(rbind(
        t(coef(summary(glm(Renda_1 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_2 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_3 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_4 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_5 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_6 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_7 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_8 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_9 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_10 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_11 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_12 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_13 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_14 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_15 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_16 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_17 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_18 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_19 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_20 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_21 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_22 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_23 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_24 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_25 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_26 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_27 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_28 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_29 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_30 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_31 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_32 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_33 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_34 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_35 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_36 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_37 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_38 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_39 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_40 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_41 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_42 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_43 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_44 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_45 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_46 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_47 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_48 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_49 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_50 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_51 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_52 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_53 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_54 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_55 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_56 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_57 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_58 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_59 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_60 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_61 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_62 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_63 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_64 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_65 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_66 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_67 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_68 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_69 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_70 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_71 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_72 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_73 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_74 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_75 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_76 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_77 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_78 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_79 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_80 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_81 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_82 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_83 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_84 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_85 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_86 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_87 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_88 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_89 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_90 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_91 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_92 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_93 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_94 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_95 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_96 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_97 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_98 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_99 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_100 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Estimate"])
        
      ))
      names(COEF_01_2002) <- c("Intercepto", "Cor", "Sexo", "Idade", "Escolaridade")
      
      # p-value's dos coeficientes dos modelos 
      PVL_01_2002 <- as_tibble(rbind(
        t(coef(summary(glm(Renda_1 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_2 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_3 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_4 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_5 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_6 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_7 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_8 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_9 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_10 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_11 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_12 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_13 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_14 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_15 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_16 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_17 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_18 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_19 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_20 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_21 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_22 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_23 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_24 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_25 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_26 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_27 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_28 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_29 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_30 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_31 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_32 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_33 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_34 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_35 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_36 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_37 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_38 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_39 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_40 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_41 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_42 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_43 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_44 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_45 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_46 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_47 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_48 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_49 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_50 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_51 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_52 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_53 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_54 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_55 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_56 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_57 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_58 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_59 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_60 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_61 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_62 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_63 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_64 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_65 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_66 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_67 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_68 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_69 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_70 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_71 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_72 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_73 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_74 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_75 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_76 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_77 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_78 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_79 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_80 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_81 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_82 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_83 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_84 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_85 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_86 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_87 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_88 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_89 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_90 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_91 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_92 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_93 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_94 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_95 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_96 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_97 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_98 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_99 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_100 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_01_2002)))[, "Pr(>|z|)"])
        
      ))
      names(PVL_01_2002) <- c("p_Int", "p_Cor", "p_Sex", "p_Ida", "p_Esc")
      
      # Combinação das matrizes
      MDL_01_2002 <- cbind(Y, COEF_01_2002, PVL_01_2002)
      
      # Criar os sinais de significância
      MDL_01_2002 <- MDL_01_2002 %>%  
        rowwise() %>%
        mutate(s_Int = case_when(p_Int <= 0.01 ~ "*", p_Int <= 0.05 ~ "**", p_Int <= 0.1 ~ "***", p_Int > 0.1 ~ "ñs"),
               s_Cor = case_when(p_Cor <= 0.01 ~ "*", p_Cor <= 0.05 ~ "**", p_Cor <= 0.1 ~ "***", p_Cor > 0.1 ~ "ñs"),
               s_Sex = case_when(p_Sex <= 0.01 ~ "*", p_Sex <= 0.05 ~ "**", p_Sex <= 0.1 ~ "***", p_Sex > 0.1 ~ "ñs"),
               s_Ida = case_when(p_Ida <= 0.01 ~ "*", p_Ida <= 0.05 ~ "**", p_Ida <= 0.1 ~ "***", p_Ida > 0.1 ~ "ñs"),
               s_Esc = case_when(p_Esc <= 0.01 ~ "*", p_Esc <= 0.05 ~ "**", p_Esc <= 0.1 ~ "***", p_Esc > 0.1 ~ "ñs"))
      
      # Colocar os sinais de significância juntos dos coeficientes
      MDL_01_2002 <- MDL_01_2002 %>%  
        relocate(s_Int, .after = Intercepto) %>%
        relocate(s_Cor, .after = Cor) %>%
        relocate(s_Sex, .after = Sexo) %>%
        relocate(s_Ida, .after = Idade) %>%
        relocate(s_Esc, .after = Escolaridade)
      
# # 2 - Primeiro Quintil - BA -------------------------------------------------
      
      # Filtro na Base a UF e a Categoria
      TBL_ENS_02_2002 <- TBL_ENS_2002 %>%
        filter(UF == UF_BA_2002, Prim_Quintil == "PQ")
      
      # Estimar os Coeficientes dos Modelos
      COEF_02_2002 <- as_tibble(rbind(
        t(coef(summary(glm(Renda_1 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_2 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_3 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_4 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_5 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_6 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_7 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_8 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_9 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_10 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_11 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_12 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_13 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_14 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_15 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_16 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_17 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_18 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_19 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_20 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_21 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_22 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_23 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_24 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_25 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_26 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_27 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_28 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_29 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_30 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_31 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_32 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_33 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_34 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_35 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_36 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_37 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_38 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_39 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_40 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_41 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_42 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_43 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_44 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_45 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_46 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_47 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_48 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_49 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_50 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_51 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_52 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_53 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_54 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_55 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_56 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_57 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_58 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_59 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_60 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_61 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_62 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_63 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_64 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_65 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_66 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_67 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_68 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_69 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_70 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_71 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_72 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_73 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_74 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_75 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_76 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_77 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_78 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_79 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_80 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_81 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_82 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_83 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_84 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_85 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_86 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_87 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_88 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_89 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_90 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_91 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_92 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_93 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_94 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_95 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_96 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_97 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_98 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_99 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_100 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Estimate"])
        
      ))
      names(COEF_02_2002) <- c("Intercepto", "Cor", "Sexo", "Idade", "Escolaridade")
      
      # p-value's dos coeficientes dos modelos 
      PVL_02_2002 <- as_tibble(rbind(
        t(coef(summary(glm(Renda_1 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_2 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_3 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_4 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_5 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_6 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_7 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_8 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_9 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_10 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_11 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_12 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_13 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_14 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_15 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_16 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_17 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_18 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_19 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_20 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_21 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_22 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_23 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_24 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_25 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_26 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_27 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_28 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_29 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_30 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_31 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_32 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_33 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_34 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_35 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_36 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_37 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_38 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_39 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_40 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_41 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_42 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_43 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_44 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_45 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_46 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_47 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_48 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_49 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_50 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_51 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_52 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_53 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_54 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_55 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_56 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_57 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_58 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_59 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_60 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_61 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_62 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_63 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_64 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_65 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_66 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_67 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_68 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_69 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_70 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_71 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_72 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_73 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_74 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_75 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_76 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_77 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_78 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_79 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_80 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_81 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_82 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_83 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_84 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_85 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_86 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_87 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_88 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_89 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_90 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_91 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_92 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_93 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_94 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_95 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_96 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_97 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_98 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_99 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_100 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_02_2002)))[, "Pr(>|z|)"])
        
      ))
      names(PVL_02_2002) <- c("p_Int", "p_Cor", "p_Sex", "p_Ida", "p_Esc")
      
      # Combinação das matrizes
      MDL_02_2002 <- cbind(Y, COEF_02_2002, PVL_02_2002)
      
      # Criar os sinais de significância
      MDL_02_2002 <- MDL_02_2002 %>%  
        rowwise() %>%
        mutate(s_Int = case_when(p_Int <= 0.01 ~ "*", p_Int <= 0.05 ~ "**", p_Int <= 0.1 ~ "***", p_Int > 0.1 ~ "ñs"),
               s_Cor = case_when(p_Cor <= 0.01 ~ "*", p_Cor <= 0.05 ~ "**", p_Cor <= 0.1 ~ "***", p_Cor > 0.1 ~ "ñs"),
               s_Sex = case_when(p_Sex <= 0.01 ~ "*", p_Sex <= 0.05 ~ "**", p_Sex <= 0.1 ~ "***", p_Sex > 0.1 ~ "ñs"),
               s_Ida = case_when(p_Ida <= 0.01 ~ "*", p_Ida <= 0.05 ~ "**", p_Ida <= 0.1 ~ "***", p_Ida > 0.1 ~ "ñs"),
               s_Esc = case_when(p_Esc <= 0.01 ~ "*", p_Esc <= 0.05 ~ "**", p_Esc <= 0.1 ~ "***", p_Esc > 0.1 ~ "ñs"))
      
      # Colocar os sinais de significância juntos dos coeficientes
      MDL_02_2002 <- MDL_02_2002 %>%  
        relocate(s_Int, .after = Intercepto) %>%
        relocate(s_Cor, .after = Cor) %>%
        relocate(s_Sex, .after = Sexo) %>%
        relocate(s_Ida, .after = Idade) %>%
        relocate(s_Esc, .after = Escolaridade)
      
# # 3 - Empregados Sem Registro - SP ------------------------------------------
      
      # Filtro na Base a UF e a Categoria
      TBL_ENS_03_2002 <- TBL_ENS_2002 %>%
        filter(UF == UF_SP_2002, ESR == "ESR")
      
      # Estimar os Coeficientes dos Modelos
      COEF_03_2002 <- as_tibble(rbind(
        t(coef(summary(glm(Renda_1 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_2 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_3 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_4 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_5 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_6 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_7 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_8 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_9 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_10 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_11 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_12 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_13 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_14 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_15 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_16 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_17 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_18 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_19 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_20 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_21 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_22 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_23 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_24 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_25 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_26 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_27 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_28 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_29 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_30 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_31 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_32 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_33 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_34 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_35 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_36 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_37 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_38 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_39 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_40 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_41 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_42 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_43 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_44 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_45 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_46 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_47 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_48 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_49 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_50 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_51 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_52 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_53 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_54 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_55 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_56 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_57 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_58 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_59 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_60 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_61 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_62 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_63 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_64 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_65 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_66 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_67 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_68 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_69 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_70 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_71 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_72 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_73 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_74 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_75 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_76 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_77 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_78 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_79 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_80 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_81 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_82 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_83 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_84 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_85 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_86 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_87 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_88 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_89 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_90 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_91 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_92 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_93 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_94 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_95 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_96 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_97 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_98 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_99 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_100 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Estimate"])
        
      ))
      names(COEF_03_2002) <- c("Intercepto", "Cor", "Sexo", "Idade", "Escolaridade")
      
      # p-value's dos coeficientes dos modelos 
      PVL_03_2002 <- as_tibble(rbind(
        t(coef(summary(glm(Renda_1 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_2 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_3 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_4 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_5 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_6 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_7 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_8 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_9 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_10 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_11 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_12 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_13 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_14 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_15 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_16 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_17 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_18 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_19 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_20 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_21 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_22 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_23 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_24 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_25 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_26 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_27 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_28 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_29 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_30 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_31 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_32 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_33 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_34 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_35 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_36 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_37 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_38 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_39 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_40 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_41 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_42 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_43 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_44 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_45 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_46 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_47 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_48 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_49 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_50 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_51 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_52 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_53 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_54 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_55 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_56 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_57 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_58 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_59 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_60 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_61 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_62 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_63 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_64 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_65 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_66 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_67 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_68 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_69 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_70 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_71 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_72 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_73 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_74 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_75 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_76 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_77 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_78 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_79 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_80 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_81 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_82 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_83 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_84 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_85 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_86 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_87 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_88 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_89 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_90 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_91 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_92 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_93 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_94 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_95 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_96 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_97 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_98 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_99 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_100 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_03_2002)))[, "Pr(>|z|)"])
        
      ))
      names(PVL_03_2002) <- c("p_Int", "p_Cor", "p_Sex", "p_Ida", "p_Esc")
      
      # Combinação das matrizes
      MDL_03_2002 <- cbind(Y, COEF_03_2002, PVL_03_2002)
      
      # Criar os sinais de significância
      MDL_03_2002 <- MDL_03_2002 %>%  
        rowwise() %>%
        mutate(s_Int = case_when(p_Int <= 0.01 ~ "*", p_Int <= 0.05 ~ "**", p_Int <= 0.1 ~ "***", p_Int > 0.1 ~ "ñs"),
               s_Cor = case_when(p_Cor <= 0.01 ~ "*", p_Cor <= 0.05 ~ "**", p_Cor <= 0.1 ~ "***", p_Cor > 0.1 ~ "ñs"),
               s_Sex = case_when(p_Sex <= 0.01 ~ "*", p_Sex <= 0.05 ~ "**", p_Sex <= 0.1 ~ "***", p_Sex > 0.1 ~ "ñs"),
               s_Ida = case_when(p_Ida <= 0.01 ~ "*", p_Ida <= 0.05 ~ "**", p_Ida <= 0.1 ~ "***", p_Ida > 0.1 ~ "ñs"),
               s_Esc = case_when(p_Esc <= 0.01 ~ "*", p_Esc <= 0.05 ~ "**", p_Esc <= 0.1 ~ "***", p_Esc > 0.1 ~ "ñs"))
      
      # Colocar os sinais de significância juntos dos coeficientes
      MDL_03_2002 <- MDL_03_2002 %>%  
        relocate(s_Int, .after = Intercepto) %>%
        relocate(s_Cor, .after = Cor) %>%
        relocate(s_Sex, .after = Sexo) %>%
        relocate(s_Ida, .after = Idade) %>%
        relocate(s_Esc, .after = Escolaridade)
      
# # 4 - Empregados Sem Registro - BA -------------------------------------------
      
      # Filtro na Base a UF e a Categoria
      TBL_ENS_04_2002 <- TBL_ENS_2002 %>%
        filter(UF == UF_BA_2002, ESR == "ESR")
      
      # Estimar os Coeficientes dos Modelos
      COEF_04_2002 <- as_tibble(rbind(
        t(coef(summary(glm(Renda_1 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_2 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_3 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_4 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_5 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_6 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_7 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_8 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_9 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_10 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_11 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_12 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_13 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_14 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_15 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_16 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_17 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_18 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_19 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_20 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_21 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_22 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_23 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_24 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_25 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_26 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_27 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_28 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_29 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_30 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_31 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_32 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_33 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_34 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_35 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_36 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_37 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_38 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_39 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_40 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_41 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_42 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_43 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_44 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_45 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_46 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_47 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_48 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_49 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_50 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_51 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_52 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_53 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_54 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_55 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_56 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_57 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_58 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_59 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_60 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_61 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_62 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_63 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_64 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_65 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_66 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_67 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_68 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_69 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_70 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_71 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_72 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_73 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_74 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_75 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_76 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_77 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_78 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_79 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_80 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_81 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_82 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_83 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_84 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_85 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_86 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_87 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_88 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_89 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_90 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_91 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_92 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_93 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_94 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_95 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_96 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_97 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_98 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_99 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_100 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Estimate"])
        
      ))
      names(COEF_04_2002) <- c("Intercepto", "Cor", "Sexo", "Idade", "Escolaridade")
      
      # p-value's dos coeficientes dos modelos 
      PVL_04_2002 <- as_tibble(rbind(
        t(coef(summary(glm(Renda_1 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_2 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_3 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_4 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_5 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_6 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_7 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_8 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_9 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_10 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_11 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_12 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_13 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_14 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_15 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_16 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_17 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_18 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_19 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_20 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_21 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_22 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_23 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_24 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_25 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_26 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_27 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_28 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_29 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_30 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_31 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_32 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_33 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_34 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_35 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_36 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_37 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_38 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_39 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_40 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_41 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_42 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_43 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_44 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_45 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_46 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_47 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_48 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_49 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_50 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_51 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_52 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_53 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_54 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_55 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_56 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_57 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_58 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_59 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_60 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_61 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_62 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_63 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_64 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_65 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_66 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_67 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_68 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_69 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_70 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_71 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_72 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_73 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_74 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_75 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_76 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_77 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_78 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_79 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_80 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_81 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_82 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_83 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_84 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_85 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_86 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_87 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_88 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_89 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_90 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_91 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_92 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_93 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_94 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_95 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_96 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_97 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_98 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_99 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_100 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_04_2002)))[, "Pr(>|z|)"])
        
      ))
      names(PVL_04_2002) <- c("p_Int", "p_Cor", "p_Sex", "p_Ida", "p_Esc")
      
      # Combinação das matrizes
      MDL_04_2002 <- cbind(Y, COEF_04_2002, PVL_04_2002)
      
      # Criar os sinais de significância
      MDL_04_2002 <- MDL_04_2002 %>%  
        rowwise() %>%
        mutate(s_Int = case_when(p_Int <= 0.01 ~ "*", p_Int <= 0.05 ~ "**", p_Int <= 0.1 ~ "***", p_Int > 0.1 ~ "ñs"),
               s_Cor = case_when(p_Cor <= 0.01 ~ "*", p_Cor <= 0.05 ~ "**", p_Cor <= 0.1 ~ "***", p_Cor > 0.1 ~ "ñs"),
               s_Sex = case_when(p_Sex <= 0.01 ~ "*", p_Sex <= 0.05 ~ "**", p_Sex <= 0.1 ~ "***", p_Sex > 0.1 ~ "ñs"),
               s_Ida = case_when(p_Ida <= 0.01 ~ "*", p_Ida <= 0.05 ~ "**", p_Ida <= 0.1 ~ "***", p_Ida > 0.1 ~ "ñs"),
               s_Esc = case_when(p_Esc <= 0.01 ~ "*", p_Esc <= 0.05 ~ "**", p_Esc <= 0.1 ~ "***", p_Esc > 0.1 ~ "ñs"))
      
      # Colocar os sinais de significância juntos dos coeficientes
      MDL_04_2002 <- MDL_04_2002 %>%  
        relocate(s_Int, .after = Intercepto) %>%
        relocate(s_Cor, .after = Cor) %>%
        relocate(s_Sex, .after = Sexo) %>%
        relocate(s_Ida, .after = Idade) %>%
        relocate(s_Esc, .after = Escolaridade)
      
# # 5 - Empregados Com Registro - SP -------------------------------------------
      
      # Filtro na Base a UF e a Categoria
      TBL_ENS_05_2002 <- TBL_ENS_2002 %>%
        filter(UF == UF_SP_2002, ECR == "ECR")
      
      # Estimar os Coeficientes dos Modelos
      COEF_05_2002 <- as_tibble(rbind(
        t(coef(summary(glm(Renda_1 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_2 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_3 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_4 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_5 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_6 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_7 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_8 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_9 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_10 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_11 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_12 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_13 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_14 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_15 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_16 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_17 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_18 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_19 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_20 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_21 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_22 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_23 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_24 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_25 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_26 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_27 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_28 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_29 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_30 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_31 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_32 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_33 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_34 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_35 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_36 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_37 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_38 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_39 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_40 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_41 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_42 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_43 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_44 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_45 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_46 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_47 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_48 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_49 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_50 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_51 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_52 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_53 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_54 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_55 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_56 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_57 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_58 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_59 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_60 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_61 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_62 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_63 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_64 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_65 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_66 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_67 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_68 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_69 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_70 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_71 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_72 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_73 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_74 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_75 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_76 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_77 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_78 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_79 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_80 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_81 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_82 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_83 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_84 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_85 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_86 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_87 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_88 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_89 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_90 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_91 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_92 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_93 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_94 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_95 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_96 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_97 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_98 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_99 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_100 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Estimate"])
        
      ))
      names(COEF_05_2002) <- c("Intercepto", "Cor", "Sexo", "Idade", "Escolaridade")
      
      # p-value's dos coeficientes dos modelos 
      PVL_05_2002 <- as_tibble(rbind(
        t(coef(summary(glm(Renda_1 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_2 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_3 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_4 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_5 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_6 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_7 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_8 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_9 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_10 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_11 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_12 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_13 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_14 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_15 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_16 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_17 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_18 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_19 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_20 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_21 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_22 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_23 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_24 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_25 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_26 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_27 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_28 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_29 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_30 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_31 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_32 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_33 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_34 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_35 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_36 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_37 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_38 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_39 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_40 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_41 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_42 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_43 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_44 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_45 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_46 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_47 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_48 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_49 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_50 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_51 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_52 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_53 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_54 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_55 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_56 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_57 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_58 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_59 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_60 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_61 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_62 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_63 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_64 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_65 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_66 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_67 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_68 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_69 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_70 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_71 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_72 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_73 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_74 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_75 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_76 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_77 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_78 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_79 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_80 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_81 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_82 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_83 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_84 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_85 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_86 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_87 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_88 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_89 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_90 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_91 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_92 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_93 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_94 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_95 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_96 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_97 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_98 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_99 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_100 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_05_2002)))[, "Pr(>|z|)"])
        
      ))
      names(PVL_05_2002) <- c("p_Int", "p_Cor", "p_Sex", "p_Ida", "p_Esc")
      
      # Combinação das matrizes
      MDL_05_2002 <- cbind(Y, COEF_05_2002, PVL_05_2002)
      
      # Criar os sinais de significância
      MDL_05_2002 <- MDL_05_2002 %>%  
        rowwise() %>%
        mutate(s_Int = case_when(p_Int <= 0.01 ~ "*", p_Int <= 0.05 ~ "**", p_Int <= 0.1 ~ "***", p_Int > 0.1 ~ "ñs"),
               s_Cor = case_when(p_Cor <= 0.01 ~ "*", p_Cor <= 0.05 ~ "**", p_Cor <= 0.1 ~ "***", p_Cor > 0.1 ~ "ñs"),
               s_Sex = case_when(p_Sex <= 0.01 ~ "*", p_Sex <= 0.05 ~ "**", p_Sex <= 0.1 ~ "***", p_Sex > 0.1 ~ "ñs"),
               s_Ida = case_when(p_Ida <= 0.01 ~ "*", p_Ida <= 0.05 ~ "**", p_Ida <= 0.1 ~ "***", p_Ida > 0.1 ~ "ñs"),
               s_Esc = case_when(p_Esc <= 0.01 ~ "*", p_Esc <= 0.05 ~ "**", p_Esc <= 0.1 ~ "***", p_Esc > 0.1 ~ "ñs"))
      
      # Colocar os sinais de significância juntos dos coeficientes
      MDL_05_2002 <- MDL_05_2002 %>%  
        relocate(s_Int, .after = Intercepto) %>%
        relocate(s_Cor, .after = Cor) %>%
        relocate(s_Sex, .after = Sexo) %>%
        relocate(s_Ida, .after = Idade) %>%
        relocate(s_Esc, .after = Escolaridade)
      
# # 6 - Empregados Com Registro - BA -------------------------------------------
      
      # Filtro na Base a UF e a Categoria
      TBL_ENS_06_2002 <- TBL_ENS_2002 %>%
        filter(UF == UF_BA_2002, ECR == "ECR")
      
      # Estimar os Coeficientes dos Modelos
      COEF_06_2002 <- as_tibble(rbind(
        t(coef(summary(glm(Renda_1 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_2 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_3 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_4 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_5 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_6 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_7 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_8 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_9 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_10 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_11 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_12 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_13 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_14 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_15 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_16 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_17 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_18 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_19 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_20 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_21 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_22 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_23 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_24 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_25 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_26 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_27 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_28 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_29 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_30 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_31 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_32 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_33 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_34 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_35 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_36 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_37 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_38 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_39 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_40 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_41 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_42 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_43 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_44 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_45 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_46 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_47 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_48 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_49 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_50 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_51 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_52 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_53 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_54 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_55 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_56 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_57 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_58 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_59 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_60 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_61 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_62 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_63 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_64 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_65 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_66 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_67 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_68 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_69 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_70 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_71 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_72 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_73 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_74 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_75 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_76 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_77 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_78 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_79 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_80 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_81 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_82 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_83 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_84 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_85 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_86 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_87 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_88 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_89 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_90 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_91 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_92 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_93 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_94 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_95 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_96 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_97 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_98 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_99 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_100 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Estimate"])
        
      ))
      names(COEF_06_2002) <- c("Intercepto", "Cor", "Sexo", "Idade", "Escolaridade")
      
      # p-value's dos coeficientes dos modelos 
      PVL_06_2002 <- as_tibble(rbind(
        t(coef(summary(glm(Renda_1 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_2 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_3 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_4 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_5 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_6 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_7 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_8 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_9 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_10 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_11 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_12 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_13 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_14 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_15 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_16 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_17 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_18 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_19 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_20 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_21 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_22 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_23 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_24 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_25 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_26 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_27 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_28 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_29 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_30 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_31 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_32 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_33 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_34 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_35 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_36 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_37 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_38 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_39 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_40 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_41 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_42 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_43 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_44 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_45 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_46 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_47 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_48 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_49 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_50 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_51 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_52 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_53 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_54 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_55 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_56 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_57 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_58 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_59 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_60 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_61 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_62 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_63 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_64 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_65 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_66 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_67 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_68 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_69 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_70 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_71 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_72 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_73 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_74 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_75 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_76 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_77 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_78 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_79 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_80 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_81 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_82 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_83 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_84 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_85 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_86 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_87 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_88 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_89 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_90 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_91 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_92 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_93 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_94 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_95 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_96 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_97 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_98 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_99 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_100 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_06_2002)))[, "Pr(>|z|)"])
        
      ))
      names(PVL_06_2002) <- c("p_Int", "p_Cor", "p_Sex", "p_Ida", "p_Esc")
      
      # Combinação das matrizes
      MDL_06_2002 <- cbind(Y, COEF_06_2002, PVL_06_2002)
      
      # Criar os sinais de significância
      MDL_06_2002 <- MDL_06_2002 %>%  
        rowwise() %>%
        mutate(s_Int = case_when(p_Int <= 0.01 ~ "*", p_Int <= 0.05 ~ "**", p_Int <= 0.1 ~ "***", p_Int > 0.1 ~ "ñs"),
               s_Cor = case_when(p_Cor <= 0.01 ~ "*", p_Cor <= 0.05 ~ "**", p_Cor <= 0.1 ~ "***", p_Cor > 0.1 ~ "ñs"),
               s_Sex = case_when(p_Sex <= 0.01 ~ "*", p_Sex <= 0.05 ~ "**", p_Sex <= 0.1 ~ "***", p_Sex > 0.1 ~ "ñs"),
               s_Ida = case_when(p_Ida <= 0.01 ~ "*", p_Ida <= 0.05 ~ "**", p_Ida <= 0.1 ~ "***", p_Ida > 0.1 ~ "ñs"),
               s_Esc = case_when(p_Esc <= 0.01 ~ "*", p_Esc <= 0.05 ~ "**", p_Esc <= 0.1 ~ "***", p_Esc > 0.1 ~ "ñs"))
      
      # Colocar os sinais de significância juntos dos coeficientes
      MDL_06_2002 <- MDL_06_2002 %>%  
        relocate(s_Int, .after = Intercepto) %>%
        relocate(s_Cor, .after = Cor) %>%
        relocate(s_Sex, .after = Sexo) %>%
        relocate(s_Ida, .after = Idade) %>%
        relocate(s_Esc, .after = Escolaridade)
      
# # 7 - Dirigentes e Gerentes - SP --------------------------------------------
      
      # Filtro na Base a UF e a Categoria
      TBL_ENS_07_2002 <- TBL_ENS_2002 %>%
        filter(UF == UF_SP_2002 & Classe == "DG")
      
      # Estimar os Coeficientes dos Modelos
      COEF_07_2002 <- as_tibble(rbind(
        t(coef(summary(glm(Renda_1 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_2 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_3 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_4 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_5 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_6 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_7 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_8 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_9 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_10 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_11 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_12 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_13 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_14 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_15 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_16 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_17 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_18 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_19 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_20 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_21 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_22 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_23 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_24 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_25 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_26 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_27 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_28 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_29 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_30 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_31 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_32 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_33 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_34 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_35 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_36 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_37 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_38 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_39 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_40 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_41 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_42 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_43 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_44 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_45 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_46 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_47 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_48 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_49 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_50 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_51 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_52 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_53 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_54 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_55 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_56 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_57 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_58 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_59 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_60 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_61 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_62 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_63 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_64 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_65 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_66 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_67 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_68 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_69 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_70 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_71 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_72 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_73 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_74 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_75 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_76 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_77 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_78 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_79 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_80 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_81 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_82 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_83 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_84 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_85 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_86 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_87 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_88 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_89 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_90 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_91 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_92 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_93 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_94 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_95 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_96 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_97 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_98 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_99 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_100 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Estimate"])
      ))
      names(COEF_07_2002) <- c("Intercepto", "Cor", "Sexo", "Idade", "Escolaridade")
      
      # p-value's dos coeficientes dos modelos 
      PVL_07_2002 <- as_tibble(rbind(
        t(coef(summary(glm(Renda_1 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_2 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_3 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_4 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_5 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_6 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_7 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_8 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_9 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_10 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_11 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_12 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_13 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_14 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_15 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_16 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_17 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_18 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_19 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_20 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_21 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_22 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_23 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_24 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_25 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_26 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_27 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_28 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_29 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_30 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_31 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_32 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_33 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_34 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_35 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_36 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_37 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_38 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_39 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_40 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_41 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_42 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_43 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_44 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_45 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_46 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_47 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_48 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_49 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_50 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_51 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_52 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_53 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_54 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_55 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_56 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_57 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_58 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_59 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_60 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_61 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_62 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_63 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_64 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_65 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_66 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_67 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_68 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_69 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_70 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_71 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_72 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_73 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_74 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_75 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_76 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_77 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_78 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_79 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_80 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_81 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_82 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_83 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_84 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_85 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_86 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_87 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_88 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_89 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_90 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_91 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_92 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_93 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_94 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_95 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_96 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_97 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_98 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_99 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_100 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_07_2002)))[, "Pr(>|z|)"])
      ))
      names(PVL_07_2002) <- c("p_Int", "p_Cor", "p_Sex", "p_Ida", "p_Esc")
      
      # Combinação das matrizes
      MDL_07_2002 <- cbind(Y, COEF_07_2002, PVL_07_2002)
      
      # Criar os sinais de significância
      MDL_07_2002 <- MDL_07_2002 %>%  
        rowwise() %>%
        mutate(s_Int = case_when(p_Int <= 0.01 ~ "*", p_Int <= 0.05 ~ "**", p_Int <= 0.1 ~ "***", p_Int > 0.1 ~ "ñs"),
               s_Cor = case_when(p_Cor <= 0.01 ~ "*", p_Cor <= 0.05 ~ "**", p_Cor <= 0.1 ~ "***", p_Cor > 0.1 ~ "ñs"),
               s_Sex = case_when(p_Sex <= 0.01 ~ "*", p_Sex <= 0.05 ~ "**", p_Sex <= 0.1 ~ "***", p_Sex > 0.1 ~ "ñs"),
               s_Ida = case_when(p_Ida <= 0.01 ~ "*", p_Ida <= 0.05 ~ "**", p_Ida <= 0.1 ~ "***", p_Ida > 0.1 ~ "ñs"),
               s_Esc = case_when(p_Esc <= 0.01 ~ "*", p_Esc <= 0.05 ~ "**", p_Esc <= 0.1 ~ "***", p_Esc > 0.1 ~ "ñs"))
      
      # Colocar os sinais de significância juntos dos coeficientes
      MDL_07_2002 <- MDL_07_2002 %>%  
        relocate(s_Int, .after = Intercepto) %>%
        relocate(s_Cor, .after = Cor) %>%
        relocate(s_Sex, .after = Sexo) %>%
        relocate(s_Ida, .after = Idade) %>%
        relocate(s_Esc, .after = Escolaridade)
      
# # 8 - Dirigentes e Gerentes - BA --------------------------------------------
      
      TBL_ENS_08_2002 <- TBL_ENS_2002 %>%
        filter(UF == UF_BA_2002 & Classe == "DG")
      
      # Estimar os Coeficientes dos Modelos
      COEF_08_2002 <- as_tibble(rbind(
        t(coef(summary(glm(Renda_1 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_2 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_3 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_4 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_5 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_6 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_7 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_8 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_9 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_10 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_11 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_12 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_13 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_14 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_15 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_16 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_17 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_18 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_19 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_20 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_21 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_22 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_23 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_24 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_25 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_26 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_27 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_28 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_29 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_30 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_31 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_32 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_33 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_34 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_35 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_36 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_37 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_38 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_39 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_40 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_41 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_42 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_43 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_44 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_45 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_46 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_47 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_48 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_49 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_50 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_51 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_52 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_53 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_54 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_55 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_56 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_57 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_58 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_59 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_60 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_61 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_62 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_63 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_64 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_65 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_66 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_67 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_68 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_69 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_70 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_71 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_72 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_73 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_74 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_75 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_76 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_77 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_78 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_79 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_80 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_81 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_82 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_83 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_84 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_85 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_86 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_87 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_88 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_89 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_90 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_91 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_92 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_93 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_94 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_95 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_96 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_97 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_98 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_99 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"]),
        t(coef(summary(glm(Renda_100 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Estimate"])
        
      ))
      names(COEF_08_2002) <- c("Intercepto", "Cor", "Sexo", "Idade", "Escolaridade")
      
      # p-value's dos coeficientes dos modelos 
      PVL_08_2002 <- as_tibble(rbind(
        t(coef(summary(glm(Renda_1 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_2 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_3 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_4 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_5 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_6 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_7 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_8 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_9 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_10 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_11 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_12 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_13 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_14 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_15 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_16 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_17 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_18 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_19 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_20 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_21 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_22 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_23 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_24 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_25 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_26 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_27 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_28 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_29 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_30 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_31 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_32 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_33 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_34 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_35 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_36 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_37 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_38 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_39 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_40 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_41 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_42 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_43 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_44 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_45 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_46 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_47 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_48 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_49 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_50 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_51 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_52 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_53 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_54 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_55 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_56 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_57 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_58 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_59 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_60 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_61 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_62 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_63 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_64 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_65 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_66 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_67 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_68 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_69 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_70 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_71 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_72 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_73 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_74 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_75 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_76 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_77 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_78 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_79 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_80 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_81 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_82 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_83 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_84 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_85 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_86 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_87 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_88 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_89 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_90 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_91 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_92 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_93 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_94 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_95 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_96 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_97 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_98 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_99 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"]),
        t(coef(summary(glm(Renda_100 ~ Cor + Sexo + Idade + Ano_Esc, family = binomial(link = "probit"), data  =  TBL_ENS_08_2002)))[, "Pr(>|z|)"])
        
      ))
      names(PVL_08_2002) <- c("p_Int", "p_Cor", "p_Sex", "p_Ida", "p_Esc")
      
      # Combinação das matrizes
      MDL_08_2002 <- cbind(Y, COEF_08_2002, PVL_08_2002)
      
      # Criar os sinais de significância
      MDL_08_2002 <- MDL_08_2002 %>%  
        rowwise() %>%
        mutate(s_Int = case_when(p_Int <= 0.01 ~ "*", p_Int <= 0.05 ~ "**", p_Int <= 0.1 ~ "***", p_Int > 0.1 ~ "ñs"),
               s_Cor = case_when(p_Cor <= 0.01 ~ "*", p_Cor <= 0.05 ~ "**", p_Cor <= 0.1 ~ "***", p_Cor > 0.1 ~ "ñs"),
               s_Sex = case_when(p_Sex <= 0.01 ~ "*", p_Sex <= 0.05 ~ "**", p_Sex <= 0.1 ~ "***", p_Sex > 0.1 ~ "ñs"),
               s_Ida = case_when(p_Ida <= 0.01 ~ "*", p_Ida <= 0.05 ~ "**", p_Ida <= 0.1 ~ "***", p_Ida > 0.1 ~ "ñs"),
               s_Esc = case_when(p_Esc <= 0.01 ~ "*", p_Esc <= 0.05 ~ "**", p_Esc <= 0.1 ~ "***", p_Esc > 0.1 ~ "ñs"))
      
      # Colocar os sinais de significância juntos dos coeficientes
      MDL_08_2002 <- MDL_08_2002 %>%  
        relocate(s_Int, .after = Intercepto) %>%
        relocate(s_Cor, .after = Cor) %>%
        relocate(s_Sex, .after = Sexo) %>%
        relocate(s_Ida, .after = Idade) %>%
        relocate(s_Esc, .after = Escolaridade)
      
# ANEXO 1 - MODELO DOS ENSAIOS --------------------------------------------
# # DIREGENTES E GERENTES -----------------------------------------------
# # # 1 - Ocupados de 30 anos e nível superior completo - SP --------------
      
      MDLE_01_2002 <- MDL_07_2002 %>%
        rowwise() %>%
        mutate(Homem_Branco = pnorm(Intercepto + ZERO * Cor + ZERO * Sexo + 30 * Idade + 15 * Escolaridade),
               Mulher_Branca = pnorm(Intercepto + ZERO * Cor + UM * Sexo + 30 * Idade + 15 * Escolaridade),
               Homem_Negro = pnorm(Intercepto + UM * Cor + ZERO * Sexo + 30 * Idade + 15 * Escolaridade),
               Mulher_Negra = pnorm(Intercepto + UM * Cor + UM * Sexo + 30 * Idade + 15 * Escolaridade))
      
# # # 2 - Ocupados de 30 anos e ensino médio completo - SP ----------------
      
      MDLE_02_2002 <- MDL_07_2002 %>%
        rowwise() %>%
        mutate(Homem_Branco = pnorm(Intercepto + ZERO * Cor + ZERO * Sexo + 30 * Idade + 11 * Escolaridade),
               Mulher_Branca = pnorm(Intercepto + ZERO * Cor + UM * Sexo + 30 * Idade + 11 * Escolaridade),
               Homem_Negro = pnorm(Intercepto + UM * Cor + ZERO * Sexo + 30 * Idade + 11 * Escolaridade),
               Mulher_Negra = pnorm(Intercepto + UM * Cor + UM * Sexo + 30 * Idade + 11 * Escolaridade))
      
# # # 3 - Ocupados de 30 anos e nível superior completo - BA --------------
      
      MDLE_03_2002 <- MDL_08_2002 %>%
        rowwise() %>%
        mutate(Homem_Branco = pnorm(Intercepto + ZERO * Cor + ZERO * Sexo + 30 * Idade + 15 * Escolaridade),
               Mulher_Branca = pnorm(Intercepto + ZERO * Cor + UM * Sexo + 30 * Idade + 15 * Escolaridade),
               Homem_Negro = pnorm(Intercepto + UM * Cor + ZERO * Sexo + 30 * Idade + 15 * Escolaridade),
               Mulher_Negra = pnorm(Intercepto + UM * Cor + UM * Sexo + 30 * Idade + 15 * Escolaridade))
      
# # # 4 - Ocupados de 30 anos e ensino médio completo - BA ----------------
      
      MDLE_04_2002 <- MDL_08_2002 %>%
        rowwise() %>%
        mutate(Homem_Branco = pnorm(Intercepto + ZERO * Cor + ZERO * Sexo + 30 * Idade + 11 * Escolaridade),
               Mulher_Branca = pnorm(Intercepto + ZERO * Cor + UM * Sexo + 30 * Idade + 11 * Escolaridade),
               Homem_Negro = pnorm(Intercepto + UM * Cor + ZERO * Sexo + 30 * Idade + 11 * Escolaridade),
               Mulher_Negra = pnorm(Intercepto + UM * Cor + UM * Sexo + 30 * Idade + 11 * Escolaridade))
      
# # EMPREGADOS COM REGISTRO -----------------------------------------------
# # # 5 - Ocupados de 18 anos de idade e ensino médio completo - SP -------
      
      MDLE_05_2002 <- MDL_05_2002 %>%
        rowwise() %>%
        mutate(Homem_Branco = pnorm(Intercepto + ZERO * Cor + ZERO * Sexo + 18 * Idade + 11 * Escolaridade),
               Mulher_Branca = pnorm(Intercepto + ZERO * Cor + UM * Sexo + 18 * Idade + 11 * Escolaridade),
               Homem_Negro = pnorm(Intercepto + UM * Cor + ZERO * Sexo + 18 * Idade + 11 * Escolaridade),
               Mulher_Negra = pnorm(Intercepto + UM * Cor + UM * Sexo + 18 * Idade + 11 * Escolaridade))
      
# # # 6 - Ocupados de 18 anos de idade e ensino médio completo - BA -------
      
      MDLE_06_2002 <- MDL_06_2002 %>%
        rowwise() %>%
        mutate(Homem_Branco = pnorm(Intercepto + ZERO * Cor + ZERO * Sexo + 18 * Idade + 11 * Escolaridade),
               Mulher_Branca = pnorm(Intercepto + ZERO * Cor + UM * Sexo + 18 * Idade + 11 * Escolaridade),
               Homem_Negro = pnorm(Intercepto + UM * Cor + ZERO * Sexo + 18 * Idade + 11 * Escolaridade),
               Mulher_Negra = pnorm(Intercepto + UM * Cor + UM * Sexo + 18 * Idade + 11 * Escolaridade))
      
# # # 7 - Ocupados de 35 anos de idade e antigo primário completo - SP ----
      
      MDLE_07_2002 <- MDL_05_2002 %>%
        rowwise() %>%
        mutate(Homem_Branco = pnorm(Intercepto + ZERO * Cor + ZERO * Sexo + 35 * Idade + 4 * Escolaridade),
               Mulher_Branca = pnorm(Intercepto + ZERO * Cor + UM * Sexo + 35 * Idade + 4 * Escolaridade),
               Homem_Negro = pnorm(Intercepto + UM * Cor + ZERO * Sexo + 35 * Idade + 4 * Escolaridade),
               Mulher_Negra = pnorm(Intercepto + UM * Cor + UM * Sexo + 35 * Idade + 4 * Escolaridade))
      
# # # 8 - Ocupados de 35 anos de idade e antigo primário completo - BA -----
      
      MDLE_08_2002 <- MDL_06_2002 %>%
        rowwise() %>%
        mutate(Homem_Branco = pnorm(Intercepto + ZERO * Cor + ZERO * Sexo + 35 * Idade + 4 * Escolaridade),
               Mulher_Branca = pnorm(Intercepto + ZERO * Cor + UM * Sexo + 35 * Idade + 4 * Escolaridade),
               Homem_Negro = pnorm(Intercepto + UM * Cor + ZERO * Sexo + 35 * Idade + 4 * Escolaridade),
               Mulher_Negra = pnorm(Intercepto + UM * Cor + UM * Sexo + 35 * Idade + 4 * Escolaridade))
      
# # EMPREGADOS SEM REGISTRO ---------------------------------------------
# # # 9 - Ocupados de 20 anos e Ensino fundamental completo - BA ----------
      
      MDLE_09_2002 <- MDL_04_2002 %>%
        rowwise() %>%
        mutate(Homem_Branco = pnorm(Intercepto + ZERO * Cor + ZERO * Sexo + 20 * Idade + 8 * Escolaridade),
               Mulher_Branca = pnorm(Intercepto + ZERO * Cor + UM * Sexo + 20 * Idade + 8 * Escolaridade),
               Homem_Negro = pnorm(Intercepto + UM * Cor + ZERO * Sexo + 20 * Idade + 8 * Escolaridade),
               Mulher_Negra = pnorm(Intercepto + UM * Cor + UM * Sexo + 20 * Idade + 8 * Escolaridade))
      
# # # 10 - Ocupados de 40 anos e Antigo primário completo - BA ------------
      
      MDLE_10_2002 <- MDL_04_2002 %>%
        rowwise() %>%
        mutate(Homem_Branco = pnorm(Intercepto + ZERO * Cor + ZERO * Sexo + 40 * Idade + 4 * Escolaridade),
               Mulher_Branca = pnorm(Intercepto + ZERO * Cor + UM * Sexo + 40 * Idade + 4 * Escolaridade),
               Homem_Negro = pnorm(Intercepto + UM * Cor + ZERO * Sexo + 40 * Idade + 4 * Escolaridade),
               Mulher_Negra = pnorm(Intercepto + UM * Cor + UM * Sexo + 40 * Idade + 4 * Escolaridade))
      
# # # 11 - Ocupados de 20 anos e Ensino fundamental completo - SP ---------
      
      MDLE_11_2002 <- MDL_03_2002 %>%
        rowwise() %>%
        mutate(Homem_Branco = pnorm(Intercepto + ZERO * Cor + ZERO * Sexo + 20 * Idade + 8 * Escolaridade),
               Mulher_Branca = pnorm(Intercepto + ZERO * Cor + UM * Sexo + 20 * Idade + 8 * Escolaridade),
               Homem_Negro = pnorm(Intercepto + UM * Cor + ZERO * Sexo + 20 * Idade + 8 * Escolaridade),
               Mulher_Negra = pnorm(Intercepto + UM * Cor + UM * Sexo + 20 * Idade + 8 * Escolaridade))
      
# # # 12 - Ocupados de 40 anos e Antigo primário completo - SP ------------
      
      MDLE_12_2002 <- MDL_03_2002 %>%
        rowwise() %>%
        mutate(Homem_Branco = pnorm(Intercepto + ZERO * Cor + ZERO * Sexo + 40 * Idade + 4 * Escolaridade),
               Mulher_Branca = pnorm(Intercepto + ZERO * Cor + UM * Sexo + 40 * Idade + 4 * Escolaridade),
               Homem_Negro = pnorm(Intercepto + UM * Cor + ZERO * Sexo + 40 * Idade + 4 * Escolaridade),
               Mulher_Negra = pnorm(Intercepto + UM * Cor + UM * Sexo + 40 * Idade + 4 * Escolaridade))
      
# ANEXO 1 - GRÁFICOS ENSAIOS ----------------------------------------------
# # DIREGENTES E GERENTES -------------------------------------------------
# # # 1 - Ocupados de 30 anos e nível superior completo -SP  ------------------
 
      # Limite inferior de Renda
      GF_01_RI_2002 <- 400
      # Limite superior de Renda
      GF_01_RF_2002 <- 1800
      
      # Criar a tabela Renda, Coorte e Probabilidade
      RCP_01_2002 <- rbind(
        add_column(cbind(Y, Coorte = "Homem Branco"), Prob = MDLE_01_2002$Homem_Branco),
        add_column(cbind(Y, Coorte = "Homem Não Branco"), Prob = MDLE_01_2002$Homem_Negro),
        add_column(cbind(Y, Coorte = "Mulher Branca"), Prob = MDLE_01_2002$Mulher_Branca),
        add_column(cbind(Y, Coorte = "Mulher Não Branca"), Prob = MDLE_01_2002$Mulher_Negra))

      # Criar do gráfico
      GF_01_2002 <- ggplot() +
        geom_line(data = MDLE_01_2002[MDLE_01_2002$Renda >= GF_01_RI_2002 & MDLE_01_2002$Renda <= GF_01_RF_2002, ], 
                  aes(x = Renda, y = Mulher_Negra), size = 0.7) +
        geom_line(data = MDLE_01_2002[MDLE_01_2002$Renda >= GF_01_RI_2002 & MDLE_01_2002$Renda <= GF_01_RF_2002, ], 
                  aes(x = Renda, y = Homem_Negro), size = 0.7) +
        geom_line(data = MDLE_01_2002[MDLE_01_2002$Renda >= GF_01_RI_2002 & MDLE_01_2002$Renda <= GF_01_RF_2002, ], 
                  aes(x = Renda, y = Mulher_Branca), size = 0.7) +
        geom_line(data = MDLE_01_2002[MDLE_01_2002$Renda >= GF_01_RI_2002 & MDLE_01_2002$Renda <= GF_01_RF_2002, ], 
                  aes(x = Renda, y = Homem_Branco), size = 0.7) +
        geom_point(data = RCP_01_2002[RCP_01_2002$Renda >= GF_01_RI_2002 & RCP_01_2002$Renda <= GF_01_RF_2002, ], 
                   aes(x = Renda, y = Prob, shape = Coorte, fill = Coorte), size = 3.0) +
        theme_classic() +
        theme(panel.grid.major.y = element_line(color = "black"), 
              panel.background = element_rect(color = "black"),
              legend.position = "bottom",
              legend.title = element_text(colour = "white"),
              axis.text = element_text(color = "black"),
              text = element_text(family = "Times New Roman")) +
        coord_cartesian(ylim = c(0.00, round(max(RCP_01_2002[RCP_01_2002$Renda >= GF_01_RI_2002 & RCP_01_2002$Renda <= GF_01_RF_2002, ]$Prob, 
                                                 na.rm = TRUE) + 0.05, 
                                             digits = 2)), 
                        xlim = c(GF_01_RI_2002-50, GF_01_RF_2002+50)) +
        scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
                                                          decimal.mark = ",", 
                                                          big.mark = ""), 
                           breaks = seq(0.00, round(max(RCP_01_2002[RCP_01_2002$Renda >= GF_01_RI_2002 & RCP_01_2002$Renda <= GF_01_RF_2002, ]$Prob, 
                                                        na.rm = TRUE) + 0.05, 
                                                    digits = 2), 
                                        by = 0.10)) +
        scale_x_continuous(expand = expansion(add = c(0, 0)), 
                           breaks = seq(0, 10000, by = 100)) +
        scale_shape_manual(values = c(4, 24, 22, 18)) +
        scale_fill_manual(values = c("black", "grey90", "grey30", "black")) +
        labs(x = "renda", y = "probabilidade")
      
# # # 2 - Ocupados de 30 anos e ensino médio completo - SP ----------------
      
      # Limite inferior de Renda
      GF_02_RI_2002 <- 400
      # Limite superior de Renda
      GF_02_RF_2002 <- 1900
      
      # Criar a tabela Renda, Coorte e Probabilidade
      RCP_02_2002 <- rbind(
        add_column(cbind(Y, Coorte = "Homem Branco"), Prob = MDLE_02_2002$Homem_Branco),
        add_column(cbind(Y, Coorte = "Homem Não Branco"), Prob = MDLE_02_2002$Homem_Negro),
        add_column(cbind(Y, Coorte = "Mulher Branca"), Prob = MDLE_02_2002$Mulher_Branca),
        add_column(cbind(Y, Coorte = "Mulher Não Branca"), Prob = MDLE_02_2002$Mulher_Negra))
      
      # Criar do gráfico
      GF_02_2002 <- ggplot() +
        geom_line(data = MDLE_02_2002[MDLE_02_2002$Renda >= GF_02_RI_2002 & MDLE_02_2002$Renda <= GF_02_RF_2002, ], 
                  aes(x = Renda, y = Mulher_Negra), size = 0.7) +
        geom_line(data = MDLE_02_2002[MDLE_02_2002$Renda >= GF_02_RI_2002 & MDLE_02_2002$Renda <= GF_02_RF_2002, ], 
                  aes(x = Renda, y = Homem_Negro), size = 0.7) +
        geom_line(data = MDLE_02_2002[MDLE_02_2002$Renda >= GF_02_RI_2002 & MDLE_02_2002$Renda <= GF_02_RF_2002, ], 
                  aes(x = Renda, y = Mulher_Branca), size = 0.7) +
        geom_line(data = MDLE_02_2002[MDLE_02_2002$Renda >= GF_02_RI_2002 & MDLE_02_2002$Renda <= GF_02_RF_2002, ], 
                  aes(x = Renda, y = Homem_Branco), size = 0.7) +
        geom_point(data = RCP_02_2002[RCP_02_2002$Renda >= GF_02_RI_2002 & RCP_02_2002$Renda <= GF_02_RF_2002, ], 
                   aes(x = Renda, y = Prob, shape = Coorte, fill = Coorte), size = 3.0) +
        theme_classic() +
        theme(panel.grid.major.y = element_line(color = "black"), 
              panel.background = element_rect(color = "black"),
              legend.position = "bottom",
              legend.title = element_text(colour = "white"),
              axis.text = element_text(color = "black"),
              text = element_text(family = "Times New Roman")) +
        coord_cartesian(ylim = c(0.00, round(max(RCP_02_2002[RCP_02_2002$Renda >= GF_02_RI_2002 & RCP_02_2002$Renda <= GF_02_RF_2002, ]$Prob, 
                                                 na.rm = TRUE) + 0.05, 
                                             digits = 2)), 
                        xlim = c(GF_02_RI_2002-50, GF_02_RF_2002+50)) +
        scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
                                                          decimal.mark = ",", 
                                                          big.mark = ""), 
                           breaks = seq(0.00, round(max(RCP_02_2002[RCP_02_2002$Renda >= GF_02_RI_2002 & RCP_02_2002$Renda <= GF_02_RF_2002, ]$Prob, 
                                                        na.rm = TRUE) + 0.05, 
                                                    digits = 2), 
                                        by = 0.10)) +
        scale_x_continuous(expand = expansion(add = c(0, 0)), 
                           breaks = seq(0, 10000, by = 100)) +
        scale_shape_manual(values = c(4, 24, 22, 18)) +
        scale_fill_manual(values = c("black", "grey90", "grey30", "black")) +
        labs(x = "renda", y = "probabilidade")
      
# # # 3 - Ocupados de 30 anos e nível superior completo - BA --------------
      
      # Limite inferior de Renda
      GF_03_RI_2002 <- 400
      # Limite superior de Renda
      GF_03_RF_2002 <- 1900
      
      # Criar a tabela Renda, Coorte e Probabilidade
      RCP_03_2002 <- rbind(
        add_column(cbind(Y, Coorte = "Homem Branco"), Prob = MDLE_03_2002$Homem_Branco),
        add_column(cbind(Y, Coorte = "Homem Não Branco"), Prob = MDLE_03_2002$Homem_Negro),
        add_column(cbind(Y, Coorte = "Mulher Branca"), Prob = MDLE_03_2002$Mulher_Branca),
        add_column(cbind(Y, Coorte = "Mulher Não Branca"), Prob = MDLE_03_2002$Mulher_Negra))
      
      # Criar do gráfico
      GF_03_2002 <- ggplot() +
        geom_line(data = MDLE_03_2002[MDLE_03_2002$Renda >= GF_03_RI_2002 & MDLE_03_2002$Renda <= GF_03_RF_2002, ], 
                  aes(x = Renda, y = Mulher_Negra), size = 0.7) +
        geom_line(data = MDLE_03_2002[MDLE_03_2002$Renda >= GF_03_RI_2002 & MDLE_03_2002$Renda <= GF_03_RF_2002, ], 
                  aes(x = Renda, y = Homem_Negro), size = 0.7) +
        geom_line(data = MDLE_03_2002[MDLE_03_2002$Renda >= GF_03_RI_2002 & MDLE_03_2002$Renda <= GF_03_RF_2002, ], 
                  aes(x = Renda, y = Mulher_Branca), size = 0.7) +
        geom_line(data = MDLE_03_2002[MDLE_03_2002$Renda >= GF_03_RI_2002 & MDLE_03_2002$Renda <= GF_03_RF_2002, ], 
                  aes(x = Renda, y = Homem_Branco), size = 0.7) +
        geom_point(data = RCP_03_2002[RCP_03_2002$Renda >= GF_03_RI_2002 & RCP_03_2002$Renda <= GF_03_RF_2002, ], 
                   aes(x = Renda, y = Prob, shape = Coorte, fill = Coorte), size = 3.0) +
        theme_classic() +
        theme(panel.grid.major.y = element_line(color = "black"), 
              panel.background = element_rect(color = "black"),
              legend.position = "bottom",
              legend.title = element_text(colour = "white"),
              axis.text = element_text(color = "black"),
              text = element_text(family = "Times New Roman")) +
        coord_cartesian(ylim = c(0.00, round(max(RCP_03_2002[RCP_03_2002$Renda >= GF_03_RI_2002 & RCP_03_2002$Renda <= GF_03_RF_2002, ]$Prob, 
                                                 na.rm = TRUE) + 0.05, 
                                             digits = 2)), 
                        xlim = c(GF_03_RI_2002-50, GF_03_RF_2002+50)) +
        scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
                                                          decimal.mark = ",", 
                                                          big.mark = ""), 
                           breaks = seq(0.00, round(max(RCP_03_2002[RCP_03_2002$Renda >= GF_03_RI_2002 & RCP_03_2002$Renda <= GF_03_RF_2002, ]$Prob, 
                                                        na.rm = TRUE) + 0.05, 
                                                    digits = 2), 
                                        by = 0.10)) +
        scale_x_continuous(expand = expansion(add = c(0, 0)), 
                           breaks = seq(0, 10000, by = 100)) +
        scale_shape_manual(values = c(4, 24, 22, 18)) +
        scale_fill_manual(values = c("black", "grey90", "grey30", "black")) +
        labs(x = "renda", y = "probabilidade")
      
# # # 4 - Ocupados de 30 anos e ensino médio completo - BA ----------------
      
      # Limite inferior de Renda
      GF_04_RI_2002 <- 400
      # Limite superior de Renda
      GF_04_RF_2002 <- 1900
      
      # Criar a tabela Renda, Coorte e Probabilidade
      RCP_04_2002 <- rbind(
        add_column(cbind(Y, Coorte = "Homem Branco"), Prob = MDLE_04_2002$Homem_Branco),
        add_column(cbind(Y, Coorte = "Homem Não Branco"), Prob = MDLE_04_2002$Homem_Negro),
        add_column(cbind(Y, Coorte = "Mulher Branca"), Prob = MDLE_04_2002$Mulher_Branca),
        add_column(cbind(Y, Coorte = "Mulher Não Branca"), Prob = MDLE_04_2002$Mulher_Negra))
      
      # Criar do gráfico
      GF_04_2002 <- ggplot() +
        geom_line(data = MDLE_04_2002[MDLE_04_2002$Renda >= GF_04_RI_2002 & MDLE_04_2002$Renda <= GF_04_RF_2002, ], 
                  aes(x = Renda, y = Mulher_Negra), size = 0.7) +
        geom_line(data = MDLE_04_2002[MDLE_04_2002$Renda >= GF_04_RI_2002 & MDLE_04_2002$Renda <= GF_04_RF_2002, ], 
                  aes(x = Renda, y = Homem_Negro), size = 0.7) +
        geom_line(data = MDLE_04_2002[MDLE_04_2002$Renda >= GF_04_RI_2002 & MDLE_04_2002$Renda <= GF_04_RF_2002, ], 
                  aes(x = Renda, y = Mulher_Branca), size = 0.7) +
        geom_line(data = MDLE_04_2002[MDLE_04_2002$Renda >= GF_04_RI_2002 & MDLE_04_2002$Renda <= GF_04_RF_2002, ], 
                  aes(x = Renda, y = Homem_Branco), size = 0.7) +
        geom_point(data = RCP_04_2002[RCP_04_2002$Renda >= GF_04_RI_2002 & RCP_04_2002$Renda <= GF_04_RF_2002, ], 
                   aes(x = Renda, y = Prob, shape = Coorte, fill = Coorte), size = 3.0) +
        theme_classic() +
        theme(panel.grid.major.y = element_line(color = "black"), 
              panel.background = element_rect(color = "black"),
              legend.position = "bottom",
              legend.title = element_text(colour = "white"),
              axis.text = element_text(color = "black"),
              text = element_text(family = "Times New Roman")) +
        coord_cartesian(ylim = c(0.00, round(max(RCP_04_2002[RCP_04_2002$Renda >= GF_04_RI_2002 & RCP_04_2002$Renda <= GF_04_RF_2002, ]$Prob, 
                                                 na.rm = TRUE) + 0.05, 
                                             digits = 2)), 
                        xlim = c(GF_04_RI_2002-50, GF_04_RF_2002+50)) +
        scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
                                                          decimal.mark = ",", 
                                                          big.mark = ""), 
                           breaks = seq(0.00, round(max(RCP_04_2002[RCP_04_2002$Renda >= GF_04_RI_2002 & RCP_04_2002$Renda <= GF_04_RF_2002, ]$Prob, 
                                                        na.rm = TRUE) + 0.05, 
                                                    digits = 2), 
                                        by = 0.10)) +
        scale_x_continuous(expand = expansion(add = c(0, 0)), 
                           breaks = seq(0, 10000, by = 100)) +
        scale_shape_manual(values = c(4, 24, 22, 18)) +
        scale_fill_manual(values = c("black", "grey90", "grey30", "black")) +
        labs(x = "renda", y = "probabilidade")
      
# # EMPREGADOS COM REGISTRO -----------------------------------------------
# # # 5 - Ocupados de 18 anos de idade e ensino médio completo - SP -------
      
      # Limite inferior de Renda
      GF_05_RI_2002 <- 300
      # Limite superior de Renda
      GF_05_RF_2002 <- 1100
      
      # Criar a tabela Renda, Coorte e Probabilidade
      RCP_05_2002 <- rbind(
        add_column(cbind(Y, Coorte = "Homem Branco"), Prob = MDLE_05_2002$Homem_Branco),
        add_column(cbind(Y, Coorte = "Homem Negro"), Prob = MDLE_05_2002$Homem_Negro),
        add_column(cbind(Y, Coorte = "Mulher Branca"), Prob = MDLE_05_2002$Mulher_Branca),
        add_column(cbind(Y, Coorte = "Mulher Negra"), Prob = MDLE_05_2002$Mulher_Negra))
      
      # Criar do gráfico
      GF_05_2002 <- ggplot() +
        geom_line(data = MDLE_05_2002[MDLE_05_2002$Renda >= GF_05_RI_2002 & MDLE_05_2002$Renda <= GF_05_RF_2002, ], 
                  aes(x = Renda, y = Mulher_Negra), size = 0.7) +
        geom_line(data = MDLE_05_2002[MDLE_05_2002$Renda >= GF_05_RI_2002 & MDLE_05_2002$Renda <= GF_05_RF_2002, ], 
                  aes(x = Renda, y = Homem_Negro), size = 0.7) +
        geom_line(data = MDLE_05_2002[MDLE_05_2002$Renda >= GF_05_RI_2002 & MDLE_05_2002$Renda <= GF_05_RF_2002, ], 
                  aes(x = Renda, y = Mulher_Branca), size = 0.7) +
        geom_line(data = MDLE_05_2002[MDLE_05_2002$Renda >= GF_05_RI_2002 & MDLE_05_2002$Renda <= GF_05_RF_2002, ], 
                  aes(x = Renda, y = Homem_Branco), size = 0.7) +
        geom_point(data = RCP_05_2002[RCP_05_2002$Renda >= GF_05_RI_2002 & RCP_05_2002$Renda <= GF_05_RF_2002, ], 
                   aes(x = Renda, y = Prob, shape = Coorte, fill = Coorte), size = 3.0) +
        theme_classic() +
        theme(panel.grid.major.y = element_line(color = "black"), 
              panel.background = element_rect(color = "black"),
              legend.position = "bottom",
              legend.title = element_text(colour = "white"),
              axis.text = element_text(color = "black")) +
        coord_cartesian(ylim = c(0.00, round(max(RCP_05_2002[RCP_05_2002$Renda >= GF_05_RI_2002 & RCP_05_2002$Renda <= GF_05_RF_2002, ]$Prob, 
                                                 na.rm = TRUE) + 0.05, 
                                             digits = 2)), 
                        xlim = c(GF_05_RI_2002-50, GF_05_RF_2002+50)) +
        scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
                                                          decimal.mark = ",", 
                                                          big.mark = ""), 
                           breaks = seq(0.00, round(max(RCP_05_2002[RCP_05_2002$Renda >= GF_05_RI_2002 & RCP_05_2002$Renda <= GF_05_RF_2002, ]$Prob, 
                                                        na.rm = TRUE) + 0.05, 
                                                    digits = 2), 
                                        by = 0.10)) +
        scale_x_continuous(expand = expansion(add = c(0, 0)), 
                           breaks = seq(0, 10000, by = 100)) +
        scale_shape_manual(values = c(4, 24, 22, 18)) +
        scale_fill_manual(values = c("black", "grey90", "grey30", "black")) +
        labs(x = "renda", y = "probabilidade")
      
# # # 6 - Ocupados de 18 anos de idade e ensino médio completo - BA -------
      
      # Limite inferior de Renda
      GF_06_RI_2002 <- 200
      # Limite superior de Renda
      GF_06_RF_2002 <- 1100
      
      # Criar a tabela Renda, Coorte e Probabilidade
      RCP_06_2002 <- rbind(
        add_column(cbind(Y, Coorte = "Homem Branco"), Prob = MDLE_06_2002$Homem_Branco),
        add_column(cbind(Y, Coorte = "Homem Negro"), Prob = MDLE_06_2002$Homem_Negro),
        add_column(cbind(Y, Coorte = "Mulher Branca"), Prob = MDLE_06_2002$Mulher_Branca),
        add_column(cbind(Y, Coorte = "Mulher Negra"), Prob = MDLE_06_2002$Mulher_Negra))
      
      # Criar do gráfico
      GF_06_2002 <- ggplot() +
        geom_line(data = MDLE_06_2002[MDLE_06_2002$Renda >= GF_06_RI_2002 & MDLE_06_2002$Renda <= GF_06_RF_2002, ], 
                  aes(x = Renda, y = Mulher_Negra), size = 0.7) +
        geom_line(data = MDLE_06_2002[MDLE_06_2002$Renda >= GF_06_RI_2002 & MDLE_06_2002$Renda <= GF_06_RF_2002, ], 
                  aes(x = Renda, y = Homem_Negro), size = 0.7) +
        geom_line(data = MDLE_06_2002[MDLE_06_2002$Renda >= GF_06_RI_2002 & MDLE_06_2002$Renda <= GF_06_RF_2002, ], 
                  aes(x = Renda, y = Mulher_Branca), size = 0.7) +
        geom_line(data = MDLE_06_2002[MDLE_06_2002$Renda >= GF_06_RI_2002 & MDLE_06_2002$Renda <= GF_06_RF_2002, ], 
                  aes(x = Renda, y = Homem_Branco), size = 0.7) +
        geom_point(data = RCP_06_2002[RCP_06_2002$Renda >= GF_06_RI_2002 & RCP_06_2002$Renda <= GF_06_RF_2002, ], 
                   aes(x = Renda, y = Prob, shape = Coorte, fill = Coorte), size = 3.0) +
        theme_classic() +
        theme(panel.grid.major.y = element_line(color = "black"), 
              panel.background = element_rect(color = "black"),
              legend.position = "bottom",
              legend.title = element_text(colour = "white"),
              axis.text = element_text(color = "black"),,
              text = element_text(family = "Times New Roman")) +
        coord_cartesian(ylim = c(0.00, round(max(RCP_06_2002[RCP_06_2002$Renda >= GF_06_RI_2002 & RCP_06_2002$Renda <= GF_06_RF_2002, ]$Prob, 
                                                 na.rm = TRUE) + 0.05, 
                                             digits = 2)), 
                        xlim = c(GF_06_RI_2002-50, GF_06_RF_2002+50)) +
        scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
                                                          decimal.mark = ",", 
                                                          big.mark = ""), 
                           breaks = seq(0.00, round(max(RCP_06_2002[RCP_06_2002$Renda >= GF_06_RI_2002 & RCP_06_2002$Renda <= GF_06_RF_2002, ]$Prob, 
                                                        na.rm = TRUE) + 0.05, 
                                                    digits = 2), 
                                        by = 0.10)) +
        scale_x_continuous(expand = expansion(add = c(0, 0)), 
                           breaks = seq(0, 10000, by = 100)) +
        scale_shape_manual(values = c(4, 24, 22, 18)) +
        scale_fill_manual(values = c("black", "grey90", "grey30", "black")) +
        labs(x = "renda", y = "probabilidade")
      
# # # 7 - Ocupados de 35 anos de idade e antigo primário completo - SP ----
      
      # Limite inferior de Renda
      GF_07_RI_2002 <- 300
      # Limite superior de Renda
      GF_07_RF_2002 <- 800
      
      # Criar a tabela Renda, Coorte e Probabilidade
      RCP_07_2002 <- rbind(
        add_column(cbind(Y, Coorte = "Homem Branco"), Prob = MDLE_07_2002$Homem_Branco),
        add_column(cbind(Y, Coorte = "Homem Negro"), Prob = MDLE_07_2002$Homem_Negro),
        add_column(cbind(Y, Coorte = "Mulher Branca"), Prob = MDLE_07_2002$Mulher_Branca),
        add_column(cbind(Y, Coorte = "Mulher Negra"), Prob = MDLE_07_2002$Mulher_Negra))
      
      # Criar do gráfico
      GF_07_2002 <- ggplot() +
        geom_line(data = MDLE_07_2002[MDLE_07_2002$Renda >= GF_07_RI_2002 & MDLE_07_2002$Renda <= GF_07_RF_2002, ], 
                  aes(x = Renda, y = Mulher_Negra), size = 0.7) +
        geom_line(data = MDLE_07_2002[MDLE_07_2002$Renda >= GF_07_RI_2002 & MDLE_07_2002$Renda <= GF_07_RF_2002, ], 
                  aes(x = Renda, y = Homem_Negro), size = 0.7) +
        geom_line(data = MDLE_07_2002[MDLE_07_2002$Renda >= GF_07_RI_2002 & MDLE_07_2002$Renda <= GF_07_RF_2002, ], 
                  aes(x = Renda, y = Mulher_Branca), size = 0.7) +
        geom_line(data = MDLE_07_2002[MDLE_07_2002$Renda >= GF_07_RI_2002 & MDLE_07_2002$Renda <= GF_07_RF_2002, ], 
                  aes(x = Renda, y = Homem_Branco), size = 0.7) +
        geom_point(data = RCP_07_2002[RCP_07_2002$Renda >= GF_07_RI_2002 & RCP_07_2002$Renda <= GF_07_RF_2002, ], 
                   aes(x = Renda, y = Prob, shape = Coorte, fill = Coorte), size = 3.0) +
        theme_classic() +
        theme(panel.grid.major.y = element_line(color = "black"), 
              panel.background = element_rect(color = "black"),
              legend.position = "bottom",
              legend.title = element_text(colour = "white"),
              axis.text = element_text(color = "black"),
              text = element_text(family = "Times New Roman")) +
        coord_cartesian(ylim = c(0.00, round(max(RCP_07_2002[RCP_07_2002$Renda >= GF_07_RI_2002 & RCP_07_2002$Renda <= GF_07_RF_2002, ]$Prob, 
                                                 na.rm = TRUE) + 0.05, 
                                             digits = 2)), 
                        xlim = c(GF_07_RI_2002-50, GF_07_RF_2002+50)) +
        scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
                                                          decimal.mark = ",", 
                                                          big.mark = ""), 
                           breaks = seq(0.00, round(max(RCP_07_2002[RCP_07_2002$Renda >= GF_07_RI_2002 & RCP_07_2002$Renda <= GF_07_RF_2002, ]$Prob, 
                                                        na.rm = TRUE) + 0.05, 
                                                    digits = 2), 
                                        by = 0.10)) +
        scale_x_continuous(expand = expansion(add = c(0, 0)), 
                           breaks = seq(0, 10000, by = 100)) +
        scale_shape_manual(values = c(4, 24, 22, 18)) +
        scale_fill_manual(values = c("black", "grey90", "grey30", "black")) +
        labs(x = "renda", y = "probabilidade")
      
# # # 8 - Ocupados de 35 anos de idade e antigo primário completo - BA -----
      
      # Limite inferior de Renda
      GF_08_RI_2002 <- 200
      # Limite superior de Renda
      GF_08_RF_2002 <- 700
      
      # Criar a tabela Renda, Coorte e Probabilidade
      RCP_08_2002 <- rbind(
        add_column(cbind(Y, Coorte = "Homem Branco"), Prob = MDLE_08_2002$Homem_Branco),
        add_column(cbind(Y, Coorte = "Homem Negro"), Prob = MDLE_08_2002$Homem_Negro),
        add_column(cbind(Y, Coorte = "Mulher Branca"), Prob = MDLE_08_2002$Mulher_Branca),
        add_column(cbind(Y, Coorte = "Mulher Negra"), Prob = MDLE_08_2002$Mulher_Negra))
      
      # Criar do gráfico
      GF_08_2002 <- ggplot() +
        geom_line(data = MDLE_08_2002[MDLE_08_2002$Renda >= GF_08_RI_2002 & MDLE_08_2002$Renda <= GF_08_RF_2002, ], 
                  aes(x = Renda, y = Mulher_Negra), size = 0.7) +
        geom_line(data = MDLE_08_2002[MDLE_08_2002$Renda >= GF_08_RI_2002 & MDLE_08_2002$Renda <= GF_08_RF_2002, ], 
                  aes(x = Renda, y = Homem_Negro), size = 0.7) +
        geom_line(data = MDLE_08_2002[MDLE_08_2002$Renda >= GF_08_RI_2002 & MDLE_08_2002$Renda <= GF_08_RF_2002, ], 
                  aes(x = Renda, y = Mulher_Branca), size = 0.7) +
        geom_line(data = MDLE_08_2002[MDLE_08_2002$Renda >= GF_08_RI_2002 & MDLE_08_2002$Renda <= GF_08_RF_2002, ], 
                  aes(x = Renda, y = Homem_Branco), size = 0.7) +
        geom_point(data = RCP_08_2002[RCP_08_2002$Renda >= GF_08_RI_2002 & RCP_08_2002$Renda <= GF_08_RF_2002, ], 
                   aes(x = Renda, y = Prob, shape = Coorte, fill = Coorte), size = 3.0) +
        theme_classic() +
        theme(panel.grid.major.y = element_line(color = "black"), 
              panel.background = element_rect(color = "black"),
              legend.position = "bottom",
              legend.title = element_text(colour = "white"),
              axis.text = element_text(color = "black"),
              text = element_text(family = "Times New Roman")) +
        coord_cartesian(ylim = c(0.00, round(max(RCP_08_2002[RCP_08_2002$Renda >= GF_08_RI_2002 & RCP_08_2002$Renda <= GF_08_RF_2002, ]$Prob, 
                                                 na.rm = TRUE) + 0.05, 
                                             digits = 2)), 
                        xlim = c(GF_08_RI_2002-50, GF_08_RF_2002+50)) +
        scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
                                                          decimal.mark = ",", 
                                                          big.mark = ""), 
                           breaks = seq(0.00, round(max(RCP_08_2002[RCP_08_2002$Renda >= GF_08_RI_2002 & RCP_08_2002$Renda <= GF_08_RF_2002, ]$Prob, 
                                                        na.rm = TRUE) + 0.05, 
                                                    digits = 2), 
                                        by = 0.10)) +
        scale_x_continuous(expand = expansion(add = c(0, 0)), 
                           breaks = seq(0, 10000, by = 100)) +
        scale_shape_manual(values = c(4, 24, 22, 18)) +
        scale_fill_manual(values = c("black", "grey90", "grey30", "black")) +
        labs(x = "renda", y = "probabilidade")
      
# # # EMPREGADOS SEM REGISTRO ---------------------------------------------
# # # 9 - Ocupados de 20 anos e Ensino fundamental completo - BA ----------
      
      # Limite inferior de Renda
      GF_09_RI_2002 <- 200
      # Limite superior de Renda
      GF_09_RF_2002 <- 700
      
      # Criar a tabela Renda, Coorte e Probabilidade
      RCP_09_2002 <- rbind(
        add_column(cbind(Y, Coorte = "Homem Branco"), Prob = MDLE_09_2002$Homem_Branco),
        add_column(cbind(Y, Coorte = "Homem Negro"), Prob = MDLE_09_2002$Homem_Negro),
        add_column(cbind(Y, Coorte = "Mulher Branca"), Prob = MDLE_09_2002$Mulher_Branca),
        add_column(cbind(Y, Coorte = "Mulher Negra"), Prob = MDLE_09_2002$Mulher_Negra))
      
      # Criar do gráfico
      GF_09_2002 <- ggplot() +
        geom_line(data = MDLE_09_2002[MDLE_09_2002$Renda >= GF_09_RI_2002 & MDLE_09_2002$Renda <= GF_09_RF_2002, ], 
                  aes(x = Renda, y = Mulher_Negra), size = 0.7) +
        geom_line(data = MDLE_09_2002[MDLE_09_2002$Renda >= GF_09_RI_2002 & MDLE_09_2002$Renda <= GF_09_RF_2002, ], 
                  aes(x = Renda, y = Homem_Negro), size = 0.7) +
        geom_line(data = MDLE_09_2002[MDLE_09_2002$Renda >= GF_09_RI_2002 & MDLE_09_2002$Renda <= GF_09_RF_2002, ], 
                  aes(x = Renda, y = Mulher_Branca), size = 0.7) +
        geom_line(data = MDLE_09_2002[MDLE_09_2002$Renda >= GF_09_RI_2002 & MDLE_09_2002$Renda <= GF_09_RF_2002, ], 
                  aes(x = Renda, y = Homem_Branco), size = 0.7) +
        geom_point(data = RCP_09_2002[RCP_09_2002$Renda >= GF_09_RI_2002 & RCP_09_2002$Renda <= GF_09_RF_2002, ], 
                   aes(x = Renda, y = Prob, shape = Coorte, fill = Coorte), size = 3.0) +
        theme_classic() +
        theme(panel.grid.major.y = element_line(color = "black"), 
              panel.background = element_rect(color = "black"),
              legend.position = "bottom",
              legend.title = element_text(colour = "white"),
              axis.text = element_text(color = "black"),
              text = element_text(family = "Times New Roman")) +
        coord_cartesian(ylim = c(0.00, round(max(RCP_09_2002[RCP_09_2002$Renda >= GF_09_RI_2002 & RCP_09_2002$Renda <= GF_09_RF_2002, ]$Prob, 
                                                 na.rm = TRUE) + 0.05, 
                                             digits = 2)), 
                        xlim = c(GF_09_RI_2002-50, GF_09_RF_2002+50)) +
        scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
                                                          decimal.mark = ",", 
                                                          big.mark = ""), 
                           breaks = seq(0.00, round(max(RCP_09_2002[RCP_09_2002$Renda >= GF_09_RI_2002 & RCP_09_2002$Renda <= GF_09_RF_2002, ]$Prob, 
                                                        na.rm = TRUE) + 0.05, 
                                                    digits = 2), 
                                        by = 0.10)) +
        scale_x_continuous(expand = expansion(add = c(0, 0)), 
                           breaks = seq(0, 10000, by = 100)) +
        scale_shape_manual(values = c(4, 24, 22, 18)) +
        scale_fill_manual(values = c("black", "grey90", "grey30", "black")) +
        labs(x = "renda", y = "probabilidade")
      
# # # 10 - Ocupados de 40 anos e Antigo primário completo - BA ------------
      
      # Limite inferior de Renda
      GF_10_RI_2002 <- 200
      # Limite superior de Renda
      GF_10_RF_2002 <- 700
      
      # Criar a tabela Renda, Coorte e Probabilidade
      RCP_10_2002 <- rbind(
        add_column(cbind(Y, Coorte = "Homem Branco"), Prob = MDLE_10_2002$Homem_Branco),
        add_column(cbind(Y, Coorte = "Homem Negro"), Prob = MDLE_10_2002$Homem_Negro),
        add_column(cbind(Y, Coorte = "Mulher Branca"), Prob = MDLE_10_2002$Mulher_Branca),
        add_column(cbind(Y, Coorte = "Mulher Negra"), Prob = MDLE_10_2002$Mulher_Negra))
      
      # Criar do gráfico
      GF_10_2002 <- ggplot() +
        geom_line(data = MDLE_10_2002[MDLE_10_2002$Renda >= GF_10_RI_2002 & MDLE_10_2002$Renda <= GF_10_RF_2002, ], 
                  aes(x = Renda, y = Mulher_Negra), size = 0.7) +
        geom_line(data = MDLE_10_2002[MDLE_10_2002$Renda >= GF_10_RI_2002 & MDLE_10_2002$Renda <= GF_10_RF_2002, ], 
                  aes(x = Renda, y = Homem_Negro), size = 0.7) +
        geom_line(data = MDLE_10_2002[MDLE_10_2002$Renda >= GF_10_RI_2002 & MDLE_10_2002$Renda <= GF_10_RF_2002, ], 
                  aes(x = Renda, y = Mulher_Branca), size = 0.7) +
        geom_line(data = MDLE_10_2002[MDLE_10_2002$Renda >= GF_10_RI_2002 & MDLE_10_2002$Renda <= GF_10_RF_2002, ], 
                  aes(x = Renda, y = Homem_Branco), size = 0.7) +
        geom_point(data = RCP_10_2002[RCP_10_2002$Renda >= GF_10_RI_2002 & RCP_10_2002$Renda <= GF_10_RF_2002, ], 
                   aes(x = Renda, y = Prob, shape = Coorte, fill = Coorte), size = 3.0) +
        theme_classic() +
        theme(panel.grid.major.y = element_line(color = "black"), 
              panel.background = element_rect(color = "black"),
              legend.position = "bottom",
              legend.title = element_text(colour = "white"),
              axis.text = element_text(color = "black"),
              text = element_text(family = "Times New Roman")) +
        coord_cartesian(ylim = c(0.00, round(max(RCP_10_2002[RCP_10_2002$Renda >= GF_10_RI_2002 & RCP_10_2002$Renda <= GF_10_RF_2002, ]$Prob, 
                                                 na.rm = TRUE) + 0.05, 
                                             digits = 2)), 
                        xlim = c(GF_10_RI_2002-50, GF_10_RF_2002+50)) +
        scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
                                                          decimal.mark = ",", 
                                                          big.mark = ""), 
                           breaks = seq(0.00, round(max(RCP_10_2002[RCP_10_2002$Renda >= GF_10_RI_2002 & RCP_10_2002$Renda <= GF_10_RF_2002, ]$Prob, 
                                                        na.rm = TRUE) + 0.05, 
                                                    digits = 2), 
                                        by = 0.10)) +
        scale_x_continuous(expand = expansion(add = c(0, 0)), 
                           breaks = seq(0, 10000, by = 100)) +
        scale_shape_manual(values = c(4, 24, 22, 18)) +
        scale_fill_manual(values = c("black", "grey90", "grey30", "black")) +
        labs(x = "renda", y = "probabilidade")
      
# # # 11 - Ocupados de 20 anos e Ensino fundamental completo - SP ---------
      
      # Limite inferior de Renda
      GF_11_RI_2002 <- 300
      # Limite superior de Renda
      GF_11_RF_2002 <- 800
      
      # Criar a tabela Renda, Coorte e Probabilidade
      RCP_11_2002 <- rbind(
        add_column(cbind(Y, Coorte = "Homem Branco"), Prob = MDLE_11_2002$Homem_Branco),
        add_column(cbind(Y, Coorte = "Homem Negro"), Prob = MDLE_11_2002$Homem_Negro),
        add_column(cbind(Y, Coorte = "Mulher Branca"), Prob = MDLE_11_2002$Mulher_Branca),
        add_column(cbind(Y, Coorte = "Mulher Negra"), Prob = MDLE_11_2002$Mulher_Negra))
      
      # Criar do gráfico
      GF_11_2002 <- ggplot() +
        geom_line(data = MDLE_11_2002[MDLE_11_2002$Renda >= GF_11_RI_2002 & MDLE_11_2002$Renda <= GF_11_RF_2002, ], 
                  aes(x = Renda, y = Mulher_Negra), size = 0.7) +
        geom_line(data = MDLE_11_2002[MDLE_11_2002$Renda >= GF_11_RI_2002 & MDLE_11_2002$Renda <= GF_11_RF_2002, ], 
                  aes(x = Renda, y = Homem_Negro), size = 0.7) +
        geom_line(data = MDLE_11_2002[MDLE_11_2002$Renda >= GF_11_RI_2002 & MDLE_11_2002$Renda <= GF_11_RF_2002, ], 
                  aes(x = Renda, y = Mulher_Branca), size = 0.7) +
        geom_line(data = MDLE_11_2002[MDLE_11_2002$Renda >= GF_11_RI_2002 & MDLE_11_2002$Renda <= GF_11_RF_2002, ], 
                  aes(x = Renda, y = Homem_Branco), size = 0.7) +
        geom_point(data = RCP_11_2002[RCP_11_2002$Renda >= GF_11_RI_2002 & RCP_11_2002$Renda <= GF_11_RF_2002, ], 
                   aes(x = Renda, y = Prob, shape = Coorte, fill = Coorte), size = 3.0) +
        theme_classic() +
        theme(panel.grid.major.y = element_line(color = "black"), 
              panel.background = element_rect(color = "black"),
              legend.position = "bottom",
              legend.title = element_text(colour = "white"),
              axis.text = element_text(color = "black"),
              text = element_text(family = "Times New Roman")) +
        coord_cartesian(ylim = c(0.00, round(max(RCP_11_2002[RCP_11_2002$Renda >= GF_11_RI_2002 & RCP_11_2002$Renda <= GF_11_RF_2002, ]$Prob, 
                                                 na.rm = TRUE) + 0.05, 
                                             digits = 2)), 
                        xlim = c(GF_11_RI_2002-50, GF_11_RF_2002+50)) +
        scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
                                                          decimal.mark = ",", 
                                                          big.mark = ""), 
                           breaks = seq(0.00, round(max(RCP_11_2002[RCP_11_2002$Renda >= GF_11_RI_2002 & RCP_11_2002$Renda <= GF_11_RF_2002, ]$Prob, 
                                                        na.rm = TRUE) + 0.05, 
                                                    digits = 2), 
                                        by = 0.10)) +
        scale_x_continuous(expand = expansion(add = c(0, 0)), 
                           breaks = seq(0, 10000, by = 100)) +
        scale_shape_manual(values = c(4, 24, 22, 18)) +
        scale_fill_manual(values = c("black", "grey90", "grey30", "black")) +
        labs(x = "renda", y = "probabilidade")
      
# # # 12 - Ocupados de 40 anos e Antigo primário completo - SP ------------
      
      # Limite inferior de Renda
      GF_12_RI_2002 <- 300
      # Limite superior de Renda
      GF_12_RF_2002 <- 800
      
      # Criar a tabela Renda, Coorte e Probabilidade
      RCP_12_2002 <- rbind(
        add_column(cbind(Y, Coorte = "Homem Branco"), Prob = MDLE_12_2002$Homem_Branco),
        add_column(cbind(Y, Coorte = "Homem Negro"), Prob = MDLE_12_2002$Homem_Negro),
        add_column(cbind(Y, Coorte = "Mulher Branca"), Prob = MDLE_12_2002$Mulher_Branca),
        add_column(cbind(Y, Coorte = "Mulher Negra"), Prob = MDLE_12_2002$Mulher_Negra))
      
      # Criar do gráfico
      GF_12_2002 <- ggplot() +
        geom_line(data = MDLE_12_2002[MDLE_12_2002$Renda >= GF_12_RI_2002 & MDLE_12_2002$Renda <= GF_12_RF_2002, ], 
                  aes(x = Renda, y = Mulher_Negra), size = 0.7) +
        geom_line(data = MDLE_12_2002[MDLE_12_2002$Renda >= GF_12_RI_2002 & MDLE_12_2002$Renda <= GF_12_RF_2002, ], 
                  aes(x = Renda, y = Homem_Negro), size = 0.7) +
        geom_line(data = MDLE_12_2002[MDLE_12_2002$Renda >= GF_12_RI_2002 & MDLE_12_2002$Renda <= GF_12_RF_2002, ], 
                  aes(x = Renda, y = Mulher_Branca), size = 0.7) +
        geom_line(data = MDLE_12_2002[MDLE_12_2002$Renda >= GF_12_RI_2002 & MDLE_12_2002$Renda <= GF_12_RF_2002, ], 
                  aes(x = Renda, y = Homem_Branco), size = 0.7) +
        geom_point(data = RCP_12_2002[RCP_12_2002$Renda >= GF_12_RI_2002 & RCP_12_2002$Renda <= GF_12_RF_2002, ], 
                   aes(x = Renda, y = Prob, shape = Coorte, fill = Coorte), size = 3.0) +
        theme_classic() +
        theme(panel.grid.major.y = element_line(color = "black"), 
              panel.background = element_rect(color = "black"),
              legend.position = "bottom",
              legend.title = element_text(colour = "white"),
              axis.text = element_text(color = "black"),
              text = element_text(family = "Times New Roman")) +
        coord_cartesian(ylim = c(0.00, round(max(RCP_12_2002[RCP_12_2002$Renda >= GF_12_RI_2002 & RCP_12_2002$Renda <= GF_12_RF_2002, ]$Prob, 
                                                 na.rm = TRUE) + 0.05, 
                                             digits = 2)), 
                        xlim = c(GF_12_RI_2002-50, GF_12_RF_2002+50)) +
        scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
                                                          decimal.mark = ",", 
                                                          big.mark = ""), 
                           breaks = seq(0.00, round(max(RCP_12_2002[RCP_12_2002$Renda >= GF_12_RI_2002 & RCP_12_2002$Renda <= GF_12_RF_2002, ]$Prob, 
                                                        na.rm = TRUE) + 0.05, 
                                                    digits = 2), 
                                        by = 0.10)) +
        scale_x_continuous(expand = expansion(add = c(0, 0)), 
                           breaks = seq(0, 10000, by = 100)) +
        scale_shape_manual(values = c(4, 24, 22, 18)) +
        scale_fill_manual(values = c("black", "grey90", "grey30", "black")) +
        labs(x = "renda", y = "probabilidade")
      
# ANEXO 2 - TABELA DOS COEFICIENTES ---------------------------------------
# # DIRIGENTES E GERENTES -------------------------------------------------
      
      # Montar o data frame
      DF_DG_2002 <- cbind(Y,
                          Intercepto_SP = MDL_07_2002$Intercepto,
                          s_Int_SP = MDL_07_2002$s_Int,
                          Intercepto_BA = MDL_08_2002$Intercepto,
                          s_Int_BA = MDL_08_2002$s_Int,
                          Cor_SP = MDL_07_2002$Cor,
                          s_Cor_SP = MDL_07_2002$s_Cor,
                          Cor_BA = MDL_08_2002$Cor,
                          s_Cor_BA = MDL_08_2002$s_Cor,
                          Sexo_SP = MDL_07_2002$Sexo,
                          s_Sex_SP = MDL_07_2002$s_Sex,
                          Sexo_BA = MDL_08_2002$Sexo,
                          s_Sex_BA = MDL_08_2002$s_Sex,
                          Idade_SP = MDL_07_2002$Idade,
                          s_Ida_SP = MDL_07_2002$s_Ida,
                          Idade_BA = MDL_08_2002$Idade,
                          s_Ida_BA = MDL_08_2002$s_Ida,
                          Escolaridade_SP = MDL_07_2002$Escolaridade,
                          s_Esc_SP = MDL_07_2002$s_Esc,
                          Escolaridade_BA = MDL_08_2002$Escolaridade,
                          s_Esc_BA = MDL_08_2002$s_Esc
      )
      
      # Criar a tabela
      TBL_DG_2002 <- gt(DF_DG_2002[DF_DG_2002$Renda >= min(c(GF_01_RI_2002, GF_02_RI_2002, GF_03_RI_2002, GF_04_RI_2002)) & 
                                     DF_DG_2002$Renda <= max(c(GF_01_RF_2002, GF_02_RF_2002, GF_03_RF_2002, GF_04_RF_2002)), ]) %>%
        tab_options(table.font.names = "Times New Roman",
                    heading.border.bottom.color = "black",
                    column_labels.border.top.color = "black",
                    column_labels.border.bottom.color = "black",
                    table_body.border.bottom.color = "black",
                    table_body.border.top.color = "black",
                    table_body.hlines.style = "white",
                    table.font.color = "black",
                    table.border.top.color = "black",
                    table.border.bottom.color = "black") %>%
        cols_label(Renda = "Y", 
                   Intercepto_SP ="São Paulo", s_Int_SP = "",
                   Cor_SP = "São Paulo", s_Cor_SP = "",
                   Sexo_SP = "São Paulo", s_Sex_SP = "",
                   Idade_SP = "São Paulo", s_Ida_SP = "",
                   Escolaridade_SP = "São Paulo", s_Esc_SP = "",
                   Intercepto_BA ="Bahia", s_Int_BA = "",
                   Cor_BA = "Bahia", s_Cor_BA = "",
                   Sexo_BA = "Bahia", s_Sex_BA = "",
                   Idade_BA = "Bahia", s_Ida_BA = "",
                   Escolaridade_BA = "Bahia", s_Esc_BA = "") %>%
        tab_spanner(label = "Intercepto", 
                    columns = c(Intercepto_SP, 
                                s_Int_SP, 
                                Intercepto_BA, 
                                s_Int_BA)) %>%
        tab_spanner(label = "Cor ou Raça", 
                    columns = c(Cor_SP,
                                s_Cor_SP, 
                                Cor_BA, 
                                s_Cor_BA)) %>%
        tab_spanner(label = "Sexo", 
                    columns = c(Sexo_SP, 
                                s_Sex_SP, 
                                Sexo_BA, 
                                s_Sex_BA)) %>%
        tab_spanner(label = "Idade", 
                    columns = c(Idade_SP, 
                                s_Ida_SP, 
                                Idade_BA, 
                                s_Ida_BA)) %>%
        tab_spanner(label = "Escolaridade",
                    columns = c(Escolaridade_SP, 
                                s_Esc_SP, 
                                Escolaridade_BA, 
                                s_Esc_BA)) %>%
        tab_header(md(paste("**Dirigentes e Gerentes - Amostra:**<b>", 
                            nrow(TBL_ENS_07_2002), 
                            "</b>**(SP);**<b>", 
                            nrow(TBL_ENS_08_2002), 
                            "</b>**(BA)**"))) %>%
        fmt_number(columns = contains("_"), 
                   rows = everything(), 
                   decimals = 6, 
                   scale_by = 1, 
                   sep_mark = ".",
                   dec_mark = ",",
                   drop_trailing_dec_mark = TRUE)%>%
        tab_style(style = list(cell_fill("white"), 
                               cell_text("black")), 
                  locations = list(cells_body(), 
                                   cells_column_labels(), 
                                   cells_column_spanners(), 
                                   cells_title())) %>%
        opt_align_table_header()
      
      # Visualizar a tabela                  
      TBL_DG_2002
      
      # Salvar a tabela
      TBL_DG_2002_html <- tempfile(fileext = ".html")
      gtsave(TBL_DG_2002, TBL_DG_2002_html)
      webshot2::webshot(url = paste0("file:///", TBL_DG_2002_html), 
                        file = "TBL_DG_2002.png",
                        vwidth = 1050,
                        zoom = 5)
      
# # EMPREGADOS COM REGISTRO -----------------------------------------------
      
      # Montar o data frame
      DF_CR_2002 <- cbind(Y,
                          Intercepto_SP = MDL_05_2002$Intercepto,
                          s_Int_SP = MDL_05_2002$s_Int,
                          Intercepto_BA = MDL_06_2002$Intercepto,
                          s_Int_BA = MDL_06_2002$s_Int,
                          Cor_SP = MDL_05_2002$Cor,
                          s_Cor_SP = MDL_05_2002$s_Cor,
                          Cor_BA = MDL_06_2002$Cor,
                          s_Cor_BA = MDL_06_2002$s_Cor,
                          Sexo_SP = MDL_05_2002$Sexo,
                          s_Sex_SP = MDL_05_2002$s_Sex,
                          Sexo_BA = MDL_06_2002$Sexo,
                          s_Sex_BA = MDL_06_2002$s_Sex,
                          Idade_SP = MDL_05_2002$Idade,
                          s_Ida_SP = MDL_05_2002$s_Ida,
                          Idade_BA = MDL_06_2002$Idade,
                          s_Ida_BA = MDL_06_2002$s_Ida,
                          Escolaridade_SP = MDL_05_2002$Escolaridade,
                          s_Esc_SP = MDL_05_2002$s_Esc,
                          Escolaridade_BA = MDL_06_2002$Escolaridade,
                          s_Esc_BA = MDL_06_2002$s_Esc
      )
      
      # Criar a tabela
      TBL_CR_2002 <- gt(DF_CR_2002[DF_CR_2002$Renda >= min(c(GF_05_RI_2002, GF_06_RI_2002, GF_07_RI_2002, GF_08_RI_2002)) & 
                                     DF_CR_2002$Renda <= max(c(GF_05_RF_2002, GF_06_RF_2002, GF_07_RF_2002, GF_08_RF_2002)), ]) %>%
        tab_options(table.font.names = "Times New Roman",
                    heading.border.bottom.color = "black",
                    column_labels.border.top.color = "black",
                    column_labels.border.bottom.color = "black",
                    table_body.border.bottom.color = "black",
                    table_body.border.top.color = "black",
                    table_body.hlines.style = "white",
                    table.font.color = "black",
                    table.border.top.color = "black",
                    table.border.bottom.color = "black") %>%
        cols_label(Renda = "Y", 
                   Intercepto_SP ="São Paulo", s_Int_SP = "",
                   Cor_SP = "São Paulo", s_Cor_SP = "",
                   Sexo_SP = "São Paulo", s_Sex_SP = "",
                   Idade_SP = "São Paulo", s_Ida_SP = "",
                   Escolaridade_SP = "São Paulo", s_Esc_SP = "",
                   Intercepto_BA ="Bahia", s_Int_BA = "",
                   Cor_BA = "Bahia", s_Cor_BA = "",
                   Sexo_BA = "Bahia", s_Sex_BA = "",
                   Idade_BA = "Bahia", s_Ida_BA = "",
                   Escolaridade_BA = "Bahia", s_Esc_BA = "") %>%
        tab_spanner(label = "Intercepto", 
                    columns = c(Intercepto_SP, 
                                s_Int_SP, 
                                Intercepto_BA, 
                                s_Int_BA)) %>%
        tab_spanner(label = "Cor ou Raça", 
                    columns = c(Cor_SP,
                                s_Cor_SP, 
                                Cor_BA, 
                                s_Cor_BA)) %>%
        tab_spanner(label = "Sexo", 
                    columns = c(Sexo_SP, 
                                s_Sex_SP, 
                                Sexo_BA, 
                                s_Sex_BA)) %>%
        tab_spanner(label = "Idade", 
                    columns = c(Idade_SP, 
                                s_Ida_SP, 
                                Idade_BA, 
                                s_Ida_BA)) %>%
        tab_spanner(label = "Escolaridade",
                    columns = c(Escolaridade_SP, 
                                s_Esc_SP, 
                                Escolaridade_BA, 
                                s_Esc_BA)) %>%
        tab_header(md(paste("**Empregados com Registro - Amostra:**<b>", 
                            nrow(TBL_ENS_05_2002), 
                            "</b>**(SP);**<b>", 
                            nrow(TBL_ENS_06_2002), 
                            "</b>**(BA)**"))) %>%
        fmt_number(columns = contains("_"), 
                   rows = everything(), 
                   decimals = 6, 
                   scale_by = 1, 
                   sep_mark = ".",
                   dec_mark = ",",
                   drop_trailing_dec_mark = TRUE)%>%
        tab_style(style = list(cell_fill("white"), 
                               cell_text("black")), 
                  locations = list(cells_body(), 
                                   cells_column_labels(), 
                                   cells_column_spanners(), 
                                   cells_title())) %>%
        opt_align_table_header()
      
      # Visualizar a tabela                  
      TBL_CR_2002
      
      # Salvar a tabela
      TBL_CR_2002_html <- tempfile(fileext = ".html")
      gtsave(TBL_CR_2002, TBL_CR_2002_html)
      webshot2::webshot(url = paste0("file:///", TBL_CR_2002_html), 
                        file = "TBL_CR_2002.png",
                        vwidth = 1050,
                        zoom = 5)
      
      
# # EMPREGADOS SEM REGISTRO -----------------------------------------------
      
      # Montar o data frame
      DF_SR_2002 <- cbind(Y,
                          Intercepto_SP = MDL_03_2002$Intercepto,
                          s_Int_SP = MDL_03_2002$s_Int,
                          Intercepto_BA = MDL_04_2002$Intercepto,
                          s_Int_BA = MDL_04_2002$s_Int,
                          Cor_SP = MDL_03_2002$Cor,
                          s_Cor_SP = MDL_03_2002$s_Cor,
                          Cor_BA = MDL_04_2002$Cor,
                          s_Cor_BA = MDL_04_2002$s_Cor,
                          Sexo_SP = MDL_03_2002$Sexo,
                          s_Sex_SP = MDL_03_2002$s_Sex,
                          Sexo_BA = MDL_04_2002$Sexo,
                          s_Sex_BA = MDL_04_2002$s_Sex,
                          Idade_SP = MDL_03_2002$Idade,
                          s_Ida_SP = MDL_03_2002$s_Ida,
                          Idade_BA = MDL_04_2002$Idade,
                          s_Ida_BA = MDL_04_2002$s_Ida,
                          Escolaridade_SP = MDL_03_2002$Escolaridade,
                          s_Esc_SP = MDL_03_2002$s_Esc,
                          Escolaridade_BA = MDL_04_2002$Escolaridade,
                          s_Esc_BA = MDL_04_2002$s_Esc
      )
      
      # Criar a tabela
      TBL_SR_2002 <- gt(DF_SR_2002[DF_SR_2002$Renda >= min(c(GF_09_RI_2002, GF_10_RI_2002, GF_11_RI_2002, GF_12_RI_2002)) & 
                                     DF_SR_2002$Renda <= max(c(GF_09_RF_2002, GF_10_RF_2002, GF_11_RF_2002, GF_12_RF_2002)), ]) %>%
        tab_options(table.font.names = "Times New Roman",
                    heading.border.bottom.color = "black",
                    column_labels.border.top.color = "black",
                    column_labels.border.bottom.color = "black",
                    table_body.border.bottom.color = "black",
                    table_body.border.top.color = "black",
                    table_body.hlines.style = "white",
                    table.font.color = "black",
                    table.border.top.color = "black",
                    table.border.bottom.color = "black") %>%
        cols_label(Renda = "Y", 
                   Intercepto_SP ="São Paulo", s_Int_SP = "",
                   Cor_SP = "São Paulo", s_Cor_SP = "",
                   Sexo_SP = "São Paulo", s_Sex_SP = "",
                   Idade_SP = "São Paulo", s_Ida_SP = "",
                   Escolaridade_SP = "São Paulo", s_Esc_SP = "",
                   Intercepto_BA ="Bahia", s_Int_BA = "",
                   Cor_BA = "Bahia", s_Cor_BA = "",
                   Sexo_BA = "Bahia", s_Sex_BA = "",
                   Idade_BA = "Bahia", s_Ida_BA = "",
                   Escolaridade_BA = "Bahia", s_Esc_BA = "") %>%
        tab_spanner(label = "Intercepto", 
                    columns = c(Intercepto_SP, 
                                s_Int_SP, 
                                Intercepto_BA, 
                                s_Int_BA)) %>%
        tab_spanner(label = "Cor ou Raça", 
                    columns = c(Cor_SP,
                                s_Cor_SP, 
                                Cor_BA, 
                                s_Cor_BA)) %>%
        tab_spanner(label = "Sexo", 
                    columns = c(Sexo_SP, 
                                s_Sex_SP, 
                                Sexo_BA, 
                                s_Sex_BA)) %>%
        tab_spanner(label = "Idade", 
                    columns = c(Idade_SP, 
                                s_Ida_SP, 
                                Idade_BA, 
                                s_Ida_BA)) %>%
        tab_spanner(label = "Escolaridade",
                    columns = c(Escolaridade_SP, 
                                s_Esc_SP, 
                                Escolaridade_BA, 
                                s_Esc_BA)) %>%
        tab_header(md(paste("**Empregados sem Registro - Amostra:**<b>", 
                            nrow(TBL_ENS_03_2002), 
                            "</b>**(SP);**<b>", 
                            nrow(TBL_ENS_04_2002), 
                            "</b>**(BA)**"))) %>%
        fmt_number(columns = contains("_"), 
                   rows = everything(), 
                   decimals = 6, 
                   scale_by = 1, 
                   sep_mark = ".",
                   dec_mark = ",",
                   drop_trailing_dec_mark = TRUE)%>%
        tab_style(style = list(cell_fill("white"), 
                               cell_text("black")), 
                  locations = list(cells_body(), 
                                   cells_column_labels(), 
                                   cells_column_spanners(), 
                                   cells_title())) %>%
        opt_align_table_header()
      
      # Visualizar a tabela                  
      TBL_SR_2002
      
      # Salvar a tabela
      TBL_SR_2002_html <- tempfile(fileext = ".html")
      gtsave(TBL_SR_2002, TBL_SR_2002_html)
      webshot2::webshot(url = paste0("file:///", TBL_SR_2002_html), 
                        file = "TBL_SR_2002.png",
                        vwidth = 1050,
                        zoom = 5)
      
# # PRIMEIRO QUINTIL ------------------------------------------------------
      
      # Montar o data frame
      DF_PQ_2002 <- cbind(Y,
                          Intercepto_SP = MDL_01_2002$Intercepto,
                          s_Int_SP = MDL_01_2002$s_Int,
                          Intercepto_BA = MDL_02_2002$Intercepto,
                          s_Int_BA = MDL_02_2002$s_Int,
                          Cor_SP = MDL_01_2002$Cor,
                          s_Cor_SP = MDL_01_2002$s_Cor,
                          Cor_BA = MDL_02_2002$Cor,
                          s_Cor_BA = MDL_02_2002$s_Cor,
                          Sexo_SP = MDL_01_2002$Sexo,
                          s_Sex_SP = MDL_01_2002$s_Sex,
                          Sexo_BA = MDL_02_2002$Sexo,
                          s_Sex_BA = MDL_02_2002$s_Sex,
                          Idade_SP = MDL_01_2002$Idade,
                          s_Ida_SP = MDL_01_2002$s_Ida,
                          Idade_BA = MDL_02_2002$Idade,
                          s_Ida_BA = MDL_02_2002$s_Ida,
                          Escolaridade_SP = MDL_01_2002$Escolaridade,
                          s_Esc_SP = MDL_01_2002$s_Esc,
                          Escolaridade_BA = MDL_02_2002$Escolaridade,
                          s_Esc_BA = MDL_02_2002$s_Esc
      )
      
      # Criar a tabela
      TBL_PQ_2002 <- gt(DF_PQ_2002[DF_PQ_2002$Renda >= 100 & DF_PQ_2002$Renda <= 600, ]) %>%
        tab_options(table.font.names = "Times New Roman",
                    heading.border.bottom.color = "black",
                    column_labels.border.top.color = "black",
                    column_labels.border.bottom.color = "black",
                    table_body.border.bottom.color = "black",
                    table_body.border.top.color = "black",
                    table_body.hlines.style = "white",
                    table.font.color = "black",
                    table.border.top.color = "black",
                    table.border.bottom.color = "black") %>%
        cols_label(Renda = "Y", 
                   Intercepto_SP ="São Paulo", s_Int_SP = "",
                   Cor_SP = "São Paulo", s_Cor_SP = "",
                   Sexo_SP = "São Paulo", s_Sex_SP = "",
                   Idade_SP = "São Paulo", s_Ida_SP = "",
                   Escolaridade_SP = "São Paulo", s_Esc_SP = "",
                   Intercepto_BA ="Bahia", s_Int_BA = "",
                   Cor_BA = "Bahia", s_Cor_BA = "",
                   Sexo_BA = "Bahia", s_Sex_BA = "",
                   Idade_BA = "Bahia", s_Ida_BA = "",
                   Escolaridade_BA = "Bahia", s_Esc_BA = "") %>%
        tab_spanner(label = "Intercepto", 
                    columns = c(Intercepto_SP, 
                                s_Int_SP, 
                                Intercepto_BA, 
                                s_Int_BA)) %>%
        tab_spanner(label = "Cor ou Raça", 
                    columns = c(Cor_SP,
                                s_Cor_SP, 
                                Cor_BA, 
                                s_Cor_BA)) %>%
        tab_spanner(label = "Sexo", 
                    columns = c(Sexo_SP, 
                                s_Sex_SP, 
                                Sexo_BA, 
                                s_Sex_BA)) %>%
        tab_spanner(label = "Idade", 
                    columns = c(Idade_SP, 
                                s_Ida_SP, 
                                Idade_BA, 
                                s_Ida_BA)) %>%
        tab_spanner(label = "Escolaridade",
                    columns = c(Escolaridade_SP, 
                                s_Esc_SP, 
                                Escolaridade_BA, 
                                s_Esc_BA)) %>%
        tab_header(md(paste("**Primeiro Quintil - Amostra:**<b>", 
                            nrow(TBL_ENS_01_2002), 
                            "</b>**(SP);**<b>", 
                            nrow(TBL_ENS_02_2002), 
                            "</b>**(BA)**"))) %>%
        fmt_number(columns = contains("_"), 
                   rows = everything(), 
                   decimals = 6, 
                   scale_by = 1, 
                   sep_mark = ".",
                   dec_mark = ",",
                   drop_trailing_dec_mark = TRUE)%>%
        tab_style(style = list(cell_fill("white"), 
                               cell_text("black")), 
                  locations = list(cells_body(), 
                                   cells_column_labels(), 
                                   cells_column_spanners(), 
                                   cells_title())) %>%
        opt_align_table_header()
      
      # Visualizar a tabela                  
      TBL_PQ_2002
      
      # Salvar a tabela
      TBL_PQ_2002_html <- tempfile(fileext = ".html")
      gtsave(TBL_PQ_2002, TBL_PQ_2002_html)
      webshot2::webshot(url = paste0("file:///", TBL_PQ_2002_html), 
                        file = "TBL_PQ_2002.png",
                        vwidth = 1150,
                        zoom = 5)
      
      