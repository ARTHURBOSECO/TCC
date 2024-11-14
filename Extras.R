
# TESTES ------------------------------------------------------------------


  library(writexl)

  pnadc2022T4_E1 <- pnadc2022T4_5.6 %>%
    filter(UF == UF_BA_2022 & Cod_Ocup <= 1439 & Cod_Ocup >= 1111)
  
  pnadc2022T4_E2 <- pnadc2022T4_5.6 %>%
    filter(UF == UF_SP_2002 & Cod_Ocup <= 1439 & Cod_Ocup >= 1111)
  
  median(pnadc2002T4_E1$RendaE)
  median(pnadc2002T4_E2$RendaE)
  
  TBL_ESC<- t(as_tibble(rbind(
  t(as_tibble(rbind(
    t(median(data.frame(filter(pnad2002, UF == UF_BA_2002, Cor == Negros, Sexo == Homem))$Ano_Esc)),
    t(median(data.frame(filter(pnad2002, UF == UF_BA_2002, Cor == Negros, Sexo == Mulher))$Ano_Esc)),
    t(median(data.frame(filter(pnad2002, UF == UF_BA_2002, Cor == Brancos, Sexo == Homem))$Ano_Esc)),
    t(median(data.frame(filter(pnad2002, UF == UF_BA_2002, Cor == Brancos, Sexo == Mulher))$Ano_Esc))
  ))),
  t(as_tibble(rbind(
    t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Cor == Negros, Sexo == Homem))$Ano_Esc)),
    t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Cor == Negros, Sexo == Mulher))$Ano_Esc)),
    t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Cor == Brancos, Sexo == Homem))$Ano_Esc)),
    t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Cor == Brancos, Sexo == Mulher))$Ano_Esc))
  ))),
  t(as_tibble(rbind(
    t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Cor == Negros, Sexo == Homem))$Ano_Esc)),
    t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Cor == Negros, Sexo == Mulher))$Ano_Esc)),
    t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Cor == Brancos, Sexo == Homem))$Ano_Esc)),
    t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Cor == Brancos, Sexo == Mulher))$Ano_Esc))
  ))),
  t(as_tibble(rbind(
    t(median(data.frame(filter(pnad2002, UF == UF_BA_2002, Cor == Negros, Sexo == Homem))$Ano_Esc)),
    t(median(data.frame(filter(pnad2002, UF == UF_BA_2002, Cor == Negros, Sexo == Mulher))$Ano_Esc)),
    t(median(data.frame(filter(pnad2002, UF == UF_BA_2002, Cor == Brancos, Sexo == Homem))$Ano_Esc)),
    t(median(data.frame(filter(pnad2002, UF == UF_BA_2002, Cor == Brancos, Sexo == Mulher))$Ano_Esc))
  ))),
  t(as_tibble(rbind(
    t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Cor == Negros, Sexo == Homem))$Ano_Esc)),
    t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Cor == Negros, Sexo == Mulher))$Ano_Esc)),
    t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Cor == Brancos, Sexo == Homem))$Ano_Esc)),
    t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Cor == Brancos, Sexo == Mulher))$Ano_Esc))
  ))),
  t(as_tibble(rbind(
    t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Cor == Negros, Sexo == Homem))$Ano_Esc)),
    t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Cor == Negros, Sexo == Mulher))$Ano_Esc)),
    t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Cor == Brancos, Sexo == Homem))$Ano_Esc)),
    t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Cor == Brancos, Sexo == Mulher))$Ano_Esc))
  )))
  )))
  
  names(TBL_ESC) <- c("2002", 
                      "2012", 
                      "2022", 
                      "2002", 
                      "2012", 
                      "2022"
                      )

  write_xlsx(as.data.frame(TBL_ESC), "TBL_ESC.xlsx")
  

# TABELA DE ESCOLARIDADE --------------------------------------------------

  TBL_ESC <- t(as_tibble(cbind(
    t(as_tibble(cbind(
      t(median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Classe == "DG"))$Ano_Esc)),
      t(median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Classe == "DG"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Classe == "DG"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Classe == "DG"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, Classe == "DG"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, Classe == "DG"))$Ano_Esc)),
      t(median(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem, Classe == "DG"))$Ano_Esc)),
      t(median(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher, Classe == "DG"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem, Classe == "DG"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Classe == "DG"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Classe == "DG"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Classe == "DG"))$Ano_Esc))
    ))),
    t(as_tibble(cbind(
      t(median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, ECR == "ECR"))$Ano_Esc)),
      t(median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, ECR == "ECR"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, ECR == "ECR"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, ECR == "ECR"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, ECR == "ECR"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, ECR == "ECR"))$Ano_Esc)),
      t(median(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem, ECR == "ECR"))$Ano_Esc)),
      t(median(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher, ECR == "ECR"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem, ECR == "ECR"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher, ECR == "ECR"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, ECR == "ECR"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, ECR == "ECR"))$Ano_Esc))
    ))),
    t(as_tibble(cbind(
      t(median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, ESR == "ESR"))$Ano_Esc)),
      t(median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, ESR == "ESR"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, ESR == "ESR"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, ESR == "ESR"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, ESR == "ESR"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, ESR == "ESR"))$Ano_Esc)),
      t(median(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem, ESR == "ESR"))$Ano_Esc)),
      t(median(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher, ESR == "ESR"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem, ESR == "ESR"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher, ESR == "ESR"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, ESR == "ESR"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, ESR == "ESR"))$Ano_Esc))
    ))),
    t(as_tibble(cbind(
      t(median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Prim_Quintil == "PQ"))$Ano_Esc)),
      t(median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Prim_Quintil == "PQ"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Prim_Quintil == "PQ"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Prim_Quintil == "PQ"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, Prim_Quintil == "PQ"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, Prim_Quintil == "PQ"))$Ano_Esc)),
      t(median(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem, Prim_Quintil == "PQ"))$Ano_Esc)),
      t(median(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher, Prim_Quintil == "PQ"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem, Prim_Quintil == "PQ"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Prim_Quintil == "PQ"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Prim_Quintil == "PQ"))$Ano_Esc)),
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Prim_Quintil == "PQ"))$Ano_Esc))
    )))
  )))
  
  t(mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Negros, Classe == "DG"))$Ano_Esc) -
    mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Negros, Classe == "DG"))$Ano_Esc))
  
  t(mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Brancos, Classe == "DG"))$Ano_Esc) -
    mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Brancos, Classe == "DG"))$Ano_Esc))
  
  t(mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Cor == Negros, Classe == "DG"))$Ano_Esc) -
    mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Cor == Negros, Classe == "DG"))$Ano_Esc))
  
  t(mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2002, Sexo == Mulher, Cor == Brancos, Classe == "DG"))$Ano_Esc) -
    mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2002, Sexo == Homem, Cor == Brancos, Classe == "DG"))$Ano_Esc))
  
  t(mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Cor == Negros, Classe == "DG"))$Ano_Esc) -
    mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2012, Sexo == Homem, Cor == Negros, Classe == "DG"))$Ano_Esc))
  
  t(mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Cor == Brancos, Classe == "DG"))$Ano_Esc) -
    mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Cor == Brancos, Classe == "DG"))$Ano_Esc))
#  
  t(mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Negros, ECR == "ECR"))$Ano_Esc) -
      mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Negros, ECR == "ECR"))$Ano_Esc))
  
  t(mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Brancos, ECR == "ECR"))$Ano_Esc) -
      mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Brancos, ECR == "ECR"))$Ano_Esc))
  
  t(mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Cor == Negros, ECR == "ECR"))$Ano_Esc) -
      mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Cor == Negros, ECR == "ECR"))$Ano_Esc))
  
  t(mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2002, Sexo == Mulher, Cor == Brancos, ECR == "ECR"))$Ano_Esc) -
      mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2002, Sexo == Homem, Cor == Brancos, ECR == "ECR"))$Ano_Esc))
  
  t(mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Cor == Negros, ECR == "ECR"))$Ano_Esc) -
      mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2012, Sexo == Homem, Cor == Negros, ECR == "ECR"))$Ano_Esc))
  
  t(mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Cor == Brancos, ECR == "ECR"))$Ano_Esc) -
      mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Cor == Brancos, ECR == "ECR"))$Ano_Esc))
#
  t(mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Negros, ESR == "ESR"))$Ano_Esc) -
      mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Negros, ESR == "ESR"))$Ano_Esc))
  
  t(mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Brancos, ESR == "ESR"))$Ano_Esc) -
      mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Brancos, ESR == "ESR"))$Ano_Esc))
  
  t(mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Cor == Negros, ESR == "ESR"))$Ano_Esc) -
      mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Cor == Negros, ESR == "ESR"))$Ano_Esc))
  
  t(mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2002, Sexo == Mulher, Cor == Brancos, ESR == "ESR"))$Ano_Esc) -
      mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2002, Sexo == Homem, Cor == Brancos, ESR == "ESR"))$Ano_Esc))
  
  t(mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Cor == Negros, ESR == "ESR"))$Ano_Esc) -
      mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2012, Sexo == Homem, Cor == Negros, ESR == "ESR"))$Ano_Esc))
  
  t(mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Cor == Brancos, ESR == "ESR"))$Ano_Esc) -
      mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Cor == Brancos, ESR == "ESR"))$Ano_Esc))
#
  t(mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc) -
      mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc))
  
  t(mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc) -
      mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc))
  
  t(mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc) -
      mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc))
  
  t(mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2002, Sexo == Mulher, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc) -
      mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2002, Sexo == Homem, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc))
  
  t(mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc) -
      mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2012, Sexo == Homem, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc))
  
  t(mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc) -
      mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc))
  
  TBL_ESC_I <- t(as_tibble(cbind(
    t(as_tibble(cbind(
      t(mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Negros, Classe == "DG"))$Ano_Esc) -
          mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Negros, Classe == "DG"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Brancos, Classe == "DG"))$Ano_Esc) -
          mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Brancos, Classe == "DG"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Cor == Negros, Classe == "DG"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Cor == Negros, Classe == "DG"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Cor == Brancos, Classe == "DG"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Cor == Brancos, Classe == "DG"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, Cor == Negros, Classe == "DG"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, Cor == Negros, Classe == "DG"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, Cor == Brancos, Classe == "DG"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, Cor == Brancos, Classe == "DG"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher, Cor == Negros, Classe == "DG"))$Ano_Esc) -
          mean(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem, Cor == Negros, Classe == "DG"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher, Cor == Brancos, Classe == "DG"))$Ano_Esc) -
          mean(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem, Cor == Brancos, Classe == "DG"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Cor == Negros, Classe == "DG"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem, Cor == Negros, Classe == "DG"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Cor == Brancos, Classe == "DG"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem, Cor == Brancos, Classe == "DG"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Cor == Negros, Classe == "DG"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Cor == Negros, Classe == "DG"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Cor == Brancos, Classe == "DG"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Cor == Brancos, Classe == "DG"))$Ano_Esc))
    ))),
    
    t(as_tibble(cbind(
      t(mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Negros, ECR == "ECR"))$Ano_Esc) -
          mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Negros, ECR == "ECR"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Brancos, ECR == "ECR"))$Ano_Esc) -
          mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Brancos, ECR == "ECR"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Cor == Negros, ECR == "ECR"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Cor == Negros, ECR == "ECR"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Cor == Brancos, ECR == "ECR"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Cor == Brancos, ECR == "ECR"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, Cor == Negros, ECR == "ECR"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, Cor == Negros, ECR == "ECR"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, Cor == Brancos, ECR == "ECR"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, Cor == Brancos, ECR == "ECR"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher, Cor == Negros, ECR == "ECR"))$Ano_Esc) -
          mean(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem, Cor == Negros, ECR == "ECR"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher, Cor == Brancos, ECR == "ECR"))$Ano_Esc) -
          mean(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem, Cor == Brancos, ECR == "ECR"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Cor == Negros, ECR == "ECR"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem, Cor == Negros, ECR == "ECR"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Cor == Brancos, ECR == "ECR"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem, Cor == Brancos, ECR == "ECR"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Cor == Negros, ECR == "ECR"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Cor == Negros, ECR == "ECR"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Cor == Brancos, ECR == "ECR"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Cor == Brancos, ECR == "ECR"))$Ano_Esc))
      
    ))),
    
    t(as_tibble(cbind(
      t(mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Negros, ESR == "ESR"))$Ano_Esc) -
          mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Negros, ESR == "ESR"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Brancos, ESR == "ESR"))$Ano_Esc) -
          mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Brancos, ESR == "ESR"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Cor == Negros, ESR == "ESR"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Cor == Negros, ESR == "ESR"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Cor == Brancos, ESR == "ESR"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Cor == Brancos, ESR == "ESR"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, Cor == Negros, ESR == "ESR"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, Cor == Negros, ESR == "ESR"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, Cor == Brancos, ESR == "ESR"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, Cor == Brancos, ESR == "ESR"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Negros, ESR == "ESR"))$Ano_Esc) -
          mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Negros, ESR == "ESR"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Brancos, ESR == "ESR"))$Ano_Esc) -
          mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Brancos, ESR == "ESR"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Cor == Negros, ESR == "ESR"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Cor == Negros, ESR == "ESR"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2002, Sexo == Mulher, Cor == Brancos, ESR == "ESR"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2002, Sexo == Homem, Cor == Brancos, ESR == "ESR"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Cor == Negros, ESR == "ESR"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2012, Sexo == Homem, Cor == Negros, ESR == "ESR"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Cor == Brancos, ESR == "ESR"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Cor == Brancos, ESR == "ESR"))$Ano_Esc))
      
      ))),
    
    t(as_tibble(cbind(
      t(mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc) -
          mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc) -
          mean(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc) -
          mean(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc) -
          mean(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc)),
      
      t(mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc) -
          mean(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc))
    )))
  )))
  
  TBL_ESC_II <- t(as_tibble(cbind(
    t(as_tibble(cbind(
      t(median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Negros, Classe == "DG"))$Ano_Esc) -
          median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Negros, Classe == "DG"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Brancos, Classe == "DG"))$Ano_Esc) -
          median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Brancos, Classe == "DG"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Cor == Negros, Classe == "DG"))$Ano_Esc) -
          median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Cor == Negros, Classe == "DG"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Cor == Brancos, Classe == "DG"))$Ano_Esc) -
          median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Cor == Brancos, Classe == "DG"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, Cor == Negros, Classe == "DG"))$Ano_Esc) -
          median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, Cor == Negros, Classe == "DG"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, Cor == Brancos, Classe == "DG"))$Ano_Esc) -
          median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, Cor == Brancos, Classe == "DG"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher, Cor == Negros, Classe == "DG"))$Ano_Esc) -
          median(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem, Cor == Negros, Classe == "DG"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher, Cor == Brancos, Classe == "DG"))$Ano_Esc) -
          median(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem, Cor == Brancos, Classe == "DG"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Cor == Negros, Classe == "DG"))$Ano_Esc) -
          median(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem, Cor == Negros, Classe == "DG"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Cor == Brancos, Classe == "DG"))$Ano_Esc) -
          median(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem, Cor == Brancos, Classe == "DG"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Cor == Negros, Classe == "DG"))$Ano_Esc) -
          median(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Cor == Negros, Classe == "DG"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Cor == Brancos, Classe == "DG"))$Ano_Esc) -
          median(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Cor == Brancos, Classe == "DG"))$Ano_Esc))
    ))),
    
    t(as_tibble(cbind(
      t(median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Negros, ECR == "ECR"))$Ano_Esc) -
          median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Negros, ECR == "ECR"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Brancos, ECR == "ECR"))$Ano_Esc) -
          median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Brancos, ECR == "ECR"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Cor == Negros, ECR == "ECR"))$Ano_Esc) -
          median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Cor == Negros, ECR == "ECR"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Cor == Brancos, ECR == "ECR"))$Ano_Esc) -
          median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Cor == Brancos, ECR == "ECR"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, Cor == Negros, ECR == "ECR"))$Ano_Esc) -
          median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, Cor == Negros, ECR == "ECR"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, Cor == Brancos, ECR == "ECR"))$Ano_Esc) -
          median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, Cor == Brancos, ECR == "ECR"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher, Cor == Negros, ECR == "ECR"))$Ano_Esc) -
          median(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem, Cor == Negros, ECR == "ECR"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher, Cor == Brancos, ECR == "ECR"))$Ano_Esc) -
          median(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem, Cor == Brancos, ECR == "ECR"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Cor == Negros, ECR == "ECR"))$Ano_Esc) -
          median(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem, Cor == Negros, ECR == "ECR"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Cor == Brancos, ECR == "ECR"))$Ano_Esc) -
          median(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem, Cor == Brancos, ECR == "ECR"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Cor == Negros, ECR == "ECR"))$Ano_Esc) -
          median(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Cor == Negros, ECR == "ECR"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Cor == Brancos, ECR == "ECR"))$Ano_Esc) -
          median(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Cor == Brancos, ECR == "ECR"))$Ano_Esc))
      
    ))),
    
    t(as_tibble(cbind(
      t(median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Negros, ESR == "ESR"))$Ano_Esc) -
          median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Negros, ESR == "ESR"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Brancos, ESR == "ESR"))$Ano_Esc) -
          median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Brancos, ESR == "ESR"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Cor == Negros, ESR == "ESR"))$Ano_Esc) -
          median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Cor == Negros, ESR == "ESR"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Cor == Brancos, ESR == "ESR"))$Ano_Esc) -
          median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Cor == Brancos, ESR == "ESR"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, Cor == Negros, ESR == "ESR"))$Ano_Esc) -
          median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, Cor == Negros, ESR == "ESR"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, Cor == Brancos, ESR == "ESR"))$Ano_Esc) -
          median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, Cor == Brancos, ESR == "ESR"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Negros, ESR == "ESR"))$Ano_Esc) -
          median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Negros, ESR == "ESR"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Brancos, ESR == "ESR"))$Ano_Esc) -
          median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Brancos, ESR == "ESR"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Cor == Negros, ESR == "ESR"))$Ano_Esc) -
          median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Cor == Negros, ESR == "ESR"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2002, Sexo == Mulher, Cor == Brancos, ESR == "ESR"))$Ano_Esc) -
          median(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2002, Sexo == Homem, Cor == Brancos, ESR == "ESR"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Cor == Negros, ESR == "ESR"))$Ano_Esc) -
          median(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2012, Sexo == Homem, Cor == Negros, ESR == "ESR"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Cor == Brancos, ESR == "ESR"))$Ano_Esc) -
          median(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Cor == Brancos, ESR == "ESR"))$Ano_Esc))
      
    ))),
    
    t(as_tibble(cbind(
      t(median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc) -
          median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc) -
          median(data.frame(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc) -
          median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc) -
          median(data.frame(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc) -
          median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc) -
          median(data.frame(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc) -
          median(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc) -
          median(data.frame(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc) -
          median(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc) -
          median(data.frame(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc) -
          median(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Cor == Negros, Prim_Quintil == "PQ"))$Ano_Esc)),
      
      t(median(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc) -
          median(data.frame(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Cor == Brancos, Prim_Quintil == "PQ"))$Ano_Esc))
    )))
  )))
  
  write_xlsx(as.data.frame(TBL_ESC), "TBL_ESC.xlsx")
  write_xlsx(as.data.frame(TBL_ESC_I), "TBL_ESC_I.xlsx")
  write_xlsx(as.data.frame(TBL_ESC_II), "TBL_ESC_II.xlsx")


# TABELA DE ESCOLARIDADE II -----------------------------------------------

# CONSTANTES --------------------------------------------------------------

# BAHIA 2012 --------------------------------------------------------------

  # POPULAÇÃO DE DIRIGENTES E GERENTES
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Classe == "DG")))
  
  # POPULAÇÃO DE EMPREGADOS COM REGISTRO
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ECR == "ECR")))
  
  # POPULAÇÃO DE EMPREGADOS SEM REGISTRO
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ESR == "ESR")))
  
  # POPULAÇÃO DO PRIMEIRO QUINTIL
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Prim_Quintil == "PQ")))
  
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Classe == "DG", Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Classe == "DG")))
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, ECR == "ECR", Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ECR == "ECR")))
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, ESR == "ESR", Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ESR == "ESR")))
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Prim_Quintil == "PQ", Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Prim_Quintil == "PQ")))
  
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Classe == "DG", Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Classe == "DG")))
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, ECR == "ECR", Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ECR == "ECR")))
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, ESR == "ESR", Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ESR == "ESR")))
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Prim_Quintil == "PQ", Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Prim_Quintil == "PQ")))
  
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Classe == "DG", Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Classe == "DG")))
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, ECR == "ECR", Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ECR == "ECR")))
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, ESR == "ESR", Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ESR == "ESR")))
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Prim_Quintil == "PQ", Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Prim_Quintil == "PQ")))
  
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Classe == "DG", Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Classe == "DG")))
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, ECR == "ECR", Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ECR == "ECR")))
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, ESR == "ESR", Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ESR == "ESR")))
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Prim_Quintil == "PQ", Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Prim_Quintil == "PQ")))
  
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Classe == "DG", Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Classe == "DG")))
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, ECR == "ECR", Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ECR == "ECR")))
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, ESR == "ESR", Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ESR == "ESR")))
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Prim_Quintil == "PQ", Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Prim_Quintil == "PQ")))
  
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Classe == "DG", Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Classe == "DG")))
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, ECR == "ECR", Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ECR == "ECR")))
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, ESR == "ESR", Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ESR == "ESR")))
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Prim_Quintil == "PQ", Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Prim_Quintil == "PQ")))
  
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Classe == "DG", Ano_Esc >= 12, Ano_Esc <= 15)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Classe == "DG")))
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, ECR == "ECR", Ano_Esc >= 12, Ano_Esc <= 15)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ECR == "ECR")))
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, ESR == "ESR", Ano_Esc >= 12, Ano_Esc <= 15)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ESR == "ESR")))
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Prim_Quintil == "PQ", Ano_Esc >= 12, Ano_Esc <= 15)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Prim_Quintil == "PQ")))
  
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Classe == "DG", Ano_Esc >= 12, Ano_Esc <= 15)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Classe == "DG")))
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, ECR == "ECR", Ano_Esc >= 12, Ano_Esc <= 15)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ECR == "ECR")))
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, ESR == "ESR", Ano_Esc >= 12, Ano_Esc <= 15)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ESR == "ESR")))
  as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Prim_Quintil == "PQ", Ano_Esc >= 12, Ano_Esc <= 15)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Prim_Quintil == "PQ")))
  
  TBLO <- as.tibble(
  cbind(
  rbind(
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Classe == "DG", Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Classe == "DG")))*100)),
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, ECR == "ECR", Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ECR == "ECR")))*100)),
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, ESR == "ESR", Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ESR == "ESR")))*100)),
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Prim_Quintil == "PQ", Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Prim_Quintil == "PQ")))*100))
  ),
  rbind(
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Classe == "DG", Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Classe == "DG")))*100)),
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, ECR == "ECR", Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ECR == "ECR")))*100)),
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, ESR == "ESR", Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ESR == "ESR")))*100)),
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Prim_Quintil == "PQ", Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Prim_Quintil == "PQ")))*100))
  ),
  rbind(
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Classe == "DG", Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Classe == "DG")))*100)),
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, ECR == "ECR", Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ECR == "ECR")))*100)),
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, ESR == "ESR", Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ESR == "ESR")))*100)),
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Prim_Quintil == "PQ", Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Prim_Quintil == "PQ")))*100))
  ),
  rbind(
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Classe == "DG", Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Classe == "DG")))*100)),
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, ECR == "ECR", Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ECR == "ECR")))*100)),
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, ESR == "ESR", Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ESR == "ESR")))*100)),
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Prim_Quintil == "PQ", Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Prim_Quintil == "PQ")))*100))
  ),
  rbind(
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Classe == "DG", Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Classe == "DG")))*100)),
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, ECR == "ECR", Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ECR == "ECR")))*100)),
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, ESR == "ESR", Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ESR == "ESR")))*100)),
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Prim_Quintil == "PQ", Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Prim_Quintil == "PQ")))*100))
  ),
  rbind(
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Classe == "DG", Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Classe == "DG")))*100)),
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, ECR == "ECR", Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ECR == "ECR")))*100)),
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, ESR == "ESR", Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ESR == "ESR")))*100)),
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Prim_Quintil == "PQ", Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Prim_Quintil == "PQ")))*100))
  ),
  rbind(
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Classe == "DG", Ano_Esc >= 12, Ano_Esc <= 15)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Classe == "DG")))*100)),
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, ECR == "ECR", Ano_Esc >= 12, Ano_Esc <= 15)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ECR == "ECR")))*100)),
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, ESR == "ESR", Ano_Esc >= 12, Ano_Esc <= 15)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ESR == "ESR")))*100)),
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Prim_Quintil == "PQ", Ano_Esc >= 12, Ano_Esc <= 15)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Prim_Quintil == "PQ")))*100))
  ),
  rbind(
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Classe == "DG", Ano_Esc >= 12, Ano_Esc <= 15)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Classe == "DG")))*100)),
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, ECR == "ECR", Ano_Esc >= 12, Ano_Esc <= 15)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ECR == "ECR")))*100)),
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, ESR == "ESR", Ano_Esc >= 12, Ano_Esc <= 15)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ESR == "ESR")))*100)),
    t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Prim_Quintil == "PQ", Ano_Esc >= 12, Ano_Esc <= 15)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Prim_Quintil == "PQ")))*100))
  )))
  
  write_xlsx(as.data.frame(TBLO), "TBLO.xlsx")
  
# TABELA ESC RESUMO SEXO --------------------------------------------------

  t((as.numeric(nrow(filter()))/as.numeric(nrow(filter())))*100)
  
  TABELA_ESC_RESUMO_SEXO <- as.tibble(
  cbind(
  rbind(
  t((as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Ano_Esc <= 4)))/as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem))))*100),
  t((as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem))))*100),
  t((as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem))))*100),
  t((as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Ano_Esc >= 12)))/as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem))))*100)
  ),
  rbind(
  t((as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Ano_Esc <= 4)))/as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher))))*100),
  t((as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher))))*100),
  t((as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher))))*100),
  t((as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Ano_Esc >= 12)))/as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher))))*100)
  ),
  rbind(
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem))))*100),
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem))))*100),
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem))))*100),
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Ano_Esc >= 12)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem))))*100)
  ),
  rbind(
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher))))*100),
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher))))*100),
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher))))*100),
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Ano_Esc >= 12)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher))))*100)
  ),
  rbind(
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem))))*100),
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem))))*100),
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem))))*100),
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, Ano_Esc >= 12)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem))))*100)
  ),
  rbind(
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher))))*100),
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher))))*100),
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher))))*100),
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, Ano_Esc >= 12)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher))))*100)
  ),
  rbind(
  t((as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem, Ano_Esc <= 4)))/as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem))))*100),
  t((as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem, Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem))))*100),
  t((as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem, Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem))))*100),
  t((as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem, Ano_Esc >= 12)))/as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem))))*100)
  ),
  rbind(
  t((as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher, Ano_Esc <= 4)))/as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher))))*100),
  t((as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher, Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher))))*100),
  t((as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher, Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher))))*100),
  t((as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher, Ano_Esc >= 12)))/as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher))))*100)
  ),
  rbind(
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem, Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem))))*100),
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem, Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem))))*100),
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem, Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem))))*100),
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem, Ano_Esc >= 12)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem))))*100)
  ),
  rbind(
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher))))*100),
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher))))*100),
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher))))*100),
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Ano_Esc >= 12)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher))))*100)
  ),
  rbind(
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem))))*100),
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem))))*100),
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem))))*100),
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Ano_Esc >= 12)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem))))*100)
  ),
  rbind(
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher))))*100),
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher))))*100),
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher))))*100),
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Ano_Esc >= 12)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher))))*100)
  )))
  
  write_xlsx(as.data.frame(TABELA_ESC_RESUMO_SEXO), "TABELA_ESC_RESUMO_SEXO.xlsx")

  TABELA_ESC_RESUMO_SEXO_I <- as.tibble(
  cbind(
  rbind(
  t((as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Ano_Esc <= 4)))/as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Ano_Esc <= 4))))*100),
  t((as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Ano_Esc >= 5, Ano_Esc <= 8))))*100),
  t((as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Ano_Esc >= 9, Ano_Esc <= 11))))*100),
  t((as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Sexo == Homem, Ano_Esc >= 12)))/as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Ano_Esc >= 12))))*100)
  ),
  rbind(
  t((as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Ano_Esc <= 4)))/as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Ano_Esc <= 4))))*100),
  t((as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Ano_Esc >= 5, Ano_Esc <= 8))))*100),
  t((as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Ano_Esc >= 9, Ano_Esc <= 11))))*100),
  t((as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Sexo == Mulher, Ano_Esc >= 12)))/as.numeric(nrow(filter(pnad2002, UF == UF_BA_2002, Ano_Esc >= 12))))*100)
  ),
  rbind(
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Ano_Esc <= 4))))*100),
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Ano_Esc >= 5, Ano_Esc <= 8))))*100),
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Ano_Esc >= 9, Ano_Esc <= 11))))*100),
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Homem, Ano_Esc >= 12)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Ano_Esc >= 12))))*100)
  ),
  rbind(
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Ano_Esc <= 4))))*100),
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Ano_Esc >= 5, Ano_Esc <= 8))))*100),
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Ano_Esc >= 9, Ano_Esc <= 11))))*100),
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Sexo == Mulher, Ano_Esc >= 12)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Ano_Esc >= 12))))*100)
  ),
  rbind(
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Ano_Esc <= 4))))*100),
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Ano_Esc >= 5, Ano_Esc <= 8))))*100),
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Ano_Esc >= 9, Ano_Esc <= 11))))*100),
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Homem, Ano_Esc >= 12)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Ano_Esc >= 12))))*100)
  ),
  rbind(
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Ano_Esc <= 4))))*100),
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Ano_Esc >= 5, Ano_Esc <= 8))))*100),
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Ano_Esc >= 9, Ano_Esc <= 11))))*100),
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Sexo == Mulher, Ano_Esc >= 12)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Ano_Esc >= 12))))*100)
  ),
  rbind(
  t((as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem, Ano_Esc <= 4)))/as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Ano_Esc <= 4))))*100),
  t((as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem, Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Ano_Esc >= 5, Ano_Esc <= 8))))*100),
  t((as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem, Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Ano_Esc >= 9, Ano_Esc <= 11))))*100),
  t((as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Sexo == Homem, Ano_Esc >= 12)))/as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Ano_Esc >= 12))))*100)
  ),
  rbind(
  t((as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher, Ano_Esc <= 4)))/as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Ano_Esc <= 4))))*100),
  t((as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher, Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Ano_Esc >= 5, Ano_Esc <= 8))))*100),
  t((as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher, Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Ano_Esc >= 9, Ano_Esc <= 11))))*100),
  t((as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Sexo == Mulher, Ano_Esc >= 12)))/as.numeric(nrow(filter(pnad2002, UF == UF_SP_2002, Ano_Esc >= 12))))*100)
  ),
  rbind(
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem, Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Ano_Esc <= 4))))*100),
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem, Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Ano_Esc >= 5, Ano_Esc <= 8))))*100),
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem, Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Ano_Esc >= 9, Ano_Esc <= 11))))*100),
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Homem, Ano_Esc >= 12)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Ano_Esc >= 12))))*100)
  ),
  rbind(
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Ano_Esc <= 4))))*100),
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Ano_Esc >= 5, Ano_Esc <= 8))))*100),
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Ano_Esc >= 9, Ano_Esc <= 11))))*100),
  t((as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Sexo == Mulher, Ano_Esc >= 12)))/as.numeric(nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Ano_Esc >= 12))))*100)
  ),
  rbind(
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Ano_Esc <= 4))))*100),
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Ano_Esc >= 5, Ano_Esc <= 8))))*100),
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Ano_Esc >= 9, Ano_Esc <= 11))))*100),
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Homem, Ano_Esc >= 12)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Ano_Esc >= 12))))*100)
  ),
  rbind(
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Ano_Esc <= 4)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Ano_Esc <= 4))))*100),
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Ano_Esc >= 5, Ano_Esc <= 8)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Ano_Esc >= 5, Ano_Esc <= 8))))*100),
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Ano_Esc >= 9, Ano_Esc <= 11)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Ano_Esc >= 9, Ano_Esc <= 11))))*100),
  t((as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Sexo == Mulher, Ano_Esc >= 12)))/as.numeric(nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Ano_Esc >= 12))))*100)
  )))
  
  write_xlsx(as.data.frame(TABELA_ESC_RESUMO_SEXO_I), "TABELA_ESC_RESUMO_SEXO_I.xlsx")

# RELEVANCIA ECR e  --------------------------------------------------------------

  nrow(filter(pnad2002, UF == UF_BA_2002, Ano_Esc <= 4, ECR == "ECR"))/nrow(filter(pnad2002, UF == UF_BA_2002, ECR == "ECR"))
  nrow(filter(pnad2002, UF == UF_SP_2002, Ano_Esc <= 4, ECR == "ECR"))/nrow(filter(pnad2002, UF == UF_SP_2002, ECR == "ECR"))

  nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, Ano_Esc <= 4, ECR == "ECR"))/nrow(filter(pnadc2012T4_5.6, UF == UF_BA_2012, ECR == "ECR"))
  nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, Ano_Esc <= 4, ECR == "ECR"))/nrow(filter(pnadc2012T4_5.6, UF == UF_SP_2012, ECR == "ECR"))
  
  nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, Ano_Esc <= 4, ECR == "ECR"))/nrow(filter(pnadc2022T4_5.6, UF == UF_BA_2022, ECR == "ECR"))
  nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, Ano_Esc <= 4, ECR == "ECR"))/nrow(filter(pnadc2022T4_5.6, UF == UF_SP_2022, ECR == "ECR"))

# COMBINAR GRÁFICOS -------------------------------------------------------

  install.packages('gridExtra')
  install.packages('gtable')
  install.packages('grid')
  library(gridExtra)
  library(gtable)
  library(grid)

  # Função para extrair a legenda de um gráfico
  get_legend <- function(my_plot) {
    tmp <- ggplotGrob(my_plot)
    legend <- gtable_filter(tmp, "guide-box")
    return(legend)
  }
  
  legend <- get_legend(GF_01_2002 + theme(text = element_text(size = 16)))
  legend <- gtable::gtable_add_padding(legend, padding = unit(c(-70, 0, 0, -40), "pt"))
  
  grid::grid.draw(legend)

# VERTICAL ----------------------------------------------------------------

  GF_01 <- grid.arrange(GF_01_2002 + labs(title = "2002") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10),
                                                         axis.text.x = element_text(angle = 45, hjust = 1)), 
               GF_01_2012 + labs(title = "2012") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10),
                                                         axis.text.x = element_text(angle = 45, hjust = 1)), 
               GF_01_2022 + labs(title = "2022") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10),
                                                         axis.text.x = element_text(angle = 45, hjust = 1)),
               legend,
               ncol = 1,
               heights = c(1, 1, 1, 0.17)
  )

  GF_02 <- grid.arrange(GF_02_2002 + labs(title = "2002") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10),
                                                         axis.text.x = element_text(angle = 45, hjust = 1)), 
               GF_02_2012 + labs(title = "2012") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10),
                                                         axis.text.x = element_text(angle = 45, hjust = 1)), 
               GF_02_2022 + labs(title = "2022") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10),
                                                         axis.text.x = element_text(angle = 45, hjust = 1)),
               legend,
               ncol = 1,
               heights = c(1, 1, 1, 0.17)
  )
  
  GF_03 <- grid.arrange(GF_03_2002 + labs(title = "2002") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10),
                                                         axis.text.x = element_text(angle = 45, hjust = 1)), 
               GF_03_2012 + labs(title = "2012") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10),
                                                         axis.text.x = element_text(angle = 45, hjust = 1)), 
               GF_03_2022 + labs(title = "2022") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10),
                                                         axis.text.x = element_text(angle = 45, hjust = 1)),
               legend,
               ncol = 1,
               heights = c(1, 1, 1, 0.17)
  )
  
  GF_04 <- grid.arrange(GF_04_2002 + labs(title = "2002") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10),
                                                         axis.text.x = element_text(angle = 45, hjust = 1)), 
               GF_04_2012 + labs(title = "2012") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10),
                                                         axis.text.x = element_text(angle = 45, hjust = 1)), 
               GF_04_2022 + labs(title = "2022") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10),
                                                         axis.text.x = element_text(angle = 45, hjust = 1)),
               legend,
               ncol = 1,
               heights = c(1, 1, 1, 0.17)
  )
  
  GF_05 <- grid.arrange(GF_05_2002 + labs(title = "2002") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10)), 
               GF_05_2012 + labs(title = "2012") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10)), 
               GF_05_2022 + labs(title = "2022") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10)),
               legend,
               ncol = 1,
               heights = c(1, 1, 1, 0.17)
  )
  
  GF_06 <- grid.arrange(GF_06_2002 + labs(title = "2002") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10)), 
               GF_06_2012 + labs(title = "2012") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10)), 
               GF_06_2022 + labs(title = "2022") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10)),
               legend,
               ncol = 1,
               heights = c(1, 1, 1, 0.17)
  )
  
  GF_07 <- grid.arrange(GF_07_2002 + labs(title = "2002") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10)), 
               GF_07_2012 + labs(title = "2012") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10)), 
               GF_07_2022 + labs(title = "2022") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10)),
               legend,
               ncol = 1,
               heights = c(1, 1, 1, 0.17)
  )
  
  GF_08 <- grid.arrange(GF_08_2002 + labs(title = "2002") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10)), 
               GF_08_2012 + labs(title = "2012") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10)), 
               GF_08_2022 + labs(title = "2022") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10)),
               legend,
               ncol = 1,
               heights = c(1, 1, 1, 0.17)
  )
  
  GF_09 <- grid.arrange(GF_09_2002 + labs(title = "2002") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10)), 
               GF_09_2012 + labs(title = "2012") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10)), 
               GF_09_2022 + labs(title = "2022") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10)),
               legend,
               ncol = 1,
               heights = c(1, 1, 1, 0.17)
  )
  
  GF_10 <- grid.arrange(GF_10_2002 + labs(title = "2002") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10)), 
               GF_10_2012 + labs(title = "2012") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10)), 
               GF_10_2022 + labs(title = "2022") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10)),
               legend,
               ncol = 1,
               heights = c(1, 1, 1, 0.17)
  )
  
  GF_11 <- grid.arrange(GF_11_2002 + labs(title = "2002") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10)), 
               GF_11_2012 + labs(title = "2012") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10)), 
               GF_11_2022 + labs(title = "2022") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10)),
               legend,
               ncol = 1,
               heights = c(1, 1, 1, 0.17)
  )
  
  GF_12 <- grid.arrange(GF_12_2002 + labs(title = "2002") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10)), 
               GF_12_2012 + labs(title = "2012") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10)), 
               GF_12_2022 + labs(title = "2022") + theme(legend.position = "none",
                                                         plot.title = element_text(face = "bold"),
                                                         text = element_text(family = "Times New Roman", 
                                                                             size = 10)),
               legend,
               ncol = 1,
               heights = c(1, 1, 1, 0.17)
  )

  GF_01 <- grid.arrange(GF_01_2002 + labs(title = "2002") + theme(legend.position = "none",
                                                                  plot.title = element_text(face = "bold"),
                                                                  text = element_text(family = "Times New Roman", 
                                                                                      size = 10),
                                                                  axis.text.x = element_text(angle = 45, hjust = 1)), 
                        GF_01_2012 + labs(title = "2012") + theme(legend.position = "none",
                                                                  plot.title = element_text(face = "bold"),
                                                                  text = element_text(family = "Times New Roman", 
                                                                                      size = 10),
                                                                  axis.text.x = element_text(angle = 45, hjust = 1)), 
                        GF_01_2022 + labs(title = "2022") + theme(legend.position = "none",
                                                                  plot.title = element_text(face = "bold"),
                                                                  text = element_text(family = "Times New Roman", 
                                                                                      size = 10),
                                                                  axis.text.x = element_text(angle = 45, hjust = 1)),
                        legend,
                        ncol = 1,
                        heights = c(1, 1, 1, 0.17)
  )

# HORIZONTAL --------------------------------------------------------------

  GF_01 <- grid.arrange(
    GF_01_2002 +
      labs(title = "2002", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.text = element_text(color = "transparent"),
            legend.title = element_text(color = "transparent"),
            legend.key = element_blank(),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    GF_01_2012 +
      labs(title = "2012", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    GF_01_2022 +
      labs(title = "2022", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.text = element_text(color = "transparent"),
            legend.title = element_text(color = "transparent"),
            legend.key = element_rect(fill = "transparent", color = NA),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    nrow = 1)
  
  GF_02 <- grid.arrange(
    GF_02_2002 +
      labs(title = "2002", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.text = element_text(color = "transparent"),
            legend.title = element_text(color = "transparent"),
            legend.key = element_blank(),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    GF_02_2012 +
      labs(title = "2012", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    GF_02_2022 +
      labs(title = "2022", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.text = element_text(color = "transparent"),
            legend.title = element_text(color = "transparent"),
            legend.key = element_rect(fill = "transparent", color = NA),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    nrow = 1)

  GF_03 <- grid.arrange(
    GF_03_2002 +
      labs(title = "2002", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.text = element_text(color = "transparent"),
            legend.title = element_text(color = "transparent"),
            legend.key = element_blank(),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    GF_03_2012 +
      labs(title = "2012", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    GF_03_2022 +
      labs(title = "2022", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.text = element_text(color = "transparent"),
            legend.title = element_text(color = "transparent"),
            legend.key = element_rect(fill = "transparent", color = NA),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    nrow = 1)
  
  GF_04 <- grid.arrange(
    GF_04_2002 +
      labs(title = "2002", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.text = element_text(color = "transparent"),
            legend.title = element_text(color = "transparent"),
            legend.key = element_blank(),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    GF_04_2012 +
      labs(title = "2012", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    GF_04_2022 +
      labs(title = "2022", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.text = element_text(color = "transparent"),
            legend.title = element_text(color = "transparent"),
            legend.key = element_rect(fill = "transparent", color = NA),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    nrow = 1)
  
  GF_05 <- grid.arrange(
    GF_05_2002 +
      labs(title = "2002", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.text = element_text(color = "transparent"),
            legend.title = element_text(color = "transparent"),
            legend.key = element_blank(),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    GF_05_2012 +
      labs(title = "2012", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    GF_05_2022 +
      labs(title = "2022", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.text = element_text(color = "transparent"),
            legend.title = element_text(color = "transparent"),
            legend.key = element_rect(fill = "transparent", color = NA),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    nrow = 1)
  
  GF_06 <- grid.arrange(
    GF_06_2002 +
      labs(title = "2002", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.text = element_text(color = "transparent"),
            legend.title = element_text(color = "transparent"),
            legend.key = element_blank(),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    GF_06_2012 +
      labs(title = "2012", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    GF_06_2022 +
      labs(title = "2022", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.text = element_text(color = "transparent"),
            legend.title = element_text(color = "transparent"),
            legend.key = element_rect(fill = "transparent", color = NA),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    nrow = 1)
  
  GF_07 <- grid.arrange(
    GF_07_2002 +
      labs(title = "2002", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.text = element_text(color = "transparent"),
            legend.title = element_text(color = "transparent"),
            legend.key = element_blank(),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    GF_07_2012 +
      labs(title = "2012", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    GF_07_2022 +
      labs(title = "2022", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.text = element_text(color = "transparent"),
            legend.title = element_text(color = "transparent"),
            legend.key = element_rect(fill = "transparent", color = NA),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    nrow = 1)
  
  GF_08 <- grid.arrange(
    GF_08_2002 +
      labs(title = "2002", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.text = element_text(color = "transparent"),
            legend.title = element_text(color = "transparent"),
            legend.key = element_blank(),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    GF_08_2012 +
      labs(title = "2012", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    GF_08_2022 +
      labs(title = "2022", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.text = element_text(color = "transparent"),
            legend.title = element_text(color = "transparent"),
            legend.key = element_rect(fill = "transparent", color = NA),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    nrow = 1)
  
  GF_09 <- grid.arrange(
    GF_09_2002 +
      labs(title = "2002", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.text = element_text(color = "transparent"),
            legend.title = element_text(color = "transparent"),
            legend.key = element_blank(),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    GF_09_2012 +
      labs(title = "2012", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    GF_09_2022 +
      labs(title = "2022", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.text = element_text(color = "transparent"),
            legend.title = element_text(color = "transparent"),
            legend.key = element_rect(fill = "transparent", color = NA),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    nrow = 1)
  
  GF_10 <- grid.arrange(
    GF_10_2002 +
      labs(title = "2002", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.text = element_text(color = "transparent"),
            legend.title = element_text(color = "transparent"),
            legend.key = element_blank(),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    GF_10_2012 +
      labs(title = "2012", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    GF_10_2022 +
      labs(title = "2022", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.text = element_text(color = "transparent"),
            legend.title = element_text(color = "transparent"),
            legend.key = element_rect(fill = "transparent", color = NA),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    nrow = 1)
  
  GF_11 <- grid.arrange(
    GF_11_2002 +
      labs(title = "2002", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.text = element_text(color = "transparent"),
            legend.title = element_text(color = "transparent"),
            legend.key = element_blank(),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    GF_11_2012 +
      labs(title = "2012", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    GF_11_2022 +
      labs(title = "2022", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.text = element_text(color = "transparent"),
            legend.title = element_text(color = "transparent"),
            legend.key = element_rect(fill = "transparent", color = NA),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    nrow = 1)
  
  GF_12 <- grid.arrange(
    GF_12_2002 +
      labs(title = "2002", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.text = element_text(color = "transparent"),
            legend.title = element_text(color = "transparent"),
            legend.key = element_blank(),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    GF_12_2012 +
      labs(title = "2012", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    GF_12_2022 +
      labs(title = "2022", x = "renda", y = "probabilidade") +
      theme(legend.position = "none",
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.text = element_text(color = "transparent"),
            legend.title = element_text(color = "transparent"),
            legend.key = element_rect(fill = "transparent", color = NA),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            text = element_text(family = "Times New Roman", size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14)) +
      scale_x_continuous(expand = expansion(add = c(0, 0)),
                         breaks = seq(0, 10000, by = 200)),
    nrow = 1)
  
# GRAFICOS EM PNG ---------------------------------------------------------

  ?grid.arrange
  
  ggsave("GF_01.png", grid.arrange(GF_01, legend ,ncol = 1, heights = c(10, 1)), width = 12, height = 4, limitsize = FALSE)
  ggsave("GF_02.png", grid.arrange(GF_02, legend ,ncol = 1, heights = c(10, 1)), width = 12, height = 4, limitsize = FALSE)
  ggsave("GF_03.png", grid.arrange(GF_03, legend ,ncol = 1, heights = c(10, 1)), width = 12, height = 4, limitsize = FALSE)
  ggsave("GF_04.png", grid.arrange(GF_04, legend ,ncol = 1, heights = c(10, 1)), width = 12, height = 4, limitsize = FALSE)
  ggsave("GF_05.png", grid.arrange(GF_05, legend ,ncol = 1, heights = c(10, 1)), width = 12, height = 4, limitsize = FALSE)
  ggsave("GF_06.png", grid.arrange(GF_06, legend ,ncol = 1, heights = c(10, 1)), width = 12, height = 4, limitsize = FALSE)
  ggsave("GF_07.png", grid.arrange(GF_07, legend ,ncol = 1, heights = c(10, 1)), width = 12, height = 4, limitsize = FALSE)
  ggsave("GF_08.png", grid.arrange(GF_08, legend ,ncol = 1, heights = c(10, 1)), width = 12, height = 4, limitsize = FALSE)
  ggsave("GF_09.png", grid.arrange(GF_09, legend ,ncol = 1, heights = c(10, 1)), width = 12, height = 4, limitsize = FALSE)
  ggsave("GF_10.png", grid.arrange(GF_10, legend ,ncol = 1, heights = c(10, 1)), width = 12, height = 4, limitsize = FALSE)
  ggsave("GF_11.png", grid.arrange(GF_11, legend ,ncol = 1, heights = c(10, 1)), width = 12, height = 4, limitsize = FALSE)
  ggsave("GF_12.png", grid.arrange(GF_12, legend ,ncol = 1, heights = c(10, 1)), width = 12, height = 4, limitsize = FALSE)

# COEFICIENTES EM DOCX ----------------------------------------------------

  gtsave(TBL_PQ_2002, filename = "TBL_PQ_2002.docx")
  gtsave(TBL_SR_2002, filename = "TBL_SR_2002.docx")
  gtsave(TBL_CR_2002, filename = "TBL_CR_2002.docx")
  gtsave(TBL_DG_2002, filename = "TBL_DG_2002.docx")
  gtsave(TBL_PQ_2012, filename = "TBL_PQ_2012.docx")
  gtsave(TBL_SR_2012, filename = "TBL_SR_2012.docx")
  gtsave(TBL_CR_2012, filename = "TBL_CR_2012.docx")
  gtsave(TBL_DG_2012, filename = "TBL_DG_2012.docx")
  gtsave(TBL_PQ_2022, filename = "TBL_PQ_2022.docx")
  gtsave(TBL_SR_2022, filename = "TBL_SR_2022.docx")
  gtsave(TBL_CR_2022, filename = "TBL_CR_2022.docx")
  gtsave(TBL_DG_2022, filename = "TBL_DG_2022.docx")
  
