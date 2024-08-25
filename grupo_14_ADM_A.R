library(readxl)
APSEcono <- read_excel("C:/Users/Ana Flávia/OneDrive - Insper - Institudo de Ensino e Pesquisa/Quarto semestre/Econometria/APSEcono.xlsx", 
                       col_types = c("text", "text", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric"))

View(APSEcono)
 
#Análise Descritiva

#ALAGOAS

#Pib Per Capita

medidas_al_pib = summary(APSEcono$`PIB per capita`[APSEcono$Estado == "Alagoas"])

sd_al_pib = sd(APSEcono$`PIB per capita`[APSEcono$Estado == "Alagoas"])

var_al_pib = var(APSEcono$`PIB per capita`[APSEcono$Estado == "Alagoas"])

cov_al_pib = cov(APSEcono$`PIB per capita`[APSEcono$Estado == "Alagoas"],
             APSEcono$IDEB[APSEcono$Estado == "Alagoas"], 
             use = "pairwise.complete.obs")

cor_al_pib = cor(APSEcono$`PIB per capita`[APSEcono$Estado == "Alagoas"],
             APSEcono$IDEB[APSEcono$Estado == "Alagoas"], 
             use = "pairwise.complete.obs")

boxplot(APSEcono$`PIB per capita`[APSEcono$Estado == "Alagoas"], 
        main = "Boxplot PIB per capita - Alagoas ")

plot(APSEcono$`PIB per capita`[APSEcono$Estado == "Alagoas"],
     APSEcono$IDEB[APSEcono$Estado == "Alagoas"],
     main = "Dispersão PIB per capita - Alagoas",
     ylab = "IDEB - Alagoas", xlab = "PIB per capita - Alagoas")

hist(APSEcono$`PIB per capita`[APSEcono$Estado == "Alagoas"],
     main = "Histograma - Alagoas", ylab = "Frequência",
     xlab = "PIB per capita - Alagoas")

pib_al = as.data.frame(rbind(as.matrix(medidas_al_pib), sd_al_pib, var_al_pib, cov_al_pib, cor_al_pib))
pib_al = format(pib_al, scientific = F)
row.names(pib_al) = c('Mínimo', "1º Quartil", "Mediana","Média","3º Quartil",
                      "Máximo","Desvio padrão","Variância",
                      "Covariância","Correlação")
colnames(pib_al) = c("Valores")
View(pib_al)

#Docentes / n° matrícula

medidas_al_docentes = summary(APSEcono$`docentes / matrícula`[APSEcono$Estado == "Alagoas"])

sd_al_docentes = sd(APSEcono$`docentes / matrícula`[APSEcono$Estado == "Alagoas"])

var_al_docentes = var(APSEcono$`docentes / matrícula`[APSEcono$Estado == "Alagoas"])

cov_al_docentes = cov(APSEcono$`docentes / matrícula`[APSEcono$Estado == "Alagoas"],
                 APSEcono$IDEB[APSEcono$Estado == "Alagoas"], 
                 use = "pairwise.complete.obs")

cor_al_docentes = cor(APSEcono$`docentes / matrícula`[APSEcono$Estado == "Alagoas"],
                 APSEcono$IDEB[APSEcono$Estado == "Alagoas"], 
                 use = "pairwise.complete.obs")

plot(APSEcono$`docentes / matrícula`[APSEcono$Estado == "Alagoas"],
     APSEcono$IDEB[APSEcono$Estado == "Alagoas"],
     main = "Dispersão docentes - Alagoas",
     xlab = "docentes", ylab = "IDEB")

boxplot(APSEcono$`docentes / matrícula`[APSEcono$Estado == "Alagoas"],
        main = "Boxplot docentes - Alagoas")

hist(APSEcono$`docentes / matrícula`[APSEcono$Estado == "Alagoas"],
     main = "Histograma docentes", ylab = "Frequência", xlab = "Docentes")

docentes_al = as.data.frame(rbind(as.matrix(medidas_al_docentes), 
                sd_al_docentes, var_al_docentes, cov_al_docentes, cor_al_docentes))
docentes_al = format(docentes_al, scientific = F)
row.names(docentes_al) = c('Mínimo', "1º Quartil", "Mediana","Média","3º Quartil",
                      "Máximo","Desvio padrão","Variância",
                      "Covariância","Correlação")
colnames(docentes_al) = c("Valores")
View(docentes_al)

#Matrícula / população

medidas_al_matr = summary(APSEcono$`matrícula/população`[APSEcono$Estado == "Alagoas"])

sd_al_matr = sd(APSEcono$`matrícula/população`[APSEcono$Estado == "Alagoas"])

var_al_matr = var(APSEcono$`matrícula/população`[APSEcono$Estado == "Alagoas"])

cov_al_matr = cov(APSEcono$`matrícula/população`[APSEcono$Estado == "Alagoas"],
                 APSEcono$IDEB[APSEcono$Estado == "Alagoas"], 
                 use = "pairwise.complete.obs")

cor_al_matr = cor(APSEcono$`matrícula/população`[APSEcono$Estado == "Alagoas"],
                 APSEcono$IDEB[APSEcono$Estado == "Alagoas"], 
                 use = "pairwise.complete.obs")

plot(APSEcono$`matrícula/população`[APSEcono$Estado == "Alagoas"],
     APSEcono$IDEB[APSEcono$Estado == "Alagoas"],
     main = "Dispersão matrículas - Alagoas",
     xlab = "Matrículas", ylab = "IDEB")

boxplot(APSEcono$`matrícula/população`[APSEcono$Estado == "Alagoas"],
        main = "Boxplot matrículas - Alagoas")

hist(APSEcono$`matrícula/população`[APSEcono$Estado == "Alagoas"],
     main = "Histograma matrículas - Alagoas", ylab = "Frequência",
     xlab = "Matrículas")

matriculas_al = as.data.frame(rbind(as.matrix(medidas_al_matr), 
                      sd_al_matr, var_al_matr, cov_al_matr, cor_al_matr))
matriculas_al = format(matriculas_al, scientific = F)
row.names(matriculas_al) = c('Mínimo', "1º Quartil", "Mediana","Média","3º Quartil",
                           "Máximo","Desvio padrão","Variância",
                           "Covariância","Correlação")
colnames(matriculas_al) = c("Valores")
View(matriculas_al)

#IDEB

medidas_al_IDEB = summary(APSEcono$IDEB[APSEcono$Estado == "Alagoas"])

sd_al_IDEB = sd(APSEcono$IDEB[APSEcono$Estado == "Alagoas"], na.rm = TRUE)

var_al_IDEB = var(APSEcono$IDEB[APSEcono$Estado == "Alagoas"], na.rm = TRUE)

plot(APSEcono$IDEB[APSEcono$Estado == "Alagoas"],
     main = "Dispersão IDEB - Alagoas", ylab = "IDEB")

boxplot(APSEcono$IDEB[APSEcono$Estado == "Alagoas"],
        main = "Boxplot IDEB - Alagoas")

hist(APSEcono$IDEB[APSEcono$Estado == "Alagoas"],
     main = "Histograma IDEB - Alagoas", xlab = "IDEB")

IDEB_al = as.data.frame(rbind(as.matrix(medidas_al_IDEB), 
                                sd_al_IDEB, var_al_IDEB))
IDEB_al = format(IDEB_al, scientific = F)
row.names(IDEB_al) = c('Mínimo', "1º Quartil", "Mediana","Média","3º Quartil",
                             "Máximo","Desvio padrão","Variância")
colnames(IDEB_al) = c("Valores")
View(IDEB_al)


#########################################################

#DEMAIS ESTADOS

#Pib Per Capita

medidas_0_pib = summary(APSEcono$`PIB per capita`[APSEcono$d_alag == "0"])

sd_0_pib = sd(APSEcono$`PIB per capita`[APSEcono$d_alag == "0"])

var_0_pib = var(APSEcono$`PIB per capita`[APSEcono$d_alag == "0"])

cov_0_pib = cov(APSEcono$`PIB per capita`[APSEcono$d_alag == "0"],
                 APSEcono$IDEB[APSEcono$d_alag == "0"], 
                 use = "pairwise.complete.obs")

cor_0_pib = cor(APSEcono$`PIB per capita`[APSEcono$d_alag == "0"],
                 APSEcono$IDEB[APSEcono$d_alag == "0"], 
                 use = "pairwise.complete.obs")

plot(APSEcono$`PIB per capita`[APSEcono$d_alag == "0"],
     APSEcono$IDEB[APSEcono$d_alag == "0"],
     main = "PIB per capita dummy = 0", xlab ="PIB per capita",
     ylab = "IDEB")

boxplot(APSEcono$`PIB per capita`[APSEcono$d_alag == "0"],
        main = "PIB per capita dummy = 0")

hist(APSEcono$`PIB per capita`[APSEcono$d_alag == "0"],
     main = "PIB per capita dummy = 0", xlab ="PIB per capita")

PIB_0 = as.data.frame(rbind(as.matrix(medidas_0_pib), 
                                    sd_0_pib, var_0_pib, cov_0_pib, cor_0_pib))
PIB_0 = format(PIB_0, scientific = F)
row.names(PIB_0) = c('Mínimo', "1º Quartil", "Mediana","Média","3º Quartil",
                             "Máximo","Desvio padrão","Variância",
                             "Covariância","Correlação")
colnames(PIB_0) = c("Valores")
View(PIB_0)

#Docentes / matrícula

medidas_0_docentes = summary(APSEcono$`docentes / matrícula`[APSEcono$d_alag == "0"])

sd_0_docentes = sd(APSEcono$`docentes / matrícula`[APSEcono$d_alag == "0"])

var_0_docentes = var(APSEcono$`docentes / matrícula`[APSEcono$d_alag == "0"])

cov_0_docentes = cov(APSEcono$`docentes / matrícula`[APSEcono$d_alag == "0"],
                 APSEcono$IDEB[APSEcono$d_alag == "0"], 
                 use = "pairwise.complete.obs")

cor_0_docentes = cor(APSEcono$`docentes / matrícula`[APSEcono$d_alag == "0"],
                 APSEcono$IDEB[APSEcono$d_alag == "0"], 
                 use = "pairwise.complete.obs")

plot(APSEcono$`docentes / matrícula`[APSEcono$d_alag == "0"],
     APSEcono$IDEB[APSEcono$d_alag == "0"], xlab = "Docentes",
     main = "Dispersão docentes - dummy =0", ylab = "IDEB")

boxplot(APSEcono$`docentes / matrícula`[APSEcono$d_alag == "0"],
        main = "Boxplot docentes - dummy = 0")

hist(APSEcono$`docentes / matrícula`[APSEcono$d_alag == "0"],
     main = "histograma docentes - dummy = 0",
     xlab = "docentes", ylab = "frequência")

docentes_0 = as.data.frame(rbind(as.matrix(medidas_0_docentes), 
                             sd_0_docentes, var_0_docentes, cov_0_docentes, cor_0_docentes))
docentes_0 = format(docentes_0, scientific = F)
row.names(docentes_0) = c('Mínimo', "1º Quartil", "Mediana","Média","3º Quartil",
                      "Máximo","Desvio padrão","Variância",
                      "Covariância","Correlação")
colnames(docentes_0) = c("Valores")
View(docentes_0)

#Matrícula / população

medidas_0_matr = summary(APSEcono$`matrícula/população`[APSEcono$d_alag == "0"])

sd_0_matr = sd(APSEcono$`matrícula/população`[APSEcono$d_alag == "0"])

var_0_matr = var(APSEcono$`matrícula/população`[APSEcono$d_alag == "0"])

cov_0_matr = cov(APSEcono$`matrícula/população`[APSEcono$d_alag == "0"],
                  APSEcono$IDEB[APSEcono$d_alag == "0"], 
                  use = "pairwise.complete.obs")

cor_0_matr = cor(APSEcono$`matrícula/população`[APSEcono$d_alag == "0"],
                  APSEcono$IDEB[APSEcono$d_alag == "0"], 
                  use = "pairwise.complete.obs")

plot(APSEcono$`matrícula/população`[APSEcono$d_alag == "0"],
     APSEcono$IDEB[APSEcono$d_alag == "0"], xlab = "matrícula",
     main = "dispersão matrícula - dummy = 0", ylab = "IDEB")

boxplot(APSEcono$`matrícula/população`[APSEcono$d_alag == "0"],
        main = "boxplot matrícula - dummy = 0")

hist(APSEcono$`matrícula/população`[APSEcono$d_alag == "0"],
     main = "histograma matrícula - dummy = 0", xlab = "matrícula")

matriculas_0 = as.data.frame(rbind(as.matrix(medidas_0_matr), 
                                  sd_0_matr, var_0_matr, cov_0_matr, cor_0_matr))
matriculas_0 = format(matriculas_0, scientific = F)
row.names(matriculas_0) = c('Mínimo', "1º Quartil", "Mediana","Média","3º Quartil",
                           "Máximo","Desvio padrão","Variância",
                           "Covariância","Correlação")
colnames(matriculas_0) = c("Valores")
View(matriculas_0)
     
#IDEB

medidas_0_IDEB = summary(APSEcono$IDEB[APSEcono$d_alag == "0"])

sd_0_IDEB = sd(APSEcono$IDEB[APSEcono$d_alag == "0"], na.rm = TRUE)

var_0_IDEB = var(APSEcono$IDEB[APSEcono$d_alag == "0"], na.rm = TRUE)

plot(APSEcono$IDEB[APSEcono$d_alag == "0"], ylab = "IDEB",
     main = "Dispersão IDEB - dummy = 0 ")

boxplot(APSEcono$IDEB[APSEcono$d_alag == "0"],
        main = "Boxplot IDEB - dummy = 0")

hist(APSEcono$IDEB[APSEcono$d_alag == "0"], xlab = "IDEB",
     main = "Histograma IDEB - dummy = 0")

IDEB_0 = as.data.frame(rbind(as.matrix(medidas_0_IDEB), 
                                sd_0_IDEB, var_0_IDEB))
IDEB_0 = format(IDEB_0, scientific = F)
row.names(IDEB_0) = c('Mínimo', "1º Quartil", "Mediana","Média","3º Quartil",
                             "Máximo","Desvio padrão","Variância")
colnames(IDEB_0) = c("Valores")
View(IDEB_0)

#################################################

#Formas funcionais

##########################################################
#PIB per capita

par(mfrow = c(2,2))

#log - lin

plot(log(APSEcono$IDEB[APSEcono$Estado == "Alagoas"])~
     APSEcono$`PIB per capita`[APSEcono$Estado == "Alagoas"], 
     ylab = "IDEB", xlab = "PIB per capita - Alagoas",
     main = "log -lin")
abline(lm(log(APSEcono$IDEB[APSEcono$Estado == "Alagoas"])~
             APSEcono$`PIB per capita`[APSEcono$Estado == "Alagoas"]))

#lin-log

plot(APSEcono$IDEB[APSEcono$Estado == "Alagoas"]~
          log(APSEcono$`PIB per capita`[APSEcono$Estado == "Alagoas"]),
   ylab = "IDEB", xlab = "PIB per capita - Alagoas",
   main = "lin-log")
abline(lm(APSEcono$IDEB[APSEcono$Estado == "Alagoas"]~
            log(APSEcono$`PIB per capita`[APSEcono$Estado == "Alagoas"])))

#log log

plot(log(APSEcono$IDEB[APSEcono$Estado == "Alagoas"])~
          log(APSEcono$`PIB per capita`[APSEcono$Estado == "Alagoas"]),
        ylab = "IDEB", xlab = "PIB per capita - Alagoas",
        main = "log-log")
abline(lm(log(APSEcono$IDEB[APSEcono$Estado == "Alagoas"])~
            log(APSEcono$`PIB per capita`[APSEcono$Estado == "Alagoas"])))

#lin lin

plot(APSEcono$IDEB[APSEcono$Estado == "Alagoas"]~
     APSEcono$`PIB per capita`[APSEcono$Estado == "Alagoas"],
   ylab = "IDEB", xlab = "PIB per capita - Alagoas",
   main = "lin-lin")
abline(lm(APSEcono$IDEB[APSEcono$Estado == "Alagoas"]~
            APSEcono$`PIB per capita`[APSEcono$Estado == "Alagoas"]))

###########################################################
#Docentes

par(mfrow = c(2,2))

#log - lin

plot(log(APSEcono$IDEB[APSEcono$Estado == "Alagoas"])~
          APSEcono$`docentes / matrícula`[APSEcono$Estado == "Alagoas"], 
        ylab = "IDEB", xlab = "Docentes",
        main = "log-lin")
abline(lm(log(APSEcono$IDEB[APSEcono$Estado == "Alagoas"])~
            APSEcono$`docentes / matrícula`[APSEcono$Estado == "Alagoas"]))

#lin log

plot(APSEcono$IDEB[APSEcono$Estado == "Alagoas"]~
          log(APSEcono$`docentes / matrícula`[APSEcono$Estado == "Alagoas"]),
        ylab = "IDEB", xlab = "Docentes",
        main = "lin-log")
abline(lm(APSEcono$IDEB[APSEcono$Estado == "Alagoas"]~
            log(APSEcono$`docentes / matrícula`[APSEcono$Estado == "Alagoas"])))

#log log

plot(log(APSEcono$IDEB[APSEcono$Estado == "Alagoas"])~
          log(APSEcono$`docentes / matrícula`[APSEcono$Estado == "Alagoas"]),
        ylab = "IDEB", xlab = "Docentes",
        main = "log-log")
abline(lm(log(APSEcono$IDEB[APSEcono$Estado == "Alagoas"])~
            log(APSEcono$`docentes / matrícula`[APSEcono$Estado == "Alagoas"])))

#lin lin

plot(APSEcono$IDEB[APSEcono$Estado == "Alagoas"]~
          APSEcono$`docentes / matrícula`[APSEcono$Estado == "Alagoas"],
   ylab = "IDEB", xlab = "Docentes",
   main = "lin-lin")
abline(lm(APSEcono$IDEB[APSEcono$Estado == "Alagoas"]~
            APSEcono$`docentes / matrícula`[APSEcono$Estado == "Alagoas"]))

#############################################################################
#Matrículas

par(mfrow = c(2,2))

#log - lin

plot(log(APSEcono$IDEB[APSEcono$Estado == "Alagoas"])~
          APSEcono$`matrícula/população`[APSEcono$Estado == "Alagoas"], 
        ylab = "IDEB", xlab = "Matrículas",
        main = "log -lin")
abline(lm(log(APSEcono$IDEB[APSEcono$Estado == "Alagoas"])~
            APSEcono$`matrícula/população`[APSEcono$Estado == "Alagoas"]))

#lin log

plot(APSEcono$IDEB[APSEcono$Estado == "Alagoas"]~
          log(APSEcono$`matrícula/população`[APSEcono$Estado == "Alagoas"]),
        ylab = "IDEB", xlab = "Matrículas",
        main = "lin-log")
abline(lm(APSEcono$IDEB[APSEcono$Estado == "Alagoas"]~
            log(APSEcono$`matrícula/população`[APSEcono$Estado == "Alagoas"])))

#log log

plot(log(APSEcono$IDEB[APSEcono$Estado == "Alagoas"])~
          log(APSEcono$`matrícula/população`[APSEcono$Estado == "Alagoas"]),
        ylab = "IDEB", xlab = "Matrículas",
        main = "log-log")
abline(lm(log(APSEcono$IDEB[APSEcono$Estado == "Alagoas"])~
            log(APSEcono$`matrícula/população`[APSEcono$Estado == "Alagoas"])))

#lin lin

lm(plot(APSEcono$IDEB[APSEcono$Estado == "Alagoas"]~
          APSEcono$`matrícula/população`[APSEcono$Estado == "Alagoas"],
        ylab = "IDEB", xlab = "Matrículas",
        main = "lin-lin"))
abline(lm(APSEcono$IDEB[APSEcono$Estado == "Alagoas"]~
            APSEcono$`matrícula/população`[APSEcono$Estado == "Alagoas"]))

#########################################################################

eq_final = lm(APSEcono$IDEB ~ APSEcono$`matrícula/população`+ 
                APSEcono$`docentes / matrícula` 
              + APSEcono$`PIB per capita` + APSEcono$d_alag)

print(eq_final)
summary(eq_final)

par(mfrow = c(2,2))
plot(eq_final)

residuos = residuals(eq_final)
plot(residuos)
hist(residuos, main = "histograma dos resíduos")

qqnorm(residuos)
qqline(residuos)

moments::jarque.test(residuos)

lmtest::bptest(eq_final)

robusto = lm_robust((APSEcono$IDEB ~ APSEcono$`matrícula/população` + 
                       APSEcono$`docentes / matrícula` + 
                       APSEcono$`PIB per capita` + APSEcono$d_alag))

summary(robusto)

