
# Aula 01 -------------------------------------------------------------------------------------

#'##########################################################################
# AULA 01                                                                 #
#'##########################################################################

#'Extra 1: função para cálculo do desvio padrão
#'
dvpad<-function(x){
xbar <-sum(x)/length(x)
sqd <- sum((x - xbar)^2)
var<-sqd/(length(x)-1)
 return(var) 
}

a<-c(4,4,4)
dvpad(a)

#'
#'Extra 2: manipulação

dados_mani_6 <- dados_mani_3 %>%
  group_by(trat) %>% summarise(
    mediaY = mean(y, na.rm = TRUE),
    SDY = sd(y, na.rm = TRUE),
    EPMY=SDY/sqrt(length(y)), #erro padrão da média
    medialogY = mean(logy, na.rm = TRUE),
    SDlogY = sd(logy, na.rm = TRUE),
    EPMlogY=SDlogY/sqrt(length(y)), #erro padrão da média
    mediaraiz_Y = mean(raiz_y, na.rm = TRUE),
    SDraiz_Y = sd(raiz_y, na.rm = TRUE),
    EPMraiz_Y=SDraiz_Y/sqrt(length(y)) #erro padrão da média
    
  )


# Aula 04 -------------------------------------------------------------------------------------
# Conjunto de dados Eucaliptus ---------------------------------------------------------------------------
# Modelo de efeito fixo -----------------------------------------------------------------------
#'##########################################################################
# Passo 1 e 2:                                                             #
#'##########################################################################
#'
eucalip <- read_excel("dados/eucalip.xlsx")
str(eucalip) #Checagem da estrutura dos dados
#'
#'Transformar em fator
eucalip<-transform(eucalip, clone=as.factor(clone))
str(eucalip) #Checagem da estrutura dos dados
#'
#'##########################################################################
# Passo 3 - 5:                                                             #
#'##########################################################################
#' 
# Obtendo a tabela da ANOVA, com teste F para clones ------------------------------------------
#'
#'Modelo de efeito fixo
#'
#'Volume = u + clone + erro
#'
#'
modfixo <- lm(volmad~clone, eucalip)
anova(modfixo)
#'
#'Teste F: hipóteses
#'H0: mu_1 = mu_2 =... mu_i = mu;
#'Ha: pelos menos um contraste de médias difere de zero.
#'
#'CONCLUSÃO: Existe diferênça entre os clones de Eucaliptus camaldulensis!
#'
#'TESTE TUKEY
#'
# Teste de médias -----------------------------------------------------------------------------
#'
#'Recurso da biblioteca agricolae
Tukey <- HSD.test(modfixo,"clone",alpha=0.05,console=TRUE)

#'
#'
# Conjunto de dados alfafa ---------------------------------------------------------------------------
# Modelo de efeito fixo -----------------------------------------------------------------------
#'##########################################################################
# Passo 1 e 2:                                                             #
#'##########################################################################
#'
alfafa <- read_excel("dados/alfafa.xlsx")
str(alfafa)

alfafa<-transform(alfafa, variedade=as.factor(variedade), 
                  bloco=as.factor(bloco))
str(alfafa)


# Obtendo a tabela da ANOVA, com teste F variedades ------------------------------------------
#'
#'Modelo de efeito fixo
#'
#'prod = u + bloco + variedade+ erro
#'
#'
#'Modelo de efeito fixo
#'
modfixo <- lm(prod ~ bloco+variedade, data=alfafa)
anova(modfixo)

#'Teste F: hipóteses
#'H0: mu_1 = mu_2 =... mu_i = mu;
#'Ha: pelos menos um contraste de médias difere de zero.
#'
#'CONCLUSÃO: Existe diferênça entre variedades de alfafa!
#'
#'TESTE TUKEY
#'
# Teste de médias -----------------------------------------------------------------------------
#'
#'Recurso da biblioteca agricolae
Tukey <- HSD.test(modfixo,"variedade",alpha=0.05,console=TRUE)


#'
#'Calculo do coeficiente de herdabilidade
#'
#'Componentes de variância
#'
#'variedade ~ N(0, sigma2_varie)
#'
#'erro ~ N(0, sigma2_res)
#'
#'
mod_var <- lmer(prod~(1|variedade)+bloco, alfafa)
summary(mod_var)

#'
#'Coeficiente de herdabilidade
(var_total<-0.02768+(0.04765/6))
(h2<-0.02768/var_total)


# Aula05 --------------------------------------------------------------------------------------

#'O conjunto de dados está no formato .csv
#'
pardocas <-read.table("dados/pardocas.csv",header = T, sep = ",")
head(pardocas)
str(pardocas)

