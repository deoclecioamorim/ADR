###############################################################################
####### Análise de dados com o R (ADR) ########################################
####### Deoclecio Jardim Amorim  ##############################################
####### Eduardo Mariano          ##############################################
###############################################################################

#Link do curso
"https://uspdigital.usp.br/apolo/apoObterCurso?cod_curso=640400020&cod_edicao=24001&numseqofeedi=1"

###Conteúdo
#'
#'-Introdução aos modelos mistos: tipos de efeito (aleatório e fixo);
#'-Interpretação dos componentes de variância.
#'
# Opções de controle -----------------------------------------------------------------------------
options(prompt = "R", continue = "+  ", width = 70, useFancyQuotes = FALSE)
options(OutDec=".")#Separador decimal, útil para gráficos
options(scipen = 100) #Elinando notação cientifica
rm(list=ls(all=T))#Limpar memoria

# Pacotes -------------------------------------------------------------------------------------
if (!require(lme4))install.packages("lme4", dep = TRUE)
if (!require(lmerTest))install.packages("lmerTest", dep = TRUE)
if (!require(emmeans))install.packages("emmeans", dep = TRUE)
if (!require(multcomp))install.packages("multcomp", dep = TRUE)


# DIC: tratamento com efeito aleatório --------------------------------------------------------

eucalip <- read_excel("dados/eucalip.xlsx")
str(eucalip)

eucalip<-transform(eucalip, clone=as.factor(clone))
str(eucalip)

#'
#'Modelo de efeito fixo
#'
modfixo <- lm(volmad~clone, eucalip)
anova(modfixo)

#'
#'Máxima verossimilha (ML)
modclone1 <- lmer(volmad~1+(1|clone), eucalip, 
                REML = FALSE)
summary(modclone1)


#'Máxima verossimilha restrita (REML)
modclone2 <- lmer(volmad~1+(1|clone), eucalip, 
                REML = TRUE)

summary(modclone1)
summary(modclone2)



# DBC: bloco de efeito de aleatório -----------------------------------------------------------

alfafa <- read_excel("dados/alfafa.xlsx")
str(alfafa)

alfafa<-transform(alfafa, variedade=as.factor(variedade), 
                  bloco=as.factor(bloco))
str(alfafa)

#'
#'Modelo de efeito fixo
#'
modfixo <- lm(prod~variedade+bloco, alfafa)
anova(modfixo)

#'
#'Modelo de efeito aleatório para bloco
#'
mod_aleat_bloco <- lmer(prod~variedade+(1|bloco), alfafa)
summary(mod_aleat_bloco)


#'Anova tipo 3 teste para efeito fixo de variedade
anova(mod_aleat_bloco, ddf = "Kenward-Roger")
#'
#'Médias marginais
#'E(Y)=XB
#'
emmeans(mod_aleat_bloco, pairwise ~ variedade, adjust = "tukey")
letter <- emmeans(mod_aleat_bloco, ~ variedade, adjust = "tukey")
cld(letter,
    alpha = 0.05,
    Letters = letters,
    reversed = TRUE)

#'
#'Calculo do coeficiente de herdabilidade
#'
mod_var <- lmer(prod~(1|variedade)+bloco, alfafa)
summary(mod_var)

#'
#'Coeficiente de herdabilidade
(var_total<-0.02768+(0.04765/6))
(h2<-0.02768/var_total)

#'
#'

# Modelo hierárquico (fatores aninhados) -------------------------------------------------------
#'
#'Avaliando a contribuição genética de touro e vaca no peso de bezerros
#'recém nascido.
#'
pesobezerros <- read_excel("dados/pesobezerros.xlsx")
str(pesobezerros)

pesobezerros<-transform(pesobezerros,  touro=as.factor(touro), vaca=as.factor(vaca ))
str(pesobezerros)

#'
#'Modelos de efeito fixo
#'

modfixo <- lm(peso_bezerro ~ touro/vaca, pesobezerros)
anova(modfixo)

#'
#'Máxima verossimilha restrita (REML)
mod_misto <- lmer(peso_bezerro ~1+(1|touro/vaca), pesobezerros, REML = TRUE)
summary(mod_misto)

#'
#'Particionamento da variabilidade: quanto cada fator explica a variância do peso
#Contribuição dos pais 
(var_total<-121.7525584012192+0.0000000003989+0.7361381533387)
(contri_touro<-(0.0000000003989/var_total)*100)
(contri_vaca<-(121.7525584012192/var_total)*100)
(contri_acaso<-100-contri_touro-contri_vaca)


# Medidas repetidas no tempo ------------------------------------------------------------------

# Dataset (Mariano et al., 2019)
#'
#'Foi conduzido um experimento em condições controladas para quantificação da emissão
#'de amônia de fertilizantes com base em ureia. Foram utilizadas sete fertilizantes
#'com três repetições cada. As medições de amônia foram realizadas quatro vezes na
#'mesma parcela ao longo do tempo, ou seja, um estudo de medidas repetidas.
#'
#'
#'
volatizacao <- data.frame(
  parcela = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6,
          7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10, 10, 11, 11, 11, 11, 12, 12, 
          12, 12, 13, 13, 13, 13, 14, 14, 14, 14, 15, 15, 15, 15, 16, 16, 16, 16, 17,
          17, 17, 17, 18, 18, 18, 18, 19, 19, 19, 19, 20, 20, 20, 20, 21, 21, 21, 21),
  fert = c(rep("UR", 12), rep("B-Cu-UR", 12), rep("NBPT-UR", 12), rep("PSC-UR", 12),
           rep("SC-UR", 12), rep("HS-UR", 12), rep("DCD-NBPT-UR", 12)),
  nh3 = c(0.011, 1.470, 5.335, 5.793, 0.014, 1.107, 4.986, 5.256, 0.012, 0.915, 4.892,
          5.754, 0.003, 0.044, 0.710, 2.469, 0.003, 0.040, 0.765, 2.575, 0.003, 0.039,
          0.820, 2.522, 0.000, 0.300, 0.076, 0.321, 0.000, 0.008, 0.078, 0.401, 0.000,
          0.002, 0.083, 0.370, 0.000, 0.006, 0.063, 0.565, 0.001, 0.014, 0.138, 0.565,
          0.001, 0.014, 0.137, 0.735, 0.003, 0.298, 2.456, 4.628, 0.003, 0.358, 2.364,
          4.137, 0.006, 0.450, 2.934, 4.693, 0.017, 2.098, 7.094, 5.256, 0.019, 1.834,
          5.953, 5.231, 0.026, 2.377, 6.230, 4.360, 0.000, 0.003, 0.083, 0.384, 0.000,
          0.002, 0.072, 0.599, 0.000, 0.001, 0.086, 0.446),
  tempo = rep(1:4, 21)
)

#'
#'Checando estrutura dos dados
#'
str(volatizacao)
#'
#'Transformando em fatores
#'
volatizacao<-transform(volatizacao, 
                       parcela= as.factor(parcela), 
                       fert=as.factor(fert),
                       tempo=as.factor(tempo))

str(volatizacao)
#'


#'Máxima verossimilha restrita (REML)
modrep <- lmer(nh3 ~ fert * tempo + (1| parcela) , data = volatizacao, 
                REML = TRUE)

summary(modrep)

#'
#'Anova tipo 3 teste para efeitos fixos
anova(modrep, ddf = "Kenward-Roger")
#'
#'Médias marginais
#'E(Y)=XB
#'
emmeans(modrep, pairwise ~ fert, adjust = "tukey")
letter <- emmeans(modrep, ~ fert, adjust = "tukey")
cld(letter,
    alpha = 0.05,
    Letters = letters,
    reversed = TRUE)








