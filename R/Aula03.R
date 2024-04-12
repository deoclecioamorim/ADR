#'##############################################################################
#'###### Análise de dados com o R (ADR) ########################################
#'###### Deoclecio Jardim Amorim        ########################################
#'###### Eduardo Mariano                ########################################
#'##############################################################################

#Link do curso
"https://uspdigital.usp.br/apolo/apoObterCurso?cod_curso=640400020&cod_edicao=24001&numseqofeedi=1"

###Conteúdo
#'
#'-Teste de médias
#'-Análise de regressão linear
#'-Regressão linear múltipla
#'

# Pacotes -------------------------------------------------------------------------------------
if (!require(readxl))install.packages("readxl", dep = TRUE)
if (!require(tidyverse))install.packages("tidyverse", dep = TRUE)
if (!require(ExpDes.pt))install.packages("ExpDes.pt", dep = TRUE)
if (!require(knitr))install.packages("knitr", dep = TRUE)
if (!require(kableExtra))install.packages("kableExtra", dep = TRUE)
if (!require(car))install.packages("car", dep = TRUE)
if (!require(emmeans))install.packages("emmeans", dep = TRUE)
if (!require(lmtest))install.packages("lmtest", dep = TRUE)
if (!require(lawstat))install.packages("lawstat", dep = TRUE)

# Conjunto de dados ---------------------------------------------------------------------------
#'
#'Um pesquisador pretende comparar quatro variedades de pêssego quanto ao enraizamento 
#'de estacas. Para tanto, realizou um experimento de acordo com o delineamento inteiramente 
#'casualizado com cinco repetições,sendo cada parcela um vaso com vinte estacas. 
#'Passado o tempo necessário, o pesquisador anotou o número de estacas enraizadas, 
#'apresentado na Tabela a seguir:
#'
library(knitr)

y<- c( 2, 2, 1, 1, 0,
       1, 0, 0, 1, 1,
       12, 10, 14, 17, 11,
       7, 9, 15, 8, 10)
trat<- rep(c("A","B","C","D"), each=5)
dados<- data.frame(trat, y)

tabela <- dados %>%
  kable() %>%
  kable_classic(full_width = FALSE) # Estilo clássico

tabela

# ANOVA ---------------------------------------------------------------------------------------
#'
#'
dados<-transform(dados, trat=as.factor(trat))
str(dados)

#com lm()
mod1<-lm(y ~ trat, dados)
anova(mod1)

par(mfrow = c(2, 2))
plot(mod1)
par(mfrow = c(1, 1))
#'
#'Resíduos estudentizados
res_Stud <- rstandard(mod1)
shapiro.test(res_Stud)
#'
# Verificando homogeneidade de variâncias
ggplot(dados, aes(x = trat, y = res_Stud)) +
  geom_dotplot(binaxis = "y", stackdir = "center", fill = "steelblue") +
  labs(x = "Variedade", y = "Resíduos Studentizados") +
  theme_minimal()

library(MASS)
boxcox(dados$y+0.01 ~ dados$trat,ylab="logaritmo da 
       verossimilhança") #lambda=0,5.

#'
#'Análise dos Dados Transformados
dados$yt<- (y+0.01)^0.5
modelot<- lm(yt ~ trat, dados)

#'
#'Checando as pressuposições
par(mfrow = c(2, 2))
plot(modelot)
par(mfrow = c(1, 1))

shapiro.test(rstandard(modelot))

#'
#'Tranformação Box-cox
boxcox(modelot,ylab="logaritmo da verossimilhança")

#'
#'Análise de variância para os dados transformados
anova(modelot)
#'
#'
#'Teste médias
Tukey <- HSD.test(modelot,"trat",alpha=0.05,console=TRUE)
Duncan <- duncan.test(modelot,"trat",alpha=0.05,console=TRUE)
LSD_Fisher <- LSD.test(modelot,"trat",alpha=0.05,console=TRUE)

#'
#'Apresentação das médias
round((tapply(dados$y, dados$trat, mean)),4) 


# Funções similares para ANOVA ----------------------------------------------------------------

#com aov()
mod2<-aov(y ~ trat, dados)
summary(mod2)
#anova(mod2)

#com glm()
mod3<-glm(y ~ trat,family = "gaussian" ,dados)
summary(mod3)
anova(mod3, test = "F")

QMT<-4000.0/2
QMR<-224.8/42
Fcal


#library(car)
Anova(mod3, test = "F")

QMT<-4000.0/2
QMR<-224.8/42
Fcal<-QMT/QMR
Fcal




# Set and get working directory
setwd("C://Users/dumar/Google Drive/R")
getwd()
library(tidyverse)
library(soiltestcorr)
data <- (soiltestcorr::freitas1966)
view(data)
#write.csv(data, "freitas1966.csv", row.names=FALSE)
plotlp <-  linear_plateau(data, STK, RY, plot = TRUE)
plotlp
ggsave(
  plot = plotlp,
  file = "linear+plateau.png",
  type = "cairo",
  width = 7,
  height = 5,
  dpi = 300
)


# Linear + Platô ------------------------------------------------------------------------------


