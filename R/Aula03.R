###############################################################################
####### Análise de dados com o R (ADR) ########################################
####### Deoclecio Jardim Amorim  ##############################################
####### Eduardo Mariano          ##############################################
###############################################################################

#Link do curso
"https://uspdigital.usp.br/apolo/apoObterCurso?cod_curso=640400020&cod_edicao=24001&numseqofeedi=1"

###Conteúdo
#'
#'-Teste de médias
#'-Análise de regressão linear
#'-Regressão linear múltipla
#'

# Pacotes -------------------------------------------------------------------------------------


# Conjunto de dados ---------------------------------------------------------------------------
#'
#'Formato .xlsx
dados <- read_excel("dados/dados_kozak2017.xlsx", sheet = 1)
head(dados)

#'Usando lm()
#'
dados<-transform(dados, trat=as.factor(trat))
str(dados)

#com lm()
mod1<-lm(y ~ trat, dados)
anova(mod1)

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


library(car)
Anova(mod3, test = "F")

QMT<-4000.0/2
QMR<-224.8/42
Fcal<-QMT/QMR
Fcal




