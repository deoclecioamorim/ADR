#'##############################################################################
#'###### Análise de dados com o R (ADR) ########################################
#'###### Deoclecio Jardim Amorim        ########################################
#'###### Eduardo Mariano                ########################################
#'##############################################################################

#Link do curso
"https://uspdigital.usp.br/apolo/apoObterCurso?cod_curso=640400020&cod_edicao=24001&numseqofeedi=1"

#'##Conteúdo
#'
#'-Teste de médias
#'-Análise de regressão linear
#'-Regressão linear múltipla
#'
# Opções de controle -----------------------------------------------------------------------------
options(prompt = "R", continue = "+  ", width = 70, useFancyQuotes = FALSE)
options(OutDec=".")#Separador decimal, útil para gráficos
rm(list=ls(all=T))#Limpar memoria


# Pacotes -------------------------------------------------------------------------------------
if (!require(readxl))install.packages("readxl", dep = TRUE)
if (!require(tidyverse))install.packages("tidyverse", dep = TRUE)
if (!require(ExpDes.pt))install.packages("ExpDes.pt", dep = TRUE)
if (!require(agricolae))install.packages("agricolae", dep = TRUE)
if (!require(knitr))install.packages("knitr", dep = TRUE)
if (!require(kableExtra))install.packages("kableExtra", dep = TRUE)
if (!require(car))install.packages("car", dep = TRUE)
if (!require(emmeans))install.packages("emmeans", dep = TRUE)
if (!require(lmtest))install.packages("lmtest", dep = TRUE)
if (!require(lawstat))install.packages("lawstat", dep = TRUE)
if (!require(MASS))install.packages("MASS", dep = TRUE)
if (!require(broom))install.packages("broom", dep = TRUE)
if (!require(jtools))install.packages("jtools", dep = TRUE)
if (!require(plotly))install.packages("plotly", dep = TRUE)


# Conjunto de dados ---------------------------------------------------------------------------
#'
#'Suponhamos um experimento (fictício) de alimentação de porcos em que 
#'se usaram quatro rações (A, B, C, D), cada uma fornecida a cinco animais
#'escolhidos ao caso. Os aumentos de peso observados, em quilogramas,
#'constam abaixo:
#'

y<- c(40,24,36,15,65,
      45,40,51,46,38,
      44,32,25,34,70,
      19,17,18,13,16)
trat<- rep(c("A","B","C","D"), each=5)
dados<- data.frame(trat, y)

tabela <- dados %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 15)
tabela

# ANOVA ---------------------------------------------------------------------------------------
#'
#'Transformar em fator
#'
dados<-transform(dados, trat=as.factor(trat))
str(dados)
#'
#'Estatística descritiva desse experimento
#'
Estat_descritiva <- dados %>%
  group_by(trat) %>% summarise(
    media = mean(y, na.rm = TRUE),
    SD = sd(y, na.rm = TRUE)
  )

Estat_descritiva %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 26)

#'
#'Média geral
#'
(ybar <- mean(dados$y))
#'
#'ANOVA
#com lm()
mod1<-lm(y ~ trat, data = dados)
anova(mod1)

# Outras opções para ANOVA --------------------------------------------------------------------
# #com aov()
# mod2<-aov(y ~ trat, dados)
# summary(mod2)
# #anova(mod2)
# 
# #com glm()
# mod3<-glm(y ~ trat,family = "gaussian" ,dados)
# summary(mod3)
# anova(mod3, test = "F")
# 
# QMT<-4000.0/2
# QMR<-224.8/42
# Fcal
# 
# 
# #library(car)
# Anova(mod3, test = "F")
# 
# QMT<-4000.0/2
# QMR<-224.8/42
# Fcal<-QMT/QMR
# Fcal


# Pressuposições da ANOVA------------------------------------------------------------------------------

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
#'
#'Transformação Box-cox
#'
#library(MASS)
boxcox(dados$y ~ dados$trat,ylab="logaritmo da 
       verossimilhança") #lambda=0,5.
#'
#'Análise dos Dados Transformados
dados$yt<- log(y+0.5)
modelot<- lm(yt ~ trat, dados)

#'
#'Checando as pressuposições
par(mfrow = c(2, 2))
plot(modelot)
par(mfrow = c(1, 1))

shapiro.test(rstandard(modelot))

#'
#'Tranformação Box-cox
boxcox(modelot,lambda = seq(-4, 4, 1/10), ylab="logaritmo da verossimilhança")
#'
#'Como o 1 está dentro do intervalo não é necessário transformação!
#'
#'Análise de variância para os dados transformados
anova(modelot)
#'
#'

# Teste de médias -----------------------------------------------------------------------------
#'
#'Recurso da biblioteca agricolae
Tukey <- HSD.test(modelot,"trat",alpha=0.05,console=TRUE)
Duncan <- duncan.test(modelot,"trat",alpha=0.05,console=TRUE)
LSD_Fisher <- LSD.test(modelot,"trat",alpha=0.05,console=TRUE)

#'
#'Apresentação das médias
round((tapply(dados$y, dados$trat, mean)),4) 
#'
#'
#'Apresentação gráfica
#'
# Médias marginais ajustdas.
emm <- emmeans(mod1, specs = ~trat)
emm

results <- Tukey$groups %>%
  rownames_to_column(var = "trat") %>%
  mutate(groups = str_trim(as.character(groups)))

results <- inner_join(results, as.data.frame(emm))
results


fig1<- ggplot(data = results,
       mapping = aes(x = trat, y = emmean)) +
  geom_point(dados,mapping = aes(trat, y, color="magenta"), shape=15, show.legend = F)+
  geom_point(color = "black") +
  geom_errorbar(mapping = aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.1,size  = 0.3) +
  geom_label(mapping = aes(label = sprintf("%0.2f %s",
                                           emmean, groups)),
             nudge_y = 4.25, fill= "#00FFFF")+
  theme_test(base_size =12, base_family = "sans")+theme_bw()+
  theme(axis.text.x = element_text(color = "black", size = 8),
        axis.text.y = element_text(color = "black"),
        panel.spacing = unit(0, "cm"),
        legend.position = "top",
        panel.background = element_rect(fill = "white"))+
  theme(legend.position = "top")+
  ylab("Peso em kg")+ xlab("Tipos de ração")

fig1

#'
#'Salvando a figura
png(filename="figuras/fig1.png", # Nome do arquivo e extensão
    width = 4,    # largura
    height = 4,   # Altura
    res= 400,# Resolução em dpi
    family = "serif", #fonte
    units = "in")  # Unidades.
fig1
dev.off() # Fecha a janela gráfica


#'
#'Mudando a fonte para Arial e resolução
png(filename="figuras/fig1b.png", # Nome do arquivo e extensão
    width = 4,    # largura
    height = 4,   # Altura
    res= 600,# Resolução em dpi
    family = "sans", #fonte
    units = "in")  # Unidades.
fig1
dev.off() # Fecha a janela gráfica


jpeg(filename="figuras/fig1c.jpeg", # Nome do arquivo e extensão
    width = 4,    # largura
    height = 4,   # Altura
    res= 600,# Resolução em dpi
    family = "sans", #fonte
    units = "in")  # Unidades.
fig1
dev.off() # Fecha a janela gráfica

#'
#'Outro forma salvar figura
#'
ggsave(
  plot = fig1,
  file = "figuras/fig1d.png",
  width = 10.2,
  height = 10.2,
  units = "cm",
  dpi = 300
)

# Análise de regressão ----------------------------------------------------------------
#'
#'Suponhamos um experimento (fictício) de produção de cana-de-açurcar em que 
#'se usaram seis doses de nitrogênio (0, 50, 100, 150 e 200 kg por hectare). 
#'A variável resposta considerada aqui foi o peso dos colmos em toneladas por  
#'hectare.
#'
#'Dados
tch<- c(10,	12,	11,	9,
      15,	18,	16,	14,
      20,	22,	21,	19,
      23,	24,	23,	22,
      22,	20,	19,	18,
      12,	14,	15,	13)
dose<- rep(c(0, 50, 100, 150, 200, 250), each=4)
dados_reg<- data.frame(dose, tch)
View(dados_reg)
#'
#'#########################################################################
#                            ANOVA                                        #
#'#########################################################################
#'
#'
modANOVA<-lm(tch~as.factor(dose), dados_reg)
anova(modANOVA)
#'
#'
#'##########################################################################
#                            GRÁFICO DE DISPERSÃO                          #
#'#########################################################################
#'
#'Verificação visual do padrão dos dados
ggplot(dados_reg, aes(x = dose, y = tch)) +
  geom_point(color = "#39568CFF", size = 2.5) +
  xlab(expression(Dose~kg~ha^{-1}~de~nitrogênio))+
  ylab(expression(Produção~kg~ha^{-1}))+
  theme_classic()

#'
#'Regressão linear simples
#'
#'Modelo estatístico:
#'
#'Y = a + bx+ erro;
#'
modlinear= lm(tch ~ dose, dados_reg)
summary(modlinear)

#'
#'Y = a + b1X + b2X^2 + erro;
#' 
modquad<-lm(tch ~ dose+I(dose^2), dados_reg)
summary(modquad)


#função 'summ' do pacote 'jtools'
summ(modquad, confint = T, digits = 4, ci.width = .95)

#'
#'Verificando a falta de ajuste
#'
anova(lm(tch ~ dose + I(dose^2) + as.factor(dose)))
#'
#'
#'Plotando o resultado
#'
ggplotly(
ggplot(dados_reg, aes(dose, tch))+
      geom_point(color = "#39568CFF", shape=16, size=1)+
  geom_smooth(method = lm, formula = y ~ x+I(x^2), level = 0.95)+
  geom_hline(yintercept = 17.2, color = "grey50", size = .5) +
  labs(x = "Dose em kg/ha de nitrogênio",
       y = "Produção de colmo em t/ha") +
  theme_classic()
)


#'Gráfico estilizado
#'
# Valores de exibição
(p_valor <- 0.001)
(p_valor_text <- sprintf("p-value < %.4f", p_valor))
eq <- expression(hat(y)==9.7321+0.1767*x-0.0006*x^2) #Formato latex
r_quadrado <- expression(R^2==0.90) #Formato latex
p_max <-expression(Pmax==138.2)


graf.reg <- ggplot(dados_reg, aes(dose, tch))+
  geom_point(color = "#39568CFF", shape=20, size=2)+
  geom_smooth(method = lm, formula = y ~ x+I(x^2), level = 0.95)+
  geom_vline(xintercept = 138.2, linetype = "dashed", color = "red")+
  xlab(expression(Dose~kg~ha^{-1}~de~nitrogênio))+
  ylab(expression(Produção~kg~ha^{-1}))+
  annotate("text", x = 70, y = 26, #R-quadrado
           label = as.character(r_quadrado), parse = TRUE) +
  annotate("text", x = 70, y = 25, #Equação
           label = as.character(eq), parse = TRUE)+
  annotate("text", x = 70, y = 24, #P-valor
           label = as.character(p_valor_text), parse = TRUE)+
  annotate("text", x = 170, y = 18, #P-valor
           label = as.character(p_max), parse = TRUE)+
  theme_classic()

graf.reg






















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


