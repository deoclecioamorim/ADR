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
options(OutDec=".")#Separador decimal, útil para gráficos
options(scipen = 100) #Elinando notação cientifica
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
if (!require(GGally))install.packages("GGally", dep = TRUE)
if (!require(soiltestcorr))install.packages("soiltestcorr", dep = TRUE)



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
boxcox(mod1,ylab="logaritmo da 
       verossimilhança") #lambda=0,5.
#'
#'Análise dos Dados Transformados
#'Criando a variável transformada
dados$yt<- log(dados$y+0.5)
dados
#'
#'Criando um novo modelo para análisar
#'a variável transformada
#'
modelot<- lm(yt ~ trat, dados)
anova(modelot)
#'
#'Deve-se checar as pressuposições do modelo
#'novamente (normalidade e homogeneidade)
#'
par(mfrow = c(2, 2))
plot(modelot)
par(mfrow = c(1, 1))

#Teste Shapiro-Wilk
shapiro.test(rstandard(modelot))
#'
#'Checando-se se é necessário nova tranformação Box-cox
boxcox(modelot,lambda = seq(-4, 4, 1/10), 
       ylab="logaritmo da verossimilhança")
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
#'Suponhamos um experimento (fictício) de produção de cana-de-açúcar em que 
#'se usaram seis doses de nitrogênio (0, 50, 100, 150 e 200 kg/ha). 
#'A variável resposta considerada aqui foi a massa seca dos colmos em toneladas por  
#'hectare (tch).
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
modANOVA_fator<-lm(tch~as.factor(dose), dados_reg)
anova(modANOVA_fator)
#'
#'
#'Caso dose não seja considerada fator veja o que ocorre.
#'
modANOVA<-lm(tch~dose, dados_reg)
anova(modANOVA)
summary(modANOVA)
#'
#'#'Graus de liberdade são colocados no resíduo!!!
#'
#'Estatística descritiva desse experimento
#'
Estat_descritiva_reg <- dados_reg %>%
  group_by(dose) %>% summarise(
    media = mean(tch, na.rm = TRUE),
    SD = sd(tch, na.rm = TRUE)
  )

Estat_descritiva_reg 

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
#'Y = b0 + b1x+ erro;
#'
modlinear= lm(tch ~ dose, dados_reg)
summary(modlinear)
#'
#'Y = b0 + b1X + b2X^2 + erro;
#' 
modquad<-lm(tch ~ dose+I(dose^2), dados_reg)
summary(modquad)
#'
#'
#função 'summ' do pacote 'jtools'
summ(modquad, confint = T, digits = 4, ci.width = .95)
#'
#'
#'Cálculo manual do R2
#'
predito <- predict(modquad)
predito
R2<-(cor(dados_reg$tch, predito)^2)
R2
#'
#'Verificando a falta de ajuste
#'
anova(modANOVA_fator)
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

#'
#'##########################################################################
#                           EXERCÍCIO                                     #
#'#########################################################################
#'
#'Calcule a produção máxima teórica de cana-de-açúcar, com cálculos,
#'com base no modelo quadrático acima. 
#'
#'Gráfico estilizado
#'
# Valores de exibição
(p_valor_text <- expression(italic(p)-value < 0.001))
eq <- expression(hat(y)==9.7321+0.1767*x-0.0006*x^2) #Formato latex
r_quadrado <- expression(italic(R)^2==0.90) #Formato latex
p_max <-expression(Pmax==138.2)

graf.reg <- ggplot(dados_reg, aes(dose, tch))+
  geom_point(color = "#39568CFF", shape=20, size=2)+
  geom_smooth(method = lm, formula = y ~ x+I(x^2), level = 0.95)+
  geom_vline(xintercept = 138.2, linetype = "dashed", color = "red")+
  geom_hline(yintercept = 21.9, linetype = "dashed", color = "red")+
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

#'
#'##########################################################################
#                           EXERCÍCIO                                     #
#'#########################################################################
#'
#'Salve a figura "graf.reg" no formato png com resolução de 400 dpi e 
#'coloque na pasta figuras.
#'
#'
# Regressão linear multipla -------------------------------------------------------------------
#'
#'
#'Y = b0 + b1X1 + b2X2 + ... + bpXp+ erro;
#'
# Conjunto de dados N total-------------------------------------------------
#'
#'Vamos trabalhar com o conjunto de dados árvore para isso limpe a memória
#'do R!!!
#'
rm(list=ls(all=T))#Limpar memoria
predicaoN <- read_excel("dados/predicaoN.xlsx")
str(predicaoN)

#'
#'##########################################################################
#       Monte o modelo de regressão linear múltiplo                        #                      
# OBJETIVO: construir um modelo preditivo para estimar o N total do solo.  #
#'#########################################################################
#'
#library(GGally) 
Scatter_Matrix <- ggpairs(predicaoN, 
                          title = "Scatter Plot", 
                          axisLabels = "show") 

Scatter_Matrix

#'
#'Modelo completo
multreg<-lm(NTS ~., data = predicaoN)
summary(multreg)

#'
#'Fazendo a seleção de preditores com o critério AIC (library(MASS))
#'
multreg2<-lm(NTS ~.,  data = predicaoN)%>%stepAIC(direction = "both")
summary(multreg2)

#'
#'Modelo preditivo
predicaoN$NTS_pred<-predict(multreg2)

plot.pred <-ggplot(predicaoN, aes(x = NTS_pred, y = NTS )) + 
  geom_point(alpha = 1/2) + 
  geom_abline(intercept = 0, slope = 1,colour = "grey") +
  stat_smooth(method = lm, colour = "black", se = T) +
  scale_x_continuous(limits=c(0, 1.5)) +
  scale_y_continuous(limits=c(0, 1.5)) +
  xlab("NTS predito") + # adiciona descrição do eixo x
  ylab("NTS observado") + 
  theme_classic()

plot.pred


# Linear + Platô ------------------------------------------------------------------------------
# Set and get working directory

library(soiltestcorr)
#https://adriancorrendo.github.io/soiltestcorr/
data <- (soiltestcorr::freitas1966)
view(data)
#write.csv(data, "freitas1966.csv", row.names=FALSE)
#'
#'Modelo linear (sitaxe ggplot2)
linear_plato <-  linear_plateau(data, STK, RY, plot = TRUE)
linear_plato

#'
#'Customização do gráfico
teste<-linear_plato +
  # Main title
  ggtitle("My own plot title")+
  # Axis titles
  labs(x = "Soil Test K (ppm)",
       y = "Cotton RY(%)") +
  # Axis scales
  scale_x_continuous(limits = c(20,220),
                     breaks = seq(0,220, by = 10)) +
  # Axis limits
  scale_y_continuous(limits = c(30,100),
                     breaks = seq(30,100, by = 10))

teste

ggsave(
  plot = linear_plato,
  file = "figuras/linear+plateau.png",
  type = "cairo",
  width = 7,
  height = 5,
  dpi = 300
)

#'
#'Modelo quadrático (sitaxe ggplot2)
quadratic_plato <-  quadratic_plateau(data, STK, RY, plot = TRUE) 
quadratic_plato

ggsave(
  plot = quadratic_plato,
  file = "figuras/quadratic+plateau.png",
  type = "cairo",
  width = 7,
  height = 5,
  dpi = 300
)

