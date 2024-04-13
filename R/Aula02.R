#'##############################################################################
#'###### Análise de dados com o R (ADR) ########################################
#'###### Deoclecio Jardim Amorim        ########################################
#'###### Eduardo Mariano                ########################################
#'##############################################################################

# Opções de controle -----------------------------------------------------------------------------
options(prompt = "R", continue = "+  ", width = 70, useFancyQuotes = FALSE)
options(OutDec=".")#Separador decimal, útil para gráficos
rm(list=ls(all=T))#Limpar memoria

#Link do curso
"https://uspdigital.usp.br/apolo/apoObterCurso?cod_curso=640400020&cod_edicao=24001&numseqofeedi=1"

#'##Conteúdo
#'
#'-Introdução à visualização de dados com o ggplot2: histograma, dispersão e boxplot
#'-Análise de variância (ANOVA) e pressuposições


# Pacotes -------------------------------------------------------------------------------------

if (!require(readxl))install.packages("readxl", dep = TRUE)
if (!require(tidyverse))install.packages("tidyverse", dep = TRUE)
if (!require(dae))install.packages("dae", dep = TRUE)
if (!require(lmtest))install.packages("lmtest", dep = TRUE)
if (!require(lawstat))install.packages("lawstat", dep = TRUE)
if (!require(ExpDes.pt))install.packages("(ExpDes.pt", dep = TRUE)

# Conjunto de dados ---------------------------------------------------------------------------
#'
#'Formato .xlsx
dados <- read_excel("dados/dados_kozak2017.xlsx", sheet = 1)
head(dados)
#' 
# DataViz -------------------------------------------------------------------------------------
#'
#'Histograma

# Definir o tamanho da amostra
n <- 500

# Gerar amostras aleatórias de uma distribuição normal com média 0 e desvio padrão 1
set.seed(10)
#'
#'Variáveis y1, y2
x <- rnorm(n, mean = 0, sd = 1)
y <- rnorm(n, mean = 0, sd = 2)

# Criar um data frame com os valores da amostra
df <- data.frame(x,y)

# Calcular a média e o desvio padrão da amostra
media <- mean(df$y)
desvio_padrao <- sd(df$y)

# Criar o histograma com a densidade da distribuição normal
ggplot(df, aes(x=y)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", color = "white") +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribuição Normal Simulada",
       x = "Valor", y = "Densidade") +
  theme_minimal()


# Criar o scatter plot
ggplot(df, aes(x = x, y = y)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  labs(title = "Scatter Plot de Duas Variáveis Normais Aleatórias",
       x = "Variável X", y = "Variável Y") +
  theme_minimal()

# Desgin de experimentos ----------------------------------------------------------------------
# Quais os principios básicos de experimentação -----------------------------------------------
#'
#'-1) Príncipio da repetição;
#'
#'-2) Casualização;
#'
#'-3) Controle local.
#'
#'
#'###########################################################################
#'########## Delineamento inteiramente casualizado ##########################
#'###########################################################################
#'
#' #Ajuste do modelo
#' yij = u + t + E
#' em que u média geral, t é o efeito do 
#' tratamento e E é o efeito do acaso (ERRO EXPERIMENTAL).
#' 
# Gerando o croqui de um DIC ------------------------------------------------------------------

#Função Croqui
DIC.croqui<-function(trat, rep, semente){
  DIC.sis <- cbind(dae::fac.gen(generate = list(Rows=rep, Columns=trat)),
                   dae::fac.gen(generate = list(Treatments = LETTERS[1:trat]),
                           times = rep))
  #layout
  DIC.lay <- dae::designRandomize(allocated = DIC.sis["Treatments"],
                             recipient = DIC.sis[c("Rows", "Columns")],
                             nested.recipients = list(Columns = "Rows"),
                             seed = semente)
  #croqui
  croqui<-dae::designGGPlot(DIC.lay, labels = "Treatments", cellalpha = 0.75,
                  blockdefinition = cbind(1,trat))
  return(croqui)
}


#'##############################################################
#'
DIC.croqui(trat=6, rep=3, semente=1)



# ANOVA: pressuposições ----------------------------------------------------------------------
#'
#'###########################################################################
#'########## Questões importantes                 ##########################
#'###########################################################################
#'
#'-1) A normalidade deve ser verificada nos dados brutos ou nos resíduos
#'   (ou é irrelevante qual das duas abordagens adotamos)?
#'   
#'-2) A normalidade e a homogeneidade da variância devem ser 
#'    verificadas utilizando testes de significância ou gráficos
#'    de diagnóstico (ou ambos)?
#' 

#'
#'Vamos trabalhar com os dados do artigo: https://doi.org/10.1111/jac.12220
#'
#'###########################################################################
#'What's normal anyway? Residual plots are more telling than significance 
#'tests when checking ANOVA assumptions
#'###########################################################################

#'
#'Desenhando um possível croqui para os dados de M. Kozak e H.-P. Piepho, 2017
DIC.croqui(trat=3, rep=15, semente=1)

#'Box-plot

box_plot <- ggplot(dados, aes(trat, y = y, fill = trat)) +
  geom_boxplot(notch = FALSE, outlier.shape = NA, alpha=0.8, show.legend = FALSE) + 
  geom_hline(yintercept = 29.4, linetype = 2) +
  scale_y_continuous(breaks = seq(15, 50, 10), limits = c(10, 50)) +
  ylab("Valor de Y") +
  xlab("Tratamentos") +
  geom_jitter(height = 0.1, width = 0.15, alpha = 0.25, size = 0.5, fill = "gray") +
  scale_fill_manual(values = c("#D55E00", "#009E73", "#0072B2"))+theme_bw()

box_plot

# Verificando a normalidade -------------------------------------------------------------------
#'
#'Hipóteses
#'H0: a variável dependente segue distribuição normal;
#'Ha: a variável dependente NÃO segue distribuição normal;
#'
#'Teste Shapiro-Wilk
#'
shapiro.test(dados$y)

#'
#'Devemos prosseguir com ANOVA?
#'
#'Agora vamos dar uma olhada no gráfico quantil-quantil normal (QQ)
#'
ggplot(dados, aes(sample = y)) +
  stat_qq() +               # Add QQ plot
  stat_qq_line(color = "magenta", size = 1.5) +  # Add QQ line
  theme_minimal() + xlab("Quantis da distribuição 
       normal")+ylab("Sample Quantiles")+
  ggtitle("Normal Q-Q Plot") # Add title


#'
#'OU usando R base
#'
qqnorm(dados$y, pch = 1, frame = FALSE)
qqline(dados$y, col = "magenta", lwd = 2)
#'
#'Conclusão: Os dados não seguem uma distribuição normal, então podemos usar métodos não 
#'paramétricos ou buscar uma transformação para normalizar os dados.
#'
# Outro conjunto de dados ---------------------------------------------------------------------
#'
#'Formato .xlsx
dados2 <- read_excel("dados/dados_kozak2017.xlsx", sheet = 2)
head(dados2)

#'
#'Possível croqui desse experimento
#'
DIC.croqui(trat=3, rep=4, semente=1)




#'Teste Shapiro-Wilk
#'
shapiro.test(dados2$y)
#'
#'Os dados são normais?
#'
#'OU usando R base
#'
qqnorm(dados2$y, pch = 1, frame = FALSE)
qqline(dados2$y, col = "magenta", lwd = 2)
#'
#'
# Fazendo da maneira correta EX1: -----------------------------------------------------------------
#'
#'Hipóteses
#'H0: os resíduos seguem distribuição normal;
#'Ha: os resíduos seguem NÃO segue distribuição normal;
#'
#'###########################################################################
#'Modelo de análise de variância (ANOVA)
#'###########################################################################
#'
#'Usando lm()
#'
dados<-transform(dados, trat=as.factor(trat))
str(dados)

mod1<-lm(y ~ trat, dados)
anova(mod1)

par(mfrow = c(2, 2))
plot(mod1)
par(mfrow = c(1, 1))

#'
#'Obtenção dos resíduos
#' 
#'Resíduos simples
res <- residuals(mod1)
pred<-predict(mod1)
res<-dados$y-pred
res
#'
#'Resíduos estudentizados
res_Stud <- rstandard(mod1)
shapiro.test(res_Stud)

#'
#'Verificação da normalidade dos resíduos
qqnorm(res_Stud, xlab="Quantis da distribuição 
       normal", ylab="Resíduos Studentizados")
qqline(res_Stud, col=2)


# Verificando homogeneidade de variâncias
ggplot(dados, aes(x = trat, y = res_Stud)) +
  geom_dotplot(binaxis = "y", stackdir = "center", fill = "steelblue") +
  labs(x = "Variedade", y = "Resíduos Studentizados") +
  theme_minimal()

# Teste de Levene para verificação da homogeneidade de variâncias
anova(lm(abs(res) ~ trat, dados))

#'Ou

#library(lawstat)
levene.test(dados$y, dados$trat, location = "mean")

# Fazendo da maneira correta EX2: -----------------------------------------------------------------

#'Hipóteses
#'H0: os resíduos seguem distribuição normal;
#'Ha: os resíduos seguem NÃO segue distribuição normal;
#'
#'###########################################################################
#'Modelo de análise de variância (ANOVA)
#'###########################################################################
#'
dados2<-transform(dados2, trat=as.factor(trat))
str(dados2)

#'Usando lm()
mod2<-lm(y ~ trat, dados2)
anova(mod2)

par(mfrow = c(2, 2))
plot(mod2)
par(mfrow = c(1, 1))

#'Obtenção dos resíduos
#' 
#'Resíduos simples
res <- residuals(mod2)


#'Resíduos estudentizados
res_Stud <- rstandard(mod2)
shapiro.test(res_Stud)

#'Verificação da normalidade dos resíduos
qqnorm(res_Stud, xlab="Quantis da distribuição 
       normal", ylab="Resíduos Studentizados")
qqline(res_Stud, col=2)


# Verificando homogeneidade de variâncias
ggplot(dados2, aes(x = trat, y = res_Stud)) +
  geom_dotplot(binaxis = "y", stackdir = "center", fill = "steelblue") +
  labs(x = "Variedade", y = "Resíduos Studentizados") +
  theme_minimal()

# Teste de Levene para verificação da homogeneidade de variâncias
anova(lm(abs(res) ~ trat, dados2))

#'Ou

bartlett.test(y ~ trat, dados2)
levene.test(dados2$y, dados2$trat, location = "mean")


# Bloco ---------------------------------------------------------------------------------------
#'Formato .xlsx
dados3 <- read_excel("dados/dados_kozak2017.xlsx", sheet = 3)
head(dados3)
#' 
dados3<-transform(dados3, trat=as.factor(trat), bloco=as.factor(bloco))
str(dados3)

#'Usando lm()
mod3<-lm(y ~ trat+bloco, dados3)
anova(mod3)


par(mfrow = c(2, 2))
plot(mod3)
par(mfrow = c(1, 1))

#'Obtenção dos resíduos
#' 
#'Resíduos simples
res <- residuals(mod3)


#'Resíduos estudentizados
res_Stud <- rstandard(mod3)
shapiro.test(res_Stud)

#'Verificação da normalidade dos resíduos
qqnorm(res_Stud, xlab="Quantis da distribuição 
       normal", ylab="Resíduos Studentizados")
qqline(res_Stud, col=2)


# Verificando homogeneidade de variâncias
ggplot(dados3, aes(x = trat, y = res_Stud)) +
  geom_dotplot(binaxis = "y", stackdir = "center", fill = "steelblue") +
  labs(x = "Variedade", y = "Resíduos Studentizados") +
  theme_minimal()

# Teste de Levene para verificação da homogeneidade de variâncias
#' Ou
#library(lmtest)
bptest(mod3)


#'Teste de homogeneidade de variâncias de Oneill-Methews (2002)
#library(ExpDes.pt)
with(dados3, oneilldbc(y, trat, bloco))



