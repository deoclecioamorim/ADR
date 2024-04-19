#'##############################################################################
#'###### Análise de dados com o R (ADR) ########################################
#'###### Deoclecio Jardim Amorim  ##############################################
#'###### Eduardo Mariano          ##############################################
#'##############################################################################

#Link do curso
"https://uspdigital.usp.br/apolo/apoObterCurso?cod_curso=640400020&cod_edicao=24001&numseqofeedi=1"


#'##Conteúdo
#'
#'-Análises de correlação (Pearson e Spearman); 
#'-Análise de agrupamentos; Mapas de calor (heatmaps); 
#'-Componentes Principais.
#'
# Opções de controle -----------------------------------------------------------------------------
options(OutDec=".")#Separador decimal, útil para gráficos
options(scipen = 100) #Elinando notação cientifica
rm(list=ls(all=T))#Limpar memoria

# Pacotes ---------------------------------------------------------------------------------------
if(!require(readxl))install.packages("readxl", dep = TRUE)
library(readxl)
if(!require(tidyverse))install.packages("tidyverse", dep = TRUE)
library(tidyverse)
if(!require(ggcorrplot)) install.packages("ggcorrplot", dep = TRUE)
library(ggcorrplot)
if(!require(factoextra)) install.packages("factoextra", dep = TRUE) 
library(factoextra)
if(!require(FactoMineR)) install.packages("FactoMineR", dep = TRUE) 
library(FactoMineR)
if(!require(Hmisc)) install.packages("Hmisc", dep = TRUE) 
library(Hmisc)
if(!require(dendextend)) install.packages("dendextend", dep = TRUE) 
library(dendextend)
if(!require(openxlsx)) install.packages("openxlsx", dep = TRUE)
library(openxlsx)
#'
#'Pacote do Bioconductor
#https://www.bioconductor.org/
if(!require(BiocManager)) install.packages("BiocManager", dep = TRUE)
#BiocManager::install("ComplexHeatmap")

# Exemplo 1: Falsa causalidade ----------------------------------------------------------------
#'
#'Falsa causalidade: conforme o número de casais cegonha aumentou
#'entre os anos de 1930 e 1936 a população aumentou!
#'
ano<-c(1930, 1931, 1932, 1933, 1934, 1935, 1936)
casaiscegonhas<-c(132, 142, 166, 188, 240, 250, 252)
habitantes<-c(55400, 58400, 65000, 67700, 68900, 72300, 76000)
cegonhasbebes<-data.frame(ano, casaiscegonhas, habitantes)
View(cegonhasbebes)

ggplot(data = cegonhasbebes,
       mapping = aes(x = casaiscegonhas, y = habitantes)) +
  geom_point()+
  stat_smooth(method = lm, se = F)+theme_bw()

#'Correlação de Pearson
corrR <- round(cor(cegonhasbebes[-1], 
                   use = "pairwise.complete.obs", 
                   method = "pearson"), 2)
view(corrR)

# Exemplo 2: dados pardocas ----------------------------------------------------------
#'
#'Esse cojunto de dados refere-se a pardais sobreviventes de tempestade em 
#'em janeiro de 1898. A principais perguntas a serem respondidas com esse
#'conjunto de dados são:
#'
#'(1)- Como estão relacionadas variáveis? 
#'

# Importando dados ----------------------------------------------------------------------------
#'
#'##########################################################################
# CHEGOU A HORA DE COLOCAR A MÃO NA MASSA:                                 #
# (1) - Carregue o cojunto de dados pardocas.csv;                          #
# (2) - Guarde os dados no objeto -> pardocas;                             #
#'#########################################################################
#'COMECE A PROGRAMAR!
#'



# Análise de correlação -----------------------------------------------------------------------
pardocas_numeric <- pardocas[,2:6]
str(pardocas_numeric) #verificando se a seleção funcionou
#'
#'Correlação de Pearson
corrR <- round(cor(pardocas_numeric, 
            use = "pairwise.complete.obs", 
            method = "pearson"), 2)
view(corrR)

#'
#'Correlação de Pearson
#'Sumário da análise de correlação (valor r, nº de observações e p-valor)
corrP <- Hmisc::rcorr(as.matrix(pardocas_numeric), 
                      type = c("pearson"))
view(corrP)

#'
#'Correlação de Spearman
#'Sumário da análise de correlação (valor r, nº de observações e p-valor)
corrSp <- Hmisc::rcorr(as.matrix(pardocas_numeric), 
                      type = c("spearman"))
view(corrSp)


#Computando os p-valores da correlação de Spearman
p.mat <- cor_pmat(pardocas_numeric)
p.mat

#Gráfico das correlações
ggcorrplot::ggcorrplot(corrR, 
                       p.mat = p.mat,
                       hc.order = TRUE, 
                       type = "lower", 
                       lab = TRUE, 
                       lab_size = 3,
                       method="square", 
                       colors = c("tomato2", "white", "springgreen3"),
                       ggtheme=theme_bw)

# Análise de agrupamentos e mapa de calor ----------------------------------------------------------
#'
#'Neste exemplo vamos considerar o experimento realizado por Santos et al. 2022.
#'http://dx.doi.org/10.1016/j.scitotenv.2021.152204.
#'
#'Objetivo dessa análise construir cluster e mapa de calor para entender para 
#'entender a atividade enzimática em plantas de cártamo submetidas a estresse
#'hidríco (-10kpa e -70kpa) e diferentes doses de glifosato.
#'

#library(openxlsx)
#install.packages("BiocManager")
#BiocManager::install("ComplexHeatmap")
dadosheatmap <- read.xlsx("dados/heatmap.xlsx", rowNames = T)
View(dadosheatmap)

#'
#'##########################################################################
# Passos para análise de agrupamento                                      #
#'#########################################################################
#Passo 1: Padroniza dados 
#Z=(x-xbarra)/desvio

heatmap_padronizado<-scale(dadosheatmap)
summary(heatmap_padronizado)

#'
#'Passo 2: usa um método que calcular distância
#'
dista<-dist(heatmap_padronizado, method = "euclidean")
#'
#'Passo 3: O próximo passo é efetuar a criação dos
#'clusters com base na similaridade
#' 
dista.hc <- hclust(d=dista, method="complete")
factoextra::fviz_dend(dista.hc, cex=0.5)

dend_dose <- stats::hclust(dist(heatmap_padronizado)) # dendograma_dose
factoextra::fviz_dend(dend_dose, cex=0.5)

dend_qui <- hclust(dist(t(heatmap_padronizado))) #dendograma compostos químicos
factoextra::fviz_dend(dend_qui, cex=0.5)

#'
#'Combinando as informações dos dendogramas e mapa de 
#'calor.
Heatmap(heatmap_padronizado, name = "Atividade",
        row_title = "Baixas doses/Regimes hídricos",
        column_title = "Compostos bioquímicos",
        row_names_gp = gpar(fontsize =7),
        column_names_gp = gpar(fontsize = 7),
        column_names_rot = 45,
        row_title_gp = gpar(fontsize = 10),
        column_title_gp = gpar(fontsize = 10),
        show_heatmap_legend = FALSE,
        cluster_rows = color_branches(dend_dose, k = 3),
        cluster_columns = color_branches(dend_qui, k = 2))

#'
#'Os gradientes de cor vermelho e azul indicam maior e menor 
#'atividade, respectivamente.
#'
# Análise de Componentes Principais -----------------------------------------------------------
#'
#'Cálculos da PCA feito na mão podem ser amendrotadores, mas aqui temos R para 
#'nos ajudar!
#'
#'##########################################################################
# PCA alguns detalhes:                                                     #
# (1) - Existência de correlação para rodar a PCA;                         #
# (2) - Primeira componente captura a maior parte da variabilidade;        #
# (3) - Pode ser executada usando a matriz de covariâncias e correlações;  #
# (4) - Uma análise de PCA é composta por autovalores e autovetores;       #
# (5) - Uma boa PCA deve explicar pelos 50% nas duas primeiras componentes;#
# (6) - Trabalhar com no máximo dois no gráfico biplot.                    #
#'#########################################################################
#'
# Retomando o exemplo 2: dados pardocas ----------------------------------------------------------
#'
#'Esse cojunto de dados refere-se a pardais sobreviventes de tempestade em 
#'em janeiro de 1898. A principais perguntas a serem respondidas com esse
#'conjunto de dados são:
#'
#'(1)- Como estão relacionadas variáveis? (Já respondida previamente)
#'(2)- Por que a metade dos passáros sobreviveu?


# Importando dados ----------------------------------------------------------------------------
pardocas <-read.table("dados/pardocas.csv",header = T, sep = ",")
head(pardocas)
str(pardocas)

#'
# Matrizes de variâncias e correlações --------------------------------------------------------
#'
#'Para calcular a matriz de variâncias e covariâncias precisamos selecionar
#'as variáveis númericas!
#'
pardocas_numeric <- pardocas[,2:6]
str(pardocas_numeric) #verificando se a seleção funcinou

#Matriz de covariâncias
matcov <- cov(pardocas_numeric)
matcov

#' # PCA na unha -----------------------------------------------------
#' pardocas_pca <- stats::prcomp(pardocas_numeric, scale=T)
#' summary(pardocas_pca)
#' #'
#' #'Decomposição do summary da prcomp()
#' #'Os autovalores são as variâncias dos componentes principais
#' (autovalores_pardocas <- pardocas_pca$sdev^2)     # autovalores sdev^2
#' names(autovalores_pardocas) <- paste("PC", 1:5,sep="")
#' autovalores_pardocas
#' #'
#' #'Variância total é soma dos autovalores
#' #'
#' (somaautovalores <- sum(autovalores_pardocas))
#' #'
#' #'Proporção da variância
#' (propvar <- (autovalores_pardocas/somaautovalores)*100)
#' #'
#' #'Proporção da variância acumulada 
#' (cumvar_pardocas <- cumsum(propvar))
#' #'
#' #'Obtendo os autovetores
#' #'
#' (autovetores<-pardocas_pca$rotation)
#' #OU
#' print(pardocas_pca)

# PCA com a matriz de variâncias e correlações:com a biblioteca FactoMinerR -----------------------
res.pca <- FactoMineR::PCA(pardocas_numeric, scale.unit = T, graph = FALSE)
print(res.pca)
res.pca$eig #Autovalores
res.pca$svd$V #Autovetores
#'
#'Construindo os componentes principais
#'
#'Indice de tamanho dos pardocas
#PC1 = 0.4517989X1 + 0.4616809X2 + 0.4505416X3 + 0.4707389X4 + 0.3976754X5
#PC2 = 0.05072137X1 - 0.29956355X2 -0.32457242X3 -0.18468403X4 + 0.87648935X5 
##Scree plot
factoextra::fviz_eig(res.pca, addlabels = T)


factoextra::fviz_pca_biplot(
  res.pca,
  col.var = "black",
  alpha.var = 0.6,
  geom.ind = "point",
  #label = "all",
  habillage = as.factor(pardocas$Survival),
  repel = TRUE,
  palette = "Dark2",
  addEllipses = TRUE,
  labelsize = 4,
  gradient.cols = c("blue","green","red"),
  title = ""
) 

#'
#'
#'
# Exemplo 3: dados de ácidos graxos (PLFA) de microrganismos ---------------------------------------------
# Importando dados ----------------------------------------------------------------------------
plfa <- read_excel("dados/plfa.xlsx")
View(plfa)
str(plfa)

#'
#'Transformando em fator trat
#'
plfa <- transform(plfa, Trat=as.factor(Trat))
str(plfa)

#'
#'Verificando as correlações
#'
#Matriz de correlações

matcor_plfa<-cor(plfa[3:9], method = "pearson")
matcor_plfa

#Computando os p-valores da correlação de Spearman
p.mat_plfa <- ggcorrplot::cor_pmat(matcor_plfa)
p.mat_plfa

#Gráfico das correlações
ggcorrplot::ggcorrplot(matcor_plfa, 
                       p.mat = p.mat_plfa,
                       hc.order = TRUE, 
                       type = "lower", 
                       lab = TRUE, 
                       lab_size = 3,
                       method="square", 
                       colors = c("tomato2", "white", "springgreen3"),
                       ggtheme=theme_bw)

#'Criando a PCA
#'
pca_plfa <- FactoMineR::PCA(plfa[3:9], scale.unit = TRUE, graph = FALSE) #scale.unit = TRUE indica o uso da matriz de correlação
print(pca_plfa)
pca_plfa$eig #Autovalores
pca_plfa$svd$V #Autovetores

#'Analisando a contribuição dos componentes principais
#'
factoextra::fviz_eig(pca_plfa, addlabels = TRUE)

#'
#'Avaliando a contribuição das variáveis em cada componente
#'
factoextra::fviz_contrib(pca_plfa, choice = "var", axes = 1, top = 7)
factoextra::fviz_contrib(pca_plfa, choice = "var", axes = 2, top = 7)
#'
#'
factoextra::fviz_pca_biplot(pca_plfa,
                            col.var = "black",
                            alpha.var = 0.6,
                            geom.ind = "point",
                            habillage = plfa$Trat,
                            repel = TRUE,
                            palette = "Dark2",
                            addEllipses = TRUE,
                            labelsize = 4,
                            gradient.cols = c("blue","green","red"),
                            title = "ADR") + labs(x = "PC1 (75%)", y = "PC2 (15%)")
  




