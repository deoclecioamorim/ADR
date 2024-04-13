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
options(prompt = "R", continue = "+  ", width = 70, useFancyQuotes = FALSE)
options(OutDec=".")#Separador decimal, útil para gráficos
rm(list=ls(all=T))#Limpar memoria
