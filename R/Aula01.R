#'##############################################################################
#'###### Análise de dados com o R (ADR) ########################################
#'###### Deoclecio Jardim Amorim  ##############################################
#'###### Eduardo Mariano          ##############################################
#'##############################################################################

#Link do curso
"https://uspdigital.usp.br/apolo/apoObterCurso?cod_curso=640400020&cod_edicao=24001&numseqofeedi=1"

#'##Conteúdo
#'
#'-Instalação do R e RStudio;
#'-Operações básicas; Instalação de pacotes;
#'-Importação de dados;
#'-Manipulação de dados e estatística descritiva.
#'

#'
# Principais operações aritméticas e lógicas com o R --------------------------------------
#'
#' Operador | Descrição
#' ---------|----------
#' +        | adição
#' -        | subtração
#' *        | multiplicação
#' /        | divisão
#' :        | sequência
#' ^        | exponecial
#' <        | menor que
#' <=       | menor ou igual
#' >        | maior que
#' >=       | maior ou igual
#' ==       | igual
#' !=       | diferente
#' !        | não
#' \|       | ou
#' &        | e
#' %in%     | pertence
#'
## Exemplos
#'
#Soma
2 + 2

#Subrtração
2 - 2

#Divisão
2 / 2

#Operador lógico
#Maior
3 > 4

#Menor
3 < 4

#igualdade
3 == 4

# Funções especiais ---------------------------------------------------------------------------

#Número pi= 3,14...
pi

#função sin()
sin(pi)

#função cos()
cos(pi)

#função log() e exp()

log(0)

log(1)

exp(1)

log(exp(1))

#log na base 10
log10(100)

# Preciso de socorro --------------------------------------------------------------------------

? log()


# Objetos -------------------------------------------------------------------------------------

#'Tipos comuns
#'
#'-Vetores
#'
a <- c(2, 4, 6)
b <- c(3, 5, 7)

#Posição do elemento
a[3]

#Soma dos elementos
sum(a)

#Contagem de elementos de um vetor
length(a)
#'
#'-Matrizes
#'Transformando os vetores em uma matriz por coluna
c <- cbind(a, b)
c
# Transformando os vetores em uma matriz por linha
d <- t(c)
d

# Transformando os vetores em uma matriz por linha com função rbind()
d <- rbind(a, b)
d
class(d)


#'-dataframes
# vetores
(fisiologia <- c("c3", "c4", "c3", "c4"))
(especie <- rep(c("soja", "milho"), times = 2))
(d13c <- c(-24,-13,-26,-11))
# criar um dataframe com esses vetores
(dados <- data.frame(fisiologia, especie , d13c))

#'
#'Formato tibble
#'
library(tibble)
dados <- as.tibble(dados)
head(dados)
str(dados)

#'-listas
#'
lista <- list(vetor_a = a,
              matrix_d = d,
              df = dados)
lista

#'
#'Exemplo prático com a função anova()
mod1 <- lm(d13c ~ fisiologia, dados)
anova(mod1)

ff <- anova(mod1)
fcal <- ff$`Mean Sq` / 2
fcal
#'
#'
#'
#'-funções
#Na unha
media <- function(x) {
  soma <- sum(x)
  n <- length(x)
  xbarra <- soma / n
  return(xbarra)
}

media(a)
mean(a)

# Instalando pacotes -------------------------------------------------------------------
#'
#'O que é um pacote R?
#'
#'Um pacote agrupa código, dados, documentação e testes e é fácil
#'de compartilhar com outras pessoas.
#'
#'
#'Refêrencia básica
#'Livro: https://r-pkgs.org/
#'
#install.packages("readxl", "tidyverse", dep=TRUE)
#Tools -> Install packages

#Auto instalação
if (!require(readxl))install.packages("readxl", dep = TRUE)

#Carregar pacote
library(tidyverse)
require(readxl)

# Importação e exportação de dados -----------------------------------------------------
#'
#'Formas de importação
#'
#'1) File -> importe dataset
#'
#'2) Via comando
#'
#'Formato .csv
#'
dados_csv <-
  read.table("dados/dados_kozak2017.csv",
             header = T,
             sep = ",")
head(dados_csv)
View(dados_csv)
print(dados_csv)
dados_csv

#'
#'
#'Formato .xlsx
dados_xlsx <- read_excel("dados/dados_kozak2017.xlsx", sheet = 1)
head(dados_xlsx)
#'

#'Exportar dados no formato .xlsx
#'
library(writexl)
write_xlsx(dados_xlsx, "dados/dados_xlsx.xlsx")

#Exportar dados no formato .csv
write.table(dados, file = "dados/dados_kozak2017.csv", sep = ",")

#Agora seu R tem muita coisa que tal fazer uma limpeza
rm(list = ls(all = T))#Clear memory

# Manipulação de dados e estatística descritiva -----------------------------------------------

dados_xlsx <- read_excel("dados/dados_kozak2017.xlsx", sheet = 1)
head(dados_xlsx)

#Filtragem por tratamento
dados_mani <- filter(dados_xlsx, trat == "A")
view(dados_mani)

#Excluindo o tratamento A
dados_mani_1 <- filter(dados_xlsx, trat != "A")
view(dados_mani_1)

#Excluindo o tratamento A
dados_mani_2 <- filter(dados_xlsx, trat == "B" | trat == "C")
view(dados_mani_2)

#Transformando dados mutate()
dados_mani_3 <- mutate(dados_xlsx, logy = log(y), raiz_y = sqrt(y))
view(dados_mani_3)

#Função select()
dados_mani_4 <- select(dados_mani_3, trat, logy, raiz_y)
view(dados_mani_4)

#Função rename()
dados_mani_5 <- rename(dados_mani_3, outronome = raiz_y)
view(dados_mani_5)

#Combinando operações com pipe (%>%)
dados_mani_6 <- dados_mani_3 %>%
  group_by(trat) %>% summarise(
    mediaY = mean(y, na.rm = TRUE),
    SDY = sd(y, na.rm = TRUE),
    medialogY = mean(logy, na.rm = TRUE),
    SDlogY = sd(logy, na.rm = TRUE)
  )
view(dados_mani_6)

#Exercicio 1: calcular média e desvio da variável raiz_y
#Exercicio 2: calcular erro padrão da média de todas as variáveis




