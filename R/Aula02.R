###############################################################################
####### Análise de dados com o R (ADR) ########################################
####### Deoclecio Jardim Amorim  ##############################################
####### Eduardo Mariano          ##############################################
###############################################################################

#Link do curso
"https://uspdigital.usp.br/apolo/apoObterCurso?cod_curso=640400020&cod_edicao=24001&numseqofeedi=1"

###Conteúdo
#'
#'-Introdução à visualização de dados com o ggplot2: histograma, dispersão e boxplot
#'-Análise de variância (ANOVA) e pressuposições


# Pacotes -------------------------------------------------------------------------------------
library(dae)

# DataViz -------------------------------------------------------------------------------------



# Desgin de experimentos ----------------------------------------------------------------------

############################################################################
########### Delineamento inteiramente casualizado ##########################
############################################################################

#Trat
Trat <- gl(3,1,labels = c("T1", "T2", "T3")) 
Trat
#Repetições
Rep <-  4
Rep

#Croqui
DIC <- function(Trat,Rep,semente){
  Trat <- rep(Trat,Rep)
  N <- length(levels(Trat))*Rep # Número total de parcelas
  set.seed(semente)
  Plan<-as.data.frame(
    matrix(
      sample(Trat,N), ncol = length(levels(Trat)), nrow=Rep
    )
  )
  colnames(Plan)<-paste("Coluna", c(seq(1:length(levels(Trat)))))
  rownames(Plan)<-paste("Linha", c(seq(1:Rep)))
  return(Plan)
}

DIC(Trat, Rep, 12)



#Forma mais elegante

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

DIC.croqui(2,2,13)




# ANOVA e pressuposições ----------------------------------------------------------------------




