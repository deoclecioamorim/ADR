
#'Extra 1: função para cálculo do desvio padrão
#'
dvpad<-function(x){
xbar <-sum(x)/length(x)
sqd <- sum((x - xbar)^2)
var<-sqd/(length(x)-1)
 return(var) 
}

a<-c(4,4,4)
dvpad(a)

#'
#'Extra 2: manipulação

dados_mani_6 <- dados_mani_3 %>%
  group_by(trat) %>% summarise(
    mediaY = mean(y, na.rm = TRUE),
    SDY = sd(y, na.rm = TRUE),
    EPMY=SDY/sqrt(length(y)), #erro padrão da média
    medialogY = mean(logy, na.rm = TRUE),
    SDlogY = sd(logy, na.rm = TRUE),
    EPMlogY=SDlogY/sqrt(length(y)), #erro padrão da média
    mediaraiz_Y = mean(raiz_y, na.rm = TRUE),
    SDraiz_Y = sd(raiz_y, na.rm = TRUE),
    EPMraiz_Y=SDraiz_Y/sqrt(length(y)) #erro padrão da média
    
  )
