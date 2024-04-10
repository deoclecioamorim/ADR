
#'
#'Extra 1: manipulação

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
