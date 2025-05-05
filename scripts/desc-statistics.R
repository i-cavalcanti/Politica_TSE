library(data.table)

load("./report/votes_on_winner.rds")

#MEDIA is a variable 
cols <- c("PROP_VOTOS_VENCEDOR_STD_PRESIDENTE", "PROP_VOTOS_VENCEDOR_STD_DEPUTADO_FEDERAL", "PROP_VOTOS_VENCEDOR_STD_GOVERNADOR",
"PROP_VOTOS_VENCEDOR_STD_SENADOR", "PROP_VOTOS_VENCEDOR_STD_DEPUTADO_ESTADUAL")

#combined <- combined_com_media
stats_com_media <- combined[, c(
  lapply(.SD, function(x) sum(!is.na(x))),
  lapply(.SD, mean, na.rm = TRUE),
  lapply(.SD, min, na.rm = TRUE),
  lapply(.SD, max, na.rm = TRUE)
), by = ANO, .SDcols = cols]



#Adicionar a MEDIA muda pouco os dados, escolho entao adicionar a media 
