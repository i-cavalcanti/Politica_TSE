library(data.table)

load("./report/votes_on_winner.rds")

#combined <- combined_
cols <- as.list(names(combined))
cols <- cols[-c(1, length(cols))]
cols <- c("PROP_VOTOS_VENCEDOR_STD_DEPUTADO_FEDERAL", "PROP_VOTOS_VENCEDOR_STD_GOVERNADOR",
"PROP_VOTOS_VENCEDOR_STD_SENADOR", "PROP_VOTOS_VENCEDOR_STD_DEPUTADO_ESTADUAL")

# Build summaries dynamically
stats <- combined[, c(
  lapply(.SD, function(x) sum(!is.na(x))),
  lapply(.SD, mean, na.rm = TRUE),
  lapply(.SD, min, na.rm = TRUE),
  lapply(.SD, max, na.rm = TRUE)
), by = ANO, .SDcols = cols]




