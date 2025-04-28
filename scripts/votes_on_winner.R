library(data.table) 
library(stringi)


source("./scripts/functions.R") 
e <- c("Deputado Federal", "Governador", "Senador", "Deputado Estadual")

merged_list <- list()
years <- c(2022, 2018, 2014, 2010, 2006, 2002, 1998, 1994)
for (year in years) {

    print(paste0("Running year: ", year))
    #Open file
    filename <- paste0("D:/Bases/TSE/Resultados Eleitorais (1994 - 2024)/votacao_candidato_munzona_",year,"_BRASIL.csv")
    a <- fread(filename, encoding = "Latin-1")
    print(a[, unique(DS_SIT_TOT_TURNO)])
    #Check columns
    cols <- c("DS_CARGO", "QT_VOTOS_NOMINAIS_VALIDOS", "CD_MUNICIPIO", "NR_TURNO", "SQ_CANDIDATO", "SG_UF")
    
    if (any(!cols %in% colnames(a))) {
        
        if (cols[!cols %in% colnames(a)]=="QT_VOTOS_NOMINAIS_VALIDOS") {
            df_list <- run_votes_on_winners_2(e)
        } else {cat("Error: Other variable missing \n")}
        
        } else {
        df_list <- run_votes_on_winners_1(e)
        }

    merged <- Reduce(function(x, y) merge(x, y, by = "CD_MUNICIPIO", all = TRUE), df_list)
    merged[, ANO := year]
    merged_list[[length(merged_list) + 1]] <- merged
}
combined_sem_media <- rbindlist(merged_list)
save(combined_sem_media, file = paste0("./report/votes_on_winner_.rds"))
save(combined, file = paste0("./report/votes_on_winner.rds"))

load("./report/votes_on_winner.rds")

rm(list = ls())

# TODO  - Build descriptive statistics to test data in different time periods.
# TODO  - Understand why votes_on_winner is not running anymore.
# TODO - Test updates on checking available categories as elect.
# TODO - Test if the MEDIA should be considered elect or not elect.
# TODO - Test if the standardized values for president is the same as the non standardized values.