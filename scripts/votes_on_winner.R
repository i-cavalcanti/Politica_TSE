library(data.table) 
library(stringi)


source("./scripts/functions.R") 

merged_list <- list()
years <- c(2022, 2018, 2014, 2010, 2006, 2002, 1998)
groups <- c("Presidente", "Deputado Federal", "Governador", "Senador", "Deputado Estadual")
for (year in years) {

    print(paste0("Running year: ", year))
    #Open file
    filename <- paste0("D:/Bases/TSE/Resultados Eleitorais (1994 - 2024)/votacao_candidato_munzona_",year,"_BRASIL.csv")
    a <- fread(filename, encoding = "Latin-1")
    #Check columns
    cols <- c("DS_CARGO", "QT_VOTOS_NOMINAIS_VALIDOS", "CD_MUNICIPIO", "NR_TURNO", "SQ_CANDIDATO", "SG_UF")
    if (any(!cols %in% colnames(a))) {
        if (cols[!cols %in% colnames(a)]=="QT_VOTOS_NOMINAIS_VALIDOS") {
            setnames(a, old = "QT_VOTOS_NOMINAIS", new = "QT_VOTOS_NOMINAIS_VALIDOS")
        } else {cat("Error: Variables missing \n")}
        } 
    #Run
    df_list <- list()
    for (group in groups){
        if(group == "Presidente"){table <- run_votes_on_winners_presid(group)}
        else{table <- run_votes_on_winners(group)}
        df_list[[length(df_list) + 1]] <- table
        print(paste0(group," OK."))
    }
    #Merge
    merged <- Reduce(function(x, y) merge(x, y, by = "CD_MUNICIPIO", all = TRUE), df_list)
    merged[, ANO := year]
    merged_list[[length(merged_list) + 1]] <- merged
}
combined <- rbindlist(merged_list)
save(combined, file = paste0("./report/votes_on_winner.rds"))

#combined_ <- rbindlist(merged_list)
#save(combined_, file = paste0("./report/votes_on_winner_.rds"))

load("./report/votes_on_winner.rds")

rm(list = ls())

# TODO  - Build descriptive statistics to test data in different time periods.
# TODO  - Understand why votes_on_winner is not running anymore.
# TODO - Test updates on checking available categories as elect.
# TODO - Test if the MEDIA should be considered elect or not elect.

# TODO - Test if the standardized values for president is the same as the non standardized values.
# TODO - Add president values.

# TODO - run for the other indicators 

# TODO - Dropar regioes extras para presidente
# TODO - Add AMC in the place of municipalities



# TODO - Diversificacao
# TODO - Votos no vencedor para vereador

# TODO - Construir a estrutura para serie temporal 



# CANDIDATOS/VAGA
# CANDIDATOS/HABITANTE
#FIDELIDADE PARTIDÁRIA
# CABRESTO
