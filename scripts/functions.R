library(data.table)

#Inputs: DS_CARGO, QT_VOTOS_NOMINAIS_VALIDOS, CD_MUNICIPIO, NR_TURNO, SQ_CANDIDATO, SG_UF

run_votes_on_winners_1 <- function(e){

    df_list <- list()
    for (n in seq_along(e)) {
        c <- e[n]
        cat("Running:", c, "\n")
        
        # Filter for current cargo and make a copy
        p <- a[grepl(c, DS_CARGO, ignore.case = TRUE)]
        # Compute candidate votes per municipality
        p[, QT_VOTOS_NOMINAIS_VALIDOS_CANDIDATO_MUNICIPIO := 
            sum(QT_VOTOS_NOMINAIS_VALIDOS), by = .(CD_MUNICIPIO, NR_TURNO, SQ_CANDIDATO)]

        # Compute total valid votes per municipality
        p[, QT_VOTOS_NOMINAIS_VALIDOS_MUNICIPIO := 
            sum(QT_VOTOS_NOMINAIS_VALIDOS), by = .(CD_MUNICIPIO, NR_TURNO)]

        # Proportion of candidate votes in municipality
        p[, PROP_VOTOS_NOMINAIS_VALIDOS_CANDIDATO_MUNICIPIO := 
            QT_VOTOS_NOMINAIS_VALIDOS_CANDIDATO_MUNICIPIO / QT_VOTOS_NOMINAIS_VALIDOS_MUNICIPIO]

        # Total votes for candidate across municipalities per round
        p[, QT_VOTOS_NOMINAIS_CANDIDATO := 
            sum(QT_VOTOS_NOMINAIS_VALIDOS), by = .(SQ_CANDIDATO, NR_TURNO)]

        # Total votes in the round by state
        p[, QT_VOTOS_NOMINAIS_TURNO := 
            sum(QT_VOTOS_NOMINAIS_VALIDOS), by = .(NR_TURNO, SG_UF)]

        # Proportion of candidate's votes in the round
        p[, PROP_VOTOS_NOMINAIS_VALIDOS_CANDIDATO := 
            QT_VOTOS_NOMINAIS_CANDIDATO / QT_VOTOS_NOMINAIS_TURNO]

        # Filter elected candidates only
        p_1 <- p[
  (
    grepl("ELEITO", stri_trans_general(DS_SIT_TOT_TURNO, "Latin-ASCII"), ignore.case = TRUE) |
    grepl("MEDIA",  stri_trans_general(DS_SIT_TOT_TURNO, "Latin-ASCII"), ignore.case = TRUE)
  ) &
  !grepl("NAO", stri_trans_general(DS_SIT_TOT_TURNO, "Latin-ASCII"), ignore.case = TRUE)
]
        print(p_1[, unique(DS_SIT_TOT_TURNO)])  
        # Select relevant columns and drop duplicates
        p_2 <- unique(p_1[, .(CD_MUNICIPIO, 
                                PROP_VOTOS_NOMINAIS_VALIDOS_CANDIDATO_MUNICIPIO,
                                PROP_VOTOS_NOMINAIS_VALIDOS_CANDIDATO)])

        # Summarise by municipality
        p_2 <- p_2[, .(
            PROP_VOTOS_NOMINAIS_VALIDOS_CANDIDATO_MUNICIPIO = sum(PROP_VOTOS_NOMINAIS_VALIDOS_CANDIDATO_MUNICIPIO, na.rm = TRUE),
            PROP_VOTOS_NOMINAIS_VALIDOS_CANDIDATO = sum(PROP_VOTOS_NOMINAIS_VALIDOS_CANDIDATO, na.rm = TRUE)
        ), by = CD_MUNICIPIO]

        # Compute the standardized proportion
        p_2[, PROP_VOTOS_VENCEDOR_STD_ := 
                PROP_VOTOS_NOMINAIS_VALIDOS_CANDIDATO_MUNICIPIO / PROP_VOTOS_NOMINAIS_VALIDOS_CANDIDATO]

        p_2 <- p_2[, .(CD_MUNICIPIO, PROP_VOTOS_VENCEDOR_STD_)]

        new_colname <- paste0("PROP_VOTOS_VENCEDOR_STD_", gsub(" ", "_", toupper(c)))
        setnames(p_2, old = "PROP_VOTOS_VENCEDOR_STD_", new = new_colname)
        
        df_list[[length(df_list) + 1]] <- p_2
        }

    return(df_list)
}

run_votes_on_winners_2 <- function(e){

    df_list <- list()
    for (n in seq_along(e)) {
        c <- e[n]
        cat("Running:", c, "\n")
        
        # Filter for current cargo and make a copy
        p <- a[grepl(c, DS_CARGO, ignore.case = TRUE)]
        # Compute candidate votes per municipality
        p[, QT_VOTOS_NOMINAIS_VALIDOS_CANDIDATO_MUNICIPIO := 
            sum(QT_VOTOS_NOMINAIS), by = .(CD_MUNICIPIO, NR_TURNO, SQ_CANDIDATO)]

        # Compute total valid votes per municipality
        p[, QT_VOTOS_NOMINAIS_VALIDOS_MUNICIPIO := 
            sum(QT_VOTOS_NOMINAIS), by = .(CD_MUNICIPIO, NR_TURNO)]

        # Proportion of candidate votes in municipality
        p[, PROP_VOTOS_NOMINAIS_VALIDOS_CANDIDATO_MUNICIPIO := 
            QT_VOTOS_NOMINAIS_VALIDOS_CANDIDATO_MUNICIPIO / QT_VOTOS_NOMINAIS_VALIDOS_MUNICIPIO]

        # Total votes for candidate across municipalities per round
        p[, QT_VOTOS_NOMINAIS_CANDIDATO := 
            sum(QT_VOTOS_NOMINAIS), by = .(SQ_CANDIDATO, NR_TURNO)]

        # Total votes in the round by state
        p[, QT_VOTOS_NOMINAIS_TURNO := 
            sum(QT_VOTOS_NOMINAIS), by = .(NR_TURNO, SG_UF)]

        # Proportion of candidate's votes in the round
        p[, PROP_VOTOS_NOMINAIS_VALIDOS_CANDIDATO := 
            QT_VOTOS_NOMINAIS_CANDIDATO / QT_VOTOS_NOMINAIS_TURNO]

        # Filter elected candidates only
        p_1 <- p[
  (
    grepl("ELEITO", stri_trans_general(DS_SIT_TOT_TURNO, "Latin-ASCII"), ignore.case = TRUE) |
    grepl("MEDIA",  stri_trans_general(DS_SIT_TOT_TURNO, "Latin-ASCII"), ignore.case = TRUE)
  ) &
  !grepl("NAO", stri_trans_general(DS_SIT_TOT_TURNO, "Latin-ASCII"), ignore.case = TRUE)
]
        print(p_1[, unique(DS_SIT_TOT_TURNO)])    
        # Select relevant columns and drop duplicates
        p_2 <- unique(p_1[, .(CD_MUNICIPIO, 
                                PROP_VOTOS_NOMINAIS_VALIDOS_CANDIDATO_MUNICIPIO,
                                PROP_VOTOS_NOMINAIS_VALIDOS_CANDIDATO)])

        # Summarise by municipality
        p_2 <- p_2[, .(
            PROP_VOTOS_NOMINAIS_VALIDOS_CANDIDATO_MUNICIPIO = sum(PROP_VOTOS_NOMINAIS_VALIDOS_CANDIDATO_MUNICIPIO, na.rm = TRUE),
            PROP_VOTOS_NOMINAIS_VALIDOS_CANDIDATO = sum(PROP_VOTOS_NOMINAIS_VALIDOS_CANDIDATO, na.rm = TRUE)
        ), by = CD_MUNICIPIO]

        # Compute the standardized proportion
        p_2[, PROP_VOTOS_VENCEDOR_STD_ := 
                PROP_VOTOS_NOMINAIS_VALIDOS_CANDIDATO_MUNICIPIO / PROP_VOTOS_NOMINAIS_VALIDOS_CANDIDATO]

        p_2 <- p_2[, .(CD_MUNICIPIO, PROP_VOTOS_VENCEDOR_STD_)]

        new_colname <- paste0("PROP_VOTOS_VENCEDOR_STD_", gsub(" ", "_", toupper(c)))
        setnames(p_2, old = "PROP_VOTOS_VENCEDOR_STD_", new = new_colname)
        
        df_list[[length(df_list) + 1]] <- p_2
        }

    return(df_list)
}