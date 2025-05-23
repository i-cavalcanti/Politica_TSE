library(data.table)
library(stringi)

#Inputs: DS_CARGO, QT_VOTOS_NOMINAIS_VALIDOS, CD_MUNICIPIO, NR_TURNO, SQ_CANDIDATO, SG_UF

run_votes_on_winners <- function(group){
    cat("Running:", group, "\n")
    # Filter for current cargo and make a copy
    p <- a[grepl(group, DS_CARGO, ignore.case = TRUE)]
    cat("All available categories: ", p[, unique(DS_SIT_TOT_TURNO)], "\n")
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
    grepl("ELEITO", stri_trans_general(DS_SIT_TOT_TURNO, "Latin-ASCII"), ignore.case = TRUE) 
    | grepl("MEDIA",  stri_trans_general(DS_SIT_TOT_TURNO, "Latin-ASCII"), ignore.case = TRUE)
  ) &
  !grepl("NAO", stri_trans_general(DS_SIT_TOT_TURNO, "Latin-ASCII"), ignore.case = TRUE)
]
    cat("Selected categories as elect: ", p_1[, unique(DS_SIT_TOT_TURNO)], "\n")  
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

    new_colname <- paste0("PROP_VOTOS_VENCEDOR_STD_", gsub(" ", "_", toupper(group)))
    setnames(p_2, old = "PROP_VOTOS_VENCEDOR_STD_", new = new_colname)
    
    return(p_2)
}

get_elect <- function(p){
    total_votes <- p[, .(TOTAL_VOTOS = sum(QT_VOTOS_NOMINAIS_VALIDOS, na.rm = TRUE)), 
                 by = .(SQ_CANDIDATO, NR_TURNO)]
    winners <- total_votes[order(-TOTAL_VOTOS), .SD[1], by = NR_TURNO]
    winners <- merge(winners, unique(p[, .(SQ_CANDIDATO, NM_URNA_CANDIDATO, NR_TURNO)]), 
                 by = c("SQ_CANDIDATO", "NR_TURNO"), all.x = TRUE)

    return(winners[NR_TURNO == max(NR_TURNO), SQ_CANDIDATO])
}

run_votes_on_winners_presid <- function(group){
    cat("Running:", group, "\n")
    # 1. Filter for DS_CARGO == e[1]
    p <- a[DS_CARGO == group]

    # 2. Calculate total valid votes by CD_MUNICIPIO and NR_TURNO
    p[, QT_VOTOS_NOMINAIS_VALIDOS_TOT := sum(QT_VOTOS_NOMINAIS_VALIDOS, na.rm = TRUE), by = .(CD_MUNICIPIO, NR_TURNO)]

    # 3. Create proportion
    p[, PROP_VOTOS_VENCEDOR_STD_ := QT_VOTOS_NOMINAIS_VALIDOS / QT_VOTOS_NOMINAIS_VALIDOS_TOT]

    # 4. Filter only elected candidates
    elect <- get_elect(p)
    p_1 <- p[SQ_CANDIDATO == elect]
    cat("Candidate elect: ", p[SQ_CANDIDATO == elect, unique(NM_URNA_CANDIDATO)], "\n")   

    # 5. Group by CD_MUNICIPIO and sum the proportions
    p_2 <- p_1[, .(PROP_VOTOS_VENCEDOR_STD_ = sum(PROP_VOTOS_VENCEDOR_STD_, na.rm = TRUE)), by = CD_MUNICIPIO]
    new_colname <- paste0("PROP_VOTOS_VENCEDOR_STD_", gsub(" ", "_", toupper(group)))
    setnames(p_2, old = "PROP_VOTOS_VENCEDOR_STD_", new = new_colname)  
    return(p_2)
}

parlament_seat_competition  <- function(group){
    cat("Running:", group, "\n")
    p <- a[grepl(group, DS_CARGO, ignore.case = TRUE)]
    p_1 <- p[, .(NUM_CANDIDATOS = uniqueN(SQ_CANDIDATO)), by = CD_MUNICIPIO]
    p_2 <- parlament_elect_filter(p)
    p_3 <- p_2[, .(NUM_CANDIDATOS = uniqueN(SQ_CANDIDATO)), by = CD_MUNICIPIO]
    table <- merge(p_1, p_3, by = "CD_MUNICIPIO", all = FALSE)
    table[, VEREADORES_POR_VAGA := 
        NUM_CANDIDATOS.x / NUM_CANDIDATOS.y]
    table[, c("NUM_CANDIDATOS.x", "NUM_CANDIDATOS.y") := NULL]
    return(table)
}

parlament_seat_by_hab  <- function(group, year){
    cat("Running:", group, "\n")
    p <- a[grepl(group, DS_CARGO, ignore.case = TRUE)]
    p_1 <- p[, .(NUM_CANDIDATOS = uniqueN(SQ_CANDIDATO)), by = CD_MUNICIPIO]
    pop <- open_pop_data()[ANO_ELEICAO == year]
    table <- merge(p_1, pop, by = "CD_MUNICIPIO", all = FALSE)
    table[, VEREADORES_POR_HABITANTE := 
        NUM_CANDIDATOS / Population]
    table[, c("NUM_CANDIDATOS", "Population", "ANO_ELEICAO") := NULL]
    return(table)
}

parlament_std_dev <- function(group){
    p <- a[grepl(group, DS_CARGO, ignore.case = TRUE)]
    p[, QT_VOTOS_NOMINAIS_VALIDOS_CANDIDATO_MUNICIPIO := 
            sum(QT_VOTOS_NOMINAIS_VALIDOS), by = .(CD_MUNICIPIO, NR_TURNO, SQ_CANDIDATO)]
    p[, QT_VOTOS_NOMINAIS_VALIDOS_MUNICIPIO := 
        sum(QT_VOTOS_NOMINAIS_VALIDOS), by = .(CD_MUNICIPIO, NR_TURNO)]
    p[, PROP_VOTOS_NOMINAIS_VALIDOS_CANDIDATO_MUNICIPIO := 
        QT_VOTOS_NOMINAIS_VALIDOS_CANDIDATO_MUNICIPIO / QT_VOTOS_NOMINAIS_VALIDOS_MUNICIPIO]
    table <- p[, .(
    STD_DEV_PROP_VOTOS_MUNICIPIO = sd(PROP_VOTOS_NOMINAIS_VALIDOS_CANDIDATO_MUNICIPIO, na.rm = TRUE)
    ), by = CD_MUNICIPIO]
    return(table)
}

parlament_votes_on_winners <- function(group){
    cat("Running:", group, "\n")
    # 1. Filter for DS_CARGO == e[1]
    p <- a[DS_CARGO == group]

    # 2. Calculate total valid votes by CD_MUNICIPIO and NR_TURNO
    p[, QT_VOTOS_NOMINAIS_VALIDOS_TOT := sum(QT_VOTOS_NOMINAIS_VALIDOS, na.rm = TRUE), by = .(CD_MUNICIPIO, NR_TURNO)]

    # 3. Create proportion
    p[, PROP_VOTOS_VENCEDOR_STD_ := QT_VOTOS_NOMINAIS_VALIDOS / QT_VOTOS_NOMINAIS_VALIDOS_TOT]

    # 4. Filter only elected candidates
    p_1 <- parlament_elect_filter(p)
    cat("Selected categories as elect: ", p_1[, unique(DS_SIT_TOT_TURNO)], "\n")    

    # 5. Group by CD_MUNICIPIO and sum the proportions
    p_2 <- p_1[, .(PROP_VOTOS_VENCEDOR_STD_ = sum(PROP_VOTOS_VENCEDOR_STD_, na.rm = TRUE)), by = CD_MUNICIPIO]
    new_colname <- paste0("PROP_VOTOS_VENCEDOR_STD_", gsub(" ", "_", toupper(group)))
    setnames(p_2, old = "PROP_VOTOS_VENCEDOR_STD_", new = new_colname)  
    return(p_2)
}

parlament_elect_filter <- function(p){
    table <- p[
    (grepl("ELEITO", stri_trans_general(DS_SIT_TOT_TURNO, "Latin-ASCII"), ignore.case = TRUE) 
        | grepl("MEDIA",  stri_trans_general(DS_SIT_TOT_TURNO, "Latin-ASCII"), ignore.case = TRUE)
    ) &
    !grepl("NAO", stri_trans_general(DS_SIT_TOT_TURNO, "Latin-ASCII"), ignore.case = TRUE)
    ]
    return(table)
}

open_pop_data <- function() {
    
    #pop <- fread("./data/pop.csv")
    pop <- melt(
    fread("./data/pop.csv"),
    id.vars = "CodMunicipal",                  # columns to keep fixed
    measure.vars = c("2000", "2004", "2008", "2012", "2016", "2020", "2024"),  # columns to stack
    variable.name = "ANO_ELEICAO",
    value.name = "Population"
    )
    pop[, ANO_ELEICAO := as.integer(as.character(ANO_ELEICAO))]
    setnames(pop, old = "CodMunicipal", new = "CD_MUNICIPIO")
    return(pop)
}

