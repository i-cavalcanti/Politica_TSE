#install.packages(c("geobr", "sf", "ggplot2", "dplyr"))

library(data.table)
library(geobr)
library(ggplot2)
library(dplyr)
library(sf)


muni_sf <- read_municipality(year = 2020)
muni_dt <- as.data.table(muni_sf)
muni_dt <- muni_dt %>%
    select(code_muni, name_muni,  geom)
setnames(combined, "CD_MUNICIPIO", "code_muni")
combined$code_muni <- sprintf("11%05d", as.integer(combined$code_muni))
combined$code_muni <- as.numeric(combined$code_muni)
map_data <- muni_dt %>%
  left_join(combined, by = "code_muni")
merged_dt <- merge(combined, muni_dt, by = "code_muni", all.x = TRUE)
#setnames(merged_dt, "geom", "geometry")

ggplot(map_data) +
  geom_sf(aes(fill = PROP_VOTOS_VENCEDOR_STD_DEPUTADO_FEDERAL), color = NA) +
  facet_wrap(~ANO) +
  scale_fill_viridis_c(option = "C") +
  theme_minimal() +
  labs(title = "Variable by Municipality", fill = "PROP_VOTOS_VENCEDOR_STD_DEPUTADO_FEDERAL")
