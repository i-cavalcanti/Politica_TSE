#install.packages(c("geobr", "sf", "ggplot2", "dplyr"))

library(data.table)
library(geobr)
library(ggplot2)
library(dplyr)
library(sf)
library(shiny)
library(leaflet)

load("./report/votes_on_winner.rds")

muni_sf <- read_municipality(year = 2020)
muni_dt <- muni_sf %>%
   select(code_muni, name_muni,  geom)

dic_ibge <- read.csv("./data/municipios_brasileiros_tse.csv")
dic_ibge <- dic_ibge %>%
   select(codigo_tse, codigo_ibge)
setnames(combined, "CD_MUNICIPIO", "codigo_tse")
combined_ <- merge(combined, dic_ibge, by = "codigo_tse", all.x = TRUE)
setnames(combined_, "codigo_ibge", "code_muni")
map_data <- muni_sf %>%
  right_join(combined_, by = "code_muni")
#Alguem dropou


ggplot(map_data) +
  geom_sf(aes(fill = PROP_VOTOS_VENCEDOR_STD_DEPUTADO_FEDERAL), color = NA) +
  facet_wrap(~ANO) +
  scale_fill_viridis_c(option = "C") +
  theme_minimal() +
  labs(title = "Variable by Municipality", fill = "PROP_VOTOS_VENCEDOR_STD_PRESIDENTE")





munis <- read_municipality(code_muni = "all", year = 2020)

# Simulate a dataframe: variable by municipality and year
set.seed(42)
years <- c(2022, 2018, 2014, 2010, 2006, 2002, 1998, 1994)
my_data <- expand.grid(code_muni = unique(munis$code_muni), year = years)
my_data$my_variable <- runif(nrow(my_data), 0, 100)


ui <- fluidPage(
  titlePanel("Interactive Map of Brazil - Municipalities"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Select Year:", min = min(years), max = max(years), value = max(years), step = 2, sep = "")
    ),
    mainPanel(
      leafletOutput("map", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    # Join the shapefile with data for selected year
    year_data <- map_data %>% filter(year == input$year)
    muni_year <- munis %>%
      left_join(year_data, by = "code_muni")
    muni_year
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -55, lat = -14, zoom = 4)
  })
  
  observe({
    leafletProxy("map", data = filtered_data()) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~colorNumeric("YlOrRd", PROP_VOTOS_VENCEDOR_STD_GOVERNADOR)(PROP_VOTOS_VENCEDOR_STD_GOVERNADOR),
        weight = 0.5,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        label = ~paste0("Code: ", code_muni, "<br>Value: ", round(PROP_VOTOS_VENCEDOR_STD_GOVERNADOR, 4)),
        highlightOptions = highlightOptions(weight = 1, color = "#666", fillOpacity = 0.9, bringToFront = TRUE)
      )
  })
}

shinyApp(ui, server)
