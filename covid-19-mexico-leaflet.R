library(shiny)
library(readr)
library(leaflet)

data_mapa <- read_rds("para_mapa.rds")

ui <- fluidPage(
  includeCSS("styles.css"),
  titlePanel("Casos de COVID-19 en México"),
  sidebarPanel(
    selectInput(
      inputId = "tipo", "Resultado del diagnóstico",
      c("Positivo COVID-19" = "Positivo SARS-CoV-2",
        "Negativo COVID-19" = "No positivo SARS-CoV-2",
        "Resultado pendiente" =  "Resultado pendiente",
        "Todos los análisis"  = "Todos los análisis")
    ),
    selectInput(
      inputId = "relacion", "Tipo de conteo",
      c("Total" = "N",
        "Por 1,000 habitantes"  = "PROP")
    ),
    
    selectInput(
      inputId = "entidad", "Filtrar por entidad",
      c("Todas", unique(data_mapa$NOM_ENT))
    )
    
  ),
  mainPanel(
    leafletOutput("out_mapa")
  ),
  br(),
  div(class = "footer",
  p("Fuente:",
    tags$a(href = "https://www.gob.mx/salud/documentos/datos-abiertos-152127", 
           "Secretaría de Salud. Dirección General de Epidemiología")
  ),
  p("Código en Github:",
    tags$a(href = "https://github.com/jboscomendoza/covid-19-mexico-leaflet", 
           "jboscomendoza/covid-19-mexico-leaflet"))
  )
)


server <- function(input, output) {
  
  mapa <- renderLeaflet({
    valor_label <-  input$relacion
    valor_radius <- paste0("ESCALA_", input$relacion)
    
    para_mapa <- data_mapa[data_mapa[["VARIABLE"]] == input$tipo, ]
    if(input$entidad != "Todas") {
      para_mapa <- para_mapa[para_mapa[["NOM_ENT"]] == input$entidad, ]
    }
    
    leaflet(para_mapa) %>% 
      addProviderTiles(providers$Stamen.TonerLite) %>% 
      addCircleMarkers(
        lat = ~LAT_DECIMAL,
        lng = ~LON_DECIMAL,
        label = paste0(para_mapa$NOM_MUN, ": ", para_mapa[[valor_label]]),
        radius = para_mapa[[valor_radius]],
        color = "black", weight = 1,
        fillColor = "#ee4481", fillOpacity = 0.8, 
        labelOptions = labelOptions(noHide = FALSE, direction = "top")
      )
  })
  
  output$out_mapa = (mapa)
}


shinyApp(ui, server)
