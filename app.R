#
# Dashboard for "Temperatura superficial (LST) - Corredores biológicos"
#


# PACKAGES
library(dplyr)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(shiny)
library(shinythemes)
library(shinycssloaders)


# CONSTANTS

# Data sources
WMS_CATIE_URL <- "https://catie.info/geoserver/atlasverde/wms"
WMS_LST_MEAN_LAYER <- "temperatura-superficial-promedio"
WMS_LST_MAX_LAYER <- "temperatura-superficial-maxima"

DSN_CORRIDORS <- "data/corredores.geojson"
COLUMN_CORRIDOR_NAME <- "nombre_cb"

# Corridors
corridors <- st_read(dsn = DSN_CORRIDORS, quiet = TRUE)

# Indicators
INDICATOR_LST_MEAN <- "LST promedio"
INDICATOR_LST_MAX <- "LST máxima"


# FUNCTIONS

# Create map
create_map <-
  function() {
    leaflet() |>
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Mapa de calles (OpenStreetMap)") |>
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Mapa oscuro (CartoDB Dark Matter)") |>
      addProviderTiles(providers$Stamen.TonerLite, group = "Mapa claro (Stamen Toner Lite)") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Imágenes satelitales (ESRI World Imagery)") |>
      addWMSTiles(
        WMS_CATIE_URL,
        layers = WMS_LST_MEAN_LAYER,
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          crs = "EPSG:4326"
        ),
        group = INDICATOR_LST_MEAN
      ) |>
      addWMSLegend(
        layerId = "legend_lst_mean",
        position = "topright",
        uri = paste0(
          WMS_CATIE_URL, "?",
          "REQUEST=GetLegendGraphic&VERSION=1.0.0", 
          "&FORMAT=image/png&WIDTH=20&HEIGHT=15&LAYER=temperatura-superficial-promedio"
        )
      ) |>
      addWMSTiles(
        WMS_CATIE_URL,
        layers = WMS_LST_MAX_LAYER,
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          crs = "EPSG:4326"
        ),
        group = INDICATOR_LST_MAX
      ) |>
      addWMSLegend(
        layerId = "legend_lst_max",
        position = "topright",
        uri = paste0(
          WMS_CATIE_URL, "?",
          "REQUEST=GetLegendGraphic&VERSION=1.0.0", 
          "&FORMAT=image/png&WIDTH=20&HEIGHT=15&LAYER=temperatura-superficial-maxima"
        )
      ) |>      
      addPolygons(
        data = corridors,
        layerId = ~nombre_cb,
        fillOpacity = 0.0,
        stroke = TRUE,
        color = "black",
        weight = 3,
        popup = paste(
          paste("<strong>Corredor biológico:</strong>",  corridors[[COLUMN_CORRIDOR_NAME]])
        ),
        label = paste(
          paste("Corredor biológico:",  corridors[[COLUMN_CORRIDOR_NAME]])
        ),
        group = "Corredores biológicos"
      ) |>      
      addLayersControl(
        baseGroups = c(
          "Mapa de calles (OpenStreetMap)",
          "Mapa oscuro (CartoDB Dark Matter)",
          "Mapa claro (Stamen Toner Lite)",
          "Imágenes satelitales (ESRI World Imagery)"
        ),
        overlayGroups = c(
          INDICATOR_LST_MEAN,
          INDICATOR_LST_MAX,
          "Corredores biológicos"
        ),
        position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE)
      ) |>      
      addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE)) |>
      addMouseCoordinates() |>
      addSearchOSM() |>
      addResetMapButton() |>
      addFullscreenControl() |>
      hideGroup(INDICATOR_LST_MAX)
  }


# USER INTERFACE
ui <- fluidPage(
  theme = "bootstrap",
  tags$head(
    tags$style(
      HTML(
        '
        .texto_agradecimiento_logos_1 {
          text-align: center;
        }        
        .texto_agradecimiento_logos_2 {
          text-align: center;
        }'
      )
    )
  ),
  
  navbarPage(
    title = tags$span(
      tags$a(href = "http://atlas-verde.org/", target = "_blank", "Atlas de servicios ecosistémicos de la GAM"),
      " - ",
      "Temperatura superficial (LST) - Corredores biológicos"
    ),
    theme = shinytheme("lumen"),

    fluidRow(withSpinner(leafletOutput("map_lst"))),
    fluidRow(h1(column(width = 12))),
    fluidRow(
      column(width = 4,
        wellPanel(
          id = "wellPanelLSTMeanMin",
          style = "background-color: #d9edf7; border-color: #bce8f1;",
          h2(strong("Valor mínimo de LST promedio"), style = "margin-top: 0;"),
          h2(strong("12.5 °C"))
        )
      ),
      column(width = 4,
        wellPanel(
          id = "wellPanelLSTMeanAvg",
          style = "background-color: #d9edf7; border-color: #bce8f1;",
          h2(strong("Valor promedio de LST promedio"), style = "margin-top: 0;"),
          h2(strong("29.7 °C"))
        )
      ),
      column(width = 4,
        wellPanel(
          id = "wellPanelLSTMeanMax",
          style = "background-color: #d9edf7; border-color: #bce8f1;",
          h2(strong("Valor máximo de LST promedio"), style = "margin-top: 0;"),
          h2(strong("48.9 °C"))
        )
      )
    ),
    fluidRow(
      column(width = 4,
        wellPanel(
          id = "wellPanelLSTMaxMin",
          style = "background-color: #d9edf7; border-color: #bce8f1;",
          h2(strong("Valor mínimo de LST máxima"), style = "margin-top: 0;"),
          h2(strong("16.0 °C"))
        )
      ),
      column(width = 4,
        wellPanel(
          id = "wellPanelLSTMaxAvg",
          style = "background-color: #d9edf7; border-color: #bce8f1;",
          h2(strong("Valor promedio de LST máxima"), style = "margin-top: 0;"),
          h2(strong("35.4 °C"))
        )
      ),
      column(width = 4,
        wellPanel(
          id = "wellPanelLSTMaxMax",
          style = "background-color: #d9edf7; border-color: #bce8f1;",
          h2(strong("Valor máximo de LST máxima"), style = "margin-top: 0;"),
          h2(strong("58.1 °C"))
        )
      )
    ),    

    
  ),
  
  
  fluidRow(h1(column(width = 12))),
  fluidRow(h1(column(width = 12))),  
  h4(class = "texto_agradecimiento_logos_1", strong("Acerca del Atlas de Servicios Ecosistémicos de la GAM")),
  h4(class = "texto_agradecimiento_logos-2", "El Atlas de Servicios Ecosistémicos de la GAM es producto de la cooperación entre los Gobiernos de Alemania y Costa Rica en el marco del proyecto Biodiver_City – Establecimiento de Corredores Biológicos Interurbanos con el fin de promover el desarrollo urbano centrado en los beneficios de la naturaleza. El instrumento fue desarrollado por el CATIE, por encargo de la Cooperación alemana para el desarrollo GIZ, bajo una estrecha articulación con el MINAE, CENIGA, SINAC y con el apoyo técnico del Instituto de Estudios Ambientales Helmholtz, UFZ."),
  fluidRow(h1(column(width = 12))),
  fluidRow(
    column(width = 4, img(src = "logo-gcr20222026.png", height = 90)),
    column(width = 4, img(src = "logo-minae.png", height = 90)),
    column(width = 4, img(src = "logo-sinac.jpg", height = 90)),
    class = "text-center"
  ),
  fluidRow(h1(column(width = 12))),
  fluidRow(
    column(width = 4, img(src = "logo-catie.jpeg", height = 90)),
    column(width = 4, img(src = "logo-giz.png", height = 90)),
    column(
      width = 4,
      img(src = "logo-minambientealemania-iki.png", height = 90)
    ),
    class = "text-center"
  ),
  fluidRow(h1(column(width = 12))),
  fluidRow(h1(column(width = 12)))  
)


# SERVER LOGIC
server <- function(input, output, session) {
  # Map
  output$map_lst <- renderLeaflet({
    create_map()
  })
  
  # Initial "selected" corridor
  selected_corridor <- reactiveVal("Montes del Aguacate")
  
  # Capture click event in corridors layer for zooming and changing styles
  observeEvent(input$map_lst_shape_click, {
    click_data <- input$map_lst_shape_click
    
    if (!is.null(click_data)) {
      selected_corridor(click_data$id)
      
      # Zoom to selected polygon
      selected_corridor_polygon <- corridors |> filter(nombre_cb == click_data$id)
      leafletProxy("map_lst") |>
        fitBounds(
          lng1 = min(st_bbox(selected_corridor_polygon)[["xmin"]]),
          lat1 = min(st_bbox(selected_corridor_polygon)[["ymin"]]),
          lng2 = max(st_bbox(selected_corridor_polygon)[["xmax"]]),
          lat2 = max(st_bbox(selected_corridor_polygon)[["ymax"]])
        )      
      
    }
  })
  
  # Visibility of wellPanels for LST Mean
  # observe({
  #   layers <- input$map_lst_layers
  #   print(layers)
  #   if (INDICATOR_LST_MEAN %in% layers) {
  #     shinyjs::show("wellPanelLSTMeanMin")
  #     shinyjs::show("wellPanelLSTMeanAvg")
  #     shinyjs::show("wellPanelLSTMeanMax")
  #   } else {
  #     shinyjs::hide("wellPanelLSTMeanMin")
  #     shinyjs::hide("wellPanelLSTMeanAvg")
  #     shinyjs::hide("wellPanelLSTMeanMax")
  #   }
  # })

}


# RUN APPLICATION
shinyApp(ui = ui, server = server)