
library(shiny)
library(leaflet)
library(sf)
library(rnaturalearth)

west_africa <- subset(ne_countries(scale = "medium", continent = "Africa"), subregion == "Western Africa")
base <- read.csv("ACLED-Western_Africa.csv")

ui <- fluidPage(
  
## titre de l'application
  titlePanel("shiny map"),
## baRRE 0 DEROULER
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "pays",
        label = "Sélectionnez un pays",
        choices = c(unique(base$pays)),
        selected = c(unique(base$pays))[sample(1:length(unique(base$pays)), 1)],
        multiple = TRUE
      ),
      selectInput(
        inputId = "evenement",
        label = "Sélectionnez un événement",
        choices = c(unique(base$type)),
        selected = "Protests",
        multiple = TRUE
      ),
      selectInput(
        inputId = "annee",
        label = "Sélectionnez une année",
        choices = c(unique(base$annee)),
        selected = "2023",
        multiple = TRUE
      )
    ),

    ## emplacement    
    mainPanel(
      textOutput(outputId = "test"),
      ## taille de l'emplacement
      leafletOutput(outputId = "map", width = "100%", height = "720px") 
    )
  )
)

server <- function(input, output, session) {
  ## render pour utiliser l'emplacement reservé
  output$test<-renderText("Cette application a pour but de donnner les évenement par pays
    cliquer sur un point pour avoir l'évènement")
  base_filtre <- reactive({       ## CREATIon de base_filtre pour rendre l'application plus rapide 
    base %>%
      dplyr::filter(pays %in% input$pays) %>%  
      dplyr::filter(type %in% input$evenement) %>%
      dplyr::filter(annee %in% input$annee)
  })
  
 couleurs <- colorFactor(c("blue", "red","green","yellow","black","pink"), domain = base$type)## couleur pour la légende
  
 ## creation de la carte proprement dit
 output$map <- renderLeaflet({ 
    leaflet() %>%
      setView(lng = -10, lat = 10, zoom = 5) %>%  ## parametrer l'affichage
      addPolygons(
        data = west_africa,
        fillColor = "#00238723",
        color = "black",
        weight = 2,
        fillOpacity = 0.3
      ) %>% 
      addCircleMarkers(data = base_filtre(),color = ~couleurs(type),
                       lng = ~longitude,
                       lat = ~latitude,
                       layerId = NULL,
                       radius = 5,
                       weight = 2,
                       opacity = 0.2,
                       fill = TRUE,
                       fillOpacity = 0.2,
                       popup = ~type,  ##message au survolage
                       label = ~annee)%>% 
     ## insererer la carte globale
      addTiles() %>% 
      addLegend(position = "topright", 
                pal = couleurs, 
                values =base$type , title = " type d'évènement")
  })
}

shinyApp(ui = ui, server = server)
