#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
library(shiny)
library(sf)
library(DT)
library(dygraphs)
library(xts)
library(leaflet)
library(readr)
library(ggplot2)
library(dplyr)


datos_csv <- read.csv("data/inafed_bd_1701145840.csv")
datos_shp <- sf::read_sf("data/Municipios/01_32_mun.shp") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')


datos_shp$cve_inegi <- as.numeric(datos_shp$CVEGEO)

# Unir los datos del shapefile con el CSV
datos_mapa <- merge(datos_shp, datos_csv, by = "cve_inegi")


ui <- fluidPage(
  
  
  titlePanel("Proyecto Final para Estadistica Computacional"),
  
  tabsetPanel(               
    tabPanel("Mapa de la educación en México",
             sidebarLayout(
               
               sidebarPanel(
                 selectInput("variable", "Selecciona la Variable:", 
                             choices = c("Pob_Indigena",	"grado_escolar",	"grado_escolar_h",	"grado_escolar_m",	"Entre_3_5_En_escuela",	"Entre_3_5_En_escuela_Hombres",	"Entre_3_5_En_escuela_Mujeres",	"Mas_15_con_Sec",	"Mas15H",	"Mas15M",	"PEA", "PEA_H",	"PEA_M"))
               ),
               mainPanel(width = 9,
                         h4("La educación en México desempeña un papel crucial en el desarrollo tanto individual como colectivo de la nación. Representa la base sobre la cual se construyen las capacidades y habilidades de la población, determinando en gran medida el progreso económico, social y cultural del país. A través de un sistema educativo inclusivo y de calidad, se fomenta la igualdad de oportunidades, permitiendo que individuos de diversos orígenes sociales y culturales puedan alcanzar su máximo potencial. En este ejercicio buscamos mostrar la disparidad entre municipios. El norte con un nivel de educación proporcionalmente más alto y el sur con un rezago historico."),
                         plotOutput("map"),
                         h4("En este mapa podemos observar que para ciertas variables gráficadas como Población Indigena y Población economicamente activa por género. Además tenemos esta tabla para observar todas las observaciones de cada una de las variables."),
                         DTOutput(outputId = "table")
               )
             )
          ),
    tabPanel("Modelo lineal", 
             sidebarLayout(
               sidebarPanel(
                 helpText("Si tu % de población es:"),
                 # Asumiendo que tus datos tienen una columna 'x' y una 'y'
                 numericInput("xvalue", "Inserte porcentaje de población indigena:", 1),
                 actionButton("goButton", "Calcular")
               ),
               mainPanel(
                 verbatimTextOutput("regOutput")
               )
             )
    ),
    tabPanel("Gráficas de barras", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("variableNumerica", 
                             "Elige la Variable Numérica:", 
                             choices = c("Pob_Indigena",	"grado_escolar",	"grado_escolar_h",	"grado_escolar_m",	"Entre_3_5_En_escuela",	"Entre_3_5_En_escuela_Hombres",	"Entre_3_5_En_escuela_Mujeres",	"Mas_15_con_Sec",	"Mas15H",	"Mas15M",	"PEA", "PEA_H",	"PEA_M")), # Reemplaza con tus variables numéricas reales
                 selectInput("variableCategorica", 
                             "Elige la Variable Categórica:", 
                             choices = c("estado", "Mayoria_indigena"))  # Reemplaza con tus variables categóricas reales
               ),
               mainPanel(
                 plotOutput("barPlot")
               )
             )
    ),
    
  )
)

# server()
server <- function(input, output) {
  output$table <- renderDT(datos_csv)
  
  output$map <- renderPlot({
    # Filtra para asegurar que la variable seleccionada existe
    if (input$variable %in% c("Pob_Indigena",	"grado_escolar",	"grado_escolar_h",	"grado_escolar_m",	"Entre_3_5_En_escuela",	"Entre_3_5_En_escuela_Hombres",	"Entre_3_5_En_escuela_Mujeres",	"Mas_15_con_Sec",	"Mas15H",	"Mas15M",	"PEA", "PEA_H",	"PEA_M")) {
      # Preparar los datos para ggplot
      datos_filtrados <- datos_mapa %>%
        select(geometry, input$variable) %>%
        st_transform(crs = 4326)  # Transformar a coordenadas lat/lon si es necesario
      
      # Crear el mapa
      ggplot(datos_filtrados) +
        geom_sf(aes(fill = !!sym(input$variable)), lwd = 0.2) +
        theme_minimal()+
        theme(axis.text.x = element_text(),
              axis.ticks.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank())+
        scale_fill_continuous(
          low = "#EFF7FF",
          high = "#002C53",
          guide = "colorbar",
          labels = scales::label_number(big.mark = " ")
        )
    }
  })
  output$barPlot <- renderPlot({
    datos <- read.csv("data/inafed_bd_1701145840.csv")
    datos$estado <- as.factor(datos$estado)
    datos$Mayoria_indigena <- as.factor(datos$Mayoria_indigena)
    
    # Asegúrate de que las variables seleccionadas existan en tus datos
    variable_numerica <- datos[[input$variableNumerica]]
    variable_categorica <- datos[[input$variableCategorica]]
    
    promedios <- aggregate(variable_numerica ~ variable_categorica, data = datos, FUN = mean)
    
    ggplot(promedios, aes_string(x = names(promedios)[1], y = names(promedios)[2])) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(y = "Promedio", x = input$variableCategorica)+
      theme(axis.text.x = element_text(angle = -90))
  })
  
  # Aquí debes cargar tus datos. Por ejemplo:
  datos <- read.csv("data/inafed_bd_1701145840.csv")
  valor_a_multiplicar <- -0.02719683  # Define el valor con el que quieres multiplicar
  modelo <- lm(grado_escolar ~ Pob_Indigena, data = datos)
  coeficientes <- coef(modelo)
  
  regResult <- eventReactive(input$goButton, {
     #a<- (input$xvalue * valor_a_multiplicar) + 7.12797458 
     #"Dada la tasa de población que ingresaste, el número de años promedio de tu municipio será" a
     mensaje <- paste("El número de años de eduación promedio de tu municipio será:", (input$xvalue * valor_a_multiplicar) + 7.12797458)
     mensaje
  })
  
  
  output$regOutput <- renderText({
    regResult()
  })
}

shinyApp(ui = ui, server = server)