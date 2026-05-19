library(dplyr)
library(highcharter)
library(bslib)

url = "https://datos.salud.gob.ar/dataset/ceaa8e87-297e-4348-84b8-5c643e172500/resource/30d76bcb-b8eb-4bf3-863e-c87d41724647/download/informacion-publica-dengue-zika-nacional-anio-2022.csv"

data = read.csv2(url, encoding = "latin1")

data_orig = data %>% 
  group_by(provincia_id, semanas_epidemiologicas, evento_nombre, grupo_edad_desc) %>%
  summarise(cantidad_casos = sum(cantidad_casos))

categorias = list(
  provincia_id = seq(2, 94, 4),
  semanas_epidemiologicas = 1:52,
  evento_nombre = unique(data$evento_nombre),
  grupo_edad_desc = c(unique(data$grupo_edad_desc), "Neonato (0 hasta 28 dias)")
)

data_final = expand.grid(categorias)

data_final = data_final %>% left_join(data_orig)

data_final$cantidad_casos[is.na(data_final$cantidad_casos)] = 0

library(shiny)

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  fluidRow(
    h1("Ejemplo expand.grid"),
    align = "center"
  ),
  hr(),
  fluidRow(
    column(
      12,
      align = "center")
    ),
  br(),
  fluidRow(
    column(4,
           selectInput(
            inputId = "provId",
            label = "Seleccionar jurisdicción:",
            choices = unique(data_final$provincia_id),
            multiple = F,
            selected = unique(data_final$provincia_id)[1]
           ),
           selectInput(
             inputId = "edadId",
             label = "Seleccionar grupo de edad:",
             choices = unique(data_final$grupo_edad_desc),
             selected = unique(data_final$grupo_edad_desc)[1]
           )),
    column(8,
           h2("Con datos originales"),
           highchartOutput("datosOriginales"),
           align = "center"
           )
  ),
  fluidRow(
    column(
      4
    ),
    column(8,
           h2("Con datos expandidos"),
           highchartOutput("datosExpandidos"),
           align = "center"
           )
    
  ),
  fluidRow(column(9),
           column(3,
                  downloadButton(
                    outputId = "descargar",
                    label = "Descargar"
                  ),
                  align = "right")),
  br(),
  hr(),
  br()
)

server <- function(input, output, session) {
  bs_themer()
  
  
  output$datosOriginales = renderHighchart({
    
    dataGrafico = data_orig %>% dplyr::filter(provincia_id == input$provId & grupo_edad_desc == input$edadId)
    
    highchart() %>% hc_chart(
      type = "column"
    ) %>%
      hc_xAxis(categories = dataGrafico$semanas_epidemiologicas) %>%
      hc_add_series(name = "Casos", dataGrafico$cantidad_casos)
  })
  
  output$datosExpandidos = renderHighchart({
    
    dataGrafico = data_final %>% dplyr::filter(provincia_id == input$provId & grupo_edad_desc == input$edadId)
    
    highchart() %>% hc_chart(
      type = "column"
    ) %>%
      hc_xAxis(categories = dataGrafico$semanas_epidemiologicas) %>%
      hc_add_series(name = "Casos", dataGrafico$cantidad_casos)
  })
  
  output$descargar <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      dataGrafico = data_final %>% dplyr::filter(provincia_id == input$provId & grupo_edad_desc == input$edadId)
      write.csv(dataGrafico, file)
    }
  )
  
}

shinyApp(ui, server)
