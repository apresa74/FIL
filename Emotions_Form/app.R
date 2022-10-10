
library(shiny)
library(googlesheets4)
library(gargle)
library(tidyverse)
library(shinyjs)
library(shinyWidgets)


options(gargle_oauth_cache = "secrets",
        gargle_oauth_email = "manuel.cebral@tec.mx")

gs4_auth()

mx_time <- Sys.time()-18000


Libros <- read_sheet("1BrkZeStaYAuidjxQZO2LhnJ2a36n3USksJYiUBtXe8g",
                    sheet = "Poniatowska_Libros") %>% select(Titulo)

ui <- fluidPage(
  useShinyjs(),
  title = "Emotions Form",
  setBackgroundColor("#161c20"),
  #h1(id="big-heading", "Las emociones en Elena Poniatowska"),
  #h5(id="subtitle", "Un proyecto de Humanidades digitales para la FILMTY 2022"),
  tags$head(
    tags$style(
      HTML('
      .well {
      color: white;
      margin-top: 10px;
      }
      .col-sm-8 {
      color: white;
      width: 100%;
      }
      #footer {
      color: #ffffff;
      }
      .btn-default {
      color: #333;
      background-color: #eeff41;
      border-color: #ccc;
      }
    .btn.disabled, .btn[disabled], fieldset[disabled] .btn {
    background-color: lightgray;
    }
    #my_textinput{
    color: #eeff41;
    }
    #texto {
    font-size: 40px;
    }
     '
      ))),
#  column(6, offset = 3, id="intro",
 #        HTML("Esta es una App en la que vinculamos los libros de Elena Poniatowska
#                  con emociones y palabras que te inspiren. 
#              Selecciona un libro y escoge una emoción que relaciones con él.")),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      column(6, offset = 3,
        wellPanel(
          style = "background: #161c20",
          div(id='my_textinput' ,
              selectInput("libros",
                        "Selecciona un libro de Elena Poniatowska*:",
                        choices = Libros$Titulo,
                        selectize = FALSE),
            selectInput("emociones",
                        "Vincula una emoción*:",
                        choices = c("-Selecciona una opción-", "Alegría", "Tristeza", "Confianza", "Miedo", "Sorpresa",
                                    "Expectativa", "Asco"),
                        selectize = FALSE)),
          HTML("Además puedes proponer <b>hasta 3 palabras</b> que te hayan inspirado su lectura.<br><br>"),
          div(id='my_textinput' ,
              textInput("p1", "Palabra 1*:", placeholder = "-Propón al menos una palabra-")),
            textInput("p2", "Palabra 2:", placeholder = "-Opcional-"),
            textInput("p3", "Palabra 3:", placeholder = "-Opcional-"),
            actionButton("send", "Envía tus datos")
        )),

        # Show a plot of the generated distribution
      column(6, offset = 3,
             mainPanel(
           #plotOutput("distPlot"),
           textOutput("texto")
           )
           #,
    #  column(12, id="footer",
    #         HTML("<br>Las emociones en Elena Poniatowska es un proyecto diseñado por el grupo de Humanidades digitales del <a href='http://sitios.itesm.mx/ehe/'>Tecnologico de Monterrey</a>.
    #          Ha sido desarollado por <a href='https://github.com/mancebral/FIL'>Cebral-Loureda en R programming</a>
    #        con licencia <a rel='license' href='http://creativecommons.org/licenses/by-nc/4.0/'>Creative Commons Attribution-NonCommercial 4.0 International License</a>.<br>
    #        ")),
    #  column(12, id="footer_right",
    #         HTML("<br><a rel='license' href='http://creativecommons.org/licenses/by-nc/4.0/'><img alt='Creative Commons License' style='border-width:0' src='https://i.creativecommons.org/l/by-nc/4.0/88x31.png' /></a>
    #          <br><br>"))
      #,
      #column(6, id="footer_right",
      #       HTML("<a href='http://sitios.itesm.mx/ehe/'><img style='float: right;' src='https://raw.githubusercontent.com/mancebral/FIL/main/ehe_transpararent_1_50.png'/></a>
      #        <br><br>"))
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  useShinyjs()
  
  observeEvent(input$send, {
    tibble (input$libros, input$emociones, c(input$p1, input$p2, input$p3), mx_time) %>%
      sheet_append(., ss="1BrkZeStaYAuidjxQZO2LhnJ2a36n3USksJYiUBtXe8g",
                   sheet = "data")
    })
  
  observeEvent(input$send, {
    output$texto <- renderText({
      "😀 Gracias por participar!"
    })
  })
  
  disable("send")
  
  observe({
    shinyjs::toggleState("send", !((input$libros=="-Selecciona una opción-")|
                                        (input$emociones=="-Selecciona una opción-")|
                                        (input$p1=="")))
  })
  
    
}
    
# Run the application 
shinyApp(ui = ui, server = server)
