
library(shiny)
library(googlesheets4)
library(gargle)
library(tidyverse)
library(shinyjs)
library(shinyWidgets)


options(gargle_oauth_cache = "secrets",
        gargle_oauth_email = "manuel.cebral@tec.mx")

gs4_auth()

Sys.setenv(TZ='America/Mexico_City')

Libros <- read_sheet("1BrkZeStaYAuidjxQZO2LhnJ2a36n3USksJYiUBtXe8g",
                    sheet = "Poniatowska_Libros") %>% select(Titulo)

ui <- fluidPage(
  setBackgroundColor("#34495e"),
  h1(id="big-heading", "Las emociones en Elena Poniatowska"),
  h5(id="subtitle", "Un proyecto de Humanidades digitales para la FILMTY 2022"),
  tags$head(
    tags$style(
      HTML('
      #big-heading{
      color: #F9F871;
      text-align:center;
      font-family: Arial Black;
      }
      #subtitle{
      color: #F9F871;
      text-align:center;
      font-family: Arial MT;
      font-weight: light;
      }
      .well {
      color: white;
      margin-top: 10px;
      }
      .col-sm-8 {
      color: white;
      width: 100%;
      }
       #intro {
       margin-top: 30px;
      color: #F9F871;
       }
      #footer {
      color: #F9F871;
      }
     '
      ))),
  column(4, offset = 4, id="intro",
         HTML("Esta es una App en la que vinculamos los libros de Elena Poniatowska
                  con emociones y palabras que te inspiren. 
              Selecciona un libro y escoge una emoción que relaciones con ellos.")),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      column(4, offset = 4,
        wellPanel(
          style = "background: #34495e",
            selectInput("libros",
                        "Selecciona un libro:",
                        choices = Libros$Titulo,
                        selectize = FALSE),
            selectInput("emociones",
                        "Vincula una emoción:",
                        choices = c("Alegría", "Tristeza", "Confianza", "Miedo", "Sorpresa",
                                    "Expectativa", "Asco"),
                        selectize = FALSE),
            textInput("p1", "Palabra 1:"),
            textInput("p2", "Palabra 2:"),
            textInput("p3", "Palabra 3:"),
            actionButton("send", "Envía tus datos")
        )),

        # Show a plot of the generated distribution
      column(4, offset = 4,
             mainPanel(
           #plotOutput("distPlot"),
           textOutput("texto")
           ),
      column(12, id="footer",
             HTML("<br><br>Las emociones en Elena Poniatowska es un proyecto del Seminario de Humanidades digitales del Tecnologico de Monterrey.
              Ha sido desarollado por <a href='https://github.com/mancebral/Flourishing-Measure'>Cebral-Loureda en R programming</a>
            con licencia <a rel='license' href='http://creativecommons.org/licenses/by-nc/4.0/'>Creative Commons Attribution-NonCommercial 4.0 International License</a>.<br><br>
            <a rel='license' href='http://creativecommons.org/licenses/by-nc/4.0/'><img alt='Creative Commons License' style='border-width:0' src='https://i.creativecommons.org/l/by-nc/4.0/88x31.png' /></a>
              <br><br>"))
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$send, {
    tibble (input$libros, input$emociones, c(input$p1, input$p2, input$p3), Sys.time()) %>%
      sheet_append(., ss="1BrkZeStaYAuidjxQZO2LhnJ2a36n3USksJYiUBtXe8g",
                   sheet = "data")
    })
  
  observeEvent(input$send, {
    output$texto <- renderText({
      "😀 Gracias por participar!
      Consulta los resultados en tiempo real en el stand de Humanidades digitales de la FILMTY 2022."
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
