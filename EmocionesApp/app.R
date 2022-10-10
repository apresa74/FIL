library(shiny)
library(googlesheets4)
library(gargle)
library(tidyverse)
library(shinyjs)
library(shinyWidgets)
library(wordcloud2)
library(htmlwidgets)
library(highcharter)
library(plotly)
library(DT)

options(gargle_oauth_cache = "secrets",
        gargle_oauth_email = "manuel.cebral@tec.mx")

gs4_auth()

corpus <- readRDS("data/corpus.rds")
Sys.setenv(TZ='America/Mexico_City')


# check if there are new rows:
IsThereNewRow=function(){  #  cheap function whose values over time will be tested for equality;
  #  inequality indicates that the underlying value has changed and needs to be 
  #  invalidated and re-read using valueFunc
  read_sheet("1BrkZeStaYAuidjxQZO2LhnJ2a36n3USksJYiUBtXe8g",
                          sheet = "data") %>% nrow()
}

ReadAllData=function(){ # A function that calculates the underlying value
  read_sheet("1BrkZeStaYAuidjxQZO2LhnJ2a36n3USksJYiUBtXe8g",
             sheet = "data")
}


ui <- navbarPage("Las emociones en la obra de Elena Poniatowska App",
  setBackgroundColor("#161c20"),
  tags$head(
    tags$style(
      HTML('
                              .navbar-header { width:100% }
                              .navbar-brand {
                              float: left;
                              padding: 35px 15px;
                              font-size: 48px;
                              font-weight: bold;
                              line-height: 50px;
                              height: 50px;
                              margin-bottom: 30px;
                              }
                              .navbar-default{
                              border-color: #161c20;
                              }
                              .navbar-default .navbar-brand:focus, .navbar-default .navbar-brand:hover{
                              color: #eeff41;
                              }
                              .navbar-default .container-fluid {
                              display: flex;
                              list-style-type: none;
                              justify-content: center;
                              flex-wrap: wrap;
                              background-color: #161c20;
                              color: #eeff41;
                              }
                              
                              .tab-content {
                              font-family: "Arial"; color: #eeff41;
                              }
                              .navbar-default .navbar-brand {
                              color: #eeff41;
                              }
                               .navbar-header {
                              display: flex !important;
                              justify-content: center !important;
                               }
                              .navbar-default .navbar-nav>li>a {
                              color: #eeff41;
                              background-color: #161c20;
                              }
                              .navbar-default .navbar-nav>li>a 
                              .navbar-default .navbar-nav>li>a:focus, .navbar-default .navbar-nav>li>a:hover {
                              color: #161c20;
                              background-color: #ff00ff;
                              }
                              .navbar-default .navbar-nav>.active>a,
                              .navbar-default .navbar-nav>.active>a:focus, .navbar-default .navbar-nav>.active>a:hover {
                              color: #161c20;
                              background-color: #eeff41;
                              }
                              .nav.navbar-nav.shiny-tab-input.shiny-bound-input {
                              background-color: #000000;
                              }
                              .col-sm-4 {
                              color: #eeff41;
                              margin-top: 40px;
                              text-align: center;
                              }
                              #footer {
                              color: #ffffff;
                              }
                              #sankeyPlot {
                              text-align:center;
                              color: #ffffff;
                              display: flex;
                              list-style-type: none;
                              justify-content: center;
                              flex-wrap: wrap;
                              }
                              #wordcloud {
                              text-align:center;
                              color: #ffffff;
                              display: flex;
                              list-style-type: none;
                              justify-content: center;
                              flex-wrap: wrap;
                              }
                              .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate {
                              color: #333;
                              display: none;
                              }
                              table.dataTable thead .sorting {
                              display: none;
                              }
                              .form-control {
                              background-color:#161c20;
                              border: 1px solid #fff;
                              color: #fff;
                              }
                              #footer {
                              text-align:center;
                              justify-content: center;
                              color: #ff00ff;
                              background-color: #161c20;
                              }
                              #reference {
                              display: none;
                              }
                              #nota {
                              color: #ff00ff;
                              }
           '))),

    tabPanel("LIBROS, PALABRAS Y EMOCIONES",
           column(12,id="sankeyPlot",
                  highchartOutput("sankey", width = "120%"),
                  sliderInput("minimoLibros", "Filtra % de palabras por su Ranking:", 1, 100, value = 10, post="%", width = "60%")
                  ),
           column(8, align= "center",
                  plotlyOutput("Tiempo", width = "100%", height = "200px")                  ),
           column(4, 
                  textOutput("participation", container = tags$h1)
                  ),
           column(12, id="footer",
                  HTML("<br><br>Las emociones en la obra de Elena Poniatowska es un proyecto del Seminario de Humanidades digitales del Tecnologico de Monterrey.
                         Ha sido desarrollado por <a href='https://github.com/mancebral/FIL'>Cebral-Loureda en R programming</a>
                       con licencia <a rel='license' href='http://creativecommons.org/licenses/by-nc/4.0/'>Creative Commons Attribution-NonCommercial 4.0 International License</a>.<br><br>
                       <a rel='license' href='http://creativecommons.org/licenses/by-nc/4.0/'><img alt='Creative Commons License' style='border-width:0' src='https://i.creativecommons.org/l/by-nc/4.0/88x31.png' /></a>
                         <br><br>"))
           ),
  tabPanel("LAS PALABRAS EN SUS LIBROS",
           column(6, id= "wordcloud",
                  HTML("Esta nube muestra las palabras más escogidas
                       por los usuarios mediante la App. Puedes filtrarlas
                       según hayan sido relacionadas con cada emoción. 
                       Haciendo click en las palabras obtienes 6 apariciones
                       aleatorias de las mismas en los textos de Elena Poniatowska, 
                       los cuales se muestran en la tabla de la derecha."),
                  wordcloud2Output("wordcloud", width = "100%"),
                  tags$script(HTML(
                    "$(document).on('click', '#canvas', function() {",
                    'word = document.getElementById("wcSpan").innerHTML;',
                    "Shiny.onInputChange('selected_word', word);",
                    "});"
                  )),
                  HTML("<br><br>"), HTML("<br><br>"),
                  checkboxGroupInput("emociones", "Filtra por emoción:", 
                                     choices = c("Alegría", "Tristeza", "Confianza", "Miedo", "Sorpresa",
                                                 "Expectativa", "Asco"), 
                                     selected = c("Alegría", "Tristeza", "Confianza", "Miedo", "Sorpresa", "Expectativa", "Asco"), 
                                     inline = TRUE, width = "100%"),
                  sliderInput("minimoNube", "Tamaño de la nube:", 5, 125, value = 100, post="%", width = "60%")
                  ),
           column(6,
                  textInput("buscaPalabra", "También puedes buscar cualquier otra palabra en los textos de la autora:"),
                  column(12, id="reference",
                         textInput('out', 'Output (ideally not editable)')),
                  dataTableOutput ("TablePlot"),
                  HTML("<br><p class='nota'>*Estas frases se recogen de manera automática y aleatoria. 
                       Es posible que, entre ellas, aparezcan algunas referentes a las portadas o 
                       contraportadas, o incluso algunos errores en las palabras debidos al scanneo.</p>")
                  ),
            column(12, id="footer",
                    HTML("<br><br>Las emociones en la obra de Elena Poniatowska es un proyecto del Seminario de Humanidades digitales del Tecnologico de Monterrey.
                         Ha sido desarrollado por <a href='https://github.com/mancebral/FIL'>Cebral-Loureda en R programming</a>
                       con licencia <a rel='license' href='http://creativecommons.org/licenses/by-nc/4.0/'>Creative Commons Attribution-NonCommercial 4.0 International License</a>.<br><br>
                       <a rel='license' href='http://creativecommons.org/licenses/by-nc/4.0/'><img alt='Creative Commons License' style='border-width:0' src='https://i.creativecommons.org/l/by-nc/4.0/88x31.png' /></a>
                         <br><br>"))
  )
)

# Define server
server <- function(input, output, session) {
  
  my_input <- reactivePoll(10000, session,IsThereNewRow, ReadAllData)   
  

lims  <- reactive ({
    invalidateLater(1800000, session)
  c(a=Sys.time()-86400, b=Sys.time())
  })
  
  horas0 <- reactive({
    tibble::tibble(Dias=seq(from=lims()[1],
                            to=lims()[2],
                            by="hour"),
                   n=0) %>% 
    mutate(Dias=strptime(Dias, "%Y-%m-%d %H"))
  })
  
  my_filtered <- reactive ({my_input()  %>% 
      mutate(Palabras=tolower(Palabras)) %>% 
      count(Palabras, sort=TRUE)%>% 
      mutate(rank=percent_rank(n)*100) %>% 
      filter(rank>input$minimoLibros)
  })
  
  my_input2 <- reactive ({my_input() %>% 
      mutate(Palabras=tolower(Palabras))%>% 
      filter(Palabras %in% my_filtered()$Palabras) %>% 
      select(Libro, Emocion, Palabras) %>% as.data.frame()
  })
  
  output$sankey <- renderHighchart (hchart(data_to_sankey(my_input2()), "sankey") %>%
                                     hc_add_theme(hc_theme_alone()))
  
  output$wordcloud <- renderWordcloud2(my_input() %>% rename(word=Palabras) %>% 
                                         mutate(word=tolower(word)) %>% 
                                         filter(Emocion%in%input$emociones) %>% 
                                         count(word) %>% 
                                         filter(!if_all(word, is.na)) %>% 
                                         wordcloud2(size = input$minimoNube/100, minSize = 0, gridSize = 0, 
                                                    #maxRotation=90, minRotation=90,
                                                    fontFamily = "Segoe UI", 
                                                    fontWeight = "normal", color = "random-light",
                                                    backgroundColor = "#161c20"))
  
 dias_plot <- reactive({my_input() %>%
     mutate(Dias=strptime(Tiempo, "%Y-%m-%d %H")) %>% 
     mutate(Dias=as.POSIXct(Dias)) %>% 
     group_by(Dias) %>% 
     summarise(n=n()/3) %>% 
     ungroup() %>% 
     full_join(horas0()) %>% 
     group_by(Dias) %>% 
     slice_max(n) %>% 
     ungroup() %>% 
     ggplot(aes(Dias, n))+ 
     geom_path(color= "#eeff41", size= 0.75)+
     geom_point(color= "#eeff41")+
     scale_x_datetime(date_labels = "%Y-%m-%d %H",
                  limits=lims())+
     xlab("Participación durante las últimas 24 horas")+
     ylab(NULL)+
     theme(panel.background = element_rect(fill = '#161c20', colour = '#ffffff'))+
     theme(plot.background = element_rect(fill = '#161c20', colour = '#ffffff'))+
     theme(panel.grid.major=element_line(colour="#161c20"),
           panel.grid.minor=element_line(colour="#161c20"),
           axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.title.x = element_text(colour = "#eeff41"),
           axis.title.y = element_blank())
     })
  
   output$Tiempo <- renderPlotly ({
     invalidateLater(1809000, session)
     ggplotly(isolate(dias_plot()), dynamicTicks = FALSE) %>% 
                                    plotly::config(modeBarButtons = list(list("resetScale2d")))
     })
   
   output$participation <- renderText (paste0("Participación total: ", my_input() %>% 
                                                nrow()/3))
   
   selected_word <- reactiveVal('')
   buscaPalabra <- reactiveVal('')
   
   
   my_word <- reactive({gsub("\\:.*","",input$selected_word)})
   
   observeEvent(input$selected_word, {
     output$TablePlot <- renderDataTable({corpus %>% 
       filter(str_detect(frase, paste0("\\b", my_word(), "\\b"))) %>% 
       sample_n (6, replace = FALSE) %>% 
       mutate(frase=toupper(frase)) %>% 
       datatable(options = list(scrollX=TRUE, pageLength = 500), rownames = FALSE) %>% 
       formatStyle(1:2, backgroundColor="#161c20", color="white")
     })
   })
   
   observeEvent(input$buscaPalabra, {
     output$TablePlot <- renderDataTable({ corpus %>% 
       filter(str_detect(frase, paste0("\\b", input$buscaPalabra, "\\b"))) %>% 
       sample_n (6, replace = FALSE) %>% 
       mutate(frase=toupper(frase)) %>% 
       datatable(options = list(scrollX=TRUE, pageLength = 500), rownames = FALSE) %>% 
       formatStyle(1:2, backgroundColor="#161c20", color="white")
     })
   })
   
   output$TablePlot <- renderDataTable({
     corpus %>% 
       filter(str_detect(frase, paste0("\\b", input$out, "\\b"))) %>% 
       sample_n (6, replace = FALSE) %>% 
       mutate(frase=toupper(frase)) %>% 
       datatable(options = list(scrollX=TRUE, pageLength = 500), rownames = FALSE) %>% 
       formatStyle(1:2, backgroundColor="#161c20", color="white")
   })
     
   observeEvent(selected_word(), {print(selected_word()); updateTextInput(session, 'out', value = my_word()) })
   observeEvent(buscaPalabra(), {print(buscaPalabra()); updateTextInput(session, 'out', value = buscaPalabra()) })
   
   }

# Run the application 
shinyApp(ui = ui, server = server)
