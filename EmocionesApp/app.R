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

options(gargle_oauth_cache = "secrets",
        gargle_oauth_email = "manuel.cebral@tec.mx")

gs4_auth()
Sys.setenv(TZ='GMT')

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

ui <- fluidPage(
  setBackgroundColor("#34495e"),
  h1(id="big-heading", "Las emociones en Elena Poniatowska"),
  h5(id="subtitle", "Un proyecto de Humanidades digitales para la FILMTY 2022"),
  tags$head(
    tags$style(
      HTML('
      #big-heading{
      color: #F9F871;
      font-size: 60px;
      text-align:center;
      font-family: Arial Black;
      }
      #subtitle{
      color: #F9F871;
      font-size: 25px;
      text-align:center;
      font-family: Arial MT;
      font-weight: light;
      }
      .col-sm-4 {
    color: #F9F871;
    margin-right: 0px;
      }
      #footer {
      color: #F9F871;
      }
  '))),
  HTML('<br><br>'),
  column(12,
  highchartOutput("sankey")
  ),
  column(8, 
         wordcloud2Output("wordcloud", width = "100%")),
  column(4, align= "center",
    textOutput("participation", container = tags$h1),
    plotlyOutput("Tiempo", width = "100%")
  ),
  column(12, id="footer",
         HTML("<br>Las emociones en Elena Poniatowska es un proyecto del Seminario de Humanidades digitales del Tecnologico de Monterrey.
              Ha sido desarrollado por <a href='https://github.com/mancebral/Flourishing-Measure'>cebral-loureda en R programming</a>
            con licencia <a rel='license' href='http://creativecommons.org/licenses/by-nc/4.0/'>Creative Commons Attribution-NonCommercial 4.0 International License</a>.<br><br>
            <a rel='license' href='http://creativecommons.org/licenses/by-nc/4.0/'><img alt='Creative Commons License' style='border-width:0' src='https://i.creativecommons.org/l/by-nc/4.0/88x31.png' /></a>
              <br><br>"))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  my_input <- reactivePoll(10000, session,IsThereNewRow, ReadAllData)   
    
  #the function wordcloud2a is to replace the original wordcloud2() since it blocks the other plots
  #found here https://github.com/rstudio/shinydashboard/issues/281 
  wordcloud2a <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI", 
                           fontWeight = "bold", color = "random-light", backgroundColor = "#34495e", 
                           minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE, 
                           rotateRatio = 0.4, shape = "circle", ellipticity = 0.65, 
                           widgetsize = NULL, figPath = NULL, hoverFunction = NULL) 
  {
    if ("table" %in% class(data)) {
      dataOut = data.frame(name = names(data), freq = as.vector(data))
    }
    else {
      data = as.data.frame(data)
      dataOut = data[, 1:2]
      names(dataOut) = c("name", "freq")
    }
    if (!is.null(figPath)) {
      if (!file.exists(figPath)) {
        stop("cannot find fig in the figPath")
      }
      spPath = strsplit(figPath, "\\.")[[1]]
      len = length(spPath)
      figClass = spPath[len]
      if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
        stop("file should be a jpeg, jpg, png, bmp or gif file!")
      }
      base64 = base64enc::base64encode(figPath)
      base64 = paste0("data:image/", figClass, ";base64,", 
                      base64)
    }
    else {
      base64 = NULL
    }
    weightFactor = size * 180/max(dataOut$freq)
    settings <- list(word = dataOut$name, freq = dataOut$freq, 
                     fontFamily = fontFamily, fontWeight = fontWeight, color = color, 
                     minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor, 
                     gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation, 
                     shuffle = shuffle, rotateRatio = rotateRatio, shape = shape, 
                     ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
    chart = htmlwidgets::createWidget("wordcloud2", settings, 
                                      width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0, 
                                                                                                                              browser.padding = 0, browser.fill = TRUE))
    chart
  }
  
  Sys.sleep(5)
  
  my_input2 <- reactive ({my_input() %>% 
      mutate(Palabras=tolower(Palabras))%>% select(Libro, Emocion, Palabras) %>% as.data.frame()
  })
  
  output$sankey <- renderHighchart(hchart(data_to_sankey(my_input2()), "sankey") %>% 
                                     #hc_title(text="Las emociones en Elena Poniatowska") %>%
                                     #hc_subtitle(text="Un proyecto de Humanidades Digitales") %>% 
                                     hc_add_theme(hc_theme_flatdark()))
  
  output$wordcloud <- renderWordcloud2(my_input() %>% rename(word=Palabras) %>% 
                                         mutate(word=tolower(word)) %>% 
                                         count(word, sort = TRUE) %>% 
                                         filter(!if_all(word, is.na)) %>% 
                                         wordcloud2a())
  
  output$participation <- renderText (paste0("Participación: ", my_input() %>% 
                                        select(Tiempo) %>%
                                        distinct() %>% 
                                        nrow()))
 dias_plot <- reactive({my_input() %>%
     select(Tiempo) %>% 
     distinct() %>% 
     mutate(Dias=strptime(Tiempo, "%Y-%m-%d %H")) %>% 
     group_by(Dias) %>% 
     summarise(n=n()) %>% 
     ungroup() %>% 
     mutate(Dias=as.Date(Dias)) %>% 
     ggplot(aes(Dias, n))+ geom_path(color= "#F9F871", size= 2.5)+
     scale_x_date(date_labels = "%Y-%m-%d %H")+
     theme(panel.background = element_rect(fill = '#34495e', colour = '#516F8E'))+
     theme(plot.background = element_rect(fill = '#34495e', colour = '#516F8E'))+
     theme(panel.grid.major=element_line(colour="#516F8E"),
           panel.grid.minor=element_line(colour="#516F8E"),
           axis.text.x = element_text(angle = 45, hjust = 1, colour = "white"),
           axis.text.y = element_text(angle = 45, hjust = 1, colour = "white"),
           axis.title.x = element_text(colour = "white"),
           axis.title.y = element_text(colour = "white"))
     })
  
   output$Tiempo <- renderPlotly (ggplotly(dias_plot()))
   
   }

# Run the application 
shinyApp(ui = ui, server = server)
