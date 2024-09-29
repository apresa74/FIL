library(shiny)
library(tidyverse)
library(tidytext)
library(shinyjs)
library(shinycssloaders)
library(ggiraph)
library(tidygraph)
library(ggraph)
library(shinythemes)
library(plotly)
library(shinyWidgets)

captionsTotal <- readRDS("data/captionsTotalParagraphs.rds")
videoIds <- readRDS("data/videoIds.rds")
jatopics <- readRDS("data/jatopics.rds")
jagamma <- readRDS("data/jagamma.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")
  ),
  
  # Footer
  tags$footer(
    style = "position: fixed; bottom: 0; z-index: 9999;
    width: 100%; background-color: white; color: black;
    padding-left: 10px; padding-right: 10px; padding-top: 10px; padding-bottom: 10px;
    text-align: center; font-size: 10px;",
    HTML('<p xmlns:cc="http://creativecommons.org/ns#" xmlns:dct="http://purl.org/dc/terms/"><span property="dct:title">El multiverso de José Agustín</span> by <span property="cc:attributionName">Dr. Cebral-Loureda, Dr. Martín-Del Campo, Dr. Muñiz-Apresa</span> is licensed under <a href="https://creativecommons.org/licenses/by-nc/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">CC BY-NC 4.0<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/nc.svg?ref=chooser-v1" alt=""></a></p>
         '
    )),
  
  tags$head(tags$style("#miParrafo{font-size: 10px;
                                 font-style: italic;
                                 }"
  )
  ),
  
  theme = shinytheme("cerulean"),
    # Application title

    # Sidebar with a slider input for number of bins 
    column(6,
         column(12,
                titlePanel("El Multiverso de José Agustín"),
                radioGroupButtons(
                  inputId = "general",
                  label = "",
                  choices = c("Grafo", 
                              "Texto libre"),
                  justified = TRUE
                ),
                conditionalPanel(
                  condition = 'input.general=="Grafo"',
                  HTML("Haz click en los nodos para visualizar fragmentos de video vinculados con esas palabras. 
               También puedes variar el tamaño de la red así como enfocarte en alguno de los tópicos 
               mediante los controles de abajo.<br><br>"),
          column(6,
            checkboxGroupInput("topics", inline = TRUE,
                        "Escoje uno o varios tópicos:",
                       choices = c("LITERATURA"="1", "MEXICO"="2", "FAMILIA"="3", "ROCK"="4",
                                   "ESCRITOR"="5", "VIDA"="6"),
                       selected = c("LITERATURA"="1", "MEXICO"="2", "FAMILIA"="3", "ROCK"="4",
                                    "ESCRITOR"="5", "VIDA"="6"))),
          column(6,
           # wordcloud2Output("nube")
          sliderInput("rango", label = "Rango de la red", min = 5, max = 50, value = 10)#,
        ),
        column(12,
               girafeOutput("graph")%>% withSpinner()
              )),
        conditionalPanel(
          condition = "input.general=='Texto libre'",
          textInput("myText", "Introduce una o varias palabras sobre las que quieras encontrar videos"),
          actionButton("buscar", "Buscar correlaciones")
        )
        )),

        # Show a plot of the generated distribution
        column(6, 
               id="myCanvas",
               #style = "height: 100vh;",
               htmlOutput("video")%>% withSpinner(),
               HTML("<br>"),
               actionButton("siguiente", "Otro video →"),
               HTML("<br><br>"),
               textOutput("miParrafo"),
               plotlyOutput("gammaValues", height = 200),
           HTML("<br><br><br>"))
)

# Define server logic required
server <- function(input, output, session) {
  
  hide("siguiente")
  hide("video")
  hide("miParrafo")

  counter <- reactiveValues(countervalue = 1) # Defining & initializing the reactiveValues object
  
  observeEvent(input$siguiente, {
    if (counter$countervalue == 5) {counter$countervalue <- 1} else {
      counter$countervalue <- counter$countervalue + 1     # if the add button is clicked, increment the value by 1 and update it
    }
  })
  
  #create a function to calculate jaccard similarity between sentences
  jaccard_similarity <- function(set1, set2) {
    intersection <- length(intersect(set1, set2))
    union <- length(union(set1, set2))
    return(intersection / union)
  }
  
  query <- reactiveValues(words="incio")
  similarities <- reactiveValues(value="incio")
  
  observeEvent(input$graph_selected, {
    query$words <- tail(input$graph_selected,1)
    
  })
  
  observeEvent(input$buscar, {
    query$words <- input$myText %>%  as_tibble() %>%
        unnest_tokens(words, value) %>% pull(words)
  })
  
  # Calculate Jaccard similarity for each paragraph
  observeEvent(input$graph_selected, {
      updateActionButton(session, "siguiente", label = paste0("Otro video relacionado con ", query$words," →"))
      delay(3000, show("siguiente"))
      delay(3000, show("video"))
      delay(3000, show("miParrafo"))
      
      similarities$value <- sapply(captionsTotal$parrafo, function(paragraph) {
        paragraph_words <- unlist(strsplit(paragraph, " "))
        jaccard_similarity(query$words, paragraph_words)}
      )})
  
  observeEvent(input$buscar, {
    updateActionButton(session, "siguiente", label = paste0("Otro video relacionado con ", query$words," →"))
    delay(3000, show("siguiente"))
    delay(3000, show("video"))
    delay(3000, show("miParrafo"))
    
    similarities$value <- sapply(captionsTotal$parrafo, function(paragraph) {
      paragraph_words <- unlist(strsplit(paragraph, " "))
      jaccard_similarity(query$words, paragraph_words)}
    )})
  
  similares <- reactive(captionsTotal %>% 
    filter(parrafo%in% names(tail(sort(similarities$value),5)))
  )
  
  output$graph <- renderGirafe({
      jaGraph <- jatopics %>%
        filter(topic%in%input$topics) %>% 
        group_by(topic) %>%
        slice_max(order_by = beta, n = input$rango) %>%
        as_tbl_graph() %>%
        activate(nodes) %>%
        mutate(name=if_else(name=="1", "LITERATURA",
                            if_else(name=="2", "MEXICO",
                                    if_else(name=="3", "FAMILIA",
                                            if_else(name=="4", "ROCK",
                                                    if_else(name=="5", "ESCRITOR",
                                                            if_else(name=="6", "VIDA", name))))))) %>%
        ggraph(layout = "stress")+
        geom_point_interactive(aes(x=x, y=y, tooltip = name, data_id = name),
                               size=4, color="white")+
        geom_edge_link(aes(width=beta), color="#FF0000", show.legend = FALSE)+
        geom_node_text(aes(label=name), check_overlap = FALSE)+
        geom_node_label(aes(label=if_else(name%in%c("LITERATURA", "MEXICO", "FAMILIA",
                                                         "ROCK", "ESCRITOR", "VIDA"), name, NA)))+
        theme_void()
      
      girafe(ggobj = jaGraph, width_svg = 5, height_svg = 4,
                            options = list(opts_sizing(rescale = TRUE),
                                           opts_selection(type = "single", only_shiny = FALSE,
                                                          css = "fill:transparent;stroke:transparent;r:5pt;"),
                                           opts_hover(css = "fill:#FF0000;stroke:transparent;r:15pt;",
                                                      nearest_distance = 200) ))
    })
  
  output$miParrafo <- renderText(print(similares()$parrafo[counter$countervalue]))
    
  output$video <- renderUI(
      HTML(paste0('<div class="video-container"><iframe width="100%" src="https://www.youtube.com/embed/',
                  similares()$vid[counter$countervalue], '?autoplay=1&start=', round(similares()$start[counter$countervalue], 0), '" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>
                  </div>'))
    )
  
    
    observeEvent(input$graph_selected, {
      output$gammaValues <- renderPlotly({
        tempPlot <- jagamma %>%
          filter(similares()$vid[counter$countervalue]==document) %>%
          mutate(topic=reorder(as_factor(topic), gamma)) %>% 
          group_by(topic) %>% 
          filter(row_number()==1) %>% 
          ungroup() %>% 
          distinct() %>% 
          ggplot(aes(gamma, topic))+
          geom_col(fill="#ff0000")+
          geom_col(aes(1, topic), color="black", fill= "transparent", show.legend = FALSE)+
          ylab(NULL)+
          xlab(NULL)+
          ggtitle("Vinculación del video con los tópicos")+
          theme_minimal()
        
        ggplotly(tempPlot)%>% 
          config(displayModeBar = FALSE) %>% 
          layout(xaxis=list(fixedrange=TRUE)) %>% 
          layout(yaxis=list(fixedrange=TRUE))
        
        })
    })
    
    }

# Run the application 
shinyApp(ui = ui, server = server)
