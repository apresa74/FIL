library(shiny)
library(tidyverse)
library(tidytext)
library(shinyjs)
library(shinycssloaders)
library(ggiraph)
library(tidygraph)
library(ggraph)
library(shinythemes)


captionsTotal <- readRDS("data/captionsTotalParagraphs.rds")
videoIds <- readRDS("data/videoIds.rds")
jatopics <- readRDS("data/jatopics.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  useShinyjs(), 
  
  # Footer
  tags$footer(
    style = "position: fixed; bottom: 0; z-index: 9999;
    width: 100%; background-color: white; color: black;
    padding-left: 10px; padding-right: 10px; padding-top: 10px; padding-bottom: 10px;
    text-align: center; font-size: 10px;",
    HTML('<p xmlns:cc="http://creativecommons.org/ns#" xmlns:dct="http://purl.org/dc/terms/"><span property="dct:title">El multiverso de José Agustín</span> by <span property="cc:attributionName">Dr. Cebral-Loureda, Dr. Martín-Del Campo, Dr. Muñiz-Apresa</span> is licensed under <a href="https://creativecommons.org/licenses/by-nc/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">CC BY-NC 4.0<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/nc.svg?ref=chooser-v1" alt=""></a></p>
         '
    )),
  
  theme = shinytheme("cerulean"),
    # Application title

    # Sidebar with a slider input for number of bins 
    column(5,
         column(12,
                titlePanel("El Multiverso de José Agustín"),
                
          HTML("Haz click en los nodos para visualizar fragmentos de video de José Agustín vinculados con esas palabras. 
               También puedes variar el tamaño de la red así como enfocarte en alguno de los tópicos 
               mediante los controles de abajo.<br><br>"),
            checkboxGroupInput("topics", inline = TRUE,
                        "Escoje un tópico:",
                       choices = c("LITERATURA"="1", "MEXICO"="2", "FAMILIA"="3", "ROCK"="4",
                                   "ESCRITOR"="5", "VIDA"="6"),
                       selected = c("LITERATURA"="1", "MEXICO"="2", "FAMILIA"="3", "ROCK"="4",
                                    "ESCRITOR"="5", "VIDA"="6")),
           # wordcloud2Output("nube")
          #textInput("myText", "Introduce una o varias palabras sobre las que quieras encontrar videos"),
          sliderInput("rango", label = "Rango de la red", min = 5, max = 50, value = 10)
        ),
        column(12,
               htmlOutput("video")%>% withSpinner(),
               actionButton("siguiente", "Otro video →"),
               HTML("<br>"),
               textOutput("miParrafo"))
        ),

        # Show a plot of the generated distribution
        column(7,
           girafeOutput("graph")%>% withSpinner(),
           #verbatimTextOutput("test"),
           HTML("<br><br><br>")
        )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  hide("siguiente")
  
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
  
  # query_words <- reactive({input$myText %>%  as_tibble() %>% 
  #     unnest_tokens(words, value) %>% pull(words)
  # })
  query_words <- reactive(tail(input$graph_selected,1))
  
  # Calculate Jaccard similarity for each paragraph
  similarities <- eventReactive(input$graph_selected, {
    updateActionButton(session, "siguiente", label = paste0("Otro video relacionado con ", query_words()," →"))
    show("siguiente")
    
    sapply(captionsTotal$parrafo, function(paragraph) {
      paragraph_words <- unlist(strsplit(paragraph, " "))
      jaccard_similarity(query_words(), paragraph_words)}
    )})
  
  similares <- reactive(captionsTotal %>% 
    filter(parrafo%in% names(tail(sort(similarities()),5)))
  )
  
  output$graph <- renderGirafe({
      jaGraph <- jatopics %>%
        filter(topic%in%input$topics) %>% 
        group_by(topic) %>%
        slice_max(order_by = beta, n = input$rango) %>%
        as_tbl_graph() %>%
        activate(nodes) %>%
        #left_join(videoIds, by = c("name"="vid")) %>%
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

      girafe(ggobj = jaGraph, width_svg = 5, height_svg = 5,
                            options = list(opts_sizing(rescale = TRUE),
                                           opts_selection(type = "single", only_shiny = FALSE,
                                                          css = "fill:transparent;stroke:transparent;r:5pt;"),
                                           opts_hover(css = "fill:#FF0000;stroke:transparent;r:15pt;",
                                                      nearest_distance = 200) ))
    })
  
  output$miParrafo <- renderText(print(similares()$parrafo[counter$countervalue]))
  
  #output$test <- renderPrint(print(similares()))
  
    
    output$video <- renderUI(
      HTML(paste0('<iframe width="100%" height="500" src="https://www.youtube.com/embed/',
                  similares()$vid[counter$countervalue], '?autoplay=1&start=', round(similares()$start[counter$countervalue], 0), '" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>'))

    )
    
    
    }

# Run the application 
shinyApp(ui = ui, server = server)

