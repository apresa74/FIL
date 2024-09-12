
library(shiny)
library(tidyverse)
library(ggiraph)
library(ggimage)
#library(wordcloud2)
library(shinycssloaders)


captionsTotal <- readRDS("data/captionsTotal.rds")
videoIds <- readRDS("data/videoIds.rds")
jagamma <- readRDS("data/jagamma.rds")
jatopics <- readRDS("data/jatopics.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("El Multiverso de José Agustín"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("topics",
                        "Escoje un tópico:",
                       choices = c("LITERATURA"="1", "MEXICO"="2", "FAMILIA"="3", "ROCK"="4", 
                                   "ESCRITOR"="5", "VIDA"="6", "TODOS"),
                       selected = "TODOS")#,
           # wordcloud2Output("nube")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           girafeOutput("graph")%>% withSpinner(),
           htmlOutput("video")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$topics, if(input$topics=="TODOS"){

    output$graph <- renderGirafe({
      jagammaGraph <- jagamma %>% 
        group_by(topic) %>% 
        slice_max(order_by = gamma, n = 10) %>% 
        as_tbl_graph() %>% 
        activate(nodes) %>% 
        left_join(videoIds, by = c("name"="vid")) %>% 
        mutate(name=if_else(name=="1", "LITERATURA",
                            if_else(name=="2", "MEXICO",
                                    if_else(name=="3", "FAMILIA",
                                            if_else(name=="4", "ROCK",
                                                    if_else(name=="5", "ESCRITOR",
                                                            if_else(name=="6", "VIDA", name))))))) %>% 
        ggraph(layout = "stress")+
        geom_point_interactive(aes(x=x, y=y, tooltip = videoTitle.x, data_id = name), 
                               size=4, color="white")+
        geom_edge_link(aes(width=gamma), color="lightblue")+
        geom_image(aes(x=x, y=y, image=thumbnail_maxres.x), size=0.2)+
        geom_node_text(aes(label=if_else(name%in%c("LITERATURA", "MEXICO", "FAMILIA",
                                                  "ROCK", "ESCRITOR", "VIDA"), name, "")))+
        theme_void()
      
      girafe(ggobj = jagammaGraph, width_svg = 8, height_svg = 5,
                            options = list(opts_sizing(rescale = TRUE)))
    })
  } else {
    
    output$graph <- renderGirafe({
      jagammaGraph <- jagamma %>% 
        filter(topic==input$topics) %>% 
        slice_max(order_by = gamma, n = 50) %>% 
        as_tbl_graph() %>% 
        activate(nodes) %>% 
        left_join(videoIds, by = c("name"="vid")) %>% 
        mutate(name=if_else(name=="1", "LITERATURA",
                            if_else(name=="2", "MEXICO",
                                    if_else(name=="3", "FAMILIA",
                                            if_else(name=="4", "ROCK",
                                                    if_else(name=="5", "ESCRITOR",
                                                            if_else(name=="6", "VIDA", name))))))) %>% 
        ggraph(layout = "stress")+
        geom_point_interactive(aes(x=x, y=y, tooltip = videoTitle.x, data_id = name), 
                               size=4, color="white")+
        geom_edge_link(aes(width=gamma), color="lightblue")+
        geom_image(aes(x=x, y=y, image=thumbnail_maxres.x), size=0.2)+
        geom_node_text(aes(label=if_else(name%in%c("LITERATURA", "MEXICO", "FAMILIA",
                                                   "ROCK", "ESCRITOR", "VIDA"), name, "")))+
        theme_void()
      
      girafe(ggobj = jagammaGraph, width_svg = 8, height_svg = 5,
             options = list(opts_sizing(rescale = TRUE)))
    })
      
    } )
    
    selected_video <- reactive({
      input$graph_selected
    })
    
    output$video <- renderUI(
      HTML(paste0('<iframe width="560" height="315" src="https://www.youtube.com/embed/', 
                  tail(input$graph_selected,1),"?autoplay=1", '" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>'))
    )
    
    # output$nube <- renderWordcloud2(totalCaptions %>% 
    #                                   filter(vid=="1K61RfkWqvI") %>% 
    #                                   mutate(texto=paste(text, collapse = " ")) %>% 
    #                                   unnest_tokens(palabra, texto) %>% 
    #                                   count(palabra, sort = TRUE) %>% 
    #                                   select(word=palabra, freq=n) %>% 
    #                                   wordcloud2(size=0.75))
    }

# Run the application 
shinyApp(ui = ui, server = server)
