## Libraries
## This is developed in Rstudio 4.0 with all packages updated to the latest versions
### This is tested on MoVe environmnent
options(repos = c(CRAN = "https://cloud.r-project.org"))

require(shiny)
install.packages("remotes")
library(remotes)
remotes::install_github("RinteRface/fullPage")
library(fullPage)
require(tidyverse)
require(ggplot2)
require(shinyWidgets)
require(plotly)
require(ggplot2)
require(wordcloud2)
require(lubridate)

### Data Reading
# Error handling for data reading
data_1 <- tryCatch(
  {
    read_csv('games_cleaned_enriched.csv')
  },
  error = function(e) {
    cat("Error in reading data:", e$message, "\n")
    NULL
  }
)

# Check if data is loaded
if (is.null(data_1)) {
  stop("Data could not be read. Please check the file path and try again.")
}

# Creating additional UI and servers elements
mod_topic_1_1_ui <- function(id){
  ns <- NS(id)
  pageContainer(
    class = 'light',
    h1('analysis of accessibility in games'),
    shinyWidgets::radioGroupButtons(
      inputId = ns('value'),
      label = 'subtopics',
      choices = c('game prices','languages'),
      checkIcon = list(yes = icon('ok',lib = 'glyphicon'))
    ),
    uiOutput(ns('dynamic_ui'))
  )
}

mod_topic_1_1_server <- function(input,output,session){
  ns <- session$ns
  output$dynamic_ui <- renderUI({
    switch(input$value,
           'game prices' = fullPage(center =TRUE,h3('view game price distribution'),
                                         sliderInput(ns('range'),'price range:',min = 0, max=1000, value = c(0,1000)),
                                         plotlyOutput(ns('b'))),
           'languages' = fullPage(center = TRUE,h3('view game languages over time'),
                                  selectInput(ns('checkbox_input'),'view languages:', choices = c('top 10' = 10,'top 20'=20,'all'=104)),
                                  plotlyOutput(ns('a_2')))
    )
  })

  
  output$b <- renderPlotly({
    req(input$range)
    data <- read.csv('games_cleaned_enriched.csv')%>%
      mutate(
        price_str = sprintf('%0.2f',Price)
      )%>%
      group_by(price_str,Price)%>%
      summarize(counts = n())%>%
      filter(Price >= input$range[1] & Price <= input$range[2])
    
    plot_ly(data = data, x = ~Price, type = 'histogram', nbinsx = 30) %>%
      layout(xaxis = list(title = 'Price'),
             yaxis = list(title = 'Count'),
             plot_bgcolor = '#ff9900ff',
             paper_bgcolor = '#ff9900ff')
    
    
  })
  
  output$a_2 <- renderPlotly({
    req(input$checkbox_input)
    top_n <- ifelse(is.infinite(input$checkbox_input), Inf, as.numeric(input$checkbox_input))
    
    data <- read_csv('Languages .csv')%>%
      rename(
        supported_lang = 'Supported languages'
      )%>%
      group_by(supported_lang)%>%
      summarize(counts = n())%>%
      arrange(desc(counts))
      
      if (!is.infinite(top_n)) {
        data <- data %>%
          slice_head(n = top_n)
      }
      
      plot_ly(data = data, labels = ~supported_lang, values = ~counts, type = 'pie', hole = 0.4) %>%
        layout(plot_bgcolor = '#ff9900ff',
               paper_bgcolor = '#ff9900ff')
  })
}
    
## Done as in 27/5/2024: Done the modular ui's and servers for topic 1, will wait for consult time and feebdack

# Creating additional UI and servers elements
mod_topic_1_2_ui <- function(id){
  ns <- NS(id)
  pageContainer(
    class = 'light',
    h1('analysis of diversity in game development'),
    plotlyOutput(ns('plot_1_2'))
  )
}

mod_topic_1_2_server <- function(input,output,session){
  ns <- session$ns
  output$plot_1_2 <- renderPlotly({
    data <- read.csv('games_cleaned_enriched.csv')%>%
      mutate(
        Release.date = mdy(Release.date),
        Release.year = year(Release.date)
      )%>%
      group_by(Release.year, is.independent)%>%
      summarize(counts = n(),.groups = 'drop')
    data_wide <- data %>%
      pivot_wider(names_from = is.independent, values_from = counts)
    plot_ly(data = data_wide, x =~Release.year)%>%
      add_lines(y = ~True, name = 'independent', line = list('color' = 'red'))%>%
      add_lines(y = ~False, name = 'not independent', line = list('color' = 'blue'))%>%
      layout(xaxis = list(title = 'Year'),
             yaxis = list(title = 'Counts'),
             plot_bgcolor = '#ff9900ff',
             paper_bgcolor = '#ff9900ff')
 })
}

#additional ui and server elements
mod_1_3_ui <- function(id){
  ns <- NS(id)
  pageContainer(
    class = 'light',
    h1('analysis of genres in games'),
    shinyWidgets::radioGroupButtons(
      inputId = ns('value'),
      label = 'subtopics',
      choices = c('a','b'),
      checkIcon = list(yes = icon('ok',lib = 'glyphicon'))
    ),
    uiOutput(ns('dynamic_ui'))
  )
}

mod_1_3_server <- function(input, output, session) {
  ns <- session$ns
  output$dynamic_ui <- renderUI({
    switch(input$value,
           'a' = pageContainer(center =TRUE,h1('view popular genre combination'),
                               selectInput(ns('dropdown_input'),'view genres:', choices = c('top 10' = 10,'top 20'=20,'all'=Inf)),
                               plotlyOutput(ns('a'))),
           'b' = pageContainer(center = TRUE,h1('view competeive vs single player game'),
                               checkboxGroupInput(ns('checkboxinput'),'view genres:',choices = c('singleplayer only' = 'True_False',
                                                                                                 'single and multiplayer' = 'True_True',
                                                                                                 'multiplayer only' = 'False_True')),
                               plotlyOutput(ns('b')))
    )
  })
  
  output$a <- renderPlotly({
    req(input$dropdown_input)
    top_n <- as.numeric(input$dropdown_input)
    
    data <- read.csv('game and genre .csv') %>%
      group_by(Genres) %>%
      summarize(counts = n()) %>%
      arrange(desc(counts))
    
    if (!is.infinite(top_n)) {
      data <- data %>%
        slice_head(n = top_n)
    }
    
    plot_ly(data = data, labels = ~Genres, values = ~counts, type = 'pie', hole = 0.4) %>%
      layout(title = 'Distribution of Top Genres',paper_bgcolor = '#ff9900ff')
  })
  
  output$b <- renderPlotly({
    req(input$checkboxinput)
    condition <- input$checkboxinput
    data <- read.csv('games_cleaned_enriched.csv')%>%
      mutate(
        Release.date = mdy(Release.date),
        Release.year = year(Release.date)
      )%>%
      group_by(Release.year, is.singleplayer, is.multiplayer)%>%
      summarize(counts = n(),.groups = 'drop')
    data_wide <- data %>%
      pivot_wider(names_from = c(is.singleplayer, is.multiplayer), values_from = counts)%>%
      select(-False_False)
    plot <- plot_ly(data = data_wide, x = ~Release.year)
    
    if ('True_False' %in% condition) {
      plot <- plot %>% add_lines(y = ~True_False, name = 'Singleplayer Only', line = list(color = 'red'))
    }
    if ('True_True' %in% condition) {
      plot <- plot %>% add_lines(y = ~True_True, name = 'Single and Multiplayer', line = list(color = 'blue'))
    }
    if ('False_True' %in% condition) {
      plot <- plot %>% add_lines(y = ~False_True, name = 'Multiplayer Only', line = list(color = 'green'))
    }
    
    plot %>%
      layout(
        xaxis = list(title = 'Release Year'),
        yaxis = list(title = 'Count'),
        plot_bgcolor = '#ff9900ff',
        paper_bgcolor = '#ff9900ff' 
      )
  })
}
### Done the templates for topic 1

## Create module for topic 2
module_2_ui <-  function(id){
  ns <- NS(id)
  pageContainer(
    class = 'light',
    h1('Analysis of top 100 games in steam as of March 2024'),
    shinyWidgets::radioGroupButtons(
      inputId = ns('value'),
      label = 'subtopics',
      choices = c('view top 10 topics','view word cloud display'),
      checkIcon = list(yes = icon('ok',lib = 'glyphicon'))
  ),
  uiOutput(ns('dynamic_ui')))
}

module_2_server <- function(input,output,session){
  ns <- session$ns
  
  output$dynamic_ui <- renderUI({
    switch(input$value,
           'view top 10 topics' = pageContainer(center = TRUE,h3('top 10 topics in positive and negative reviews'),h4('right is positive left is negative'),uiOutput(ns('top_10_pos_neg'))),
           'view word cloud display' = pageContainer(center = TRUE, h3('most discussed words in the reviews'), uiOutput(ns('wc_pos_neg'))))
  })
  
  output$top_10_pos_neg <- renderUI({
    fluidPage(
      column(6, plotlyOutput(ns('top_10_topics_pos'))),
      column(6, plotlyOutput(ns('top_10_topics_neg')))
    )
  })
  
  output$wc_pos_neg <- renderUI({
    pageContainer(selectInput(ns('dropdown_input'),'view genres:', choices = c('positive' = 'positive','negative'='negative')),
                  wordcloud2Output(ns('wordcloud')))
  })
  
  output$top_10_topics_neg <- renderPlotly({
    data <- read_csv('freq_neg.csv')
    data_new <- data[-1,]%>%
      slice_head(n = 10)
    plot_ly(data = data_new, labels = ~Topic, parents = '',
            type = 'treemap',
            value = ~Count,
            textinfo = 'label+value+percent entry',
            text = ~paste("Topic: ", Topic, "<br>Size:", Count, "<br>Detail:", Representation),
            hoverinfo = 'text')%>%
      layout(plot_bgcolor = '#5cecffff',
             paper_bgcolor = '#5cecffff')
  })
  
  output$top_10_topics_pos <- renderPlotly({
    data <- read_csv('freq_pos.csv')
    data_new <- data[-1,]%>%
      slice_head(n = 10)
    plot_ly(data = data_new, labels = ~Topic, parents = '',
            type = 'treemap',
            value = ~Count,
            textinfo = 'label+value+percent entry',
            text = ~paste("Topic: ", Topic, "<br>Size:", Count, "<br>Detail:", Representation),
            hoverinfo = 'text')%>%
      layout(plot_bgcolor = '#5cecffff',
             paper_bgcolor = '#5cecffff')
  })
  
  output$wordcloud <- renderWordcloud2({
    req(input$dropdown_input)
    cond <- input$dropdown_input
    if (cond == 'positive') {
      data <- read.csv('positive_word_count.csv')
      print(head(data))
      wordcloud2(data = data, size = 1,backgroundColor = '#5cecffff' )
    } else if (cond == 'negative') {
      data <- read.csv('negative_word_count.csv')
      print(head(data))
      wordcloud2(data = data, size = 1, backgroundColor = '#5cecffff')
    }
  })

  
}

options <- list(
  sectionsColor = c('#f4ff61ff', '#ff9900ff', '#5cecffff','#ff61c6ff'),
  parallax = TRUE
)

ui <- fullPage(
  center = TRUE,
  opts = options,
  menu = c(
    "Intro" = "intro",
    "Analysis on games" = "topic 1",
    "Analysis on game reviews" = "topic 2"
  ),
  fullSectionImage(
    menu = "intro",
    img= 'https://wallpapersmug.com/download/1920x1080/d162e4/retro-game-synthwave.jpg',
    h1("DVP project", style = "color: yellow"),
    h2("steam games analysis", style = 'color : yellow'),
    class = 'intro-section'
  ),
  pageSection(
    menu = "topic 1",
    fullSlide(
      h1('Accessibility'),
      mod_topic_1_1_ui('mod_1'),
      p('accessibility is determined by price and age limits'),
      
    ),
    fullSlide(
      center = TRUE,
      h1('Diversity'),
      mod_topic_1_2_ui('mod_2'),
      p('the plot above show the proportion of "indie" or independent developers vs non independent or corporate companies, The drop in 2024 may be because of lack in data on 2024')
    ),
    fullSlide(
      center = TRUE,
      h1('Genres'),
      mod_1_3_ui('mod_3'))
    ),
  fullSection(
    center = TRUE,
    module_2_ui('mod_4')
  ),
  tags$head(
    tags$link(rel = "preconnect", href="https://fonts.googleapis.com"),
    tags$link(rel="preconnect" ,href="https://fonts.gstatic.com", crossorigin = 'anonymous'),
    tags$link(href="https://fonts.googleapis.com/css2?family=Pixelify+Sans:wght@400..700&display=swap", rel="stylesheet"),
    tags$style(HTML("
      h1{
                    font-family: 'Pixelify Sans', sans-serif;
      }
      h2{
                    font-family: 'Pixelify Sans', sans-serif;
      }"
      ))
  )
  )

server <- function(input, output){
  callModule(mod_topic_1_1_server,'mod_1')
  callModule(mod_topic_1_2_server,'mod_2')
  callModule(mod_1_3_server,'mod_3')
  callModule(module_2_server,'mod_4')
}

shinyApp(ui, server)
