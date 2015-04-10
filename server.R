library('shiny')
library('dplyr')
library('tidyr')
library('data.table')
library('ggplot2')
library('readr')
library('stringr')
library('wordcloud')
library('DT')
source('fte_theme_ggplot.R')

## data
load('names_data.RData')
data$name <- str_replace_all(data$name, '<f8>', 'ø')
data$name <- str_replace_all(data$name, '<d8>', 'Ø')
data$name <- str_replace_all(data$name, '<e6>', 'æ')
data$name <- str_replace_all(data$name, '<c6>', 'Æ')
data$name <- str_replace_all(data$name, '<e5>', 'å')
data$name <- str_replace_all(data$name, '<c5>', 'Å')

shinyServer(function(input, output) {
  
  names_dat <- reactive({
    names_dat <- data %>%
      filter(gender == input$gender, year >= input$range[1], year <= input$range[2]) %>%
      group_by(name) %>%
      summarise(Popularitet = sum(value))
    
    #if (!is.null(input$names) && input$names != "") {
    #  names_dat <- names_dat %>% filter(name %like% input$names)
    #}
    
    return(names_dat)
  })
  
  
  output$plot <- renderPlot({
    choose_dat <- input$mychooser
    
    if(length(choose_dat$right) == 0) {
      plot_dat <- data_frame(name = '', year = 1880:2014, value = NA_real_)
      p <- ggplot(data = plot_dat, aes(x = year, y = value, color = name)) +
        geom_line() +
        scale_x_continuous(breaks = seq(from = 1880, to = 2014, by = 10)) +
        scale_y_continuous(limits = c(0, 1)) +
        fte_theme() +
        theme(legend.title=element_blank()) +
        labs(x = 'År', y = 'Prosent av nyfødte')
      print(p)
    } else {
      plot_dat <- filter(data, name %in% choose_dat$right)
      p <- ggplot(data = plot_dat, aes(x = year, y = value, color = name)) +
        geom_line() +
        scale_x_continuous(breaks = seq(from = 1880, to = 2014, by = 10)) +
        fte_theme() +
        theme(legend.title=element_blank()) +
        guides(colour = guide_legend(override.aes = list(size=4))) +
        labs(x = 'År', y = 'Prosent av nyfødte')
      print(p)
    }
    
  })
  
  
  output$wordplot <- renderPlot({
    choose_dat <- input$mychooser
    
    if(length(choose_dat$right) == 0) {
      wordcloud('', 1, min.freq = 1,
                scale = c(4, 0.9), max.words = 100, random.order = FALSE, rot.per = 0.2, 
                use.r.layout = FALSE)
    } else {
      ## the name to compare to
      center_name <- rev(choose_dat$right)[1]
      center_name_gender <- filter(data, name == center_name)$gender[1]
      
      data2 <- data %>%
        filter(gender == center_name_gender)
      
      ## calculate Levenshtein (edit) distance to every name
      dist <- data_frame(name = unique(data2$name),
                         dist = as.numeric(adist(center_name, unique(data2$name), 
                                                 ignore.case = TRUE))) %>%
        arrange(dist) %>%
        slice(1:50) %>%
        as.data.frame()  
      
      temp <- dist$dist[dist$dist != 0]
      numbers <- table(temp)
      freq <- c(max(temp) + 1, rep(rev(as.numeric(names(numbers))), as.numeric(numbers)))
      
      gender_color <- ifelse(center_name_gender == 'male', "Blues", "Reds")
      wordcloud(dist$name, freq, min.freq = 1,
                scale = c(4, 0.9), max.words = 100, random.order = FALSE, rot.per = 0.2, 
                use.r.layout = FALSE, colors = brewer.pal(8, gender_color))
    }
    
  })
  
  
  output$table <- renderDataTable({
    dat <- names_dat()
    
    rank <- rev(table(dat$Popularitet))
    dat2 <- data %>%
      filter(gender == input$gender, year >= input$range[1], year <= input$range[2]) %>%
      group_by(name) %>%
      summarise(Popularitet = sum(value)) %>%
      arrange(desc(Popularitet)) %>%
      mutate(Navn = name,
             Rangering = rep(1:length(rank), as.numeric(rank))) %>%
      filter(Popularitet !=0) %>%
      select(-Popularitet, -name)
    
    datatable(dat2, rownames = FALSE, options = list(
      pageLength = 10,
      lengthMenu = c(10, 20, 50, 100)
    ))
  })
  
  
})

