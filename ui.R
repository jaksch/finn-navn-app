library('shiny')
library('dplyr')
library('stringr')
library('DT')
source("chooser.R")

## data
load('names_data.RData')
data$name <- str_replace_all(data$name, '<f8>', 'ø')
data$name <- str_replace_all(data$name, '<d8>', 'Ø')
data$name <- str_replace_all(data$name, '<e6>', 'æ')
data$name <- str_replace_all(data$name, '<c6>', 'Æ')
data$name <- str_replace_all(data$name, '<e5>', 'å')
data$name <- str_replace_all(data$name, '<c5>', 'Å')
names <- data$name %>%
  unique() %>%
  sort()

shinyUI(fluidPage(
  title = 'Finn navne!',
  titlePanel('Finn et navn til mini!'),
  fluidRow(
    column(3,
           wellPanel(
             h4(HTML('Velg mellom alle jente- og guttenavn gitt til nyfødte i Norge mellom
                      1880 til 2014 og sammenlign deres popularitet år for år. <br> <br>
                      For det sist tilføjet navn i den højre boxen gir wordcloud plottet foreslag 
                      til lignende navn. <br> <br>
                      Man velger et navn ved å merkere det iden venstre box og trykke på pilen til 
                      højre for å få det over i den højre box. <br> <br>
                      Du kan søke i den venstre box ved å merkere et navn og så skrive')),
             br(),
             chooserInput("mychooser", "Available frobs", "Selected frobs",
                          names, c('Sissel'), size = 10, multiple = TRUE),
             br()
             )
           ),
    column(5,
           wellPanel(
             h4('Andel av nyfødte pr år'),
             plotOutput('plot'),
             br(),
             h4('Lignende navn som det sist valgte navn'),
             plotOutput('wordplot')
             )
           ),
    column(4,
           wellPanel(
             h3('Mest populære navne'),
             h4(HTML('Se hvilke navne som var populære i et eller flere år. <br> <br>
                     Velg mellom gutte eller jente navne, samt hvilke perioder. <br> <br>
                     Navnene er rangeret etter popularitet, du kan arrangere navnene alfabetisk ved 
                     å trykke på "Navn" og tilbake til rangering ved å trykke på "Rangering".')),
             br(),
             radioButtons('gender', 'Velg guttenavn eller jentenavn',
                          c('Gut' = 'male',
                            'Jente' = 'female')
             ),
             br(),
             sliderInput("range", "Fødselsår", min = 1880, max = 2014, value = c(1880, 2014),
                         width = '80%', sep = ''),
             br(),
             dataTableOutput('table')
             )
           )
    )
))




