# Application that get the data from french hospital 
# get daily changes and plot results

library(shiny)
library (RCurl)
library(tidyverse)
library(lubridate)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Données covid-19"),
  
  # Sidebar with a 3 select input
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Choisissez la variable qui vous intéresse :", 
                  choices=c("nombre de décès",
                            "nombre de patients hospitalisés",
                            "nombre de patients en réanimation",
                            "nombre de retour à domicile"
                  )
      ),
      selectInput("region", "Choisissez la région :", 
                  choices=c("France", 
                            "France Métropolitaine",
                            "Bretagne et 44",
                            "Ile et vilaine",
                            "Loire atlantique",
                            "Côte d'armor")
      ),
      selectInput("cumul", "Voulez-vous des données cumulées :", 
                  choices=c("non", "oui")
      )
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # get data from data.gouv.fr
  dataCovidreact <- reactive({
    URLDataCovid <- getURL("https://www.data.gouv.fr/fr/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7")
    URLDataCovid <- getURL(strsplit(URLDataCovid, '"')[[1]][4])
    dataCovid <- read.csv (text = URLDataCovid, sep = ";")
    
    # remove data differanciating sexes
    dataCovid <- filter(dataCovid, sexe == 0)
    
    # transform the date
    dataCovid$jour <- parse_date_time(dataCovid$jour, orders = "ymd")
    dataCovid
    
  })
  
  output$distPlot <- renderPlot({
    
    # parametres
    variable = input$variable
    cumul = input$cumul
    region = input$region
    
    # get data
    dataCovid <- dataCovidreact()
    
    # Data for 
    variables = c("hosp", "rea",  "rad",  "dc")
    
    FranceMetro = c("01",  "02",  "03",  "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "21", "22", "23", "24", "25", "26", "27", "28", 
                    "29", "2A", "2B", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", 
                    "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", 
                    "81", "82", "83", "84", "85", "86", "87", "88", "89", "90", "91", "92", "93", "94", "95")
    
    Bretagne = c("44","56","29","35", "22")
    
    filterParam <- switch (region,
                           "France Métropolitaine" = FranceMetro,
                           "Bretagne et 44" = Bretagne,
                           "Ile et vilaine" = "35",
                           "Loire atlantique" = "44",
                           "Côte d'armor" = "22")
    
    label = variable
    
    variable <- switch (variable,
                        "nombre de patients hospitalisés" = "hosp",
                        "nombre de patients en réanimation" = "rea",
                        "nombre de retour à domicile" = "rad",
                        "nombre de décès" = "dc"
    )
    
    label = paste(switch (cumul,
                           non = "Variation journalière du",
                           oui = "Cumul du")
                  , label)
    
    if (region != "France"){
      dataCovid <- filter(dataCovid, as.character(dep) %in% filterParam)
    }
    
    deadPerDay <- dataCovid %>%
      group_by(jour) %>%
      summarise_at( .vars = variables, .funs = sum)
    
    if (cumul == "non")
      deadPerDay[ , variable] <-  deadPerDay[ , variable] - c(0, unlist(deadPerDay[1:(nrow(deadPerDay)-1) , variable]))
    
    deadPerDay$joursemaine <- weekdays(deadPerDay$jour)
    
    lines <- deadPerDay$jour[deadPerDay$joursemaine == "lundi"]
    
    if (min(deadPerDay[ , variable]) < 0){
      minValue <- min(deadPerDay[ , variable])
    } else {
      minValue <- 0
    } 
    
    
    ggplot(deadPerDay, aes_string(x = "jour", y = variable)) +
      geom_vline(xintercept = lines, linetype="dotted", 
                 size = 1) +
      geom_point() +
      ylim(minValue, NA) +
      geom_smooth() +
      ylab(label) 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

