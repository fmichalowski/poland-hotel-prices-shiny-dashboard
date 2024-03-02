library(shiny)
library(plotly)
library(purrr)
library(ggplot2)
library(readxl)
library(shinythemes)
library(bslib)
library(rsconnect)
library(rlang)

# Wczytywanie danych
path1 <- as_string(setwd(getwd()))
path2 <- as_string('/data.xlsx')
path <- as_string(paste(path1, path2, sep = ''))
data <- read_excel(path)
dataOg <- data
dataOg$City <- with(dataOg, reorder(City, Reviews, sum, decreasing = TRUE))

# Podział danych na podgrupy
dataWa <- data[data$City == 'Warsaw',]
dataKr <- data[data$City == 'Cracow',]
dataWr <- data[data$City == 'Wroclaw',]
dataLo <- data[data$City == 'Lodz',]
dataPo <- data[data$City == 'Poznan',]
dataWa$City <- as.factor(dataWa$City)
dataKr$City <- as.factor(dataKr$City)
dataWr$City <- as.factor(dataWr$City)
dataLo$City <- as.factor(dataLo$City)
dataPo$City <- as.factor(dataPo$City)

dataList <- list("Total" = dataOg,
                 "Warsaw" = dataWa,
                 "Cracow" = dataKr,
                 "Wroclaw" = dataWr,
                 "Lodz" = dataLo,
                 "Poznan" = dataPo)

# Interfejs użytkownika
ui <- fluidPage(
  theme = shinytheme('darkly'),
  navbarPage("Hotels",
             tabPanel("Main plot",
                      titlePanel("Accommodation offers"),
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          helpText("Using the options below, you can define the dataset being analyzed."),
                          selectInput("selectCity", h4("The choice of city:"), 
                                      choices = c("Total", "Warsaw", "Cracow", "Wroclaw", "Lodz", "Poznan"),
                                      selected = "Total"),
                          
                          sliderInput(inputId='Price', "Price per night (in PLN):",
                                      min = 100, max = 4500, step = 10,
                                      value = c(100, 4500), ticks = FALSE),
                          
                          sliderInput(inputId='Rating', "User rating:",
                                      min = 1, max = 10, value = c(1, 10)),
                          
                          sliderInput(inputId='Reviews', "Number of reviews:",
                                      min = 0, max = 25000, value = c(10, 25000),
                                      ticks= FALSE, step = 10),
                          helpText("Using the option below, you can select the variable to be displayed on the main chart."),
                          varSelectInput('variable', 'Variable selection:',
                                         data = data[2:4], selected = "Price"),
                          
                          submitButton("Submit")
                        ),
                        mainPanel(
                          fluidRow(
                            column(
                              h4("About dataset:"),
                              p("The data has been scraped from the website of a company offering online accommodation services. This agency acts as an intermediary, presenting users with offers from many hotels, hostels, and guesthouses in one place."),
                              p("The data pertains to available accommodations for 2 people in hotels, hostels, and guesthouses during the weekend of February 3-4, 2024, in the five largest cities in Poland. The database comprises 3079 records and 4 variables."),
                              em(div("City - the city where the accommodation is located"),
                                 div("Price - the price for the accommodation (in PLN)"),
                                 div("Rating - average user rating on the portal (on a scale from 1 to 10)"),
                                 div("Reviews - the number of reviews provided by users")),
                              hr(),
                              verbatimTextOutput('out1'),
                              plotOutput(outputId = "mainPlot"),
                              h4("Descriptive statistics:"),
                              verbatimTextOutput("summary"),
                              width = 12
                            )
                          )
                        )
                      )
             ),
             tabPanel("Comparison of variables",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          helpText("Using the options below, you can select the variables to be displayed on the x-axis and y-axis."),
                          selectInput("x", h4("X-axis:"), 
                                      choices = c("Price", "Rating", "Reviews"),
                                      selected = "Price"),
                          selectInput("y", h4("Y-axis:"), 
                                      choices = c("Price", "Rating", "Reviews"),
                                      selected = "Price"),
                          helpText("Using the option below, you can decide whether the chart should display one regression line or multiple regression lines."),
                          checkboxInput("checkbox", "Single regression line", value = TRUE),
                          submitButton("Submit")
                        ),
                        mainPanel(
                          fluidRow(
                            column(
                              h4("Comparison of variables"),
                              p("In this tab, you can analyze the relationships between variables and observe the trajectory of the regression line."),
                              p("You can define the range of the dataset analyzed in the main chart tab."),
                              hr(),
                              plotOutput(outputId = "distPlot1"),
                              h4("Descriptive statistics:"),
                              verbatimTextOutput("summary2"),
                              width = 12
                            )
                          )
                        )
                      )
             )
  )
)

# Serwer
server <- function(input, output) {
  datasetInput <- reactive({
    ## Wybór miasta
    dataset <- switch(input$selectCity,
                      "Total" = dataOg,
                      "Warsaw" = dataWa,
                      "Cracow" = dataKr,
                      "Wroclaw" = dataWr,
                      "Lodz" = dataLo,
                      "Poznan" = dataPo)
    
    ## Wybór przedziału cenowego
    dataset <- dataset[(dataset$Price >= input$Price[1]) & (dataset$Price <= input$Price[2]),]
    
    ## Wybór przedziału ocen
    dataset <- dataset[(dataset$Rating >= input$Rating[1]) & (dataset$Rating <= input$Rating[2]),]
    
    ## Wybór przedziału opinii
    dataset <- dataset[(dataset$Reviews >= input$Reviews[1]) & (dataset$Reviews <= input$Reviews[2]),]
    
    return(dataset)
  })
  
  output$distPlot1 <- renderPlot({
    dataset <- datasetInput()
    dataset$City <- factor(dataset$City)
    color <- switch(input$selectCity,
                    'Total' = '#428BCA',
                    "Warsaw" = '#F58D86',
                    "Cracow" = '#B1B32F',
                    "Wroclaw" = '#2FC893',
                    "Lodz" = '#2FBCF4',
                    "Poznan" = '#E885F1')
    colorp <- switch(input$selectCity,
                     "Warsaw" = '#F58D86',
                     "Cracow" = '#B1B32F',
                     "Wroclaw" = '#2FC893',
                     "Lodz" = '#2FBCF4',
                     "Poznan" = '#E885F1')
    if (input$checkbox == TRUE){
      if (length(unique(dataset$City)) != 1)
      {k <- ggplot(dataset, aes_string(x=input$x, y=input$y, color="City", group = 1)) +
        geom_point(alpha = 1/10) +
        geom_smooth(colour = color)  +
        theme(axis.text=element_text(size=12, face="bold", colour='#F2FFFF'),
              axis.title=element_text(size=14, face="bold", colour='#F2FFFF')) +
        theme(plot.background = element_rect(fill = "#303030")) +
        theme(panel.background = element_rect(fill = "#303030")) +
        theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#428BCA")) +
        theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#428BCA")) +
        theme(legend.position = "none")}
      else
      {k <- ggplot(dataset, aes_string(x=input$x, y=input$y, color="City", group = 1)) +
        geom_point(alpha = 1/10, colour=color) +
        geom_smooth(colour = color)  +
        theme(axis.text=element_text(size=12, face="bold", colour='#F2FFFF'),
              axis.title=element_text(size=14, face="bold", colour='#F2FFFF')) +
        theme(plot.background = element_rect(fill = "#303030")) +
        theme(panel.background = element_rect(fill = "#303030")) +
        theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#428BCA")) +
        theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#428BCA"))}
    }
    if (input$checkbox == FALSE)
    {
      if (length(unique(dataset$City)) != 1)
      {k <- ggplot(dataset, aes_string(x=input$x, y=input$y, color="City")) +
        geom_point(alpha = 1/10) +
        geom_smooth()  +
        theme(axis.text=element_text(size=12, face="bold", colour='#F2FFFF'),
              axis.title=element_text(size=14, face="bold", colour='#F2FFFF')) +
        theme(plot.background = element_rect(fill = "#303030")) +
        theme(panel.background = element_rect(fill = "#303030")) +
        theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#428BCA")) +
        theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#428BCA")) +
        theme(legend.position = "none")}
      else
      {k <- ggplot(dataset, aes_string(x=input$x, y=input$y, color="City", group = 1)) +
        geom_point(alpha = 1/10, colour=color) +
        geom_smooth(colour = color)  +
        theme(axis.text=element_text(size=12, face="bold", colour='#F2FFFF'),
              axis.title=element_text(size=14, face="bold", colour='#F2FFFF')) +
        theme(plot.background = element_rect(fill = "#303030")) +
        theme(panel.background = element_rect(fill = "#303030")) +
        theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#428BCA")) +
        theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#428BCA"))}
    }
    k
  })
  
  output$mainPlot <- renderPlot({
    data1 <- datasetInput()
    color <- switch(input$selectCity,
                    "Warsaw" = '#F58D86',
                    "Cracow" = '#B1B32F',
                    "Wroclaw" = '#2FC893',
                    "Lodz" = '#2FBCF4',
                    "Poznan" = '#E885F1')
    
    if (length(unique(data1$City)) != 1) {
      p <- ggplot(data1, aes(x=City, y=!!input$variable, fill=City)) +
        geom_violin(alpha=0.5, linewidth = 0) +
        theme(axis.text=element_text(size=12, face="bold", colour='#F2FFFF'),
              axis.title=element_text(size=14, face="bold", colour='#F2FFFF')) +
        theme(plot.background = element_rect(fill = "#303030")) +
        theme(panel.background = element_rect(fill = "#303030")) +
        theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#428BCA")) +
        theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#428BCA")) +
        theme(legend.position = "none")
    } else {
      p <- ggplot(data1, aes(x=!!input$variable)) +
        geom_density(fill=color, adjust=1.5, alpha=0.5, linewidth = 0) +
        ylab('') +
        theme(axis.text=element_text(size=12, face="bold", colour='#F2FFFF'),
              axis.title=element_text(size=14, face="bold", colour='#F2FFFF')) +
        theme(plot.background = element_rect(fill = "#303030")) +
        theme(panel.background = element_rect(fill = "#303030")) +
        theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#428BCA")) +
        theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#428BCA"))
    }
    
    p + scale_y_continuous(labels = scales::comma)
  })
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset[1:4])
  })
  output$summary2 <- renderPrint({
    dataset <- datasetInput()
    summary(dataset[1:4])
  })
  
}

# Uruchamianie aplikacji Shiny
shinyApp(ui = ui, server = server)