# Packages ----
library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(agridat)  # The package where the data comes from

# Loading data ----
Barley <- tibble(beaven.barley)

# ui.R ----
ui <- fluidPage(
  titlePanel('Barley Yield'),   # Add a title panel
  sidebarLayout(   # Make the layout a sidebarLayout
    sidebarPanel(
      selectInput(
        inputId = 'gen',   # Give the input a name "genotype"
        label = '1. Select genotype',   # Give the input a label to be displayed in the app
        choices = c('A' = 'a','B' = 'b','C' = 'c','D' = 'd','E' = 'e','F' = 'f','G' = 'g','H' = 'h'), 
        selected = 'a'   # Display 'A' and link to value 'a'.
        # Alternatively, use `choices = unique(Barley$gen)` to get the groups directly from the dataset.
      ),
      br(),
      selectInput(
        inputId = 'colour',
        label = '2. Select histogram colour',
        choices = sort(c('blue', 'green', 'red', 'purple', 'grey')), selected = 'grey'
      ),
      br(),
      sliderInput(
        inputId = 'bin',
        label = '3. Select number of histogram bins',
        min = 1,
        max = 25,
        value = c(10)
      ),
      br(),
      textInput(
        inputId = 'text',
        label = '4. Enter some text to be displayed', ''
      ),
      br(),
      actionButton(inputId = 'action', label = 'Go!'),
      radioButtons(inputId = 'radio', label = 'Radio Buttons', choices = c('A', 'B')),
      selectInput(inputId = 'select', label = 'select', choices = c('A', 'B')),
      sliderInput(inputId = 'slider', label = 'slider', value = 50, min = 1, max = 100),
      tags$div(
        style="color:red",
        tags$p("Visit them at:"),
        tags$a(href = "https://ourcodingclub.github.io", "Coding Club")
      )
    ),   # Inside the sidebarLayout, add a sidebarPanel
    mainPanel(
      plotOutput('plot'),
      tableOutput('mytable'),
      textOutput('mytext')
    )    # Inside the sidebarLayout, add a mainPanel
  )
)

# server .R ----
server <- function(input, output) {
  output$plot <- renderPlot(
    Barley %>% ggplot(aes(x = yield))+
      geom_histogram(
        bins = input$bin,
        fill = input$colour,
        data = Barley %>% filter(gen == input$gen),
        color = 'black'
      )+
      theme_bw()
  )
  
  output$mytext <- renderText(input$text)
  
  output$mytable <- renderTable(
    Barley %>% 
      filter(gen == input$gen) %>% 
      summarise(
        'Mean' = mean(yield),
        'Median' = median(yield),
        'STDEV' = sd(yield),
        'Min' = min(yield),
        'Max' = max(yield)
      )
  )
}

# Run the app ----
shinyApp(ui = ui, server = server)

















