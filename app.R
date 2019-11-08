########
# Shiny app to create a dynamically-filterable visualization of the diamonds app
########

# These bits get run before any of the rest of the code
library(shiny)
library(tidyverse)

# Limit the range of selectable hp to the actual range of hp
min.hp <- min(mtcars$hp)
max.hp <- max(mtcars$hp)

# Need a vector of axis variables as characters
axis_vars <- names(mtcars)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Mtcars viewer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      # This is a range slider (i.e. there's a max and min). It is set that way by "value" (the starting value), which is a 2-element vector
      sliderInput("hprange",
                  "Range of hp",
                  min = min.hp,
                  max = max.hp,
                  value = c(min.hp, max.hp)),
      
      
      # Select x and y variables
      selectInput(inputId = "xvar",
                  label = "X axis",
                  choices = axis_vars,
                  selected = "x"),
      
      selectInput(inputId = "yvar",
                  label = "Y axis",
                  choices = axis_vars,
                  selected = "y"),
      
      
      actionButton("go", 
                   "Go!",
                   icon = icon("thumbs-up")) # see available icons at http://fontawesome.io/icons/ and http://getbootstrap.com/components/#glyphicons
    ),
    
    # Show a plot of diamonds data frame. This output doesn't care what that plot is, only that it will be associated with output$diamonds_plot
    mainPanel(
      plotOutput("mtcars_plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Filter mtcars based on hp - this doesn't strictly need to be in reactive,
  # ...but maybe we want to expand the app later, in which case it'll be useful to have it in reactive()
  filt_dia <- reactive({
    mtcars %>%
      filter(hp >= min(input$hprange)) %>%
      filter(hp <= max(input$hprange))
  })
  
  # Make the plot
  # eventReactive listens for a change in state of input$go, and only runs the code when the state changes
  # Note use of aes_string, since input$xvar returns a string.
  p_mtcars <- eventReactive(input$go, {
    ggplot(filt_dia(), aes_string(x = input$xvar, y = input$yvar)) + # Note that you need () after filt_dia, since filt_dia() is a function to get the object you want, not the actual object
      geom_point()
  })
  
  
  # Create diagnostic output window to show what kind of output the double slider creates
  output$diagnostic <- renderText(
    input$hprange
  )
  
  # Create a dynamic plot plot
  # I moved the ggplot into its own reactive context.
  # Note that creating a ggplot object is very fast - it is actually drawing the plot that is slow.
  output$mtcars_plot <- renderPlot(
    p_mtcars()
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)