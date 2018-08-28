

#*****************************************
# Playing with the Hello Shiny example
#*****************************************

library("shiny")
library("here")
library("tidyverse")


#TODO: ----------------------------------- 

#*****************************************

# 0) load functions: -------------------
# source(here("src", "testfn_function.R"))

# define test function: 
testfn <- function(int.vec) {
      quantile(int.vec, probs = c(0.9, .99))
}

# test the fn: 
testval.nonreactive <- testfn(rnorm(100))







#*****************************************
# 1) Define UI for app that draws a histogram ----
#*****************************************

ui <- fluidPage(
      
      # > App title ----
      titlePanel("Basic Queuing Theory Results"),

      h3("Problem statement"),  
      
      # you can use HTML tags: 
      # Reference: https://shiny.rstudio.com/articles/tag-glossary.html 
      # "Shiny provides a list of functions named tags. Each function in the 
      # list creates an HTML tag that you can use to layout your Shiny App."
      
      # div( ) creates a section of an HTML doc 
      tags$div(
            tags$p("First paragraph"), 
            tags$p("Second paragraph"), 
            tags$p("Third paragraph")
      ),
      
      # > customize sidebar: ----------------
      # >> Sidebar layout with input and output definitions ----
      sidebarLayout(
            
            # other layout options: http://shiny.rstudio.com/articles/layout-guide.html 
            # > fluidRow( )
            # > tabSetPanel( ) : to be used within mainPanel below
            # > navlistPanel( )
            # > navbarPage( ) 
            # > navbarMenu()
            
            
            # > Sidebar panel for inputs ----
            sidebarPanel(
                  
                  # Input: Slider for the number of bins 
                  sliderInput(inputId = "bins",
                              label = "Number of bins:",
                              min = 5,
                              max = 50,
                              value = 30),
                  
                  # Input: Slider that doesn't really do anything 
                  sliderInput(inputId = "test",
                              label = "Test slider:",
                              min = 1,
                              max = 100,
                              value = 30)
                  
            ),
            
            # Main panel for displaying outputs 
            mainPanel(
                  
                  # > Output: Histogram ----
                  plotOutput(outputId = "distPlot")
                  
            )
      )
)





#************************************************************************
# 2) Define server logic required to draw a histogram ----
#************************************************************************

server <- function(input, output) {
      
      # Histogram example  ----
      # This expression that generates a histogram is wrapped in a call
      # to renderPlot to indicate that:
      #
      # 1. It is "reactive" and therefore should be automatically
      #    re-executed when inputs (input$bins) change
      # 2. Its output type is a plot
      
      output$distPlot <- renderPlot({
            
            # number of bins: 
            # bins.from.input.slider <- seq(min(mtcars$mpg), max(mtcars$mpg), 
                                          # length.out = input$bins + 1)  # reference the input with ID "bins" 
            
            # testing the imported function:
            testval <- testfn(rnorm(100))[2] %>% 
                  unname %>% 
                  as.character
            
            mtcars %>%
                  ggplot(aes(x = mpg)) + 
                  geom_histogram(bins = input$bins) + 
                  labs(title = paste0("Histogram of mtcars$mpg \nTest value: ", 
                                      input$test, # reference to input with ID "test"
                                      "\nTest imported fn (reactive): ",
                                      testval, 
                                      "\nTest imported fn (nonreactive): ",
                                      testval.nonreactive))
            
      })
      
}




#*******************************************************************************
# 3) Call to shinyApp: ----------------
#*******************************************************************************

shinyApp(ui = ui, 
         server = server)


