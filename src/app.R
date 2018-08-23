

#*****************************************
# Playing with the Hello Shiny example
#*****************************************

library(shiny)

#TODO: ----------------------------------- 
# > where are we passing input to the function server( )? 

#*****************************************



#*****************************************
# 1) Define UI for app that draws a histogram ----
ui <- fluidPage(
      
      # App title ----
      titlePanel("Example 1"),
      h1("heading 1"),  # you can use this instead of titlePanel? 
      
      h3("H3 is fine without tags and so is code here"),
      
      # you can use HTML tags: --------------
      # display blockquote: 
      tags$blockquote("block quote requires tags - less common than h3(), h1() or code()"),
      
      # display code: 
      code("data.frame(a=1:10, b=1:10)"), 
      
      # display body text: 
      tags$body("\nlist of tags: http://shiny.rstudio.com/articles/html-tags.html"), 
      
      
      # customize sidebar: ----------------
      # Sidebar layout with input and output definitions ----
      sidebarLayout(
            
            # other layout options: http://shiny.rstudio.com/articles/layout-guide.html 
            # > fluidRow( )
            # > tabSetPanel( ) : to be used within mainPanel below
            # > navlistPanel( )
            # > navbarPage( ) 
            # > navbarMenu()
            
            
            # Sidebar panel for inputs ----
            sidebarPanel(
                  
                  # Input: Slider for the number of bins ----
                  sliderInput(inputId = "bins",
                              label = "Number of bins:",
                              min = 5,
                              max = 50,
                              value = 30),
                  
                  # Input: Slider that doesn't really do anything ----
                  sliderInput(inputId = "test",
                              label = "Test slider:",
                              min = 1,
                              max = 100,
                              value = 30)
                  
            ),
            
            # Main panel for displaying outputs ----
            mainPanel(
                  
                  # Output: Histogram ----
                  plotOutput(outputId = "distPlot")
                  
            )
      )
)



# 2) Define server logic required to draw a histogram ----
server <- function(input, output) {
      
      # Histogram of the Old Faithful Geyser Data ----
      # with requested number of bins
      # This expression that generates a histogram is wrapped in a call
      # to renderPlot to indicate that:
      #
      # 1. It is "reactive" and therefore should be automatically
      #    re-executed when inputs (input$bins) change
      # 2. Its output type is a plot
      output$distPlot <- renderPlot({
            
            x    <- faithful$waiting
            bins <- seq(min(x), max(x), 
                        length.out = input$bins + 1)  # this is where we reference the input that the user chooses using the slider 
            
            hist(x, 
                 breaks = bins, 
                 col = "#75AADB", 
                 border = "black",
                 xlab = "Waiting time to next eruption (in mins)",
                 main = paste0("Histogram of waiting times \n Test value: ", 
                               input$test))  # reference to input with ID "test"
            
      })
      
}


# 3) Call to shinyApp: ----------------
shinyApp(ui = ui, 
         server = server)






# 4) Run the app --------------
# runApp("App-1")
# paste this in console and then run, otherwise you get error: 
# ERROR: Can't call `runApp()` from within `runApp()`. If your application code contains `runApp()`, please remove it.

# alternative: 
# use "Run app button", or press Control+Shift+Enter

