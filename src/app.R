

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



# > prob of 0 people in system: ----------
p_0.prob <- function(lambda,  # arrival rate 
                     mu,  # service rate 
                     c){  # num servers 
      
      # output: p_0(n), the pdf of the r.v. n representing the 
      
      library("purrr")
      library("tidyverse")
      
      # define vars: 
      r <-  lambda/mu 
      rho <- lambda/(c*mu) 
      state.list <- 0:(c-1)
      
      
      first.term <- r^c/(factorial(c) * (1-rho))
      
      second.term <- 
            map_dbl(state.list, function(x){
                  r^x/factorial(x)
            }) %>% 
            sum
      
      return(1/(first.term + second.term))
      
}




# > avt time in system for MMC queue: -----
avg.tis_mmc <- function(lambda, 
                        mu, 
                        c,  # num servers 
                        p_0.function = p_0.prob  # function used to calculate p_0 
){
      
      # output: average time in system (TIS) including time in queue and 
      #     time in service 
      # Note: for ED modelling, if we define service time as start to 
      #     disposition time, then we expect most of the TIS to fall under time in service 
      #     The division between "queue" and "service" can be arbitrarily selected for this system 
      
      # define vars: 
      r <-  lambda/mu 
      rho <- lambda/(c*mu)
      
      W = 1/mu + 
            (((r^c)/(factorial(c) * (c*mu) * ((1-rho)^2))) * 
                   p_0.function(lambda, mu, c))
      
      return(W)
      
}





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
                  sliderInput(inputId = "servers",
                              label = "Number of Servers:",
                              min = 1,
                              max = 50,
                              value = 5),
                  
                  # Input: Slider that doesn't really do anything 
                  sliderInput(inputId = "serv.rate",
                              label = "Service rate (patients/server/day):",
                              min = 1,
                              max = 20,
                              value = 4)
                  
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
      
      # Graph of TIS vs utilization rate: ------
      # This expression that generates a histogram is wrapped in a call
      # to renderPlot to indicate that:
      #
      # 1. It is "reactive" and therefore should be automatically
      #    re-executed when inputs (input$bins) change
      # 2. Its output type is a plot
      
      output$distPlot <- renderPlot({
            
            # max visits determined by rho < mu * c 
            max.visits <- (input$servers * input$serv.rate) - 1  
            
            # create dataframe to plot: 
            df <- data.frame(visits = seq(1, max.visits, length.out = 100),
                             tis = map_dbl(seq(1, max.visits, length.out = 100), 
                                           avg.tis_mmc, 
                                           c = input$servers, 
                                           mu = input$serv.rate, 
                                           p_0.function = p_0.prob) *24) %>% 
                  mutate(rho = visits/(input$servers * input$serv.rate), 
                         tis = ifelse(is.nan(tis), 25, tis))
            
            
            
            # draw plot: 
            df %>% ggplot(aes(x=rho, y=tis)) + 
                  geom_line() + 
                  # scale_y_continuous(limits = c(0, 24)) +
                  scale_x_continuous(breaks = seq(0.0, 1.1, 0.1), 
                                     labels = seq(0.0, 1.1, 0.1)) + 
                  # geom_vline(xintercept = 0.9, 
                             # colour = "red") + 
                  labs(title = "Average time in system versus utilization rate (rho)", 
                       subtitle = paste0("Max number of visits: ", 
                                         max.visits), 
                       x = "Resource utilization", 
                       y = "Time in system (hours)") + 
                  theme_classic(base_size = 12)
            
            
            
            # mtcars %>%
            #       ggplot(aes(x = mpg)) + 
            #       geom_histogram(bins = input$servers) + 
            #       labs(title = paste0("Histogram of mtcars$mpg \nTest value: ", 
            #                           input$serv.rate, # reference to input with ID "test"
            #                           "\nMax visits (reactive): ",
            #                           max.visits, 
            #                           "\nTest imported fn (nonreactive): ",
            #                           testval.nonreactive))
            
      })
      
}




#*******************************************************************************
# 3) Call to shinyApp: ----------------
#*******************************************************************************

shinyApp(ui = ui, 
         server = server)


