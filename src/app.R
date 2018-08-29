

#*****************************************
# Playing with the Hello Shiny example
#*****************************************

library("shiny")
library("here")
library("tidyverse")


#TODO: ----------------------------------- 
# > separate tabs for formulas and assumptions

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
                              label = "Number of Servers (c):",
                              min = 1,
                              max = 50,
                              value = 1),
                  
                  # Input: Slider that doesn't really do anything 
                  sliderInput(inputId = "serv.rate",
                              label = "Avg. Service rate (mu = patients/server/day):",
                              min = 1,
                              max = 20,
                              value = 4, 
                              step = 0.5), 
                  
                  # Input: cutoff of y-axis: 
                  numericInput(inputId = "ymax", 
                               label = "Cut off y-axis at:", 
                               value = 50), 
                  
                  # Input: vertical line to highlight arrivals rate: 
                  numericInput(inputId = "arrivals", 
                               label = "Avg. Arrivals rate (red line):", 
                               value = 3.5, 
                               step = 0.2)
                  
            ),
            
            # add main panel: 
            mainPanel(
                  tabsetPanel(type = "tabs", 
                              tabPanel("Plot", plotOutput("plot")), 
                              tabPanel("Description",
                                       
                                       h3("Problem Statement"), 
                                       
                                       tags$div(
                                             tags$p("Let's say you're building a hospital emergency department, and you need to decide how many beds to include in your design. To do so, you have to consider the demand for these beds, which depends on the average number of patients that arrive every day (the 'arrival rate'), and the average amount of time that they require a bed for treatment (the 'service time')."), 
                                             tags$p("To keep things very simple at first, let's say you expect that at most, the average arrival rate is 4 patients per day. Also, you expect that the average service time is about 6 hours (so your 'service rate' is 24/6 = 4 patients per day)."), 
                                             tags$p("Given these parameters, how many beds should you include in your design?")
                                       ),
                                       
                                       h3("Naive solution"),
                                       
                                       tags$div(
                                             tags$p("Well, this is easy - obviously, all you need is 1 bed, right? Even if 4 people show up, they will each require about 6 hours of service per day, so 1 bed can serve all 4 of them."), 
                                             tags$p("All you have to do is solve the equation: Supply for beds = Demand for beds"), 
                                             tags$p("In this case: " ), 
                                             tags$p("==> 4 patients arriving per day * 0.25 days service time (demand) = x beds * 1 day of capacity (supply)"), 
                                             tags$p("==> Therefore x = 1"), 
                                             tags$p("And we're done, right? Well, not quite. You might get into some serious trouble if you use this approach. ")
                                       ), 
                                       
                                       h3("Queuing theory insights"), 
                                       
                                       tags$div(
                                             tags$p("This is one of the fundamental insights of queuing theory: system performance rapidly deteriorates as the resource utilization rate approaches 100%. If arrivals and service times are non-deterministic, there is always a tradeoff between average time in system and utilization rate."), 
                                             tags$p("Take a look at how long your patients will be spending in your emergency department if you only include 1 bed: as the average number of arrivals approaches 4 per day, the average wait time reaches unacceptably high levels - over 40 hours. "),
                                             tags$p("Why is this happening? Because even though on average there are 4 patients a day, there will often be more. And even though on average the service time is 6 hours, there are many cases where it is higher. Finally, it is not true that patients arrive sequentially, with a new arrival ocurring only when the current patient leaves. Therefore, even though the service time may be somewhere around 6 hours, many patients will spend a lot of time waiting in a queue, as all the arrivals ahead of them get service.")
                                       ), 
                                       
                                       h3("Conclusion"), 
                                       
                                       tags$div(
                                             tags$p("In actual applications, it is very important to consider this tradeoff between utilization rate and average time in system. If you play around with the parameters for a bit, you'll find that you have 2 options for keeping the average time in system at around 6-8 hours: "), 
                                             tags$p("1. Increase number of servers to 2, while keeping service rate constant at 4 patients per day."), 
                                             tags$p("2. Increase service rate to about 6.5 patients per day while keeping number of servers constant at 1. ")
                                       )
                              ), 
                              tabPanel("Assumptions", 
                                       
                                       h3("M/M/c queue assumptions"), 
                                       tags$div(
                                             tags$p("1. Steady state: the system is assumed to be in a steady state - that is, the arrival rate and service rate have stayed constant over a long period, so that system behaviour stabilizes."), 
                                             tags$p("2. Arrivals are Poisson distributed (therefore interarrival times are exponentially distributed). This is a standard assumption for modelling arrivals to hospital emergency departments, although often one considers only a particular time window - e.g. 10:00 AM to 12:00 PM"), 
                                             tags$p("3. Service times are exponentially distributed. Again, this is a common assumption, and in some cases matches data closely. For hospital EDs, though, it is probably best to consider it an approximation until you test for it."), 
                                             tags$p("4. Queue discipline: first come first served"), 
                                             tags$p("5. Maximum queue size: infinite. In reality, there are limits to waiting spaces. When the M/M/c model shows very high time in system results, you can interpret this as meaning that in reality, waiting spaces will start to overflow, and the system will be operating very poorly. To explicitly model limited wait times, look up M/M/c/K queuing models. "), 
                                             tags$p("6. Balking and reneging do not occur. Under this assumption, customers do not leave until they have been served, and they join the queue no matter how long it currently is. In reality, both these behaviours occur, so average time in system will be lower than suggested by the M/M/c model. However, note that this is still a system failure, because the system is failing to serve customers, and they leave in frustration. ") 
                                       ), 
                                       
                                       h3("Relaxing the assumptions"), 
                                       
                                       tags$div(
                                             tags$p("In many cases, relaxing these assumptions means that system performance will become worse. Hence, results from M/M/c queuing model can be considered a best-case scenario.")
                                       )
                                       
                              ), 
                              tabPanel("References", 
                                       
                                       tags$div(
                                             tags$p("1. Gross et al. Fundamentals of Queueing Theory. 2008, p. 69."), 
                                             tags$p("2. Hall, Randolph. Patient Flow: Reducing delay in healthcare delivery. 2013, p. 365"), 
                                             tags$p("3. Zai et al. 'Queuing Theory to Guide the Implementation of a Heart Failure Inpatient Registry Program'. J Am Med Inform Assoc., 2009"), 
                                             tags$p("4. Wiler et al. 'An emergency department patient flow model based on queueing theory principles'. Acad Emerg Med., 2013")
                                       )
                                       
                                       
                              )
                        )
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
      
      output$plot <- renderPlot({
            
            # max visits determined by rho < mu * c 
            max.visits <- (input$servers * input$serv.rate) - .1  
            
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
                  scale_y_continuous(limits = c(0, input$ymax)) +
                  scale_x_continuous(breaks = seq(0.0, 1.1, 0.1), 
                                     labels = seq(0.0, 1.1, 0.1)) + 
                  
                  geom_hline(yintercept = 24,
                             colour = "grey90") +
                  
                  geom_vline(xintercept = input$arrivals/(input$servers * input$serv.rate), 
                             colour = "red") + 
                  
                  labs(title = "Average time in system versus utilization rate", 
                       subtitle = paste0("For given service rate and number of servers, utilization increases as avg. arrivals increases\nMax arrival rate per day: ", 
                                         max.visits), 
                       x = "Resource utilization", 
                       y = "Average time in system (hours)") + 
                  theme_classic(base_size = 18)
            
      }, 
      width = 800, 
      height = 600)
      
}




#*******************************************************************************
# 3) Call to shinyApp: ----------------
#*******************************************************************************

shinyApp(ui = ui, 
         server = server)


