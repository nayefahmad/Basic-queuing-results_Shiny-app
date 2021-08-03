# Queueing results app 

The app can be accessed here: https://nayef-ahmad.shinyapps.io/Basic-Queuing-Results/

Shiny app to draw interactive graphs to show basic results for M/M/c queues. Analyzing queue performance is vital to making decisions about capacity and resource allocation in service settings.

## Problem Statement
Let's say you're building a hospital emergency department (ED), and you need to decide how many beds to include in your design. To do so, you have to consider the demand for these beds, which depends on the number of patients that arrive every day (the 'arrival rate'), and the amount of time each patient requires for treatment (the 'service time').

To keep things very simple at first, let's say you expect the following pattern of demand:

> Average arrival rate: 4 patients per day

> Average service time: 6 hours or 0.25 days (so your 'service rate' is 1/0.25 = 4 patients per day)

**Given these parameters, how many beds should you include in your design?**
