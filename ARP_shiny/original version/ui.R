library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Ideology of the ARP"),
  fluidRow(
      column(3,
      p("Interact with a model of the Assemblee Representative du Peuple of Tunisia. This model puts Tunisian MPs on a political spectrum based on their voting record in the current parliament from Bawsala's ",a(href='http://http://majles.marsad.tn/2014/','Marsad Majles'),
                      " data repository. It was created by Robert Kubinec from the University of Virginia using the emIRT package in R (Kosuke Imai, James Lo & Jonathan Olmsted, 2016)."),
      
      selectInput("var", 
                  label = "Choose a type of vote to predict",
                  choices = c("Yes vs. No","Yes/No vs. Abstain"),
                  selected = "Yes vs. No"),
      
#       sliderInput("range", 
#                   label = "Range of interest:",
#                   min = 0, max = 100, value = c(0, 100)),
      checkboxGroupInput("parties",label="Choose a party to display",
                  choices=c("All",party_types),
                  selected="All"),

      sliderInput('top_hjust',"Adjust MP names' position (top);",
                  min=-5,max=5,step=0.1,value=1.5),      
      sliderInput('bottom_hjust',"Adjust MP names' position (bottom)",
                  min=-5,max=5,step=0.1,value=-0.5),
      selectizeInput('MP_names',label="Select one or more MPs to display",choices=legis_names,multiple=TRUE)),
      column(9,
             plotOutput("ideology",height="600px"))),
    #fluidRow(verbatimTextOutput("outprint")),
    
    fluidRow(h3("Individual Model Results",align="center"),DT::dataTableOutput("nawab"))
))