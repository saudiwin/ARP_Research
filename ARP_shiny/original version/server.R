library(shiny)
library(ggplot2)
library(magrittr)
library(emIRT)
source("Ggplot2_theme.R")
source("helpers.R",local=TRUE)
source("emIRT_graph.R")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$nawab <- DT::renderDataTable(model_data[[switch(input$var,"Yes vs. No"=1,"Yes/No vs. Abstain"=2)]],rownames=FALSE) 
    
#   data_plots <- reactive({
#     choice <- switch(input$var,"Yes vs. No"=1,"Yes/No vs. Abstain"=2)
#     plot_data <- make_data(type=choice)
#     return(plot_data)
#   })
    data_plots <- reactive({
      choice <- switch(input$var,"Yes vs. No"=1,"Yes/No vs. Abstain"=2)
      return(choice)
    })
  subset_filter <- reactive({
    if(!is.null(input$MP_names))) {
      return(input$MP_names)
  } else if("All" %in% input$parties) {
      return()
  } else {
      return(input$parties)
    }
  })
  subset_type_filter <- reactive({
    if(is.null(input$MP_names)) {
      return('party')
    } else {
      return('individual')
    }
  })


      output$ideology <- renderPlot({ plot(x=model_results[[data_plots()]],legis.names=model_data[[data_plots()]]$MP,
                                           parties=model_data[[data_plots()]]$Party,subset_name=subset_filter(),
                                           hjust_top=input$top_hjust,hjust_bottom=input$bottom_hjust,subset_type=subset_type_filter())
      })
#         output$outprint <- renderPrint({
#           cat(input$nawab_rows_all,sep=", ")
#           cat("\n")
#           cat(input$nawab_rows_current,sep=", ")
#         })
                      
  })