library(tidyverse)
library(shiny)
library(bslib)
library(ggsankey)
library(patchwork)
library(bsicons)

data <- read_csv(here::here('data/u01_b35_data.csv'),
                 show_col_types = FALSE)

trt_options <- data |> pull(trt) |> unique() |> sort()

ae_options <- data |>
  count(ae, trt) |>
  count(ae) |>
  filter(n == 2) |> pull(ae)

main <- layout_sidebar(
  fillable = TRUE,
  sidebar = sidebar(
    selectizeInput(
      'select_ae',
      tooltip(
        span(
          "1. Select Adverse Events",
          bs_icon("info-circle")
        ),
        "The following contains a list of adverse events that are present in both treatment arms. Select one or more adverse events to visualize the flow of patients between grades for each treatment arm.",
        placement = "right"
      ),
      choices = ae_options,
      multiple = T,
      options = list(placeholder = 'Select an Adverse Event')
    ),
    actionButton(
      'btn_ae_visualize',
      tooltip(
        span(
          "Visualize",
          bs_icon("info-circle")
        ),
        "Clicking on the following button will generate a Sankey diagram for each selected adverse event.",
        placement = "right"
      ),
      icon = icon(name = 'chart-bar', lib = 'font-awesome')
    ),
    downloadButton(
      'report',
      tooltip(
        span(
          "Download Report",
          bs_icon("info-circle")
        ),
        "Clicking on the following button will generate a PDF report containing the Sankey diagrams for each selected adverse event.",
        placement = "right"
      ),
      icon = icon(name = 'chart-bar', lib = 'font-awesome')
    )
  ),
  card(
    card_header("B35 AE Explorer"),
    card_body(
      "The following application allows you to visualize the flow of patients between grades for each treatment arm. Select one or more adverse events from the dropdown menu and click on the 'Visualize' button to generate a Sankey diagram for each selected adverse event. The 'Download Report' button will generate a PDF report containing the Sankey diagrams for each selected adverse event."
    ),
    max_height = '250px'
  ),
  card(
    card_header('Results'),
    card_body(
      uiOutput('sankey_plots')
    ),
    height = '750px'
  )
)

ui <- page_fillable(
  theme = bslib::bs_theme(preset = 'shiny'),
  main
)

shinyApp(ui, function(input, output) {
  
  plots <- eventReactive(input$btn_ae_visualize, {
    
    map(input$select_ae, \(x) {
      
      plot1 <- make_sankey_diagram(data, trt = trt_options[1], ae = x)
      plot2 <- make_sankey_diagram(data, trt = trt_options[2], ae = x)
      
      plot1 / plot2
      
    }) |> set_names(nm = input$select_ae)
    
  })
  

  observeEvent(input$btn_ae_visualize, {
    
    req(plots())
    
    output$sankey_plots <- renderUI({
      
      pmap(list(plots(), 1:length(plots()), names(plots())), \(plot, i, title) {
        
        output[[paste0('plot', i)]] <- renderPlot({plot})
        
        tagList(h1(paste0(title)),
                plotOutput(paste0('plot', i), height = '800px'),
                br(),
                hr())
        
      })
    })
    
  })
  
  output$report <- downloadHandler(
    filename = \(x) {paste0('pro_ctcae_ae_sankey_', Sys.Date(), '.pdf')},
    content = function(file) {
      
      rmarkdown::render(
        'explore_ae_template.Rmd',
        output_file = file,
        params = list('plots' = plots()),
        envir = new.env(parent = globalenv())
      )
      
    }
  )

})
