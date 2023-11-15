library(shiny)
library(tidyverse)
library(plotly)
library(bslib)

################
### Initialising
################

# Define Bootstrap theme ----
custom_theme <- bs_theme(
  version = 5,
  preset = "shiny"
)

#############
### Define UI
#############

ui <- fluidPage(
  
  theme = custom_theme,
  
  # The Plant Logo
  tags$figure(
    class = "centerFigure",
    tags$img(
      src = "https://theplant.maastrichtuniversity.nl/wp-content/uploads/2023/04/cropped-HomePage_UMNew2-1536x369.png",
      height = 50,
      alt = "Logo The Plant @ FASoS"
    )
  ),
  
  # Header
  headerPanel("Dataset Explorer"),
  
  p("A simple app for exploratory data analysis. The output in the
    'Summary' and 'Raw Output' tabs is based on the full dataset. The 
    controls in the sidebar are for the 'Plot' tab. The
    'Data Table' tab shows a maximum of 15 rows of the dataset."),
  
  # Sidebar
  sidebarLayout(
    
    sidebarPanel(
      fileInput("upload", label = "Please upload a .csv file", accept = c(".csv", ".tsv")),
      sliderInput("sampleSize", "Plot sample size (n)", min = 1, max = 100,
                  value = 1, step = 20, round = 0),
      radioButtons("sampleType", "Plot sample type",
                   choices = list("Random n" = "random", "First n" = "first")),
      numericInput("sampleSeed", "Sample seed", value = 1),
      
      selectInput("x", "X", choices = NULL),
      selectInput("y", "Y", choices = NULL),
      
      # only allow non-numeric variables for color
      selectInput("color", "Color", "None"),
      
      # checkbox for filtering out NA values
      checkboxInput("nona", "Exclude Na values", FALSE),
      
      p("Jitter and smoothing are only available when two numeric variables 
        are selected."),
      checkboxInput("jitter", "Jitter"),
      checkboxInput("smooth", "Smooth")
    ),
    
    # Main Panel
    mainPanel(
      # Output: Tabset
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotlyOutput("plot")),
                  #tabPanel("Data Snippet", verbatimTextOutput("snippet")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Raw Output", verbatimTextOutput("str")),
                  tabPanel("Data Table", tableOutput("head"))
      )
    )
  )
)

#####################
# Define server logic
#####################

server <- function(input, output, session) {
  
  showModal(modalDialog(
    title = "Explore your data",
    tags$figure(
      class = "centerFigure",
      tags$img(
        src = "https://theplant.maastrichtuniversity.nl/wp-content/uploads/2023/04/cropped-HomePage_UMNew2-1536x369.png",
        height = 50,
        alt = "Logo The Plant @ FASoS"
      )
    ),
    "Please start by uploading a comma-separated data file (file extension .csv).",
    easyClose = TRUE,
    footer = modalButton("Let's start!"),
  ))
  
  # Process csv file input
  raw_df <- reactive({
    req(input$upload)
    
    ext <- tools::file_ext(input$upload$name)
    input_df <- switch(ext,
      csv = vroom::vroom(input$upload$datapath, delim = ","),
      tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
      validate("Invalid file; Please upload a .csv or .tsv file")
    )
    colnames(input_df) <- make.names(colnames(input_df), unique=TRUE)
    input_df
  })

  # Pre-compute some variables to be used by app
  not_numeric <- reactive({
    req(raw_df())
    sapply(names(raw_df()), function(x) !is.numeric(raw_df()[[x]]))
  })
  
  df <- reactiveVal(raw_df)

  # Update input controls when file has been uploaded
  observeEvent(raw_df(), {
    updateSliderInput(session, inputId = "sampleSize", "Plot sample size (n)", min = 1, max = nrow(raw_df()),
      value = min(1000, nrow(raw_df())), step = nrow(raw_df()) / 50)
    updateSelectInput(session, inputId = "x", choices = names(df()))
    updateSelectInput(session, inputId = "y", choices = c("None", names(df())))
    updateSelectInput(session, inputId = "color", choices = c("None", names(df())[not_numeric()]))
  })

  # Generate data summaries
  output$summary <- renderPrint({
    summary(raw_df())
  })
  output$str <- renderPrint({
    str(raw_df())
  })
  
  # Get new dataset sample for plotting
  idx <- reactive({
    req(raw_df())
    if (input$sampleType == "first") {
      1:input$sampleSize
    } else {
      set.seed(input$sampleSeed)
      sample(nrow(raw_df()), input$sampleSize)
    }
  })
  
  df <- reactive({
      req(raw_df())
      df <- raw_df()[idx(), , drop = FALSE]
  })
  
  # Get filtered data subset without Na values
  # if checkbox "nona" is ticked
  subset_df <- reactive({
    req(input$x, input$y)
    if (input$nona && input$y == "None") {
      return(subset(df(), !is.na(df()[,input$x])))
    } else if (input$nona && input$y != "None") {
      return(subset(df(), (!is.na(df()[,input$x])) & (!is.na(df()[,input$y]))))
    } else {
      return(df())
    }
  })

  # Get head of selected data
  output$head <- renderTable({
    head(df(), n = 15)
  })
  
  # Get plot type
  # * 2: both numeric variables
  # * 1: one numeric, one non-numeric variable
  # * 0: both non-numeric variables
  # * -1: only one variable provided
  plot_type <- reactive({
    req(input$x, input$y)
    if (input$y != "None")
      is.numeric(raw_df()[[input$x]]) + is.numeric(raw_df()[[input$y]])
    else
      -1
  })
    
  # Create plot
  output$plot <- renderPlotly({
    if (plot_type() == 2) {
      ### Case 1 - x and y are both numeric variables: SCATTER PLOT
      ### also allow for color, jitter & smoothing
      p <- ggplot(subset_df(), aes_string(x = input$x, y = input$y))
      
      if (input$jitter)
        p <- p + geom_jitter(alpha = 0.5)
      else
        p <- p + geom_point(alpha = 0.5)
      
      if (input$smooth)
        p <- p + geom_smooth()
      
      # color change
      if (input$color != "None")
        p <- p + aes_string(color = input$color)
    } 
      
      ###  Case 2 - One numeric var, one character var: BOXPLOT
      ### allow color, don't allow jitter or smoothing    
      else if (plot_type() == 1) {
      p <- p <- ggplot(subset_df(), aes_string(x = input$x, y = input$y)) + 
        geom_boxplot()
      
      # fill change
      if (input$color != "None")
        p <- p + aes_string(fill = input$color)
    }
    
    ### Case 3 - x and y are both character values: HEATMAP
    ### don't allow color, jitter or smoothing
    else if (plot_type() == 0) {
      temp_df <- reactive(subset_df()[, c(input$x, input$y), drop = FALSE] %>%
                            group_by(across()) %>%
                            summarize(count = n())
      )
      p <- ggplot(temp_df(), 
                  mapping = aes_string(x = input$x, y = input$y, fill = "count")) +
        geom_tile() +
        scale_fill_gradient(low = "#e7e7fd", high = "#1111dd")
    }
    
    ### Case 4 - only one variable: HISTOGRAM or BAR or LINE
    ### allow color, don't allow jitter or smoothing
    else {
      p <- ggplot(subset_df(), aes_string(x = input$x))
      
      if (is.numeric(raw_df()[[input$x]])){
        updateCheckboxInput(session, "nona", value = TRUE)
        p <- p + geom_histogram()
      }
      else
        p <- p + geom_bar()
      
      # fill change
      if (input$color != "None")
        p <- p + aes_string(fill = input$color)
    }
    
    # Add title
    if (plot_type() >= 0) {
      p <- p + labs(title = paste(input$y, "vs.", input$x))
    } else {
      p <- p + labs(title = paste("Distribution of", input$x))
    }
    
    # Add styling
    p <- p +
      theme_bw(base_family = "Helvetica Neue, Helvetica, Arial, sans-serif") +
      theme(plot.title = element_text(size = rel(1.4), face = "bold", hjust = 0.5),
        axis.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.1))
        ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # x axis labels rotated and horizontally right justified

    plotly::ggplotly(p, height = 500)
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
