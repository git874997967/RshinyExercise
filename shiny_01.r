library(shiny)
runExample("01_hello")      # a histogram
runExample("02_text")       # tables and data frames
runExample("03_reactivity") # a reactive expression
runExample("04_mpg")        # global variables
runExample("05_sliders")    # slider bars
runExample("06_tabsets")    # tabbed panels
runExample("07_widgets")    # help text and submit buttons
runExample("08_html")       # Shiny app built from HTML
runExample("09_upload")     # file upload wizard
runExample("10_download")   # file download wizard
runExample("11_timer")      # an automated timer
###example1
UI = shinyUI(pageWithSidebar(
  headerPanel("hello shiny"),
  sidebarPanel (
    sliderInput(
      inputId = 'obs',
      label = 'number of ovservations',
      min = 10,
      max = 1000,
      value = 500
    )
  ),
  mainPanel (plotOutput('distPlot'))
))
Server = shinyServer(function(input, output) {
  output$distPlot = renderPlot({
    dist = rnorm(input$obs)
    hist(dist)
  })
})
shinyApp(UI, Server)
#####example2
UI = shinyUI(pageWithSidebar(
  headerPanel("Shiny Text"),
  sidebarPanel(
    selectInput(
      inputId = 'dataset',
      label = 'chooice a dataset',
      choices = c('rock', 'pressure', 'cars')
    ),
    numericInput(inputId = 'obs', label = 'Number of ovservatrions to view', 10)
  ),
  mainPanel(#  summary and View means the result rendering to that
    verbatimTextOutput('Summary'),
    tableOutput('View'))
))
Server = shinyServer(function(input, output) {
  datasetInput = reactive({
    switch(
      input$dataset,
      'rock' = rock,
      'pressure' = pressure,
      'cars' = cars
    )
  })
  # generate the summary to the dataset
  output$Summary = renderPrint({
    dataset = datasetInput()
    summary(dataset)
  })
  output$View = renderTable({
    head(x = datasetInput(), input$obs)
  })
  
})
shinyApp(UI, Server)
####example3
UI = shinyUI(pageWithSidebar(
  headerPanel("Reactivity"),
  sidebarPanel(
    # varity of form widgets text select  numeric etc.
    # this part is for input info only with input Id and label to demonstrate
    textInput(
      inputId = 'caption',
      label = "Caption",
      value = "Data summary"
    ),
    selectInput(
      inputId = 'dataset',
      label = 'Choose a dataset',
      choices = c('rock', 'pressure', 'cars')
    ),
    numericInput(inputId = 'obs', label = 'Number of observations to view', 20)
  ),
  mainPanel(
    #default out put widgets for show only  outputId
    h3(textOutput(outputId = 'Caption')),
    verbatimTextOutput(outputId = "summary"),
    tableOutput(outputId = 'view')
    
  )
))
Server = shinyServer(function(input, output) {
  #****dataset input render
  #bind each option to the dataset
  datasetInput = reactive({
    switch(
      input$dataset,
      "rock" = rock,
      "pressure" = pressure,
      "cars" = cars
    )
  })
  #renderText will render the text message to target
  output$Caption = renderText({
    input$caption
  })
  output$summary = renderPrint({
    dataset = datasetInput()
    summary(dataset)
  })
  output$view = renderTable({
    head(x = datasetInput(), n = input$obs)
  })
  
})
shinyApp(UI, Server)
####example4
mpgData <- mtcars

mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

UI = shinyUI(pageWithSidebar(
  headerPanel("Mile Per Gallon"),
  sidebarPanel(
    # several widgets here includes select and checkBox
    selectInput(
      "variable",
      "Variable:",
      list(
        "Cylinders" = "cyl",
        "Transmission" = "am",
        "Gears" = "gear"
      )
    ),
    checkboxInput(
      inputId = 'outliners',
      label = 'Show outliners',
      value = FALSE
    )
  ),
  mainPanel(h3(textOutput(outputId = 'caption')),
            plotOutput(outputId = 'mpgPlot'))
))

Server = shinyServer(function(input, output) {
  formulaText = reactive({
    paste("mpg~", input$variable, sep = '')
  })
  output$caption = renderText({
    formulaText()
  })
  output$mpgPlot = renderPlot({
    boxplot(as.formula(formulaText()),
            data = mpgData,
            outline = input$outliners)
  })
  
})
shinyApp(UI, Server)
#example5
UI = shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("Sliders"),
  
  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(
    # Simple integer interval
    sliderInput("integer", "Integer:", 
                min=0, max=1000, value=500),
    
    # Decimal interval with step value
    sliderInput("decimal", "Decimal:", 
                min = 0, max = 1, value = 0.5, step= 0.1),
    
    # Specification of range within an interval
    sliderInput("range", "Range:",
                min = 1, max = 1000, value = c(200,500)),
    
    # Provide a custom currency format for value display, with basic animation
    sliderInput("format", "Custom Format:", 
                min = 0, max = 10000, value = 0, step = 2500,
                format="$#,##0", locale="us", animate=TRUE),
    
    # Animation with custom interval (in ms) to control speed, plus looping
    sliderInput("animation", "Looping Animation:", 1, 2000, 1, step = 10, 
                animate=animationOptions(interval=300, loop=T))
  ),
  
  # Show a table summarizing the values entered
  mainPanel(
    tableOutput("values")
  )
))
Server=shinyServer(function(input, output) {
  
  # Reactive expression to compose a data frame containing all of the values
  sliderValues <- reactive({
    
    # Compose data frame
    data.frame(
      Name = c("Integer", 
               "Decimal",
               "Range",
               "Custom Format",
               "Animation"),
      Value = as.character(c(input$integer, 
                             input$decimal,
                             paste(input$range, collapse=' '),
                             input$format,
                             input$animation)), 
      stringsAsFactors=FALSE)
  }) 
  
  # Show the values using an HTML table
  output$values <- renderTable({
    sliderValues()
  })
})
shinyApp(UI, Server)

