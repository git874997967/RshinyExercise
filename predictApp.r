library(shiny)
mpgData <- mtcars

ui <- fluidPage(pageWithSidebar(
  headerPanel("Predictions on diamond price"),
  sidebarPanel(
    sliderInput(
      inputId = 'carat',
      label = 'Carat',
      min = 1,
      max = 10,
      value = 2,
      step = 0.01
    ),
    sliderInput(
      inputId = 'table',
      label = 'Tabel',
      min = 10,
      max = 15,
      value = 12,
      step = 0.01
    ),
    sliderInput(
      inputId = 'depth',
      label = 'Depth',
      min = 10,
      max = 15,
      value = 12,
      step = 0.01
    ),
    sliderInput(
      inputId = 'length',
      label = 'X',
      min = 10,
      max = 15,
      value = 12,
      step = 0.01
    ),
    sliderInput(
      inputId = 'width',
      label = 'Y',
      min = 10,
      max = 15,
      value = 12,
      step = 0.01
    ),
    sliderInput(
      inputId = 'height',
      label = 'Z',
      min = 10,
      max = 15,
      value = 12,
      step = 0.01
    ),
    selectInput(
      inputId = 'cut',
      label = 'Cut',
      list   (
        "Good" = "Good",
        "Very Good" = "V.Good",
        "Ideal" = "Ideal"
      )
    ),
    selectInput(
      inputId = 'color',
      label = 'Color',
      list   (
        "D" = "D",
        "E" = "F",
        "G" = "G",
        "H" = "H",
        "I" = "I",
        "J" = "J",
        "K" = "K",
        "L" = "L"
      )
    ),
    selectInput(
      inputId = 'clarity',
      label = 'Clarity',
      list   (
        "IF" = "IF",
        "VVS1" = "VVS1",
        "VVS2" = "VVS2",
        "VS1" = "VS1",
        "VS2" = "VS2",
        "ST1" = "ST1",
        "ST2" = "ST2",
        "I1" = "I1",
        "I2" = "I2"
      )
    ),
    selectInput(
      inputId = 'cert',
      label = 'Cert',
      list(
        "GIA" = "GIA",
        "AGS" = "AGS",
        "EGL" = "EGL",
        "IGI" = "IGI",
        "HRD" = "HRD",
        "EGL" = "EGL",
        "USA" = "USA",
        "EGL INIFI" = "EGL INIFI",
        "EGL ISRAEL" = "EGL ISRAEL"
      )
    )
    
  ),
  mainPanel(
    tableOutput(outputId = 'diamondInfo'),
    tableOutput(outputId = 'results')
  )
))

server <- function(input, output, session) {
  output$diamondInfo = renderTable({
    diamondTest = data.frame(
      input$carat,
      input$cut,
      input$color,
      input$clarity,
      input$table,
      input$depth,
      input$cert,
      input$length,
      input$width,
      input$height
    )
    colnames(diamondTest) = c('carat',
                              'cut',
                              "color",
                              'clarity',
                              'table',
                              'depth',
                              'cert',
                              'x',
                              'y',
                              'z')
    diamondTest
  })
  output$results = renderTable({
    ##### get predict values from models
    result_multiLinear =  input$carat*input$height
    result_KNN =   input$height*input$table
    result_Forest = input$depth* input$width
    results = data.frame(result_multiLinear, result_KNN, result_Forest)
    colnames(results) = c("multiLinear", "KNN", "randomForest")
    results
    ## collectdata finished lack of chose should be done later
  })
}

shinyApp(ui, server)
