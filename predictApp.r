library(shiny)
library(png)
cutInfo = read.csv('app2/cut.csv',
                   stringsAsFactors = F,
                   encoding = 'UTF-8')
colorInfo = read.csv('app2/color.csv', stringsAsFactors = F)
clarityInfo = read.csv('app2/clarity.csv', stringsAsFactors = F)
certInfo = read.csv('app2/cert.csv', stringsAsFactors = F)

ui <- fluidPage(
  titlePanel("Predictions on diamond price"),
  fluidRow(
    column(2,
           wellPanel(
             sliderInput(
               inputId = 'carat',
               label = 'Carat',
               min = 1,
               max = 10,
               value = 2,
               step = 0.01
             )
           )),
    column(2,
           wellPanel(
             sliderInput(
               inputId = 'table',
               label = 'Tabel',
               min = 10,
               max = 15,
               value = 12,
               step = 0.01
             )
           )),
    column(2,
           wellPanel(
             sliderInput(
               inputId = 'depth',
               label = 'Depth',
               min = 10,
               max = 15,
               value = 12,
               step = 0.01
             )
           )),
    column(2,
           wellPanel(
             sliderInput(
               inputId = 'length',
               label = 'X',
               min = 10,
               max = 15,
               value = 12,
               step = 0.01
             )
           )),
    column(2,
           wellPanel(
             sliderInput(
               inputId = 'width',
               label = 'Y',
               min = 10,
               max = 15,
               value = 12,
               step = 0.01
             )
           )),
    column(2,
           wellPanel(
             sliderInput(
               inputId = 'height',
               label = 'Z',
               min = 10,
               max = 15,
               value = 12,
               step = 0.01
             )
           )),
    fluidRow(
      column(3, wellPanel(
        selectInput(
          inputId = 'cut',
          label = 'Cut',
          list   (
            "Good" = "Good",
            "Very Good" = "Very Good",
            "Ideal" = "Ideal"
          )
        )
      )),
      column(3,
             wellPanel(
               selectInput(
                 inputId = 'color',
                 label = 'Color',
                 list   (
                   "D" = "D",
                   "E" = "E",
                   "F" = "F",
                   "G" = "G",
                   "H" = "H",
                   "I" = "I",
                   "J" = "J",
                   "K" = "K",
                   "L" = "L"
                 )
               )
             )),
      column(3,
             wellPanel(
               selectInput(
                 inputId = 'clarity',
                 label = 'Clarity',
                 list   (
                   "IF" = "IF",
                   "VVS1" = "VVS1",
                   "VVS2" = "VVS2",
                   "VS1" = "VS1",
                   "VS2" = "VS2",
                   "SI1" = "SI1",
                   "SI2" = "SI2",
                   "I1" = "I1",
                   "I2" = "I2"
                 )
               )
             )),
      column(3,
             wellPanel(
               selectInput(
                 inputId = 'cert',
                 label = 'Cert',
                 list(
                   "GIA" = "GIA",
                   "AGS" = "AGS",
                   "EGL" = "EGL",
                   "IGI" = "IGI",
                   "HRD" = "HRD",
                   "EGL USA" = "EGL USA",
                   "EGL INIFI" = "EGL INIFI",
                   "EGL ISRAEL" = "EGL ISRAEL"
                 )
               )
             ))
    ),
    fluidRow(column(7,
                    fluidRow(
                      wellPanel(
                        tags$div(
                          class = 'media',
                          imageOutput(
                            outputId = "cutImg",
                            width = "100%",
                            height = "80%"
                          ),
                          tags$div(class = 'media-body',
                                   h3(textOutput("cut")),
                                   p(textOutput('cutDesc')))
                          
                        ))
                    )))
  )
)

server <- function(input, output, session) {
  #cut rendering
  output$cut <- renderText({
    paste("Cut:", input$cut, seq = " ")
  })
  cutColumn = reactive({
    switch(
      input$cut,
      "Good" = 1,
      "Very Good" = 2,
      "Ideal" = 3
    )
  })
  output$cutDesc = renderText({
    cutInfo[1, cutColumn() + 1]
  })
  output$cutImg <-renderImage({
    return(list(
      src =paste('app2/',cutInfo[2,cutColumn()+1],sep=''),
      
      contentType = "image/png",
      alt = paste('app2/',cutInfo[2,cutColumn()+1],sep='')
    ))
    
  },deleteFile = FALSE)
  # output$cutImg <- renderImage({
  #   return(list(
  #     src = "app2/www/cut_VeryGood.png",
  #     contentType = "image/png",
  #     alt = "Face"
  #   ))
  #   
  # }, deleteFile = FALSE)
  
  #color rendering
  output$color = renderText({
    paste("Color:", input$color, seq = ' ')
  })
  colorColumn = reactive({
    switch(
      input$color,
      "D" = 1,
      "E" = 2,
      "F" = 3,
      "G" = 4,
      "H" = 5,
      "I" = 6,
      "J" = 7,
      "K" = 8,
      "L" = 9
      
    )
  })
  output$colorDesc = renderText({
    colorInfo[1, colorColumn() + 1]
  })
  # output$colorImg <- renderImage({
  #   return(list(
  #     src = "www/rstudio.png",
  #     contentType = "image/png",
  #     alt = "Face"
  #   ))
  #   
  # }, deleteFile = FALSE)
  #clarity rendering
  output$clarity = renderText({
    paste("Clarity:", input$clarity, seq = ' ')
  })
  clarityColumn = reactive({
    switch(
      input$clarity,
      "IF" = 1,
      "VVS1" = 2,
      "VVS2" = 3,
      "VS1" = 4,
      "VS2" = 5,
      "SI1" = 6,
      "SI2" = 7,
      "I1" = 8,
      "I2" = 9
      
    )
  })
  output$clarityDesc = renderText({
    clarityInfo[1, clarityColumn() + 1]
  })
  # cert rendering
  output$cert = renderText({
    paste("Cert:", input$cert, seq = ' ')
  })
  certColumn = reactive({
    switch(
      input$cert,
      "GIA" = 1,
      "AGS" = 2,
      "EGL" = 3,
      "IGI" = 4,
      "HRD" = 5,
      "EGL USA" = 6,
      "EGL INIFI" = 7,
      "EGL ISRAEL" = 8
      
    )
  })
  output$certDesc = renderText({
    certInfo[1, certColumn() + 1]
  })
  
  
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
    result_multiLinear =  input$carat * input$height
    result_KNN =   input$height * input$table
    result_Forest = input$depth * input$width
    results = data.frame(result_multiLinear, result_KNN, result_Forest)
    colnames(results) = c("multiLinear", "KNN", "randomForest")
    results
    ## collectdata finished lack of chose should be done later
  })
  
}

shinyApp(ui, server)
