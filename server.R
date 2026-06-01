source("modules/upload.R")
source("modules/visualization.R")
source("modules/models.R")
source("modules/variance.R")

server <- function(input, output, session) {
  data <- reactive({ req(input$file); read.csv(input$file$datapath) })

  cataChoices <- reactive({
    req(data())
    is_categorical <- sapply(data(), function(col) {
      is.character(col) || is.factor(col) || (is.numeric(col) && length(unique(col)) <= 10)
    })
    names(data())[is_categorical]
  })

  numChoices <- reactive({
    req(data())
    is_numeric <- sapply(data(), function(col) {
      is.numeric(col) && length(unique(na.omit(col))) > 10
    })
    names(data())[is_numeric]
  })

  observeEvent(data(), {
    allchoices <- colnames(data())


    updateSelectInput(session, "x_var", choices = allchoices)
    updateSelectInput(session, "y_var", choices = allchoices)

    updateSelectInput(session, "x_predict", choices = allchoices)
    updateSelectInput(session, "y_predict", choices = allchoices)

    updateSelectInput(session, "x_varience", choices = allchoices)
    updateSelectInput(session, "y_varience", choices = allchoices)

    updateSelectInput(session, "variance_plot_var", choices = allchoices)
    updateSelectInput(session, "x_variance_compare", choices = allchoices)
    updateSelectInput(session, "y_variance_compare", choices = allchoices)

  })

  uploadServer(output, data)
  visualizeServer(input, output, data)
  modelServer(input, output, data, cataChoices = cataChoices)
  varianceServer(input, output, data, cataChoices = cataChoices, numChoices = numChoices)
}