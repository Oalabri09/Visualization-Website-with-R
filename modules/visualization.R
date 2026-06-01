visualizeServer <- function(input, output, data) {
  plot_data <- eventReactive(input$go_plot, {
    req(data(), input$x_var, input$y_var)
    p <- ggplot(data(), aes(x = .data[[input$x_var]], y = .data[[input$y_var]])) +
      theme_minimal()
    if (input$plot_type == "point") p <- p + geom_point()
    else if (input$plot_type == "line") p <- p + geom_line()
    else if (input$plot_type == "bar") p <- p + geom_col()
    else p <- p + geom_smooth()
    ggplotly(p)
  })
  output$v_plot <- renderPlotly({ plot_data() })
}
