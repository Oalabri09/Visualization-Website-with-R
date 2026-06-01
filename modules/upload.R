uploadServer <- function(output, data) {
  output$dataset_info <- renderPrint({
    req(data())
    cat("Rows:", nrow(data()), "\n")
    cat("Columns:", ncol(data()), "\n")
    cat("Column names: \n")
    print(colnames(data()))
  })
  output$table <- renderDT({
    req(data())
    datatable(data(), options = list(pageLength = 10, scrollX = TRUE))
  })
}