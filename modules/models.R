modelServer <- function(input, output, data, cataChoices) {
  model_output <- eventReactive(input$run_model, {
    req(data(), input$x_predict, input$y_predict)
    modelData <- data()
    cataChoices = cataChoices()


    x_vars <- input$x_predict
    y_var <- input$y_predict

    results <- list()
    for (x in x_vars) {
      dataCopy <- modelData
      formula <- as.formula(paste(y_var, "~", x))

      if (y_var %in% cataChoices) { 
        dataCopy[[y_var]] <- as.factor(dataCopy[[y_var]])
      }
      if (x %in% cataChoices) { 
        dataCopy[[x]] <- as.factor(dataCopy[[x]])
      }

      if(y_var %in% cataChoices) {
        y_count = length(unique(na.omit(dataCopy[[y_var]])))
        if (y_count == 2) { 
          fit <- glm(formula, data = dataCopy, family = "binomial")
        } else if (y_count > 2){
          #fit <- nnet::multinom(formula, data = dataCopy, trace = FALSE)
          #Not doing multinom shit for now
          print("NOT DOING THIS YET")
          next
        } else {
          next
        }

        
      } else {
        fit <- lm(formula, data = dataCopy)
      }

      results[[x]] <- summary(fit)

    }
    return(results)
  })

  output$model_results <- renderPrint({
    result <- model_output()
    cataChoices = cataChoices()
    for (predictor in names(result)) {
      coef_matrix <- result[[predictor]]$coefficients
      cat("=================================\n")
      cat("Model predicting:", input$y_predict, "using", predictor, "\n")
      cat("=================================\n")
      print(result[[predictor]]$coefficients)

      if (input$y_predict %in% cataChoices) {
        cat("\nThis model type: Logistic Regression\n")
        if(nrow(coef_matrix > 1)){ 
          for(i in 2:nrow(coef_matrix)){
            p_value <- coef_matrix[i, 4]
            if (p_value < input$threshold) { 
              cat("\n variable componant: ", rownames(coef_matrix)[i], "\n")
              cat("\nThis is significant !!!\n")
            } else {
              cat("\n variable componant: ", rownames(coef_matrix)[i], "\n")
            }
          }
        }
      } else {
        cat("\n This model type: Linear Regression") 
        cat("\n Adjusted R_squared: ",result[[predictor]]$adj.r.squared, "\n")
        if(nrow(coef_matrix > 1)){ 
          for(i in 2:nrow(coef_matrix)){
            p_value <- coef_matrix[i, 4]
            if (p_value < input$threshold) { 
              cat("\n variable componant: ", rownames(coef_matrix)[i], "\n")
              cat("\nThis is significant !!!\n")
            } else {
              cat("\n variable componant: ", rownames(coef_matrix)[i], "\n")
            }
          }
        }
      } 


    }
  })
}