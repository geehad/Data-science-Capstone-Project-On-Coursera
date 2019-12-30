source("NextWordPredictor.R")


shinyServer(
    function(input, output) {
        output$inputValue <- renderText({input$Tcir})
        
        output$prediction <- renderText({
            
            if (input$Tcir != "" ){
            
                predict_next_word(input$Tcir)}
            
            
            })
        
    }
)