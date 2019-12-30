library(shiny)

ui <- fluidPage(
    
    mainPanel(
        h3("Word Prediction Application"),
        textInput("Tcir",label=h3("Enter your text here:")),
        submitButton('Submit'),
        h4('You entered : '),
        verbatimTextOutput("inputValue"),
        h4('Predicted word :'),
        verbatimTextOutput("prediction")
    
    )
    
)

