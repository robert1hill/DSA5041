#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- verticalLayout( # object 1,
        # object 2,
        # object 3

    # Application title
    titlePanel("T-Test Calculator - DSA 5041 Project - Robert Hill"),

    #paired? I couldn't get this to work
    #checkboxInput("checkbox1", label = "Is data paired?", value = FALSE),
    #hr(),
    #fluidRow(column(3, verbatimTextOutput("paired"))),

    # x mean
    numericInput("num1", label = "X Mean", value = 1),hr(),
    fluidRow(column(3, verbatimTextOutput("xMean"))),

    # x std dev
    numericInput("num2", label = "X Standard Deviation", value = 1),hr(),
    fluidRow(column(9, verbatimTextOutput("xStdDev"))),

    # y mean
    numericInput("num3", label = "Y Mean", value = 1),hr(),
    fluidRow(column(3, verbatimTextOutput("yMean"))),

    # y std dev
    numericInput("num4", label = "Y Standard Deviation", value = 1),hr(),
    fluidRow(column(9, verbatimTextOutput("yStdDev"))),

    # alpha
    numericInput("num5", label = "Alpha", value = 0.05),hr(),
    fluidRow(column(9, verbatimTextOutput("alpha"))),

    # Show a plot of the generated distribution

    textOutput("testType"),

    plotOutput("distPlot")

)


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {

    library(DSA5041PROJ1)

    # Generating the data based on inputs
    response <- reactive({
        set.seed(33)
        x <- rnorm(100,mean=input$num1,sd=input$num2)
        set.seed(32)
        y <- rnorm(100,mean=input$num3,sd=input$num4)

        #Running package function
        #obj <- myttest(x,y,paired=input$checkbox,alpha=input$num5)
        #list(x=x,y=y,alpha=input$num5)

        obj <- myttest(x,y,alpha=input$num5,paired=FALSE)

        list(obj=obj)
    })

    output$distPlot <- renderPlot({
        plot(response()$obj)

    })

})

# Run the application
shinyApp(ui = ui, server = server)
