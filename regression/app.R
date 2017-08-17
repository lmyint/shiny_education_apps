library(shiny)
library(readr)
library(ggplot2)

dataset <- read_csv("data/breast-cancer-wisconsin-data-expanded.csv")
dataset$diagnosis <- as.factor(dataset$diagnosis)
dataset$diagnosis_cat <- dataset$diagnosis
factor_cols <- colnames(dataset)[grep("_cat", colnames(dataset))]
for (x in factor_cols) {
    dataset[[x]] <- as.factor(dataset[[x]])
}

ui <- fluidPage(
    title = "Regression modeling",
    plotOutput("plot"),
    
    hr(),

    fluidRow(
        column(3,
            h5("Explore breast cancer data"),
            br(),
            checkboxInput("jitter", "Jitter points"),
            checkboxInput("smooth", "Smooth")
        ),
        column(4, offset = 1,
            selectInput(
                inputId = "x",
                label = "X",
                choices = names(dataset),
                selected = "diagnosis"
            ),
            checkboxInput("x_cat", label = "Make X categorical?"),
            selectInput(
                inputId = "y",
                label = "Y",
                choices = names(dataset),
                selected = names(dataset)[2]
            ),
            checkboxInput("y_cat", label = "Make Y categorical?"),
            selectInput(
                inputId = "color",
                label = "Color",
                choices = c("None", names(dataset))
            ),
            checkboxInput("color_cat", label = "Make color categorical?")
        ),
        column(4,
            selectInput(
                inputId = "facet_row",
                label = "Stratification variable (row)",
                choices = c(None=".", names(dataset[sapply(dataset, is.factor)]))
            ),
            selectInput(
                inputId = "facet_col",
                label = "Stratification variable (column)",
                choices = c(None='.', names(dataset[sapply(dataset, is.factor)]))
            )
        )
    )
)

server <- function(input, output) {
    output$plot <- renderPlot({
        x_var_str <- input$x
        if (input$x_cat) {
            x_var_str <- paste0(x_var_str, "_cat")
        }

        y_var_str <- input$y
        if (input$y_cat) {
            y_var_str <- paste0(y_var_str, "_cat")
        }

        color_var_str <- input$color
        if (input$color_cat) {
            color_var_str <- paste0(color_var_str, "_cat")
        }

        p <- ggplot(dataset, aes_string(x=x_var_str, y=y_var_str)) + geom_point()
        
        if (input$color != "None") {
            p <- p + aes_string(color=color_var_str)
        }
        
        facets <- paste(input$facet_row, "~", input$facet_col)
        if (facets != ". ~ .") {
            p <- p + facet_grid(facets)
        }
        
        if (input$jitter) {
            p <- p + geom_jitter()
        }
        if (input$smooth) {
            p <- p + geom_smooth()
        }
        
        print(p)
    })
    
}

shinyApp(ui = ui, server = server)