library(shiny)
library(MatchIt)
library(cem)

data(lalonde)

ui <- fluidPage(
    titlePanel("Matching methods"),
    sidebarLayout(
        sidebarPanel(
            width = 3,
            checkboxGroupInput(
                inputId = "vars_match_on",
                label = "Variables to match between treated and control:",
                choices = c(
                    "Age" = "age",
                    "Years in school" = "educ",
                    "Is African-American" = "black",
                    "Is Hispanic" = "hispan",
                    "Is married" = "married",
                    "No high school degree" = "nodegree",
                    "Income in 1974" = "re74",
                    "Income in 1975" = "re75"
                )
            ),
            selectInput(
                inputId = "match_method",
                label = "Matching method",
                choices = c(
                    "Exact" = "exact",
                    "Coarsened exact matching" = "cem",
                    "Propensity score" = "nearest"
                )
            ),
            actionButton("match", "Perform matching")
        ),
        mainPanel(
            width = 9,
            fluidRow(
                verbatimTextOutput("summary_balance"),
                plotOutput("plot_balance")
            )
        )
    )
)

server <- function(input, output) {
    match_formula <- reactive({
        form_str <- paste("treat ~", paste(input$vars_match_on, collapse = "+"))
        as.formula(form_str)
    })

    observeEvent(input$match, {
        match_res <- matchit(match_formula(), data = lalonde, method = input$match_method)
        num_matches <- match_res$nn
        if (input$match_method != "exact") {
            output$plot_balance <- renderPlot({ plot(summary(match_res, standardize = TRUE, interactive = FALSE)) })
            output$summary_balance <- renderPrint({ 
                sum_all <- summary(match_res)[["sum.all"]][,c("Means Treated", "Means Control", "SD Control", "Mean Diff")]
                sum_matched <- summary(match_res)[["sum.matched"]][,c("Means Treated", "Means Control", "SD Control", "Mean Diff")]
                print(num_matches)
                print(sum_all)
                print(sum_matched)
            })
        } else {
            output$summary_balance <- renderPrint({ 
                print(num_matches)
            })
        }
    })
}

shinyApp(ui = ui, server = server)