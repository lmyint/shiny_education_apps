library(shiny)
library(ggplot2)
library(broom)

library(readr)
library(glmnet)
library(bestglm)

load("data/regression_data.rda")

ui <- navbarPage(
    title = "Regression modeling",
    tabPanel(
        title = "Data exploration",
        plotOutput("eda_plot"),
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
    ), ## End data exploration tab
    tabPanel(
        title = "Model fitting",
        fluidRow(
            textInput(
                inputId = "formula",
                label = "Enter predictors to include using \"formula\" notation:",
            ),
            actionButton("fit_model", "Fit model"),
            br(),
            p("What if the data you collected were slightly different?"),
            actionButton("resample", "Fit model on other datasets")
        ),
        tableOutput("model_fit_results"),
        plotOutput("plot_coeff_variability")
    ) ## End model fitting tab
)

server <- function(input, output) {
    state <- reactiveValues(
        actual_fit = NULL
    )

    ## EDA tab =====================================================================
    output$eda_plot <- renderPlot({
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
    
    ## Model fitting tab ===========================================================
    observeEvent(input$fit_model, {
        form <- as.formula(paste("diagnosis ~", input$formula))
        fit <- glm(form, family = binomial, data = dataset)
        state$actual_fit <- tidy(fit)
        print(state$actual_fit)
        output$model_fit_results <- renderTable({ state$actual_fit })
    })

    observeEvent(input$resample, {
        results_resample <- do.call(rbind, lapply(seq_len(NUM_RESAMPLE), function(i) {
            form <- as.formula(paste("diagnosis ~", input$formula))
            idx <- sample.int(nrow(dataset), size = SIZE_RESAMPLE)
            fit <- glm(form, family = binomial, data = dataset[idx,])
            tidy(fit)
        }))
        vars <- unique(results_resample$term)
        output$plot_coeff_variability <- renderPlot({
            nrow <- ceiling(nrow(state$actual_fit)/5)
            par(mfrow = c(nrow, 5))
            for (v in vars) {
                coeff_est <- results_resample$estimate[results_resample$term==v]
                plot(density(coeff_est), xlab = "Coefficient estimate", ylab = "Density", main = v)
                abline(v = state$actual_fit$estimate[state$actual_fit$term==v], lwd = 2, lty = "dashed")
            }
        })
    })
}

shinyApp(ui = ui, server = server)