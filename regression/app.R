library(shiny)
library(ggplot2)
library(broom)

load("data/regression_data.rda")

NUM_RESAMPLE <- 100
SIZE_RESAMPLE <- round(nrow(train)*0.6)

optimal_p <- function(fit, data) {
    logodds <- predict(fit)
    odds <- exp(logodds)
    prob <- odds/(1+odds)
    p_seq <- seq(0, 1, 0.001)
    true_class <- as.integer(data$diagnosis)-1
    classif_acc <- sapply(p_seq, function(p) {
        classif <- prob > p
        acc <- sum(classif==true_class)/length(true_class)
        return(acc)
    })
    p_seq[which.max(classif_acc)]
}

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
                    choices = names(train),
                    selected = "diagnosis"
                ),
                checkboxInput("x_cat", label = "Make X categorical?"),
                selectInput(
                    inputId = "y",
                    label = "Y",
                    choices = names(train),
                    selected = names(train)[2]
                ),
                checkboxInput("y_cat", label = "Make Y categorical?"),
                selectInput(
                    inputId = "color",
                    label = "Color",
                    choices = c("None", names(train))
                ),
                checkboxInput("color_cat", label = "Make color categorical?")
            ),
            column(4,
                selectInput(
                    inputId = "facet_row",
                    label = "Stratification variable (row)",
                    choices = c(None=".", names(train[sapply(train, is.factor)]))
                ),
                selectInput(
                    inputId = "facet_col",
                    label = "Stratification variable (column)",
                    choices = c(None='.', names(train[sapply(train, is.factor)]))
                )
            )
        )
    ), ## End data exploration tab
    tabPanel(
        title = "Model fitting",
        fluidRow(
            column(
                width = 7,
                textInput(
                    width = "100%",
                    inputId = "formula",
                    label = "Enter predictors to include using \"formula\" notation:",
                ),
                actionButton("fit_model", "Fit model")
            ),
            column(width = 5,
                tags$b(p("What if the data you collected were slightly different?")),
                actionButton("resample", "Fit model on other datasets")
            )
        ),
        fluidRow(
            column(width = 6,
                tableOutput("model_fit_results")
            ),
            column(width = 6,
                plotOutput("plot_coeff_variability")
            )
        ),
        fluidRow(
            column(width = 4,
                h4("Prediction accuracy"),
                p("To the right you can explore how well your model", tags$b(tags$em("predicts")), "tumor malignancy. The model fitting results above are from a training dataset which consists of 80% of the full data. To the right we can look at results from predicting malignant outcomes using your model in a test dataset made up of the other 20%. Also plotted are prediction accuracy results for two alternate variable selection techniques.")
            ),
            column(width = 8,
                plotOutput("acc_plot")
            )
        )
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

        p <- ggplot(train, aes_string(x=x_var_str, y=y_var_str)) + geom_point()
        
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
        fit <- glm(form, family = binomial, data = train)
        ## Find the optimal probability cutoff
        opt_p <- optimal_p(fit, train)
        state$actual_fit <- tidy(fit)
        output$model_fit_results <- renderTable({ state$actual_fit })
        output$acc_plot <- renderPlot({
            pred_prob_test <- predict_prob(fit, test)
            pred_outcome_test <- pred_prob_test > opt_p
            acc <- sum(pred_outcome_test==true_outcome_test)/length(true_outcome_test)
            accs <- c(acc, acc_lasso, acc_best_subsets)
            colors <- c("limegreen", paste0("dodgerblue", 4:1), "darkorange")
            par(mar = c(5.1, 10.1, 4.1, 2.1))
            plot(accs, 6:1, pch = 16, cex = 2, col = colors, xlab = "Prediction accuracy", ylab = "", yaxt = "n")
            axis(side = 2, at = 6:1, labels = c("You", paste("Best subsets:\n", 1:4, "variables"), "LASSO"), las = 2)
        })
    })

    observeEvent(input$resample, {
        results_resample <- do.call(rbind, lapply(seq_len(NUM_RESAMPLE), function(i) {
            form <- as.formula(paste("diagnosis ~", input$formula))
            idx <- sample.int(nrow(train), size = SIZE_RESAMPLE)
            fit <- glm(form, family = binomial, data = train[idx,])
            tidy(fit)
        }))
        vars <- unique(results_resample$term)
        output$plot_coeff_variability <- renderPlot({
            nrow <- ceiling(nrow(state$actual_fit)/3)
            par(mfrow = c(nrow, 3))
            for (v in vars) {
                coeff_est <- results_resample$estimate[results_resample$term==v]
                plot(density(coeff_est), xlab = "Coefficient estimate", ylab = "Density", main = v)
                abline(v = state$actual_fit$estimate[state$actual_fit$term==v], lwd = 2, lty = "dashed")
            }
        })
    })
}

shinyApp(ui = ui, server = server)