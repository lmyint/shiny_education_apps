library(shiny)
library(MatchIt)

set.seed(1)
n <- 1000
x1 <- round(rgamma(n, shape = 7.5, scale = 1))+30
x2 <- rbinom(n, size = 1, prob = 0.5)
x3 <- rbinom(n, size = 1, prob = 0.8)
x4 <- factor(sample(1:5, size = n, replace = TRUE), levels = 1:5, labels = LETTERS[1:5])
x5 <- factor(sample(1:3, size = n, replace = TRUE), levels = 1:3, labels = LETTERS[1:3])
trt_eff <- 1
dat <- data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5)
design <- model.matrix(~ ., data = dat)
coeffs <- matrix(c(0, -0.05, 0.1, 0.15, 0, 0, 1.55, 0.7, -0.8, 0.24))
log_odds_trt <- design%*%coeffs
odds_trt <- exp(log_odds_trt)
p_trt <- odds_trt/(1+odds_trt)
dat$treat <- rbinom(n, size = 1, prob = p_trt)

ui <- fluidPage(
    titlePanel("Propensity score matching"),
    sidebarLayout(
        sidebarPanel(
            width = 3,
            checkboxGroupInput(
                inputId = "vars_match_on",
                label = "Variables to match between treated and control:",
                choices = c(
                    "x1" = "x1",
                    "x2" = "x2",
                    "x3" = "x3",
                    "x4" = "x4",
                    "x5" = "x5"
                )
            ),
            sliderInput(
                inputId = "ratio",
                label = "# controls to match to each case",
                min = 1,
                max = 3,
                value = 1,
                step = 1
            ),
            sliderInput(
                inputId = "caliper",
                label = "Matching caliper size",
                min = 0,
                max = 0.5,
                value = 0,
                step = 0.01
            ),
            actionButton("match", "Perform matching")
        ),
        mainPanel(
            width = 9,
            fluidRow(
                verbatimTextOutput("summary_balance")
            ),
            fluidRow(
                plotOutput("plot_balance")
            ),
            fluidRow(
                uiOutput("plot_ps_ui")
            ),
            fluidRow(
                tableOutput("tab_selected_points")
            )
        )
    )
)

server <- function(input, output) {
    state <- reactiveValues(
        match_res = NULL,
        mdata = NULL
    )
    match_formula <- reactive({
        form_str <- paste("treat ~", paste(input$vars_match_on, collapse = "+"))
        as.formula(form_str)
    })

    observeEvent(input$match, {
        match_res <- matchit(match_formula(), data = dat, method = "nearest", ratio = input$ratio, caliper = input$caliper)
        mdata <- match_res$model$data
        mdata$logitps <- match_res$distance[rownames(mdata)]
        mdata$matched <- rownames(mdata) %in% c(match_res$match.matrix, rownames(match_res$match.matrix))

        num_matches <- match_res$nn
        output$plot_balance <- renderPlot({ plot(summary(match_res, standardize = TRUE, interactive = FALSE)) })
        output$summary_balance <- renderPrint({ 
            sum_all <- summary(match_res)[["sum.all"]][,c("Means Treated", "Means Control", "SD Control", "Mean Diff")]
            sum_matched <- summary(match_res)[["sum.matched"]][,c("Means Treated", "Means Control", "SD Control", "Mean Diff")]
            cat("How many units were matched?\n")
            print(num_matches)
            cat("\nCovariate summary: all units\n")
            print(sum_all)
            cat("\nCovariate summary: matched units\n")
            print(sum_matched)
        })
        output$plot_ps_ui <- renderUI({
            tagList(
                plotOutput("plot_ps_dens",
                    brush = brushOpts(
                        id = "ps_brush",
                        direction = "x",
                        delay = 800
                    )
                ),
                plotOutput("plot_ps_dots",
                    brush = brushOpts(
                        id = "ps_brush",
                        direction = "x",
                        delay = 800
                    )
                )
            )
        })
        state$match_res <- match_res
        state$mdata <- mdata
    })

    output$plot_ps_dens <- renderPlot({
        mdata <- state$mdata
        dens_trt <- density(mdata$logitps[mdata$treat==1], from = 0, to = 1)
        dens_ctrl <- density(mdata$logitps[mdata$treat==0], from = 0, to = 1)
        plot(dens_ctrl, xlim = c(0,1), ylim = range(dens_trt$y, dens_ctrl$y), xlab = "logit(propensity score)", main = "")
        lines(dens_trt, col = "red")
        legend("topright", legend = c("Control", "Treated"), col = c("black", "red"), lty = "solid", bty = "n")
    })
    output$plot_ps_dots <- renderPlot({
        mdata <- state$mdata
        plot(mdata$logitps, jitter(mdata$treat), xlim = c(0,1), xlab = "logit(propensity score)", ylab = "", yaxt = "n", pch = ifelse(mdata$matched, 20, 4), col = ifelse(mdata$matched, "black", "red"), main = "")
        axis(side = 2, at = c(0,1), labels = c("Control", "Treated"))
        legend("right", legend = c("Matched", "Unmatched"), pch = c(20,4), col = c("black", "red"), bty = "n")
    })
    output$tab_selected_points <- renderTable({
        mdata <- state$mdata
        if (!is.null(mdata)) {
            unique(brushedPoints(mdata, input$ps_brush, xvar = "logitps")[,1:(ncol(mdata)-1)])
        }
    })
}

shinyApp(ui = ui, server = server)
