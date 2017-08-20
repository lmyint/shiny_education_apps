library(shiny)
library(Hmisc)

num_simulations <- 1000

plot_cis <- function(cis, true_prop, title, col, xlim) {
    cis <- cis[order(cis[,"PointEst"]),]
    cov_prob <- sum(cis[,"Lower"] <= true_prop & cis[,"Upper"] >= true_prop)/nrow(cis)
    plot(1, type = "n", xlim = xlim, ylim = c(0,num_simulations), yaxt = "n", main = paste(title, "\nCoverage probability:", round(cov_prob, 3)), xlab = "", ylab = "")
    segments(x0 = cis[,"Lower"], x1 = cis[,"Upper"], y0 = seq_len(num_simulations), col = col)
    points(cis[,"PointEst"], seq_len(num_simulations), pch = 16, cex = 0.5)
    abline(v = true_prop, lty = "dashed", col = "red", lwd = 2)
}

ui <- fluidPage(
    titlePanel("Comparison of confidence interval methods"),
    sidebarLayout(
        sidebarPanel(
            width = 2,
            numericInput(
                inputId = "true_prop",
                label = "True proportion",
                value = 0.1,
                min = 0,
                max = 1,
                step = 0.01
            ),
            numericInput(
                inputId = "sample_size",
                label = "Sample size",
                value = 100,
                min = 10,
                max = 1e6,
                step = 5
            )
        ),
        mainPanel(
            width = 10,
            plotOutput("plot")
        )
    )
)

server <- function(input, output) {
    binom_data <- reactive({
        rbinom(num_simulations, size = input$sample_size, prob = input$true_prop)
    })
    output$plot <- renderPlot({
        dat <- binom_data()
        results <- do.call(rbind, lapply(dat, function(x) {
            binconf(x = x, n = input$sample_size, method = "all")
        }))
        results_asymp <- results[rownames(results)=="Asymptotic",,drop = FALSE]
        results_wilson <- results[rownames(results)=="Wilson",,drop = FALSE]
        results_exact <- results[rownames(results)=="Exact",,drop = FALSE]
        xlim <- range(results[,c("Lower", "Upper")])
        par(mfrow = c(1,3))
        plot_cis(results_asymp, true_prop = input$true_prop, title = "Standard (Normal approximation)", col = "darkorange", xlim = xlim)
        plot_cis(results_wilson, true_prop = input$true_prop, title = "Wilson score interval", col = "dodgerblue", xlim = xlim)
        plot_cis(results_exact, true_prop = input$true_prop, title = "Clopper-Pearson interval", col = "limegreen", xlim = xlim)
    })
    
}

shinyApp(ui = ui, server = server)