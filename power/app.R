library(shiny)

plot_tail <- function(x, fun, ...) {
    polygon(x = c(x, tail(x, 1), head(x, 1)), y = c(fun(x, ...), 0, 0), col = "darkorchid")
}

ui <- navbarPage(
    titlePanel(title = "Statistical power"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "typeI_error",
                label = "Type I error rate (alpha)",
                min = 0.01,
                max = 0.99,
                value = 0.05,
                step = 0.01
            ),
            sliderInput(
                inputId = "sample_size",
                label = "Sample size in each group",
                min = 10,
                max = 300,
                value = 10,
                step = 10
            ),
            sliderInput(
                inputId = "sd",
                label = "Standard deviation",
                min = 0.1,
                max = 3,
                value = 1,
                step = 0.1
            ),
            sliderInput(
                inputId = "effect_size",
                label = "True difference in means",
                min = 0.1,
                max = 5,
                value = 0.1,
                step = 0.1
            )
        ),
        mainPanel(
            ## Sampling distribution of test statistic
            plotOutput(outputId = "plot_power")
        )
    )
)

server <- function(input, output) {
    ncp_null <- 0
    stat_power <- reactive({
        power.t.test(n = input$sample_size, delta = input$effect_size, sd = input$sd, sig.level = input$typeI_error)$power
    })
    output$plot_power <- renderPlot({
        ## Calculate degrees of freedom
        n <- input$sample_size
        df <- 2*n-2
        ## Calculate t critical value
        alpha <- input$typeI_error
        critval <- qt(1-alpha/2, df = df)
        ## Compute power
        pow <- stat_power()
        ## Plot null distribution
        sdev <- input$sd
        x1 <- seq(qt(0.01, ncp = ncp_null, df = df), qt(0.99, ncp = ncp_null, df = df), 0.01)
        y1 <- dt(x1, ncp = ncp_null, df = df)
        ncp_alt <- input$effect_size/(sdev*sqrt(2/n))
        x2 <- seq(qt(0.01, ncp = ncp_null, df = df), qt(0.99, ncp = ncp_alt, df = df), 0.01)
        y2 <- dt(x2, ncp = ncp_alt, df = df)
        # pow2 <- pt(-critval, df = df, ncp = ncp_alt, lower.tail = TRUE)+pt(critval, df = df, ncp = ncp_alt, lower.tail = FALSE)
        plot(x1, y1, type = "l", xlab = "Test statistic", ylab = "Density", main = paste0("Power = ", round(pow, 2)*100, "%"), lwd = 2, xlim = range(x1, x2), ylim = c(0, max(y1, y2)))
        ## Plot alternative distribution
        lines(x2, y2, lwd = 2, lty = "dashed", col = "red")
        x2_left_tail <- seq(min(x1, x2), -critval, 0.01)
        x2_right_tail <- seq(critval, max(x1, x2), 0.01)
        
        plot_tail(x2_left_tail, fun = dt, df = df, ncp = ncp_alt)
        plot_tail(x2_right_tail, fun = dt, df = df, ncp = ncp_alt)
    })
}

shinyApp(ui = ui, server = server)