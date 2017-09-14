library(shiny)

n1 <- 2000
n2 <- 6000
p2 <- 0.02

calculate_test_stat <- function(df) {
    tab <- table(group = df$group, status = df$status)
    if (!all(dim(tab)==c(2,2))) {
        return(c(estimate = NA, test_stat = NA))
    }
    samp_sizes <- rowSums(tab)
    p <- tab[,"1"]/samp_sizes
    std_err <- sqrt(sum(p*(1-p)/samp_sizes))
    estim <- p[1] - p[2]
    test_stat <- estim/std_err
    return(c(estimate = estim, test_stat = test_stat))
}

ui <- fluidPage(
    titlePanel(title = "Sampling distributions", windowTitle = "Sampling distributions"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "sample_size",
                label = "Study sample size",
                min = 20,
                max = 1000,
                value = 100,
                step = 20
            ),
            sliderInput(
                inputId = "effect_size",
                label = "True difference in proportions",
                min = 0,
                max = 1,
                value = 0,
                step = 0.05
            ),
            actionButton(inputId = "do1", label = "Do 1 study"),
            actionButton(inputId = "do100", label = "Do 100 studies")
        ),
        mainPanel(
            fluidRow(
                ## Full and study data
                plotOutput(outputId = "plot_data")
            ),
            fluidRow(
                ## Sampling distribution of estimate and test statistic
                plotOutput(outputId = "plot_sampling_dists")
            )
        )
    )
)

server <- function(input, output) {
    state <- reactiveValues(
        data = NULL,
        stats = NULL
    )
    observeEvent(input$effect_size, {
        eff <- input$effect_size
        p1 <- min(p2+eff, 1)
        df1 <- data.frame(group = 1, x = runif(n1, 0, 3.4), y = runif(n1, 0, 10), status = rbinom(n1, size = 1, prob = p1), color = "black")
        df2 <- data.frame(group = 2, x = runif(n2, 3.6, 7), y = runif(n2, 0, 10), status = rbinom(n2, size = 1, prob = p2), color = "black")
        state$data <- rbind(df1, df2)
        state$stats <- NULL
    })
    output$plot_data <- renderPlot({
        df <- state$data
        par(mar = c(5.1,0,0,2.1))
        plot(df$x, df$y, pch = ifelse(df$status==1, 17, 20), cex = ifelse(df$status==1, 2, 0.5), col = df$color, xlab = "Exposure status", xaxt = "n", ylab = "", yaxt = "n", ylim = c(0,11))
        axis(side = 1, at = c(1.5, 5.5), labels = c("Exposed", "Unexposed"))
        legend("top", horiz = TRUE, legend = c("Case", "Control", "In study"), col = c("black", "black", "red"), pch = c(17,20,15), bty = "n")
    })

    observeEvent(input$sample_size, {
        state$stats <- NULL
    })

    observeEvent(input$do1, {
        n_sample1 <- round(input$sample_size/2)
        n_sample2 <- input$sample_size - n_sample1
        idx_group1 <- which(state$data$group==1)
        idx_group2 <- which(state$data$group==2)
        idx_selected <- c(sample(idx_group1, n_sample1), sample(idx_group2, n_sample2))
        ## Update data plot
        colors <- rep("black", nrow(state$data))
        colors[idx_selected] <- "red"
        state$data$color <- colors
        ## Calculate test statistic
        new_stats <- calculate_test_stat(state$data[idx_selected,])
        state$stats <- cbind(state$stats, new_stats)
        rownames(state$stats) <- c("estimate", "test_stat")
    })
    observeEvent(input$do100, {
        n_sample1 <- round(input$sample_size/2)
        n_sample2 <- input$sample_size - n_sample1
        idx_group1 <- which(state$data$group==1)
        idx_group2 <- which(state$data$group==2)
        new_stats <- replicate(100, {
            idx_selected <- c(sample(idx_group1, n_sample1), sample(idx_group2, n_sample2))
            calculate_test_stat(state$data[idx_selected,])
        })
        state$stats <- cbind(state$stats, new_stats)
        rownames(state$stats) <- c("estimate", "test_stat")
    })
    output$plot_sampling_dists <- renderPlot({
        if (!is.null(state$stats)) {
            par(mfrow = c(1,2))
            ## Estimate
            estim <- state$stats["estimate",]
            hist(estim, freq = FALSE, xlab = "Estimate (difference in proportions)", ylab = "Density", main = paste("Distribution of estimate for n =", input$sample_size))
            if (length(estim) >= 5) {
                lines(density(estim, na.rm = TRUE), col = "red")
            }
            ## Test statistic
            test_stats <- state$stats["test_stat",]
            hist(test_stats, freq = FALSE, xlab = "Test statistic", ylab = "Density", main = paste("Distribution of test statistic for n =", input$sample_size))
            if (length(test_stats) >= 5) {
                lines(density(test_stats, na.rm = TRUE), col = "red")
            }
        }
    })
}

shinyApp(ui = ui, server = server)
