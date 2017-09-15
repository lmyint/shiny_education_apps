library(shiny)

plot_tail <- function(x, fun, ...) {
    polygon(x = c(x, tail(x, 1), head(x, 1)), y = c(fun(x, ...), 0, 0), col = "darkorchid")
}

ppv <- function(sens, spec, prev) {
    (sens*prev)/((sens*prev) + (1-spec)*(1-prev))
}

npv <- function(sens, spec, prev) {
    (spec*(1-prev))/((spec*(1-prev)) + (1-sens)*prev)
}

ui <- navbarPage(
    title = "Statistical power",
    tabPanel(
        title = "Factors influencing power",
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
    ),
    tabPanel(
        title = "Interpreting power",
        h4("Analogy: interpreting results from a diagnostic test"),
        br(),
        sidebarLayout(
            sidebarPanel(
                width = 2,
                numericInput(
                    inputId = "sensitivity",
                    label = "Sensitivity",
                    value = 0.8,
                    min = 0,
                    max = 1,
                    step = 0.05
                ),
                numericInput(
                    inputId = "specificity",
                    label = "Specificity",
                    value = 0.8,
                    min = 0,
                    max = 1,
                    step = 0.05
                ),
                numericInput(
                    inputId = "prevalence",
                    label = "Prevalence",
                    value = 0.2,
                    min = 0,
                    max = 1,
                    step = 0.05
                )
            ),
            mainPanel(
                width = 10,
                uiOutput("ppv"),
                uiOutput("npv"),
                br(),
                splitLayout(
                    plotOutput("vary_sensitivity"),
                    plotOutput("vary_specificity"),
                    plotOutput("vary_prevalence"),
                    cellWidths = c("33.3%", "33.3%", "33.3%")
                )
            )
        ), ## End sidebar layout 1
        h4("Analogy: interpreting results from a hypothesis test"),
        p("The prior probability that the null is true is a subjective value. It is high if you believe there is a good chance that there is no difference between the groups being compared. It is low if you believe there is a good chance that there really is some difference between the groups being compared."),
        br(),
        sidebarLayout(
            sidebarPanel(
                width = 2,
                numericInput(
                    inputId = "power",
                    label = "Power",
                    value = 0.8,
                    min = 0,
                    max = 1,
                    step = 0.05
                ),
                numericInput(
                    inputId = "alpha",
                    label = "Type I error rate",
                    value = 0.05,
                    min = 0,
                    max = 1,
                    step = 0.05
                ),
                numericInput(
                    inputId = "prob_null",
                    label = "Prior probability that the null is true",
                    value = 0.2,
                    min = 0,
                    max = 1,
                    step = 0.05
                )
            ),
            mainPanel(
                width = 10,
                uiOutput("posterior_alt"),
                uiOutput("posterior_null"),
                br(),
                splitLayout(
                    plotOutput("vary_power"),
                    plotOutput("vary_alpha"),
                    plotOutput("vary_prob_null"),
                    cellWidths = c("33.3%", "33.3%", "33.3%")
                )
            )
        ) ## End sidebar layout 2
    )
)

server <- function(input, output) {
    ## First tab: factors influencing power ========================================
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

    ## Second tab: interpretation ==================================================
    prob_vec <- seq(0, 1, 0.01)

    ## Display PPV and NPV for this set of parameters
    output$ppv <- renderUI({
        ppv_value <- ppv(sens = input$sensitivity, spec = input$specificity, prev = input$prevalence)
        paste("Positive predictive value:", round(ppv_value, 4))
    })
    output$npv <- renderUI({
        npv_value <- npv(sens = input$sensitivity, spec = input$specificity, prev = input$prevalence)
        paste("Negative predictive value:", round(npv_value, 4))
    })

    ## Display plots varying one parameter at a time, holding the other two constant
    output$vary_sensitivity <- renderPlot({
        spec <- input$specificity
        prev <- input$prevalence
        ppv_vec <- ppv(sens = prob_vec, spec = spec, prev = prev)
        npv_vec <- npv(sens = prob_vec, spec = spec, prev = prev)
        plot(prob_vec, ppv_vec, type = "l", lwd = 2, xlab = "Sensitivity", ylab = "", main = paste0("PPV and NPV as a function of sensitivity.\nSpecificity = ", spec, ". Prevalence = ", prev, "."), ylim = c(0,1.1))
        lines(prob_vec, npv_vec, lwd = 2, col = "dodgerblue")
        legend("topleft", legend = c("PPV", "NPV"), col = c("black", "dodgerblue"), lwd = 2, bty = "n")
    })
    output$vary_specificity <- renderPlot({
        sens <- input$sensitivity
        prev <- input$prevalence
        ppv_vec <- ppv(sens = sens, spec = prob_vec, prev = prev)
        npv_vec <- npv(sens = sens, spec = prob_vec, prev = prev)
        plot(prob_vec, ppv_vec, type = "l", lwd = 2, xlab = "Specificity", ylab = "", main = paste0("PPV and NPV as a function of specificity.\nSensitivity = ", sens, ". Prevalence = ", prev, "."), ylim = c(0,1.1))
        lines(prob_vec, npv_vec, lwd = 2, col = "dodgerblue")
        legend("topleft", legend = c("PPV", "NPV"), col = c("black", "dodgerblue"), lwd = 2, bty = "n")
    })
    output$vary_prevalence <- renderPlot({
        sens <- input$sensitivity
        spec <- input$specificity
        ppv_vec <- ppv(sens = sens, spec = spec, prev = prob_vec)
        npv_vec <- npv(sens = sens, spec = spec, prev = prob_vec)
        plot(prob_vec, ppv_vec, type = "l", lwd = 2, xlab = "Prevalence", ylab = "", main = paste0("PPV and NPV as a function of prevalence.\nSensitivity = ", sens, ". Specificity = ", spec, "."), ylim = c(0,1.1))
        lines(prob_vec, npv_vec, lwd = 2, col = "dodgerblue")
        legend("topleft", legend = c("PPV", "NPV"), col = c("black", "dodgerblue"), lwd = 2, bty = "n")
    })

    ## Second part of the analogy

    ## Display posterior probabilities for this set of parameters
    output$posterior_alt <- renderUI({
        ppv_value <- ppv(sens = input$power, spec = 1-input$alpha, prev = 1-input$prob_null)
        paste("P(alt. true | reject):", round(ppv_value, 4))
    })
    output$posterior_null <- renderUI({
        npv_value <- npv(sens = input$power, spec = 1-input$alpha, prev = 1-input$prob_null)
        paste("P(null true | fail to reject):", round(npv_value, 4))
    })

    ## Display plots varying one parameter at a time, holding the other two constant
    output$vary_power <- renderPlot({
        spec <- 1-input$alpha
        prev <- 1-input$prob_null
        ppv_vec <- ppv(sens = prob_vec, spec = spec, prev = prev)
        npv_vec <- npv(sens = prob_vec, spec = spec, prev = prev)
        plot(prob_vec, ppv_vec, type = "l", lwd = 2, xlab = "Power", ylab = "", main = paste0("Type I error = ", 1-spec, ". P(null true) = ", 1-prev, "."), ylim = c(0,1.1))
        lines(prob_vec, npv_vec, lwd = 2, col = "dodgerblue")
        legend("topleft", legend = c("P(alt. true | reject)", "P(null true | fail to reject)"), col = c("black", "dodgerblue"), lwd = 2, bty = "n")
    })
    output$vary_alpha <- renderPlot({
        sens <- input$power
        prev <- 1-input$prob_null
        ## spec = seq(0,1) is 1-alpha = seq(0,1)
        ## So x axis should be 1-prob_vec to correspond to alpha
        ppv_vec <- ppv(sens = sens, spec = prob_vec, prev = prev)
        npv_vec <- npv(sens = sens, spec = prob_vec, prev = prev)
        plot(1-prob_vec, ppv_vec, type = "l", lwd = 2, xlab = "Type I error", ylab = "", main = paste0("Power = ", sens, ". P(null true) = ", 1-prev, "."), ylim = c(0,1.1))
        lines(1-prob_vec, npv_vec, lwd = 2, col = "dodgerblue")
        legend("topleft", legend = c("P(alt. true | reject)", "P(null true | fail to reject)"), col = c("black", "dodgerblue"), lwd = 2, bty = "n")
    })
    output$vary_prob_null <- renderPlot({
        sens <- input$power
        spec <- 1-input$alpha
        ## prev = seq(0,1) is P(H_A) = seq(0,1) is 1-P(H_0) = seq(0,1)
        ppv_vec <- ppv(sens = sens, spec = spec, prev = prob_vec)
        npv_vec <- npv(sens = sens, spec = spec, prev = prob_vec)
        plot(1-prob_vec, ppv_vec, type = "l", lwd = 2, xlab = "P(null true)", ylab = "", main = paste0("Power = ", sens, ". Type I error = ", 1-spec, "."), ylim = c(0,1.1))
        lines(1-prob_vec, npv_vec, lwd = 2, col = "dodgerblue")
        legend("topleft", legend = c("P(alt. true | reject)", "P(null true | fail to reject)"), col = c("black", "dodgerblue"), lwd = 2, bty = "n")
    })
}

shinyApp(ui = ui, server = server)
