library(shiny)

ui <- navbarPage(
    title = "Common statistical tests",
    tabPanel(
        title = "Continuous data",
        sidebarLayout(
            sidebarPanel(
                selectInput(
                    inputId = "distribution",
                    label = "Data distribution",
                    choices = c(
                        "Symmetric" = "sym",
                        "Skewed" = "skew"
                    )
                ),
                sliderInput(
                    inputId = "sample_size_cont",
                    label = "Sample size in each group",
                    min = 10,
                    max = 300,
                    value = 10,
                    step = 10
                ),
                sliderInput(
                    inputId = "effect_size_cont",
                    label = "True difference in means",
                    min = 0,
                    max = 4,
                    value = 0,
                    step = 0.1
                ),
                actionButton(inputId = "repeat_cont", label = "Repeat 1000 times")
            ),
            mainPanel(
                plotOutput(outputId = "plot_reject_cont")
            )
        )
    ),
    tabPanel(
        title = "Categorical data",
        sidebarLayout(
            sidebarPanel(
                sliderInput(
                    inputId = "sample_size_categ",
                    label = "Total table sample size",
                    min = 20,
                    max = 1000,
                    value = 20,
                    step = 40
                ),
                sliderInput(
                    inputId = "effect_size_categ",
                    label = "True difference in proportions",
                    min = 0,
                    max = 0.4,
                    value = 0,
                    step = 0.05
                ),
                actionButton(inputId = "repeat_categ", label = "Repeat 1000 times")
            ),
            mainPanel(
                plotOutput(outputId = "plot_reject_categ")
            )
        )
    )
)

server <- function(input, output) {
    observeEvent(input$repeat_cont, {
        set.seed(1)
        if (input$distribution=="sym") {
            mean1 <- 10
            sdev <- 2
            pvals <- replicate(1000, {
                mean2 <- mean1 + input$effect_size_cont
                dat <- data.frame(group1 = rnorm(input$sample_size_cont, mean1, sdev), group2 = rnorm(input$sample_size_cont, mean2, sdev))
                ttest_pval <- t.test(dat$group1, dat$group2)$p.value
                wilcox_pval <- wilcox.test(dat$group1, dat$group2)$p.value
                c(ttest = ttest_pval, wilcox = wilcox_pval)
            })
        } else {
            mu1 <- 0
            sdev <- 1
            pvals <- replicate(1000, {
                mean2 <- exp(mu1 + (sdev^2)/2) + input$effect_size_cont
                mu2 <- log(mean2) - (sdev^2)/2
                dat <- data.frame(group1 = rlnorm(input$sample_size_cont, mu1, sdev), group2 = rlnorm(input$sample_size_cont, mu2, sdev))
                ttest_pval <- t.test(dat$group1, dat$group2)$p.value
                wilcox_pval <- wilcox.test(dat$group1, dat$group2)$p.value
                c(ttest = ttest_pval, wilcox = wilcox_pval)
            })
        }
        alpha_seq <- seq(0.01, 0.1, 0.005)
        ecdf_ttest <- ecdf(pvals["ttest",])(alpha_seq)
        ecdf_wilcox <- ecdf(pvals["wilcox",])(alpha_seq)
        output$plot_reject_cont <- renderPlot({
            plot(alpha_seq, ecdf_ttest, pch = 16, xlim = range(alpha_seq), ylim = range(ecdf_ttest, ecdf_wilcox), xlab = "alpha (Nominal type I error rate)", ylab = "Proportion of p-values less than alpha")
            points(alpha_seq, ecdf_wilcox, pch = 16, col = "red")
            abline(a = 0, b = 1, col = "black", lty = "dashed")
            legend("topleft", legend = c("t-test", "Wilcoxon"), fill = c("black", "red"), bty = "n")
        })
    })
    observeEvent(input$repeat_categ, {
        prop1 <- 0.1
        prop2 <- min(prop1 + input$effect_size_categ, 1)
        n1 <- floor(input$sample_size_categ/2)
        n2 <- input$sample_size_categ - n1
        pvals <- replicate(1000, {
            repeat({
                group1 <- rbinom(n1, size = 1, prob = prop1)
                group2 <- rbinom(n2, size = 1, prob = prop2)
                tab <- table(group1, group2)
                if (all(dim(tab)==c(2,2))) {
                    break
                }
            })
            chi2_pval <- chisq.test(tab)$p.value
            fet_pval <- fisher.test(tab)$p.value
            c(chi2 = chi2_pval, fet = fet_pval)
        })
        alpha_seq <- seq(0.01, 0.1, 0.005)
        ecdf_chi <- ecdf(pvals["chi2",])(alpha_seq)
        ecdf_fet <- ecdf(pvals["fet",])(alpha_seq)
        output$plot_reject_categ <- renderPlot({
            plot(alpha_seq, ecdf_chi, pch = 16, xlim = range(alpha_seq), ylim = range(ecdf_chi, ecdf_fet), xlab = "alpha (Nominal type I error rate)", ylab = "Proportion of p-values less than alpha")
            points(alpha_seq, ecdf_fet, pch = 16, col = "red")
            abline(a = 0, b = 1, col = "black", lty = "dashed")
            legend("topleft", legend = c("Chi-squared", "Fisher's"), fill = c("black", "red"), bty = "n")
        })
    })
}

shinyApp(ui = ui, server = server)
