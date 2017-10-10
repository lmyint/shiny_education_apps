library(shiny)

n <- 40
df <- data.frame(
    age = sample(factor(1:3, levels = 1:3, labels = c("20-29","30-39","40-49")), size = n, replace = TRUE),
    sex = sample(factor(1:2, levels = 1:2, labels = c("male", "female")), size = n, replace = TRUE)
)
n_trial <- 20

ui <- fluidPage(
    titlePanel("Comparison of randomization designs"),
    sidebarLayout(
        sidebarPanel(
            width = 2,
            selectInput(
                inputId = "scheme",
                label = "Randomization scheme",
                choices = c(
                    "Bernoulli" = "bern",
                    "Complete" = "complete",
                    "Stratified" = "strat"
                )
            ),
            actionButton("conduct_trial", "Perform randomization")
        ),
        mainPanel(
            width = 10,
            fluidRow(
                textOutput("messages")
            ),
            fluidRow(
                column(tableOutput("data"), width = 3),
                column(plotOutput("plot_balance"), width = 9)
            )
        )
    )
)

server <- function(input, output) {
    state <- reactiveValues(
        data = df
    )
    output$messages <- renderText({})
    observeEvent(input$conduct_trial, {
        if (input$scheme=="bern") {
            output$messages <- renderText({ "Bernoulli randomization: flip a fair coin for the first 20 subjects" })
            treat <- c(rbinom(n_trial, size = 1, prob = 0.5), rep(NA, n-n_trial))
        } else if (input$scheme=="complete") {
            output$messages <- renderText({ "Complete randomization: want 10 treated and 10 control" })
            treat <- rep(NA, n)
            idx_treat <- sample.int(n, 10)
            idx_control <- sample(setdiff(seq_len(n), idx_treat), 10)
            treat[idx_treat] <- 1
            treat[idx_control] <- 0
        } else if (input$scheme=="strat") {
            output$messages <- renderText({ "Stratified randomization by sex: want 10 treated and 10 control" })
            treat <- rep(NA, n)
            idx_male <- which(state$data$sex=="male")
            idx_treat_male <- sample(idx_male, 5)
            idx_control_male <- sample(setdiff(idx_male, idx_treat_male), 5)
            idx_female <- which(state$data$sex=="female")
            idx_treat_female <- sample(idx_female, 5)
            idx_control_female <- sample(setdiff(idx_female, idx_treat_female), 5)
            treat[idx_treat_male] <- 1
            treat[idx_treat_female] <- 1
            treat[idx_control_male] <- 0
            treat[idx_control_female] <- 0
        }
        state$data$treat <- factor(treat, levels = c(0,1), labels = c("Control", "Treatment"))
    })
    output$data <- renderTable({
        state$data
    })
    output$plot_balance <- renderPlot({
        if ("treat" %in% names(state$data)) {
            tab_age <- table(age_category = state$data$age, treatment_group = state$data$treat)
            tab_sex <- table(sex = state$data$sex, treatment_group = state$data$treat)
            par(mfrow = c(1,3))
            barplot(table(state$data$treat), main = "Sample sizes in treatment arms")
            barplot(tab_age, beside = TRUE, main = "Age", legend.text = TRUE)
            barplot(tab_sex, beside = TRUE, main = "Sex", legend.text = TRUE)
        }
    })
}

shinyApp(ui = ui, server = server)
