fluidPage(
    titlePanel("Journal Altmetrics"),
    sidebarLayout(
        sidebarPanel("Sidebar Panel"),
        mainPanel(
            h1('Altmetric Scores'),
            plotOutput('altScorePlot'),
            br(),
            br(),
            br(),
            plotOutput('avgScorePlot'),
            br(),
            br(),
            br(),
            plotOutput('pieChart')
        )
    )
)
