library(shiny)
library(plotly)
library(shinydashboard)

header <- dashboardHeader(
    title = "Journal Metrics"
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Altmetrics", tabName = "altmetrics", icon = icon("hashtag")),
        menuItem("Bibliometrics", tabName = "biblio", icon = icon("book")),
        menuItem("Mendeley", tabName = "mendeley", icon = icon("chart-bar"))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "altmetrics",
            fluidRow(
                h1("Altmetrics"),
                selectInput("altVar",
                    label = "Select A Value to Display",
                    choices = c('Maximum', 'Minimum', 'Mean', 'Median'),
                    selected = "Maximum"
                ),
                h2('Altmetric Score'),
                plotlyOutput('alt', height="550px"),
                br(),
                h2('Journal Engagement'),
                selectInput("journ",
                    label = "Select a Journal",
                    choices = c("The Review of Economic Studies", "The Quarterly Journal of Economics", "The Academy of Management Annals", "Strategic Management Journal", "Review of Economic Studies",
                                "Quarterly Journal of Economics", "Management Science", "Journal of the European Economic Association", "Journal of the American Economic Association", "Journal of Political Economy",
                                "Journal of Marketing Research (JMR)", "Journal of Marketing", "Journal of Labor Economics", "Journal of Health Economics", "Journal of Financial Economics",
                                "Journal of Finance", "Journal of Economic Theory", "Journal of Econometrics", "Journal of Consumer Research", "Journal of Business Research",
                                "Journal of Accounting Research", "Journal of Accounting & Economics", "Information Systems Research", "Games & Economic Behavior", "European Economic Review",
                                "Economic Journal", "American Economic Review", "Administrative Science Quarterly", "Academy of Management Review", "Academy of Management Journal"),
                    selected = "American Economic Review"
                ),
                checkboxGroupInput("sources",
                    h3("Sources"),
                    choices = list(
                                    "Mendeley" = "mendeley", "News" = "news",
                                    "Blog" = "blog", "Facebook" = "facebook", "Wikipedia" = "wikipedia", "Google" = "google",
                                    "Syllabi" = "syllabi", "Twitter" = "twitter", "Policy" = "policy", "Peer Review" = "peer_review",
                                    "Patent" = "patent", "Weibo" = "weibo", "linkedIn" = "linkedIn", "Reddit" = "reddit",
                                    "Pinterest" = "pinterest", "F1000" = "f1000", "QA" = "qa", "Videos" = "videos"
                    ),
                    selected = list("twitter", "news", "blog", "facebook", "wikipedia", "google", "syllabi", "policy", "peer_review"),
                    inline = TRUE,
                    width = '600px'
                ),
                plotlyOutput('pie')
            )
        ),
        tabItem(tabName = "biblio",
            fluidRow(
                h1("Bibliometrics"),
                h2("Journal Summary"),
                selectInput("journ_summary",
                    label = "Select a Journal",
                    choices = c("None"),
                    selected="None"
                ),
                tableOutput('journ_summary'),
                h2("Documents Published vs. Total Citations"),
                plotlyOutput('pubVcite'),

                h2("H-Index &"),
                selectInput("hindex_comp",
                    label = "Select a Metric",
                    choices = c(
                        "Impact Factor",
                        "5 Year IF",
                        "Citations",
                        "Scimago SJR",
                        "BWL Handelsblatt Ranking",
                        "VWL Handelsblatt Ranking",
                        "Journal Quality"
                        ),
                    selected="None"
                ),
                plotlyOutput('hIndexGroup')

            )
        ),
        tabItem(tabName = "mendeley",
            fluidRow(
                h1("Mendeley"),
                h2("Where?"),
                plotlyOutput('map'),
                h2("Who?"),
                plotlyOutput('status'),
            )
        )
    )
)


dashboardPage(
    header,
    sidebar,
    body
)
