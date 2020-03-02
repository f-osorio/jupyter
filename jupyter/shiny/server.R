library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(plotly)


df <- read.csv('data/cleaned_altmetrics.csv', header=TRUE, sep=';')
d <- aggregate(altmetric_score ~ journal_name, df, max)
a <- aggregate(altmetric_score ~ journal_name, df, mean)
summary <- setDT(df)[, c(lapply(.SD[, c(10:27), with=FALSE], sum)), by = journal_name]
sub <- data.frame(subset(summary, journal_name == 'Journal of Finance'))

flipped <- as.data.frame(t(sub))
flipped <- setDT(flipped, keep.rownames = TRUE)[]
names(flipped)[1] <- 'key'
names(flipped)[2] <- 'value'
# remove first row which has a string
flipped <- flipped[-1,]
# make sure there are no strings
flipped$values <- as.numeric(as.character(flipped$value))

function(input, output){
    output$altScorePlot <- renderPlot({
        ggplot(d, aes(x=journal_name, y=altmetric_score)) +
            geom_bar(stat="identity") +
            theme(axis.text.x = element_text(angle=45, hjust=1)) +
            labs(title = "Highest Score", x = "Journal Name", y = "Altmetric Score")
    })

    output$avgScorePlot <- renderPlot({
        ggplot(a, aes(x=journal_name, y=altmetric_score)) +
            geom_bar(stat="identity") +
            theme(axis.text.x = element_text(angle=45, hjust=1)) +
            labs(title = "Average Score", x = "Journal Name", y = "Altmetric Score")
    })

    output$pieChart <- renderPlot({
        plot_ly(flipped, labels=~key, values = ~values, type='pie') %>%
            layout(title = 'Journal of Finance Engagement',
                   xaxis = list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE),
                   yaxis = list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE))
    })
}
