library(shinydashboard)
library(data.table)
library(plotly)
library(dplyr)
source("helpers.R")


db <- start()
alt <- query(db, "select * from alt")
jd <- query(db, "select * from biblio")
mend_geo <- query(db, "select * from mendeley_country")
mend_status <- query(db, "select * from mendeley_status")
mend_doi <- query(db, "select * from mendeley_doi")
mend_dis <- query(db, "select * from mendeley_discipline")
stop(db)

jd <- jd %>% replace(.=="null", 0)

# https://stackoverflow.com/questions/34093169/horizontal-vertical-line-in-plotly
vline <- function(x = 0, color = "red") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color)
  )
}

function(input, output, session){
    # Altmetrics
    output$alt <- renderPlotly({
        max <- aggregate(altmetric_score ~ journal_name, alt, max)
        min <- aggregate(altmetric_score ~ journal_name, alt, min)
        mean <- aggregate(altmetric_score ~ journal_name, alt, mean)
        median <- aggregate(altmetric_score ~ journal_name, alt, median)

        data <- switch(input$altVar,
            "Maximum" = max,
            "Minimum" = min,
            "Mean" = mean,
            "Median" = median
        )
        data[data == ''] <- NA # Set empty journal name to NA
        data <- na.omit(data)  # Remove NA

        fig <- plot_ly(data, x=~altmetric_score, y=~journal_name, orientation='h', type='bar', name="test")
        fig <- fig %>% layout(
            xaxis = list(title="Altmetric Score"),
            yaxis = list(title="", tickfont=list(size=10), margin=list(pad=50))
        )
    })

    output$pie <- renderPlotly({
        include <- input$sources
        target_journal <- input$journ

        summary <- setDT(alt)[, c(lapply(.SD[, c(12:29), with=FALSE], sum)), by=journal_name]
        sub <- data.frame(subset(summary, journal_name == target_journal))
        flipped <- as.data.frame(t(sub))
        flipped <- setDT(flipped, keep.rownames = TRUE)[]
        names(flipped)[1] <- 'key'
        names(flipped)[2] <- 'value'
        # remove first row which has a string >> "journal_name"
        flipped <- flipped[-1,]
        # make sure there are no strings
        flipped$values <- as.numeric(as.character(flipped$value))
        # limit to just the options selected for sources
        flipped <- flipped[flipped$key %in% include, ]

        plot_ly(flipped, labels=~key, values = ~values, type='pie') %>%
            layout(title =target_journal,
                   xaxis = list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE),
                   yaxis = list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE))
    })

    # Journal Data
    journal_list = unique(jd$journal_name)
    jl <- sort(journal_list)
    jl <- jl[-1]
    updateSelectInput(session, "journ_summary", choices=jl, selected=jl[1])

    jd[jd == ''] <- 0 # Set empty values to 0
    output$journ_summary <- renderTable({
        target_journal <- input$journ_summary
        data <- jd[jd$journal_name == target_journal, ]

    })

    output$pubVcite <- renderPlotly({
        print(colnames(jd))
        fig <- plot_ly(
            jd,
            type='scatter',
            mode='markers',
            marker=list(
                color=~if_,
                #colorbar=list(
                #    title='IF'
                #),
                colorscale='Portland',
                reversescale=F
            ),
            x = ~docs_published,
            y = ~cites,
            text = ~paste(
                    journal_name,
                    '<br>Publications:', docs_published,
                    '<br>Citations:', cites,
                    '<br>Impact Factor: ', if_
            )
        )
        fig <- layout(fig,
            xaxis=list(type="log", title="Documented Published"),
            yaxis=list(type="log", title="Citations")
        )
    })

    output$hIndexGroup <- renderPlotly({
        # Group bar graph of H index and IF
        label <- input$hindex_comp
        comp <- switch(input$hindex_comp,
            "Impact Factor" = "if_",
            "5 Year IF" = "if_5",
            "Citations" = "cites",
            "Scimago SJR" = "sjr",
            "BWL Handelsblatt Ranking" = "bwl",
            "VWL Handelsblatt Ranking" = "vwl",
            "Journal Quality" = "jourqual"
        )

        fig <- plot_ly(
            jd,
            x = ~h_index,
            y = jd[[comp]],
            type='scatter',
            mode='markers',
            marker = list(
                size=10,
                color='rgba(225, 182, 193, .9)',
                line = list(color='rgba(152, 0, 0, .8)',
                    width = 2)
            ),
            text = ~paste(
                    journal_name,
                    '<br>', label, ':', jd[[comp]]
            )
        )
        fig <- layout(fig,
            xaxis=list(title="H-Index"),
            yaxis=list(title=label)
        )
    })

    # Mendeley
    output$map <- renderPlotly({
        geo_sum <- mend_geo %>%
            group_by(country, code) %>%
                summarize(count=sum(strtoi(count)))

        df <- as.data.frame(geo_sum)
        # light grey boundaries
        l <- list(color = toRGB("grey"), width = 0.5)

        # specify map projection/options
        g <- list(
            showframe = TRUE,
            showcoastlines = FALSE,
            projection = list(type = 'Mercator')
        )
        fig <- plot_geo(df)
        fig <- fig %>% add_trace(
            z = ~count,
            color = ~count,
            colorscale = 'Heat',
            text = ~country,
            locations = ~code,
            marker = list(line = l)
        )
        fig <- fig %>% colorbar(title = 'Downloads?')
        fig <- fig %>% layout(
            title = 'Mendeley Distribution',
            geo = g
        )
        fig
    })

    output$status <- renderPlotly({
        combined <- merge(x = mend_status, y = mend_doi, by.x = 'id_doi', by.y = 'id')
        status_sum <- mend_status %>%
            group_by(status) %>%
                summarize(count=sum(strtoi(count)))

        fig <- plot_ly(status_sum,
                        x = ~status,
                        y = ~count,
                        type = 'bar'
        )

    })

}
