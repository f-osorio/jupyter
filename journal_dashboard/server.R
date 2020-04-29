library(shinydashboard)
library(data.table)
library(plotly)
library(dplyr)


alt <- read.csv('./cleaned_altmetrics.csv', header=TRUE, sep=';')
jd <- read.csv('./biblio_data.csv', header=FALSE, sep=';')
colnames(jd) <- c('handle', 'year', 'cites', 'if_', 'if_5', 'docs_published', 'h_index', 'type', 'issn1', 'issn2', 'type2', 'year3',
                  'scimago_id', 'sjr', 'type4', 'year5', 'jourqual', 'type6', 'year7', 'bwl', 'type8', 'year9', 'vwl', 'journal_name')
jd <- jd %>% replace(.=="null", 0)
mend_geo <- read.csv('./mendeley_country.csv', header=TRUE, sep=';')
mend_status <- read.csv('./mendeley_status.csv', header=TRUE, sep=';')
mend_doi <- read.csv('./mendeley_doi.csv', header=TRUE, sep=';')
alt_simp <- read.csv('./simplified_alt.csv', header=TRUE, sep=';')


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
  )}

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

        # Get average for current selection
        avg <- mean(data[['altmetric_score']])

        fig <- plot_ly(data, x=~altmetric_score, y=~journal_name, orientation='h', type='bar', name="test")
        fig <- fig %>% layout(
            xaxis = list(title="Altmetric Score"),
            yaxis = list(title="", tickfont=list(size=10), margin=list(pad=50)),
            shapes = list(vline(avg)) # add a line to indicate average across journals
        )
    })

    output$pie <- renderPlotly({
        include <- input$sources
        target_journal <- input$journ

        summary <- setDT(alt)[, c(lapply(.SD[, c(10:27), with=FALSE], sum)), by=journal_name]
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

    output$social_bar_comp <- renderPlotly({
        # group will be social media source
        selected_journals <- input$social_media_journals
        selected_types <- input$social_media_types

        alt_simp <- alt_simp[alt_simp$journal_name %in% selected_journals, ] # limit to selected journals

        keep <- c('journal_name', selected_types)
        data <- subset(alt_simp, select = keep)
        data <- setNames(data.frame(t(data)), data[,1])
        setDT(data, keep.rownames = "Sources")[]
        data = as.data.frame(data[-1,])
        #print(data)

        fig <- plot_ly(data, type='bar')
        for(i in 2:ncol(data)){
            #print(paste("???", colnames(data)[i]))
            fig <- add_trace(fig, x = ~Sources, y = data[,i], name = colnames(data)[i])
        }
        fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')

        fig
    })

    #####################
    #    Journal Data   #
    #####################
    journal_list = unique(jd$journal_name)
    jl <- sort(journal_list)
    jl <- jl[-1]
    updateSelectInput(session, "journ_summary", choices=jl, selected=jl[1])

    jd[jd == ''] <- 0 # Set empty values to 0
    jd[is.na(jd)] <- 0 # Set NA values to 0

    #print(jd$if_)
    #print(as.numeric(jd$if_))

    # https://stackoverflow.com/questions/1563961/how-to-find-top-n-of-records-in-a-column-of-a-dataframe-using-r
    n <- 10
    print('###############')
    print(jd[1, ])
    top_10_per_cites_cutoff <- quantile(jd$cites, prob=1-n/100)
    top_10_per_if_cutoff <- quantile(jd$if_, prob=1-n/100)
    top_10_per_if_5_cutoff <- quantile(jd$if_5, prob=1-n/100)
    top_10_per_hindex_cutoff <- quantile(jd$h_index, prob=1-n/100)
    top_10_per_publications_cutoff <- quantile(jd$docs_published, prob=1-n/100)
    top_10_per_sjr_cutoff <- quantile(jd$sjr, prob=1-n/100)

    #output$journ_summary <- renderTable({
    #    target_journal <- input$journ_summary
    #    data <- jd[jd$journal_name == gsub('\n', '\r', target_journal), ]  #TODO: find better solution for this
    #
    #})

    output$journ_summary <- renderUI({
        target_journal <- input$journ_summary
        data <- jd[jd$journal_name == target_journal, ]
        HTML(paste("<table>
                        <tr>
                            <th>", data$journal_name,"</th>
                            <th></th>
                            <th></th>
                        </tr>
                        <tr>
                            <td>Citations</td>
                            <td>", data$cites,"</td>
                            <td>",
                                if (data$cites > top_10_per_cites_cutoff){
                                    "<strong>Top 10%</strong>"
                                }
                            ,"</td>
                        </tr>
                        <tr>
                            <td>Impact Factor</td>
                            <td>", data$if_,"</td>
                            <td>",
                                if (data$if_ > top_10_per_if_cutoff){
                                    "<strong>Top 10%</strong>"
                                }
                            ,"</td>
                        </tr>
                        <tr>
                            <td>5 Year Impact Factor</td>
                            <td>", data$if_5,"</td>
                            <td>",
                                if (data$if_5 > top_10_per_if_5_cutoff){
                                    "<strong>Top 10%</strong>"
                                }
                            ,"</td>
                        </tr>
                        <tr>
                            <td>H Index</td>
                            <td>", data$h_index,"</td>
                            <td>",
                                if (data$h_index > top_10_per_hindex_cutoff){
                                    "<strong>Top 10%</strong>"
                                }
                            ,"</td>
                        </tr>
                        <tr>
                            <td>SJR</td>
                            <td>", data$sjr,"</td>
                            <td>",
                                if (data$sjr > top_10_per_sjr_cutoff){
                                    "<strong>Top 10%</strong>"
                                }
                            ,"</td>
                        </tr>
                        <tr>
                            <td>BWL</td>
                            <td>", data$bwl,"</td>
                            <td></td>
                        </tr>
                        <tr>
                            <td>VWL</td>
                            <td>", data$vwl,"</td>
                            <td></td>
                        </tr>
                        <tr>
                            <td>Journal Quality</td>
                            <td>", data$jourqual,"</td>
                            <td></td>
                        </tr>
                    </table>
        "))
    })

    output$pubVcite <- renderPlotly({
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
                summarize(count=sum(count))

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
                summarize(count=sum(count))

        fig <- plot_ly(status_sum,
                        x = ~status,
                        y = ~count,
                        type = 'bar'
        )

    })

}
