# add shinyWidgets for better selection options https://stackoverflow.com/questions/50218614/shiny-selectinput-to-select-all-from-dropdown
library(shinydashboard)
library(data.table)
library(plotly)
library(dplyr)
source("helpers.R")

library(rgeos)
library(rworldmap)


db <- start()

alt <- query(db, "select * from alt")
jd <- query(db, "select * from biblio")
mend_geo <- query(db, "select * from mendeley_country")
mend_status <- query(db, "select * from mendeley_status")
mend_doi <- query(db, "select * from mendeley_doi")
mend_dis <- query(db, "select * from mendeley_discipline")
alt_simp <- query(db, "select * from alt_simp")

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
    #####################
    #     Altmetrics    #
    #####################
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
        fig <- plot_ly(data, x=~altmetric_score, y=~journal_name, orientation='h', type='bar')
        fig <- fig %>% layout(
            xaxis = list(title="Altmetric Score"),
            yaxis = list(title="", tickfont=list(size=10), margin=list(pad=50)),
            shapes = list(vline(avg)) # add a line to indicate average across journals
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

    #print(jd$if_)
    #print(as.numeric(jd$if_))

    # https://stackoverflow.com/questions/1563961/how-to-find-top-n-of-records-in-a-column-of-a-dataframe-using-r
    n <- 10
    top_10_per_cites_cutoff <- quantile(strtoi(jd$cites), prob=1-n/100)
    top_10_per_if_cutoff <- quantile(as.numeric(jd$if_), prob=1-n/100)
    top_10_per_if_5_cutoff <- quantile(as.numeric(jd$if_5), prob=1-n/100)
    top_10_per_hindex_cutoff <- quantile(strtoi(jd$h_index), prob=1-n/100)
    top_10_per_publications_cutoff <- quantile(strtoi(jd$docs_published), prob=1-n/100)
    top_10_per_sjr_cutoff <- quantile(as.numeric(jd$sjr), prob=1-n/100)

    #output$journ_summary <- renderTable({
    #    target_journal <- input$journ_summary
    #    data <- jd[jd$journal_name == gsub('\n', '\r', target_journal), ]  #TODO: find better solution for this
    #
    #})

    output$journ_summary <- renderUI({
        target_journal <- input$journ_summary
        data <- jd[jd$journal_name == gsub('\n', '\r', target_journal), ]
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

    #####################
    #      Mendeley     #
    #####################
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

     q = paste("SELECT COUNT(alt.journal_name) as count, alt.journal_name, geo.code, geo.country
             FROM mendeley_country as geo
             JOIN mendeley_doi as doi
                ON geo.id_doi = doi.id
            JOIN alt_simp as alt
                ON alt.print_issn = doi.issn
            GROUP BY alt.journal_name, geo.code, geo.country")

    db <- start()
    data <- query(db, q)
    stop(db)

    available <- unique(data$journal_name)
    updateCheckboxGroupInput(session,
                                "map_comp_select",
                                choices=available,
                                selected=list(available[1], available[2]))

    output$map_comp <- renderPlotly({
        q = paste("SELECT COUNT(alt.journal_name) as count, alt.journal_name, geo.code, geo.country
             FROM mendeley_country as geo
             JOIN mendeley_doi as doi
                ON geo.id_doi = doi.id
            JOIN alt_simp as alt
                ON alt.print_issn = doi.issn
            GROUP BY alt.journal_name, geo.code, geo.country")

        db <- start()
        data <- query(db, q)
        stop(db)

        selected <- input$map_comp_select

        datalist = list()
        start <- 1
        stop <- length(selected)
        for (i in start:stop){
            journal_row <- data[which(data$journal_name == selected[i]), ]
            top10 <- top_n(ungroup(journal_row), 10, count)
            datalist[[i]] <- top10
        }
        big_data <- do.call(rbind, datalist)

        output$map_query_summary <- renderTable({
            big_data
        })

        df <- big_data %>%
            group_by(country, code) %>%
                filter(count == max(count)) # Limit a country to only its max

        df2 <- aggregate(count~., data = big_data, max)

        datalist = list()
        start <- 1
        stop <- length(selected)
        for (i in start:stop){
            journal_row <- df[which(df$journal_name == selected[i]), ]
            top1 <- top_n(ungroup(journal_row), 1, count)
            datalist[[i]] <- top1
        }
        big_data <- do.call(rbind, datalist)

        curr_countries <- big_data$country
        all_countries <- data$country

        start <- 1
        stop <- nrow(big_data)
        for (i in start:stop){
            row <- big_data[i, ]
            target <- row$journal_name
            country <- row$country

            # if country is the highest listed country for target journal, do nothing
            # otherwise update row in big_data to have the highest from df2

            # max journal for country in df2
            #temp <- top_n(df2[df2$journal_name == target, ], 1, count)
            temp <- df2[df2$journal_name == target, ]
            max <- big_data[i, ]$count
            for (j in 1:nrow(temp)){
                if (country != temp[j, ]$country && !(temp[j, ]$country %in% curr_countries) && !is.na(big_data[i, ]$country)){
                    temp_count <- temp[j, ]$count
                    if ( temp_count > max ){
                        big_data[i, ]$code <- temp[j, ]$code
                        big_data[i, ]$country <- temp[j, ]$country
                        big_data[i, ]$journal_name <- temp[j, ]$journal_name
                        big_data[i, ]$count <- temp[j, ]$count
                        curr_countries[[length(curr_countries)]] <- temp[j, ]$country
                        max <- temp_count
                    }
                }
            }
        }


        df <- big_data

        g <- list(
            scope = 'world',
            projection = list(type = 'albers'),
            showland=T,
            landcolor = toRGB("white")
        )
        fig <- plot_ly(df, z = df$count, type = 'choropleth', locations = df$code, showscale=F, text=df$journal_name,
                       hoverinfo="text",
                       hovertext=paste("Country: ", df$country,
                                       "<br>Journal: ", df$journal_name,
                                       "<br>Readers: ", df$count)

                       )

        box1 <- list(
                    x = 0.3,
                    y = 0.5,
                    yanchor = "top",
                    borderpad = 2,
                    bordercolor = rgb(0.5,0.1,0.5),     # set this same as color of cluster 1
                    borderwidth = 5,
                    text = paste(df$journal_name[1]),
                    align = "left",
                    showarrow = F
                )

        fig <- fig %>%
                layout(geo = g)

        fig <- fig %>%
                layout(annotations = list(box1))
    })


    output$map_comp2 <- renderPlotly({
        selected <- input$map_comp_select
        q = paste("SELECT COUNT(alt.journal_name) as count, alt.journal_name, geo.code, geo.country
             FROM mendeley_country as geo
             JOIN mendeley_doi as doi
                ON geo.id_doi = doi.id
            JOIN alt_simp as alt
                ON alt.print_issn = doi.issn
            GROUP BY alt.journal_name, geo.code, geo.country")

        db <- start()
        data <- query(db, q)
        stop(db)

        # Find center long/lat for countries
        wmap <- getMap(resolution="high")
        # get centroids
        centroids <- gCentroid(wmap, byid=TRUE)
        # get a data.frame with centroids
        coords <- as.data.frame(centroids)

        # Add center locations to data
        data[,'x'] <- NA
        data[,'y'] <- NA

        replacements <- c("Bahamas","Macao","Republic of Singapore","Serbia and Montenegro","United States","Hong Kong","Tanzania")
        exceptions <- c("The Bahamas","Macau S.A.R","Singapore","Montenegro","United States of America","Hong Kong S.A.R.","United Republic of Tanzania")

        for (i in 1:length(rownames(coords))){
            name <- rownames(coords[i, ])
            x <- coords[i, "x"]
            y <- coords[i, "y"]
            if (name %in% exceptions){
                i <- match(name, exceptions)
                data_name <- replacements[i]
            } else {
                data_name <- name
            }
            data[data$country == data_name, "x"] <- x
            data[data$country == data_name, "y"] <- y
        }

        data <- data[data$journal_name %in% selected, ]

        g <- list(
            scope = 'world',
            projection = list(type = 'albers'),
            showland=T,
            landcolor = toRGB("white")
        )
        fig <- plot_geo(data, sizes = c(1, 2500) )
        fig <- fig %>% add_markers(
            x = ~x, y = ~y, size=~count, color=~count,
            hoverinfo="text",
            hovertext=paste("Country: ", data$country,
                            "<br>Journal: ", data$journal_name,
                            "<br>Readers: ", data$count,
                            "<br>X: ", data$x,
                            "<br>Y: ", data$y)
        )
        fig <- fig %>% layout(title='Most Readers for Selected Journals', geo=g, autosize=T)
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

    #####################
    #       Testing     #
    #####################
    #q = "Select bib.journal_name, bib.issn1, bib.if_, bib.sjr, bib.cites, bib.bwl, bib.vwl, alt.altmetric_score, alt.mendeley
    #     FROM biblio as bib
    #     INNER JOIN alt_simp as alt
    #        ON bib.issn1 = alt.print_issn"
    q = "Select bib.journal_name, bib.issn1, bib.if_, bib.sjr, bib.cites, alt.altmetric_score, alt.mendeley
         FROM biblio as bib
         INNER JOIN alt_simp as alt
            ON bib.issn1 = alt.print_issn"
    db <- start()
    data_spider <- query(db, q)
    stop(db)

    output$query_summary <- renderTable({
        data_spider
    })

    journal_list = unique(data_spider$journal_name)
    updateCheckboxGroupInput(session, "spider_journals", choices=journal_list, selected=list(journal_list[2], journal_list[3]))

    output$spider <- renderPlotly({
        data <- data_spider
        journals <- input$spider_journals
        #measures <- c('if_', 'sjr', 'bwl', 'vwl', 'Altmetric', 'Readers', 'Citations', 'if_')
        measures <- c('Impact Factor', 'SJR', 'Altmetric', 'Readers', 'Citations', 'Impact Factor')
        fig <- plot_ly(
            type = 'scatterpolar',
            fill = 'toself'
        )

        #bwl = c('C', 'A+'),
        #vwl = c('C', 'A+'),
        maxmin = data.frame(
                        if_ = c(0, 5),
                        sjr = c(1, 15),
                        Altmetric = c(500, 10000),
                        Readers = c(3000, 250000),
                        Citations = c(100, 2000)
                    )

        for (journal in journals){
            #xy.list <- split(xy.df, seq(nrow(xy.df)))
            #journal_data <- as.character(data[data$journal_name == journal, 3:9])
            print(data[data$journal_name == journal, 3:7])
            journal_data <- as.character(data[data$journal_name == journal, 3:7])
            expanded <- c(journal_data, journal_data[1])
            fig <- fig %>%
                add_trace(
                    r = expanded,
                    theta = measures,
                    name = journal
                )
        }

        fig <- fig %>%
        layout(
            polar = list(
                radialaxis = list(
                    visible = T,
                    range = maxmin,
                    type="log"
                )
            )
        )

        fig
    })

    q = "SELECT alt.journal_name, status.status, sum(status.count) as total
         FROM mendeley_status as status
         JOIN mendeley_doi as doi
            ON status.id_doi = doi.id
         JOIN alt_simp as alt
            ON alt.print_issn = doi.issn
        GROUP BY alt.journal_name, status.status"
    db <- start()
    data <- query(db, q)
    stop(db)
    journal_list = unique(data$journal_name)
    updateCheckboxGroupInput(session, "bubble_readers_status_journals", choices=journal_list, selected=list(journal_list[1], journal_list[2]))

    output$bubble_readers_status <- renderPlotly({

        selected <- input$bubble_readers_status_journals

        data <- data[data$journal_name %in% selected, ]

        for (i in 1:length(selected)){
            s <- sum(data[data$journal_name == selected[i], 3])
            data[nrow(data)+1, ] = list("", selected[i], s)
        }
        data[['ids']] <- paste(data$journal_name, data$status, sep="")

        fig <- plot_ly(
            type='treemap',
            labels=data$status,
            parents=data$journal_name,
            values=data$total,
            branchvalues="total",
            ids=data$ids,
            hovertemplate = paste("Journal: ", data$journal_name, "<br>Status: ", data$status, "<br>Total: ", data$total),
            pathbar=list(visible= TRUE)
        )
        fig <- fig %>% layout(
            grid=list(columns=3),
            margin=list(l=0, r=0, b=0, t=0))
        fig
    })

}
