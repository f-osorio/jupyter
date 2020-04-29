# Create SQLite database for Shiny App
library(RSQLite)

db <- dbConnect(RSQLite::SQLite(), "local.sql")  #":memory:")

# Drop tables if they exist
tables <- list('biblio', 'alt', 'alt_simp', 'mendeley_doi', 'mendeley_country', 'mendeley_status', 'mendeley_discipline')
for (table in tables){
    if (RSQLite::dbExistsTable(db, table)){
        RSQLite::dbRemoveTable(db, table)
    }
}

# Add tables
RSQLite::dbWriteTable(conn = db, name = "biblio", # Save csv into database
                  value = "biblio_data.csv",
                  row.names = TRUE, header = T, sep=';',
                  colClasses='character',
                  overwrite=TRUE)

readr::read_delim_chunked("simplified_alt.csv", function(chunk, x){
    RSQLite::dbWriteTable(conn = db, chunk, name = "alt_simp", # Save csv into database
                  row.names = TRUE, header = T, sep=';',
                  colClasses='character',
                  append=TRUE)
}, delim=";")

readr::read_delim_chunked("cleaned_altmetrics.csv", function(chunk, x){
    RSQLite::dbWriteTable(conn = db, chunk, name = "alt", # Save csv into database
                  row.names = TRUE, header = T, sep=';',
                  colClasses='character',
                  append=TRUE)
}, delim=";")

readr::read_delim_chunked("mendeley_doi.csv", function(chunk, x){
    RSQLite::dbWriteTable(conn = db, chunk, name = "mendeley_doi", # Save csv into database
                  row.names = TRUE, header = T, sep=';',
                  colClasses='character',
                  append=TRUE)
}, delim=";")

RSQLite::dbWriteTable(conn = db, name = "mendeley_doi_simp", # Save csv into database
                  value = "simplified_mend_doi.csv",
                  row.names = TRUE, header = T, sep=';',
                  colClasses='character',
                  overwrite=TRUE)

RSQLite::dbWriteTable(conn = db, name = "mendeley_country", # Save csv into database
                  value = "mendeley_country.csv",
                  row.names = TRUE, header = T, sep=';',
                  colClasses='character',
                  overwrite=TRUE)

RSQLite::dbWriteTable(conn = db, name = "mendeley_status", # Save csv into database
                  value = "mendeley_status.csv",
                  row.names = TRUE, header = T, sep=';',
                  colClasses='character',
                  overwrite=TRUE)

RSQLite::dbWriteTable(conn = db, name = "mendeley_discipline", # Save csv into database
                  value = "mendeley_discipline.csv",
                  row.names = TRUE, header = T, sep=';',
                  colClasses='character',
                  overwrite=TRUE)


# Create Indexes
dbExecute(db,"CREATE INDEX biblio_issn ON biblio (issn1)")
dbExecute(db,"CREATE INDEX alt_issn ON alt (print_issn)")
dbExecute(db,"CREATE INDEX mendeley_doi_issn ON mendeley_doi (issn)")

dbDisconnect(db)
