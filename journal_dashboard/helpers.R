library(RSQLite)

start <- function(){
    db <- dbConnect(RSQLite::SQLite(), "local.sql")
    return(db)
}

stop <- function(db){
    dbDisconnect(db)
    return(0)
}

query <- function(db, q){
    start_time <- Sys.time()
    res <- dbGetQuery(db, q)
    end_time <- Sys.time()
    print(q)
    print(end_time - start_time)
    return(res)
}
