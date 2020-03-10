library(RSQLite)

data('mtcars')
mtcars$car_names <- rownames(mtcars)
rownames(mtcars) <- c()
head(mtcars)
conn <- dbConnect(RSQLite::SQLite(), "CarsDB.db")
