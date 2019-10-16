library(data.table)
# create table 1
DT1 = data.table(id = 1:5, x = letters[1:5], a = 11:15, b = 21:25)
DT3 = copy(DT1)
# create table 2 with changed values for a, b via pre-determined cols
DT2 = copy(DT1)
cols <- c("a", "b")
DT2= DT2[, (cols) := lapply(.SD, function(x) x * 2), .SDcols = cols][1:3,]

cols_new <- c("a", "b")
cols_old <- c('i.a', 'i.b')
DT1[DT2, (cols_new) := mget(cols_old), on = c(id = "id")]