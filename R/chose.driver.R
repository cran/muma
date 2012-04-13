chose.driver <-
function(scaling) {
  pwd.n = paste(getwd(), "/Preprocessing_Data_", scaling, "/NormalizedTable.csv", sep="")
  x <- read.csv(pwd.n, sep=",", header=TRUE)
  print(x[,1])
  }
