addNewRow <- function(data) {
  for(i in 1:ncol(data)) {
    new.cell <- switch(class(data[1,i]),
                       "numeric" = 0,
                       "character" = "Placeholder",
                       NULL
    )
    if(i == 1) {
      new.row <- new.cell
    } else {
      new.row <- c(new.row, new.cell)
    }
  }
  data <- rbind(data, new.row)
  return(data)
}

deleteRow <- function(data, i) {
  return(data[-i,])
}
