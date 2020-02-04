extract_province <- function(object, i, by) {
  if (i == 'global') {
    res <- cbind(name = object$areaTree[[1]], object$areaTree[[by]])
    return(res)
  } 
  
  d <- object$areaTree[1,2][[1]]
  name = d[[1]]
  if (is.character(i)) {
    i <- which(name == i)
  }
  stats <- d[i, 2][[1]]
  cbind(name=stats$name, stats[[by]])
}

.get_json <- function() {
  url <- 'https://view.inews.qq.com/g2/getOnsInfo?name=disease_h5&callback=1580373566110'
  x <- suppressWarnings(readLines(url, encoding="UTF-8"))
  x <- sub("^\\d+\\(", "", x)
  x <- sub("\\)$", "", x)
  y <- jsonlite::fromJSON(x)
  return(y$data)  
}

