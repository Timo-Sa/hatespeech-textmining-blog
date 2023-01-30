read_test_data <- function(data_path = "data/GermEval-2018-Data-master/germeval2018.test.txt") {
  
  d <- data_read(data_path, header = FALSE, quote = "")
  
  names(d) <- c("text", "c1", "c2")
  
  d$id <- as.character(1:nrow(d))
  
  return(d)
  
}
