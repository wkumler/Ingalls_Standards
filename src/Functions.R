findKEGGName <- function(mycompound) {
  
  output <- filter(kegg.names, Name == mycompound)
  
  return(output)
}