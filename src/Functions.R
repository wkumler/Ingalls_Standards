<<<<<<< HEAD
## Functions

RemoveCsv <- function(full.filepaths) {
  # Remove a .csv file extension and obtain basename from a given list of filepaths.
  #
  # Args
  #   Character strings of filepaths in a directory.
  #
  # Returns
  #   Character strings of file basenames, without a csv extension.
  #
  no.path <- substr(full.filepaths, 1, nchar(full.filepaths)-4)
  no.ID <-   gsub("\\_.*","", no.path)
  
  return(no.path)
}
=======
findKEGGName <- function(mycompound) {
  
  output <- filter(kegg.names, Name == mycompound)
  
  return(output)
}
>>>>>>> 202f49e5590ecd139477542f69b30ddc3e450259
