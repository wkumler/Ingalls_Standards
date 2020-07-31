library(httr) #For requesting info from the internet
library(dplyr) # For piping and data shaping

# Define a function that accepts a KEGG ID (format cpd:C[number]) and returns
# the ChEBI id found on the KEGG webpage. If there are multiple, return the
# first one. If not found, return NA.
grabChEBI <- function(compound_id){
  if(is.na(compound_id)){
    return(NA)
  }
  ChEBI <- compound_id %>%
    paste0("http://rest.kegg.jp/get/", .) %>%
    GET() %>%
    content() %>%
    strsplit("\\n") %>%
    unlist() %>%
    grep(pattern = "ChEBI", value = TRUE) %>%
    gsub(pattern = " *ChEBI: ", replacement = "") %>%
    gsub(pattern = " .*", replacement = "")
  if(length(ChEBI)==0){
    return(NA)
  }
  ChEBI
}

# Grab the current standards list from the internet
stans <- read.csv("data_raw/Ingalls_NOCHEBI.csv")

# Apply the function defined above to each ChEBI id
new_ChEBI <- sapply(unique(stans$C0), grabChEBI)
# Replace the item with the obnoxious name <NA> with the character "NA"
names(new_ChEBI)[which(is.na(names(new_ChEBI)))] <- "NA"

#Merge with standards for comparison
ChEBI_comparison <- new_ChEBI %>%
  data.frame(C0=names(.), new_ChEBI=paste0("CHEBI:", .)) %>%
  left_join(stans, ., by="C0")

# Select just the relevant columns for comparison
ChEBI_comparison %>%
  select(Compound.Name_figure, C0, new_ChEBI, ChEBI)

# Investigate the items that differ
ChEBI_comparison %>%
  select(Compound.Name_figure, C0, new_ChEBI, ChEBI) %>%
  filter(new_ChEBI!=ChEBI)
