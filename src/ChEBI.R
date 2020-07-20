
## Script to incorporate the ChEBI keys into the new standards sheet.

library(httr) # For requesting info from the internet
library(tidyverse)

# Define a function that accepts a KEGG ID (format cpd:C[number]) and returns
# the ChEBI ID found on the KEGG webpage. If there are multiple, return the
# first one. If not found, return NA.
grabChEBI <- function(compound_id){
  if(is.na(compound_id)) {
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
Ingalls_Lab_Standards <- read.csv("Ingalls_Lab_Standards_NEW.csv", stringsAsFactors = FALSE)

# Apply the function defined above to each ChEBI id
new_ChEBI <- sapply(unique(Ingalls_Lab_Standards$C0), grabChEBI)
# Replace the item with the obnoxious name <NA> with the character "NA"
names(new_ChEBI)[which(is.na(names(new_ChEBI)))] <- "NA"

#Merge with standards for comparison
ChEBI_comparison <- new_ChEBI %>%
  data.frame(C0=names(.), new_ChEBI=paste0("CHEBI:", .)) %>%
  left_join(Ingalls_Lab_Standards, ., by="C0")

# Select just the relevant columns for comparison
ChEBI_comparison <- ChEBI_comparison %>%
  select(Compound.Name, C0, new_ChEBI, CHEBI) %>%
  mutate(new_ChEBI = as.character(new_ChEBI))

# Investigate the items that differ
ChEBI_comparison <- ChEBI_comparison %>%
  select(Compound.Name, C0, new_ChEBI, CHEBI) %>%
  filter(new_ChEBI!=CHEBI)

# Rename columns to reflect Ingalls_Standards_NEW.csv conventions.
ChEBI_final <- ChEBI_comparison %>%
  rename(ChEBI = new_ChEBI)

# Create new standards list.
Ingalls_Lab_Standards_ChEBI <- Ingalls_Lab_Standards %>%
  left_join(ChEBI_final) %>%
  rename(ChEBI_old = CHEBI) %>%
  select(Compound.Name:Date.added, ChEBI, ChEBI_old, KEGGNAME) %>%
  unique()