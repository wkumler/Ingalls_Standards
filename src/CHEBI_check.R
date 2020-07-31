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

# Reassign the FigNames standards
latest.standards <- Ingalls_Lab_Standards_FigNames

# Apply the function defined above to each ChEBI id
new_ChEBI <- sapply(unique(latest.standards$C0), grabChEBI)
# Replace the item with the obnoxious name <NA> with the character "NA"
names(new_ChEBI)[which(is.na(names(new_ChEBI)))] <- "NA"

#Merge with standards for comparison
ChEBI_comparison <- new_ChEBI %>%
  data.frame(C0=names(.), new_ChEBI=paste0("CHEBI:", .)) %>%
  left_join(latest.standards, ., by="C0")

# Select just the relevant columns for comparison
ChEBI_comparison <- ChEBI_comparison %>%
  select(Compound.Name, C0, new_ChEBI, CHEBI)

# Investigate the items that differ
ChEBI_comparison_diff <- ChEBI_comparison %>%
  select(Compound.Name, C0, new_ChEBI, CHEBI) %>%
  filter(new_ChEBI!=CHEBI)

Ingalls_Lab_Standards_ChEBI <- ChEBI_comparison_diff %>%
  full_join(Ingalls_Lab_Standards_FigNames) %>%
  unique() %>%
  rename(ChEBI = new_ChEBI,
         ChEBI_old = CHEBI) %>%
  select(Compound.Type, Column, Compound.Name, Compound.Name_old, Compound.Name_figure,
         QE.LinRange:z, C0, ChEBI, ChEBI_old, Fraction1:KEGGNAME)
