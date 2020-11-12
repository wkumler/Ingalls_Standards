library(tidyverse)

ImportMSP <- function(Ingalls.msp.filepath) {
  # Imports .msp files containing MS2 information as text files. 
  #
  # Args
  #   Ingalls.msp.filepath: string containing the path to the relevant msp file,
  #                         containing several lines of compound-specific information 
  #                         followed by two columns, the first m.z and the second intensity.
  #
  # Returns
  #   Ingalls.msp.df: msp file, able to be manipulated by R.
  #
  Ingalls.msp <- read.delim(Ingalls.msp.filepath, header = FALSE, sep = "") 
  
  if (ncol(Ingalls.msp) > 2) {
    Ingalls.msp2 <- Ingalls.msp %>%
      unite("ColumnB", V2:ncol(Ingalls.msp), sep = " ") %>%
      rename(ColumnA = V1)
  } else {
    Ingalls.msp2 <- Ingalls.msp %>%
      rename(ColumnA = V1,
             ColumnB = V2)
  }
  
  Ingalls.msp.df <- as.data.frame(Ingalls.msp2) %>%
    mutate(Set = cumsum(ColumnA == "NAME:")) %>%
    group_by(Set) %>%
    mutate(Information.Column = ifelse(str_detect(ColumnA, "[[:alpha:]]"), paste(ColumnB[ColumnA == "NAME:"], "info"),
                                       paste(ColumnB[ColumnA == "NAME:"], "MS2"))) %>%
    ungroup()
  
  return(Ingalls.msp.df)
}

ExtractCompoundInfo <- function(msp.file, compound.info) {
  # Isolate user-defined compound parameter. Choices are compound name, 
  # retention time, or mz.
  #
  # Args
  #   msp.file: Ingalls msp file containing several lines of compound-specific information
  #             followed by two columns, the first m.z and the second intensity. 
  #             compound.info: String of the relevant parameter. Options are "retention", 
  #             "precursor", "adduction", and "metabolite".
  #
  # Returns
  #   compound.info.df: two-column dataframe containing the compound ID and the user-defined
  #                     compound parameter.
  #
  compound.info.df <- msp.file %>%
    filter(grepl(compound.info, ColumnA, ignore.case = TRUE)) %>%
    select(ColumnA, ColumnB, Set) %>%
    rename(MYCOLUMN = ColumnB) %>%
    select(Set, MYCOLUMN) %>%
    mutate(Set = as.character(Set))
  
  return(compound.info.df)
}

ExtractMS2 <- function(msp.file) {
  # Group compounds together, extract MS2 values, and collapse to one row 
  # in the accepted [m/z1, intensity1; m/z2, intensity2;] format.
  #
  # Args
  #   msp.file: Ingalls msp file containing several lines of compound-specific information
  #   followed by two columns, the first m.z and the second intensity.
  #
  # Returns
  #   ms2.values.no.ws: Isolated compound ID and MS2 values in the accepted format.
  #   
  ms2.values <- msp.file %>%
    group_by(Set) %>%
    mutate(combined = ifelse(str_detect(ColumnB, "[[:digit:]]") & str_detect(ColumnA, "[[:digit:]]"), 
                             paste0(ColumnA, ", ", ColumnB), ColumnB)) %>%
    
    select(Information.Column, combined, Set) %>%
    filter(str_detect(combined, ",")) %>%
    group_by(Set) %>% 
    summarise(MS2 = paste0(combined, collapse = "", "; ")) %>%
    ungroup()
  
  ms2.values.no.ws <- as.data.frame(apply(ms2.values, 2, function(x) gsub("\\s+;", ";", x)))
  
  return(ms2.values.no.ws)
}

## Import existing MS2 information for the standards in original msp format
HILICPos.msp <- ImportMSP("data_raw/Ingalls_HILICPos_Standards.msp")
HILICNeg.msp <- ImportMSP("data_raw/Ingalls_HILICNeg_Standards.msp")
CyanoAq.msp <- ImportMSP("data_raw/Ingalls_CyanoAq_Standards.msp")

## Extract MS2 data for each compound and rearrange to accepted format
HILICPos.MS2 <- ExtractMS2(HILICPos.msp)
HILICNeg.MS2<- ExtractMS2(HILICNeg.msp)
CyanoAq.MS2 <- ExtractMS2(CyanoAq.msp)

# Extract relevant compound data
compound.info <- c("Retention", "Precursor", "Adduction", "Metabolite")

HILICPos.rt <- ExtractCompoundInfo(HILICPos.msp, compound.info = compound.info[1]) %>%
  rename(Retention.Time = MYCOLUMN) %>%
  mutate(Retention.Time = as.numeric(Retention.Time),
         Retention.Time = (60 * Retention.Time))
HILICPos.mz <- ExtractCompoundInfo(HILICPos.msp, compound.info = compound.info[2]) %>%
  mutate(mz = MYCOLUMN)
HILICPos.adduct <- ExtractCompoundInfo(HILICPos.msp, compound.info = compound.info[3]) %>%
  mutate(Adduct.Name = MYCOLUMN)

HILICPos.name <- HILICPos.msp %>%
  filter(ColumnA == "NAME:") %>%
  select(ColumnA, ColumnB, Set) %>%
  rename(Compound.Name = ColumnB) %>%
  select(Set, Compound.Name) %>%
  mutate(Set = as.character(Set))

# Combine to df

HILICPos.final <- HILICPos.name %>%
  left_join(HILICPos.rt) %>%
  left_join(HILICPos.mz) %>%
  left_join(HILICPos.MS2)