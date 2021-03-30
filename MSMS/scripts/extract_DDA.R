library(data.table)
library(RaMS)
library(tidyverse)

## Setup ----

## Set absolute path to the Google Drive. The MS2 files live there and you must have your working directory 
## Ensure the getwd() command properly connects to the shared Drive from your filepath!

# setwd("G:\\Shared drives\\Ingalls Lab\\Collaborative_Projects\\Standards\\Ingalls_Standards\\MSMS") # Windows
setwd("~/Google Drive/Shared drives/Ingalls Lab/Collaborative_Projects/Standards/Ingalls_Standards/MSMS/") # Mac

## Check for correct working directory.
if(str_detect(getwd(), "Ingalls_Standards/MSMS")) {
  print("Good job, you're in the right directory!")
} else {
  stop("You may not be in the correct directory. Check your path and try again.")
}

# Define functions ----
extractMSMSdata <- function(compound.name, mz.standard, rt.standard, filename.standard, ppm, rt.flex) {
  # Isolate MS2 data from the Ingalls Standards MS2 data.
  # Needs to be updated!
  # Args
  #   compound.name, mz.standard, rt.standard: Parameters of the compound to retrieve MS2 for.
  #   ppm, rt.flex: User-defined flexibility for parameter selection windows.
  #
  # Returns
  #   compound.ms2: data.table of MSMS data in long format.
  potential.ms2 <- msdata$EIC_MS2[rt %between% c(rt.standard - rt.flex, rt.standard + rt.flex) & 
                             premz %between% pmppm(mz.standard, ppm) & 
                             filename == filename.standard]
  
  compound.ms2 <- potential.ms2 %>%
    group_by(voltage, premz) %>%
    summarize(MS2 = paste0(round(fragmz, digits = mz_digits), ", ", 
                           round(int, digits = int_digits)),
              .groups = "drop") %>%
    group_by(voltage) %>%
    summarize(MS2 = paste0(MS2, collapse = "; "), 
              .groups = "drop") %>%
    mutate(compound_name = compound.name) %>%
    mutate(filename = filename.standard)
  
  return(compound.ms2)
}

RetrieveDDAFiles <- function(pattern) {
  
  DDA.files <- list.files("data_raw", pattern = pattern, full.names = TRUE)
  msdata <- grabMSdata(files = DDA.files, grab_what = c("EIC", "EIC_MS2"),
                       mz = unique(compound.data$mz), ppm = ppm, verbosity=2)
  msdata$EIC <- cbind(msdata$EIC, polarity = pattern)
  msdata$EIC_MS2 <- cbind(msdata$EIC_MS2, polarity = pattern)
  
  return(msdata)
}

## Grab manual standard retention times
compound.data <- read.csv("data_raw/HILICpos_StandardMixes_All-CEs.csv") %>%
  rbind(read.csv("data_raw/HILICneg_StandardMixes_All-CEs.csv")) %>%
  select(compound_name="Precursor.Ion.Name",
         rt="Retention.Time",
         filename="Replicate.Name",
         mz="Precursor.Mz") %>% 
  mutate(rt=as.numeric(rt)) %>%
  mutate(filename=paste0(filename, ".mzML")) %>%
  filter(!is.na(rt))



## Define parameter flexibility ----
# How far away can a DDA scan be from the provided RT, in minutes?
rt.flex <- 0.2

# What's the farthest a precursor mass can be from the provided molecule's mass? (in parts-per-million)
ppm <- 10

# How many decimals of accuracy in mass do you need? (helps clean up output)
mz_digits <- 5

# How many decimals of accuracy in intensity do you need?
int_digits <- 0

## Grab MS data
msdata.pos <- RetrieveDDAFiles(pattern = "pos.*mzML")
msdata.neg <- RetrieveDDAFiles(pattern = "neg.*mzML")
msdata <- list(
  EIC=rbind(msdata.pos$EIC, msdata.neg$EIC),
  EIC_MS2=rbind(msdata.pos$EIC_MS2, msdata.neg$EIC_MS2)
)

# Test run of MSMS data extraction for known compound and filename
tmab.data <- extractMSMSdata(compound.name = compound.data$compound_name[1],
                             mz.standard = compound.data$mz[1],
                             rt.standard = compound.data$rt[1],
                             ppm = ppm,
                             rt.flex = rt.flex,
                             filename.standard = compound.data$filename[1])

MSMS.data <- mapply(extractMSMSdata, SIMPLIFY = FALSE,
       compound.name = compound.data$compound_name,
       mz.standard = compound.data$mz,
       rt.standard = compound.data$rt,
       ppm = ppm,
       rt.flex = rt.flex,
       filename.standard = compound.data$filename) %>%
  rbindlist()


write.csv(MSMS.data, file = "data_processed/Ingalls_Lab_Standards_MSMS.csv", row.names=FALSE)

# Make additional dataframe for missing compounds
missing.cmpds <- MSMS.data %>%
  filter(!is.na(MS2)) %>%
  anti_join(compound.data, ., by = c("compound_name"))


write.csv(missing.cmpds, file = "data_processed/missing_cmpds.csv", 
          row.names = FALSE)

if (file.size("data_processed/Ingalls_Lab_Standards_MSMS.csv") / 1e6 > 5) {
  stop("Beware! The output file is larger than 5MB - be careful pushing to GitHub.")
}

library(git2r)

repo <- repository()
add(repo, "data_processed/Ingalls_Lab_Standards_MSMS.csv")
add(repo, "data_processed/missing_cmpds.csv")
status(repo)
commit(repo, message = "Updated data output automatically")
