library(data.table)
library(RaMS)
library(tidyverse)

# Setup ----

## Set absolute path to the Google Drive. The MS2 files live there and you must have your working directory 
## Ensure the getwd() command properly connects to the shared Drive from your filepath!

# setwd("G:\\Shared drives\\Ingalls Lab\\Collaborative_Projects\\Standards\\Ingalls_Standards\\MSMS") # Windows
setwd("/Volumes/GoogleDrive/Shared drives/Ingalls Lab/Collaborative_Projects/Standards/Ingalls_Standards/MSMS/") # Mac

## Check for correct working directory.
if(str_detect(getwd(), "Ingalls_Standards/MSMS")) {
  print("Good job, you're in the right directory!")
} else {
  stop("You may not be in the correct directory. Check your path and try again.")
}

## Define functions
extractMSMSdata <- function(compound.name, mz.standard, rt.standard, polarity.standard, ppm, rt.flex) {
  # Isolate MS2 data from the Ingalls Standards MS2 data.
  # 
  # Args
  #   compound.name, mz.standard, rt.standard, polarity.standard: Parameters of the compound to retrieve MS2 for.
  #   ppm, rt.flex: User-defined flexibility for parameter selection windows.
  #
  # Returns
  #   compound.ms2: data.table of MSMS data in long format.
  potential.ms2 <- msdata$EIC_MS2[rt %between% c(rt.standard - rt.flex, rt.standard + rt.flex) & 
                             premz %between% pmppm(mz.standard, ppm) & 
                             polarity == polarity.standard]
  
  if (nrow(potential.ms2) == 0) {
    print(paste("No MSMS for", compound.name))
  }

  compound.ms2 <- potential.ms2 %>%
    group_by(voltage, premz) %>%
    summarize(MS2 = paste0(round(fragmz, digits = mz_digits), ", ", round(int, digits = int_digits)),
              .groups = "drop") %>%
    group_by(voltage) %>%
    summarize(MS2 = paste0(MS2, collapse = "; "), 
              .groups = "drop") %>%
    mutate(compound_name = compound.name) %>%
    mutate(polarity = polarity.standard)
  
  return(compound.ms2)
}

RetrieveDDAFiles <- function(pattern) {
  
  DDA.files <- list.files("data_raw", pattern = pattern, full.names = TRUE)
  msdata <- grabMSdata(files = DDA.files, grab_what = c("EIC", "EIC_MS2"),
                       mz = unique(Ingalls.standards$mz), ppm = ppm, verbosity=2)
  msdata$EIC <- cbind(msdata$EIC, polarity = pattern)
  msdata$EIC_MS2 <- cbind(msdata$EIC_MS2, polarity = pattern)
  
  return(msdata)
}

## Grab most recent list of standards from the Ingalls Standards folder
Ingalls.standards <- read.csv("../Ingalls_Lab_Standards_NEW.csv") %>%
  filter(Column == "HILIC") %>%
  select(compound_name = "Compound.Name", 
         mz = "m.z", 
         z,
         mix = "HILICMix", 
         rt = "RT..min.") %>%
  mutate(across(.cols = one_of("mz", "rt"), as.numeric)) %>%
  mutate(polarity = ifelse(z > 0, "pos", "neg")) %>% 
  filter(!is.na(mz))

## Define parameter flexibility
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

# Test run of MSMS data extraction for known compound
sarcosine <- extractMSMSdata(compound.name = Ingalls.standards$compound_name[49],
                             mz.standard = Ingalls.standards$mz[49],
                             rt.standard = Ingalls.standards$rt[49],
                             ppm = ppm,
                             rt.flex = rt.flex,
                             polarity.standard = Ingalls.standards$polarity[49])

MSMS.data <- mapply(extractMSMSdata, SIMPLIFY = FALSE,
       compound.name = Ingalls.standards$compound_name,
       mz.standard = Ingalls.standards$mz,
       rt.standard = Ingalls.standards$rt,
       ppm = ppm,
       rt.flex = rt.flex,
       polarity.standard = Ingalls.standards$polarity) %>%
  rbindlist()

final.MS2 <- Ingalls.standards %>%
  select(compound_name, z, polarity) %>%
  left_join(MSMS.data, by = c("compound_name", "polarity")) %>%
  select(-polarity)

write.csv(final.MS2, file = "data_processed/Ingalls_Lab_Standards_MSMS.csv", 
          row.names = FALSE)

if (file.size("data_processed/Ingalls_Lab_Standards_MSMS.csv") / 1e6 > 5) {
  stop("Beware! The output file is larger than 5MB - be careful pushing to GitHub.")
}


# Make additional dataframe for missing compounds
missing_cmpds <- final.MS2 %>%
  filter(!is.na(MS2)) %>%
  anti_join(Ingalls.standards, ., by=c("compound_name", "z"))





library(git2r)

repo <- repository()
add(repo, path = "data_processed")
commit(repo, message = "Updated MSMS sheet automatically")
