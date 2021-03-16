

# Setup ----
## Set absolute path because this script should only live in one place
setwd("G:\\Shared drives\\Ingalls Lab\\Collaborative_Projects\\Standards\\Ingalls_Standards\\MSMS")

## Load necessary packages
library(RaMS)
library(data.table)
library(tidyverse)


# Grab list of standards from the Ingalls Standards folder
ing_stans <- read.csv("../Ingalls_Lab_Standards_NEW.csv")
clean_stans <- ing_stans %>%
  filter(Column=="HILIC") %>%
  select(compound_name="Compound.Name", mz="m.z", z,
         mix="HILICMix", rt="RT..min.") %>%
  mutate(across(.cols = one_of("mz", "rt"), as.numeric)) %>%
  mutate(polarity = ifelse(z>0, "pos", "neg")) %>%
  filter(!is.na(mz))



# Settings ----

## How far away can a DDA scan be from the provided RT, in minutes?
rt_window <- 0.2

## What's the farthest a precursor mass can be from the provided
## molecule's mass? (in parts-per-million)
ppm <- 10

## How many decimals of accuracy in mass do you need? (helps clean up output)
mz_digits <- 5

## How many decimals of accuracy in intensity do you need?
int_digits <- 0





DDA_pos_files <- list.files("data_raw", pattern = "pos", full.names = TRUE)
msdata_pos <- grabMSdata(files = DDA_pos_files, grab_what = c("EIC", "EIC_MS2"),
                     mz=unique(clean_stans$mz), ppm = ppm, verbosity=2)
msdata_pos$EIC <- cbind(msdata_pos$EIC, polarity="pos")
msdata_pos$EIC_MS2 <- cbind(msdata_pos$EIC_MS2, polarity="pos")

DDA_neg_files <- list.files("data_raw", pattern = "neg", full.names = TRUE)
msdata_neg <- grabMSdata(files = DDA_neg_files, grab_what = c("EIC", "EIC_MS2"),
                         mz=unique(clean_stans$mz), ppm = ppm, verbosity=2)
msdata_neg$EIC <- cbind(msdata_neg$EIC, polarity="neg")
msdata_neg$EIC_MS2 <- cbind(msdata_neg$EIC_MS2, polarity="neg")

msdata <- list(
  EIC=rbind(msdata_pos$EIC, msdata_neg$EIC),
  EIC_MS2=rbind(msdata_pos$EIC_MS2, msdata_neg$EIC_MS2)
)


extractMSMSdata <- function(cmpd_name, mz_i, rt_i, ppm, rt_window, polarity_i){
  cmpd_ms2 <- msdata$EIC_MS2[rt%between%c(rt_i-rt_window, rt_i+rt_window) & 
                        premz%between%pmppm(mz_i, ppm) & 
                          polarity==polarity_i]
  if(nrow(cmpd_ms2)==0){
    print(paste("No MSMS for", cmpd_name))
    return(NULL)
  }
  cmpd_ms2 %>%
    group_by(voltage, premz) %>%
    summarize(frags=paste0(
      round(fragmz, digits = mz_digits), ", ", round(int, digits = int_digits)
    ), .groups="drop") %>%
    group_by(voltage) %>%
    summarize(frags=paste0(
      frags, collapse = "; "
    ), .groups="drop") %>%
    mutate(compound_name=cmpd_name) %>%
    mutate(polarity=polarity_i)
}

extractMSMSdata(cmpd_name = clean_stans$compound_name[167],
                mz_i = clean_stans$mz[167],
                rt_i = clean_stans$rt[167],
                ppm = ppm,
                rt_window = rt_window,
                polarity_i = clean_stans$polarity[167])

MSMS_data <- mapply(extractMSMSdata, SIMPLIFY = FALSE,
       cmpd_name = clean_stans$compound_name,
       mz_i = clean_stans$mz,
       rt_i = clean_stans$rt,
       ppm = ppm,
       rt_window = rt_window,
       polarity_i = clean_stans$polarity) %>%
  rbindlist()

output_df <- clean_stans %>%
  select(compound_name, z, polarity) %>%
  left_join(MSMS_data, by=c("compound_name", "polarity")) %>%
  select(-polarity)

write.csv(output_df, file = "data_processed/Ingalls_Lab_Standards_MSMS.csv", 
          row.names = FALSE)

if(file.size("data_processed/Ingalls_Lab_Standards_MSMS.csv")/1e6 > 5){
  stop("Beware! The output file is larger than 5MB - be careful pushing to GitHub.")
}




library(git2r)
repo <- repository()
add(repo, path = "data_processed")
commit(repo, message = "Updated MSMS sheet automatically")
