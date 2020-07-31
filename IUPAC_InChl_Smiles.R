## Incorporate IUPAC, InChl, and SMILES keys into Ingalls Standards csv.

library(tidyverse)

Ingalls.Standards <- read.csv("Ingalls_Lab_Standards_NEW.csv") %>%
  select(-X)

Ingalls.Standards.IUPAC.InChl.SMILES <- read.csv("data_raw/Ingalls_Stds_Alt.Names.csv") %>%
  select(-X)

Combined.Names <- Ingalls.Standards %>%
  left_join(Ingalls.Standards.IUPAC.InChl.SMILES) %>%
  select(-X.1)
  