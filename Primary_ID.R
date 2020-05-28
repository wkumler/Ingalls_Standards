
## First script to rename primary ID column on the Ingalls Standards sheet.

# Renaming of typos/specific issues.
LauraEditsIS <- Ingalls_Lab_Standards_LauraEdit %>%
  select(Compound.Type,Compound.Name_new, Compound.Name_old) %>%
  filter(Compound.Type == "Internal Standard") %>%
  filter(!Compound.Name_new == Compound.Name_old) # Remove compounds that don't change

Ingalls_Lab_Standards_LauraEdit[Ingalls_Lab_Standards_LauraEdit == "O-Propanoylcarnitine"] <- "O-Propionylcarnitine"
Ingalls_Lab_Standards_LauraEdit[Ingalls_Lab_Standards_LauraEdit == "O-Acetyl-L-carnitine"] <- "O-Acetylcarnitine"
Ingalls_Lab_Standards_LauraEdit[Ingalls_Lab_Standards_LauraEdit == "Propanoyl-CoA"] <- "Propionyl-CoA"
Ingalls_Lab_Standards_LauraEdit <- Ingalls_Lab_Standards_LauraEdit %>%
  mutate(Compound.Name_new = ifelse(Compound.Name_old == "Tryptamine", "Tryptamine", Compound.Name_new))


print(LauraEditsIS)


## Renaming of Internal Standards
Ingalls_Lab_Standards_IS <- Ingalls_Lab_Standards %>%
  left_join(Ingalls_Lab_Standards_LauraEdit %>% rename(Compound.Name = Compound.Name_old)) %>%
  rename(Compound.Name_old = Compound.Name,
         Compound.Name = Compound.Name_new) %>%
  select(Compound.Type, Column, Compound.Name, Compound.Name_old, everything()) %>%
  mutate(Compound.Name_old = 
           ifelse(Compound.Type == "Internal Standard" & Compound.Name == Compound.Name_old, 
                  Compound.Name, Compound.Name_old)) %>%
  mutate(Compound.Name = ifelse(Compound.Type == "Internal Standard", Compound.Name, NA))

#write.csv(Ingalls_Lab_Standards_IS, "Ingalls_Lab_Standards_NEW.csv")


## Abbreviations
Abbreviations <- Ingalls_Lab_Standards_LauraEdit %>%
  select(Compound.Name_new, Compound.Name_old, Compound.Type) %>% 
  mutate(Compound.Name_new = recode(Compound.Name_new,
                                    "DMSP" = "Dimethylsulfoniopropionate",
                                    "ADP" = "Adenosine diphosphate",
                                    "AMP" = "Adenosine monophosphate",
                                    "ATP" = "Adenosine triphosphate",
                                    "FAD" = "Flavin adenine dinucleotide",
                                    "GMP" = "Guanosine monophosphate",
                                    "GMP, 15N5" = "Guanosine monophosphate, 15N5",
                                    "GTP" = "Guanosine triphosphate",
                                    "PEP" = "Phosphoenolpyruvic acid",
                                    "TMAB" = "(3-Carboxypropyl)trimethylammonium",
                                    "cAMP" = "3',5'-Cyclic AMP",
                                    "cGMP" = "3',5'-Cyclic GMP",
                                    "Cys-Gly" = "L-Cysteinylglycine")) %>%
  filter(Compound.Type != "Internal Standard",
         Compound.Name_new != Compound.Name_old) %>% 
  mutate(letter.count = nchar(Compound.Name_old)) %>%
  arrange(letter.count) %>%
  filter(nchar(Compound.Name_old) < 10 | Compound.Name_old == "(3-Carboxypropyl)trimethylammonium (TMAB)",
         nchar(Compound.Name_old) < 7 | str_detect(Compound.Name_old, "-"))

### Show list of abbreviated compounds to be changed
print(Abbreviations)

### Finalize changes of abbreviated compounds
Ingalls_Lab_Standards_Abbr <- Ingalls_Lab_Standards_IS %>%
  left_join(Abbreviations %>% select(Compound.Name_new, Compound.Name_old) %>% unique()) %>%
  select(Compound.Name, Compound.Name_new, Compound.Name_old, Compound.Type, everything()) %>%
  mutate(Compound.Name = ifelse(is.na(Compound.Name), Compound.Name_new, Compound.Name)) %>%
  select(Compound.Type, Column, everything(), -Compound.Name_new) 

#write.csv(Ingalls_Lab_Standards_Abbr, "Ingalls_Lab_Standards_NEW.csv")

## Isolate different capitalizations of compounds
Capitalizations <- Ingalls_Lab_Standards_LauraEdit %>%
  select(Compound.Name_new, Compound.Name_old) %>%
  filter(!Compound.Name_new %in% Ingalls_Lab_Standards_Abbr$Compound.Name, 
         Compound.Name_new != Compound.Name_old) # drop commpounds already reassigned or matching originals

# View different capitalization sections.
Acid.Capitals <- Capitalizations %>%
  filter(str_detect(Compound.Name_new, regex("acid", ignore_case = TRUE))) %>%
  select(Compound.Name_new, Compound.Name_old)
Beta.Capitals <- Capitalizations %>%
  filter(str_detect(Compound.Name_new, regex("beta-", ignore_case = TRUE))) %>%
  select(Compound.Name_new, Compound.Name_old)
Just.Capitals <- Capitalizations %>%
  select(Compound.Name_new, Compound.Name_old) %>%
  filter(!Compound.Name_new %in% c(Acid.Capitals$Compound.Name_new, Beta.Capitals$Compound.Name_new),
         tolower(Compound.Name_new) == tolower(Compound.Name_old),
         Compound.Name_new != Compound.Name_old) %>%
  unique()

Complete.Caps <- Acid.Capitals %>%
  rbind(Beta.Capitals) %>%
  rbind(Just.Capitals)
print(Complete.Caps)

### Finalize changes of capitalized compounds
Ingalls_Lab_Standards_Caps <- Ingalls_Lab_Standards_Abbr %>%
  left_join(Complete.Caps) %>%
  select(Compound.Name, Compound.Name_new, Compound.Name_old, Compound.Type, everything()) %>%
  mutate(Compound.Name = ifelse(is.na(Compound.Name), Compound.Name_new, Compound.Name)) %>%
  select(Compound.Type, Column, everything(), -Compound.Name_new) %>%
  unique()

#write.csv(Ingalls_Lab_Standards_Caps, "Ingalls_Lab_Standards_NEW.csv")

# Isolate Symbols  
Symbols <- Ingalls_Lab_Standards_Caps %>%
  left_join(Ingalls_Lab_Standards_LauraEdit) %>%
  select(Compound.Name, Compound.Name_new, Compound.Name_old, Compound.Type) %>%
  filter(is.na(Compound.Name)) %>%
  filter(Compound.Name_new != Compound.Name_old) %>%
  select(Compound.Name_new, Compound.Name_old) %>%
  filter_all(any_vars(str_detect(., "[^[:alnum:] ]"))) %>%
  unique()

print(Symbols)

# Finalize changes of compounds with symbols
Ingalls_Lab_Standards_Symbols <- Ingalls_Lab_Standards_Caps %>%
  left_join(Symbols) %>%
  select(Compound.Name, Compound.Name_new, Compound.Name_old, Compound.Type, everything()) %>%
  mutate(Compound.Name = ifelse(is.na(Compound.Name), Compound.Name_new, Compound.Name)) %>%
  select(Compound.Type, Column, everything(), -Compound.Name_new) %>%
  unique()

#write.csv(Ingalls_Lab_Standards_Symbols, "Ingalls_Lab_Standards_NEW.csv")

## Adjust Vitamins to descriptive names
Ingalls_Lab_Standards_Vitamins <- Ingalls_Lab_Standards_Symbols %>%
  mutate(Compound.Name = ifelse(Compound.Name_old == "Methyl indole 3 carboxylate", "Methyl indole-3-carboxylate", Compound.Name)) %>%
  mutate(Compound.Name = ifelse(Compound.Name_old == "Vitamin B1", "Thiamine", Compound.Name)) %>%
  mutate(Compound.Name = ifelse(Compound.Name_old == "Vitamin B2, 13C4, 15N2", "Riboflavin-dioxopyrimidine, 13C4, 15N2", Compound.Name)) %>%
  mutate(Compound.Name = ifelse(Compound.Name_old == "Vitamin B2", "Riboflavin", Compound.Name)) %>%
  mutate(Compound.Name = ifelse(Compound.Name_old == "Vitamin B6", "Pyridoxine", Compound.Name)) %>%
  mutate(Compound.Name = ifelse(Compound.Name_old == "Vitamin B7", "Biotin", Compound.Name)) %>%
  mutate(Compound.Name = ifelse(Compound.Name_old == "Vitamin C", "Ascorbic acid", Compound.Name)) %>%
  mutate(Compound.Name = ifelse(Compound.Name_old == "Vitamin D2", "Calciferol", Compound.Name)) %>%
  mutate(Compound.Name = ifelse(Compound.Name_old == "Vitamin K1", "Phytonadione", Compound.Name)) %>%
  mutate(Compound.Name = ifelse(Compound.Name_old == "Vitamin K2", "Menaquinone", Compound.Name)) %>%
  mutate(Compound.Name = ifelse(Compound.Name_old == "AMP, 15N5", "Adenosine monophosphate, 15N5", Compound.Name)) %>%
  mutate(Compound.Name = ifelse(Compound.Name_old == "Indole 3 methyl acetate", "Indole-3-methyl acetate", Compound.Name))

#write.csv(Ingalls_Lab_Standards_Vitamins, "Ingalls_Lab_Standards_NEW.csv")

## All other compounds not yet changed
Unchanged <- Ingalls_Lab_Standards_Vitamins %>%
  left_join(Ingalls_Lab_Standards_LauraEdit) %>%
  select(Compound.Name, Compound.Name_new, Compound.Name_old) %>%
  filter(is.na(Compound.Name)) %>%
  filter(Compound.Name_new != Compound.Name_old)


### Add changes of still-unedited compounds - Including those that remain as the original name.
Ingalls_Lab_Standards_Extras <- Ingalls_Lab_Standards_Vitamins %>%
  left_join(Unchanged) %>%
  select(Compound.Name, Compound.Name_new, Compound.Name_old, Compound.Type, everything()) %>%
  mutate(Compound.Name = ifelse(is.na(Compound.Name), Compound.Name_new, Compound.Name)) %>%
  mutate(Compound.Name = ifelse(is.na(Compound.Name), Compound.Name_old, Compound.Name)) %>%
  select(Compound.Type, Column, everything(), -Compound.Name_new) %>%
  unique()

#write.csv(Ingalls_Lab_Standards_Extras, "Ingalls_Lab_Standards_NEW.csv")
