# Script for Figure Names

complete.standards <- Ingalls_Lab_Standards_Extras %>%
  mutate(Compound.Name_figure = Compound.Name_old) %>%
  select(Compound.Type, Compound.Name, Compound.Name_old, Compound.Name_figure) %>%
  mutate(old.letter.count = nchar(Compound.Name_old)) %>%
  arrange(old.letter.count) 

complete.standards$Compound.Name_figure <- str_replace(complete.standards$Compound.Name_figure, "Acid", "acid")

Internal.Standards <- complete.standards %>%
  filter(Compound.Type == "Internal Standard")

figure.names <- complete.standards %>%
  filter(Compound.Type != "Internal Standard") %>%
  mutate(Compound.Name_figure = ifelse(old.letter.count <= 14, Compound.Name, Compound.Name_figure)) %>%
  mutate(Compound.Name_figure = ifelse(old.letter.count <= 7, Compound.Name_old, Compound.Name_figure)) %>%
  mutate(has.prefix = ifelse(str_detect(Compound.Name_figure, "L-|D-"), TRUE, FALSE)) %>%
  mutate(Compound.Name_figure = ifelse((has.prefix == TRUE & old.letter.count <= 14), 
                                       substring(Compound.Name_figure, 3, length(Compound.Name_figure)), Compound.Name_figure)) %>%
  select(-has.prefix) %>%
  mutate(Compound.Name_figure = recode(Compound.Name_figure,
                                    # Small fixes
                                    "B-ionine" = "b-Ionine",
                                    "beta-Alanine" = "b-Alanine",
                                    "Betaine" = "Glycine betaine",
                                    "N-Acetylserine" = "N-acetylserine",
                                    "RP B12" = "Cyano B12",
                                    ###### Vitamins ######
                                    "Ascorbic acid" = "Vitamin C",
                                    ###### Abbreviations ######
                                    "(3-Carboxypropyl)trimethylammonium (TMAB)" = "TMAB",
                                    #"3-Indoleacetonitrile" = "3-IAN",
                                    #"3 Indolepropionic acid" = "3-IPA",
                                    "3 Indolebutyric acid" = "Indolebutyric acid",
                                    #"4-hydroxybenzaldehyde" = "4-HBZ",
                                    "5-(2-Hydroxyethyl)-4-methylthiazole" = "Sulfurol",
                                    #"5-Methylcytosine" = "5-mC",
                                    "6-Methyladenine" = "N6-Methyladenine",
                                    "7-dehydrocholesterol" = "7-DHC",
                                    #"Acetyl-L-carnitine" = "ALC",
                                    #"Acetylglutamic acid" = "NAG",
                                    "Adenosyl Methionine" = "SAM",
                                    "Adenosyl Homocysteine" = "SAH",
                                    "Aminobenzoic acid" = "PABA",
                                    "Aminobutyric acid" = "GABA",
                                    "Amino Propanesulfonic acid" = "APA",
                                    "Arachidonic acid" = "ARA",
                                    "Argininosuccinic acid" = "Argininosuccinate",
                                    "B-ionylidene-acetaldehyde" = "b-Ionylidene acetaldehyde",
                                    "beta-Glutamic acid" = "b-Glutamic acid",
                                    "Cysteinesulfinic acid" = "CSA",
                                    "Dimethyl-benzimidazole" = "DMB",
                                    "Dimethyl Glycine" = "DMG",
                                    "Dimethyl glycine" = "DMG",
                                    "Dimethylsulfonioacetate (DMS-Ac)" = "DMS-Ac",
                                    "Ethyl 3 aminobenzoate" = "Ethyl aminobenzoate",
                                    #"Glucose 1 phosphate" = "G1P",
                                    #"Glucose 6 phosphate" = "G6P",
                                    #"Glucosylglycerol" = "GG", 
                                    "Glutathione Disulfide" = "GSSG",
                                    "Glutamylphenylalanine" = "GMPA",
                                    "Glycerophosphocholine" = "GPP",
                                    "Homocysteine Thiolactone" = "Homocysteine thiolactone",
                                    "Homoserine lactone" = "AHL", 
                                    #"Indole 3 acetamide" = "I3A",
                                    "Indoleacrylic acid" = "IAA",
                                    "Isobutyryl-carnitine" = "IBC",
                                    "Riboflavin Monophosphate" = "RMP",
                                    "Ribose 5 phosphate" = "R5P",
                                    "MESNA (Sodium 2-mercaptoethanesulfonate)" = "MESNA",
                                    "Methionine Sulfoxide" = "Methionine sulfoxide",
                                    "Methylmalonyl carnitine" = "MMC",
                                    "Methylphosphonic acid" = "MPA",
                                    "Methylthioadenosine" = "MTA",
                                    #"N-Acetylglucosamine" = "GlcNAc",
                                    #"N-Acetyl-Serine" = "NAS",
                                    "N-Acetyl-Serine" = "N-Acetylserine",
                                    #"N-acetyltaurine" = "NAT",
                                    #"N-methyltaurine" = "NMT",
                                    #"p-Coumaric acid" = "p-CA",
                                    #"p-Coumoroyl-HSL" = "p-CHSL",
                                    "Propionyl-L-carnitine" = "O-Propionylcarnitine",
                                    "Pyridoxal Phosphate" = "PLP",
                                    "Succinic semialdehyde" = "SSA",
                                    "Thiamine monophosphate" = "TMP",
                                    "thiamine pyrophosphate" = "TPP",
                                    "Thiamine pyrophosphate" = "TPP",
                                    "trans cinnamic acid" = "trans-Cinnamic acid",
                                    "trans Hydroxyl proline" = "THP",
                                    "Trimethylamine N-oxide" = "TMAO",
                                    "Trimethylammonium Propionate (TMAP)" = "TMAP",
                                    "Tropodithietic acid" = "Thiotropocin",
                                    #"UDP-glucosamine" = "UDPG",
                                    ###### Shorten ######
                                    "1-stearoyl-2-oleoyl-sn-glycero-3-phosphoethanolamine" = "1,2,3-phosphoethanolamine",
                                    "1-palmitoyl-2-oleoyl-sn-glycero-3-phosphocholine" = "1,2,3-phosphocholine",
                                    "2-(3,5-Dichlorophenylcarbamoyl)-1,2-dimethylcyclopropane-1-carboxylic acid" = "cinnamoyl-HSL",
                                    #"3-Sulfopyruvic acid" = "3-SP acid",
                                    "3',5'-Cyclic diGMP" = "Cyclic diGMP",
                                    #"4-Hydroxyisoleucine" = "4-OHIle",
                                    "5-Hydroxyectoine" = "Hydroxyectoine",
                                    "Adenosylcobalamin" = "Adenosyl B12",
                                    "alpha-Tocotrienol" = "a-Tocotrienol",
                                    #"beta-Glutamic acid" = "b-Glu",
                                    "beta-Carotene" = "b-Carotene",
                                    "beta-Cyclocitral" = "b-Cyclocitral",
                                    "beta-Ionine" = "b-Ionine",
                                    "cis-Aconitic acid" = "Aconitic acid",
                                    "Decarboxylated S-Adenosylmethionine" = "D-SAM",
                                    "Ethyl Dihydroxybenzoate" = "EDHB",
                                    "Fructose 6 phosphate" = "F6-phosphate",
                                    "glycerol 3 phosphate" = "G3-phosphate",
                                    "Hydroxocobalamin" = "Hydroxo B12",
                                    "Indole 3 carbinol" = "Indole-3-carbinol",
                                    "Indole 3 carboxylic acid" = "Indole-3-carboxylic acid",
                                    "Indole 3 Lactic acid" = "Indoleactate",
                                    "Indole 3 methyl acetate" = "Indole-3-methyl acetate",
                                    "Indole 3 Pyruvic acid" = "I3P acid",
                                    "Isobutyryl-carnitine" = "Isobutyryl-carnitine",
                                    "Keto-?-(methylthio)butyric Acid" = "Methylthiobutyric acid",
                                    "Methylcobalamin" = "Methyl B12",
                                    "Methyl indole 3 carboxylate" = "Methyl indole-3-carboxylate",
                                    "Methyl Malonyl CoA" = "Methylmalonyl CoA",
                                    "N-(3-Oxodecanoyl)homoserine lactone" = "3OC10-HSL",
                                    "N-(3-Oxododecanoyl)homoserine lactone" = "3O12-HSL",
                                    "N-(3-Oxohexanoyl)homoserine lactone" = "3OC6-HSL",
                                    "N-(3-Oxooctanoyl)homoserine lactone" = "3OC8-HSL",
                                    "N-Acetylmuramic acid" = "MurNAc",
                                    #"N(e)-Acetyl-Lysine" = "N6-AL",
                                    "Phosphoglyceric acid" = "3-Phosphoglycerate",
                                    "Stachydrine hydrochloride" = "Proline betaine",
                                    "Tocopherol (Vit E)" = "Vit E",
                                    ###### Different name ######
                                    "B-apo-8'-carotenal" = "Apocarotenal", ###
                                    "Ethanesulfonic acid" = "Esylic acid", ###
                                    "Imidazoleacrylic acid" = "Urocanate",
                                    "Ophthalmic acid" = "Ophthalmate",
                                    "L-Pyroglutamic acid" = "Pidolic acid",
                                    "S-Farnesyl-L-cysteine Methyl Ester" = "S-Fcme",
                                    ###### Vitamins ######
                                    "Biotin" = "Vit B7",
                                    "Calciferol" = "Vit D2",
                                    "Folic acid" = "Vit B9",
                                    "Menaquinone" = "Vit K2",
                                    "Nicotinic acid" = "Vit B2",
                                    "Pantothenic acid" = "Vit B5",
                                    "Phytonadione" = "Vit K1",
                                    "Pyridoxine" = "Vit B6",
                                    "Riboflavin" = "Vit B1",
                                    "Vitamin C" = "Vit C")) %>%
  unique() %>%
  mutate(new.letter.count = nchar(Compound.Name_figure))

Ingalls_Lab_Standards_FigNames <- Ingalls_Lab_Standards_Extras %>%
  left_join(figure.names) %>%
  mutate(Compound.Name_figure = ifelse(Compound.Type == "Internal Standard", Compound.Name, Compound.Name_figure)) %>%
  select(Compound.Type, Column, Compound.Name, Compound.Name_old, Compound.Name_figure, everything()) %>%
  select(-old.letter.count, -new.letter.count) 
  
