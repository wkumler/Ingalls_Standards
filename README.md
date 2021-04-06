Ingalls Standards Repository
================
RLionheart
6/3/2020

<!--This document was created by knitting the README.Rmd file. Please edit that instead.-->

This repository contains all the relevant information for most recent
list of the Ingalls Lab Standards. This includes the original standards
list (titled “Ingalls\_Lab\_Standards.csv”) in the data\_raw/ directory,
and the new standards list (titled “Ingalls\_Lab\_Standards\_NEW.csv”).

If you are looking to simply use the new Ingalls Standards list,
download Ingalls\_Lab\_Standards\_NEW.csv and use accordingly. If you
would like to view a previous version of the standards, or see how
changes were made, the **Structural\_Changes.Rmd** markdown file manages
the log history of changes to the original csv.

### Overview

The original version of the Ingalls\_Lab\_Standards.csv lives in
Katherine’s GitHub at the following link: [Ingalls Lab
Standards](https://github.com/kheal/Example_Untargeted_Metabolomics_Workflow)

------------------------------------------------------------------------

#### Changes made in v1.0

*Capitalization*  
\* All “Acid”s changed to “acid”, as per KEGG conventions. Example:
3-Sulfopyruvic Acid –&gt; 3-Sulfopyruvic acid  
\* First appearance of compound in complete name capitalized. Example:
7-dehydrocholesterol –&gt; 7-Dehydrocholesterol, thiamine pyrophosphate
–&gt; Thiamine pyrophosphate  
\* “B-compoundName” changed to beta-CompoundName. Example: B-ionine
–&gt; beta\_Ionine  
\* Second part of compound name lowercase. Example: Glutathione
Disulfide –&gt; Glutathione disulfide

*Abbreviations*  
To alleviate long commpound names, the primary ID will remain as the
KEGG name or reasonable abbreviation but a more manageable name will go
under the “Figure Names” column.

*Symbols*  
All symbols that are part of the KEGG name will remain in place. A
syntactically correct column will be added for upload to CMAP, etc.

*Other specific changes*  
\* RP B12 –&gt; Cyanocobalamin  
\* All B12s –&gt; Methylcobalamin, Hydroxocobalamin, etc.  
\* Betaine –&gt; Glycine betaine  
\* Dimethyl glycine –&gt; Dimethylglycine  
\* All Vitamins changed to their descriptive name: Vitamine B1 –&gt;
Thiamine, Vitamin B2 –&gt; Riboflavin, etc.

#### Changes made in v2.0

*Figure Names column added* \* Add in a Figure Name column.  
\* Replace vitamins with full names. \* Bug fixes in primary and figure
name columns.

#### Changes made in v3.0

-   ChEBI names added (Jul 20)
-   ChEBI names fixed! (Jul 31)

#### Changes made in v4.0

-   Add column with IUPAC, InCHI, SMILES names.

#### Changes made in v5.0

-   Incorporate known Classyfire compound designation column.

#### Changes made in v6.0

-   Create new column with SQL-safe compound names, no special
    characters.
-   Update a few figure names in the Fig Names column.
-   Remove all leading numbers in the SQL-Safe column for CMAP
    ingestion.

#### Long-term Changes

-   Set up system to keep note of changes to official KEGG compound
    names whether or not those changes are enacted.  
-   Add stereoisomer orientation column.  
-   List of all commonly used names for a compound (eg Glutamic acid ==
    Glutamate).

------------------------------------------------------------------------

## Change Log

4/8/20: Internal standards updated to reflect consistent naming
scheme.  
4/20/20: Abbreviated compounds switched to full-length KEGG names.  
4/20/20: Adjusted compounds with incorrect capitalization.  
4/22/20: Added or removed symbols as required by KEGG names.  
4/23/20: Update last compounds not falling into the above groups.
4/23/20: Complete renaming of compounds, including those that didn’t
change from the original names. 4/28/20: Adjust vitamins to descriptive
names. 4/29/20: Adjust abbreviations for consistency. 5/8/20: FINAL
NAMES DECIDED FOR PRIMARY ID. 5/28/20: Bug fixes in final names.
5/28/20: First suggestions for Figure Name column. 6/3/20: Final Figure
Names decided. 7/30/20: ChEBI names scraped from website and added in
new column. 8/3/20: IUPAC, InCHI, and SMILES columns added to standards.
8/10/20: Classyfire classifications added for those compounds that have
them. 10/14/20: First version of SQL-safe names introduced. 10/22/20:
Remove all trailing numbers from SQL-safe names. This is the column to
be used for CMAP.
