MSMS data for Ingalls Lab Standards
================

This folder handles MSMS information for the Ingalls Lab Standards.

## Structure:
### data_raw

 - .mzML files: Open-source versions of the QExactive HF data obtained by Laura when
running our standard mixes in DDA mode. (These files are not synced with GitHub)
 - .csv file: A Skyline output file containing manually corrected retention times for
each compound, as noted by Laura

### scripts

 - extract_DDA.R: An R script that uses the `RaMS` library and a few other dependencies
to open the .mzML files, extract the MS2 information for each compound, and create a 
simple output .csv file containing compound name, fragmentation energy, and fragment data.

### data_processed

 - Ingalls_Lab_Standards_MSMS.csv: The file produced by extract_DDA.R, containing 
compound name, fragmentation energy (aka voltage), and the actual fragments observed.
The fragment data is encoded as a single string per compound and voltage as `fragment 
mass, intensity;`. Charge (z) is included as a a disambiguating column between
fragments from positive and negative mode.