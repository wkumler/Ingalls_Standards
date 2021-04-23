MSMS data for Ingalls Lab Standards
================

This folder handles MSMS information for the Ingalls Lab Standards.

## Structure:
### data_raw 
*(This folder is NOT synced with GitHub due to size, instead find the required data on [our shared Google Drive)](https://drive.google.com/drive/folders/1k32PVbBVRGE7lMWOGLqORcbBhhbyhZ_a)*

 - .mzML files: Open-source versions of the QExactive HF data acquired when
running our standard mixes in DDA mode. 
 - .csv file: A Skyline output file containing manually corrected retention times for
each compound. 

For questions on acquisition and manual retention times, contact Laura T. Carlson (truxal@uw.edu).

### scripts

 - extract_DDA.R: An R script that uses the `RaMS` library to open the .mzML files, 
extract the MS2 information for each compound, and create a 
simple output .csv file containing compound name, fragmentation energy, and fragment data.

### data_processed

 - Ingalls_Lab_Standards_MSMS.csv: The file produced by extract_DDA.R, containing 
compound name, fragmentation energy (aka voltage), and the actual fragments observed.
The fragment data is encoded as a single string per compound and voltage as `fragment 
mass, intensity;`. Charge (z) is included as a a distinguishing column between
fragments from positive and negative mode.

## Setup:

In order to access the .mzML files, you will need to set your working directory to access the shared Ingalls Drive. In the extract_DDA.R script, please ensure you have local access to the drive and change the setwd() command accordingly.
