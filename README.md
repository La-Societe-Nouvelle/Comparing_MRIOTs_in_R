# MRIOTs Comparison Procedure

This repository provides a ready-to-use procedure for comparing the Environmentally-Extended Multi-Regional Input-Output Tables (MRIOTs) from EXIOBASE, ICIO, and FIGARO on a cell-by-cell basis.

## Procedure Overview

The procedure is organized as follows:

1) **Downloading and storing raw MRIOTs.** Given the large file sizes of these datasets, a stable and high-speed internet connection is recommended.

2) **Aligning MRIOTs classifications to an ISIC Rev.4 44 industries x 44 countries Framework.**

3) **Cell-by-Cell Comparison of Hybrid MRIOTs.**

The procedure encompasses four key components of the input-output framework: Transaction flows matrix (noted Z), Final Demand (D), Value-Added (VA) and GHG emissions (emissions).

Users are encouraged to run and customize the procedure by adapting the `main.R` script according to their specific needs.

## Notes

**Note 1:** Due to EXIOBASE’s outdated and non-bijective classification system, hybridization requires specific weighting adjustments. This procedure applies value-added weightings from FIGARO to ensure consistency.

**Note 2:** The economic values in EXIOBASE are presented in current euros, using OECD exchange rate data for conversion.

**Note 3:** The correspondence table between EXIOBASE and FIGARO originates from an INSEE working paper:
[https://www.insee.fr/fr/statistiques/7624261](https://www.insee.fr/fr/statistiques/7624261) (Bourgeois, Gervois, Lafrogne-Joussier).
It provides a valuable and verifiable resource for aligning EXIOBASE with the ISIC Rev.4 classification.

## HYBRID CLASSIFICATION SYSTEM

### INDUSTRIES (ISIC Revision 4)

"A01T02","A03",
"BZ",
"C10T12","C13T15","C16","C17T18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","CM",
"D35",
"EZ",
"FZ",
"GZ",
"H49","H50","H51","H52","H53",
"IZ",
"JA","J61","JC",
"KZ",
"LZ",
"MZ",
"NZ",
"OZ",
"PZ",
"QZ",
"RZ",
"SZ",
"TZ",
"UZ"

### COUNTRIES (ISO 2)

"AT","AU",
"BE","BG","BR",
"CA","CH","CN","CY","CZ",
"DE","DK",
"EE","ES",
"FI","FR",
"GB","GR",
"HR","HU",
"ID","IE","IN","IT",
"JP",
"KR",
"LT","LU","LV",
"MT","MX",
"NL","NO",
"PL","PT",
"RO","ROW","RU",#Rest of the World ROW
"SE","SI","SK",
"TR",
"US",
"ZA"

### VALUE-ADDED COMPONENTS

"TLS", # Taxes less subsidies on intermediate and final products
"VA"   # Value added at basic prices

### FINAL DEMAND COMPONENTS

"P3_S13", # Final consumption expenditure of general government
"P3_S14", # Final consumption expenditure of households
"P3_S15", # Final consumption expenditure of non-profit institutions serving households
"P51G",   # Gross fixed capital formation
"P5M"     # Changes in inventories and acquisition less disposals of valuables

### EMISSIONS

The emissions are represented as tons of CO2 equivalent, using the Global Warming Potential (GWP) values from the Fifth Assessment Report (AR5) by the Intergovernmental Panel on Climate Change (IPCC).
