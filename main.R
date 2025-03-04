library(rstudioapi)

source(paste0(dirname(getSourceEditorContext()$path),"/utils.R"))

verbose = T

####HYBRIDATION OF Z

# COMMON INDUSTRIES AND COUNTRIES

# c("A01T02","A03","BZ","C10T12","C13T15","C16","C17T18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30",
# "CM","D35","EZ","FZ","GZ","H49","H50","H51","H52","H53","IZ","JA","J61","JC","KZ","LZ","MZ","NZ","OZ","PZ","QZ","RZ","SZ","TZ","UZ")

# c("AT","BE","BG","CY","CZ","DE","DK","EE","ES","FI","FR","GR","HR","HU","IE","IT","LT","LU","LV","MT","NL","PL","PT",
# "RO","SE","SI","SK","GB","US","JP","CN","CA","KR","BR","IN","MX","RU","AU","CH","TR","NO","ID","ZA","ROW")

###HYBRIDATION OF VA

### FIGARO
# D21X31	Taxes less subsidies on products
# OP_NRES	Purchases of non-residents in the domestic territory
# OP_RES	Direct purchase abroad by residents
# D1	Compensation of employees
# D29X39	Other net taxes on production
# B2A3G	Gross operating surplus

### ICIO
# TLS	Taxes less subsidies on intermediate and final products
# VA	Value added at basic prices

### EXIOBASE
# Taxes less subsidies on products purchased: Total	M.EUR
# Other net taxes on production	M.EUR
# Compensation of employees; wages, salaries, & employers' social contributions: Low-skilled	M.EUR
# Compensation of employees; wages, salaries, & employers' social contributions: Medium-skilled	M.EUR
# Compensation of employees; wages, salaries, & employers' social contributions: High-skilled	M.EUR
# Operating surplus: Consumption of fixed capital	M.EUR
# Operating surplus: Rents on land	M.EUR
# Operating surplus: Royalties on resources	M.EUR
# Operating surplus: Remaining net operating surplus	M.EUR

### Hybrid
# TLS	Taxes less subsidies on intermediate and final products
# VA	Value added at basic prices

###HYBRIDATION OF DEMAND

###FIGARO

# P3_S13	Final consumption expenditure of general government
# P3_S14	Final consumption expenditure of households
# P3_S15	Final consumption expenditure of non-profit institutions serving households
# P51G	Gross fixed capital formation
# P5M	Changes in inventories and acquisition less disposals of valuables

###ICIO

# HFCE	Household Final Consumption Expenditure
# NPISH	Non-Profit Institutions Serving Households
# GGFC	General Government Final Consumption
# GFCF	Gross Fixed Capital Formation
# INVNT	Changes in Inventories and Valuables
# DPABR	Direct purchases abroad by residents



###EXIOBASE

# Final consumption expenditure by households
# Final consumption expenditure by non-profit organisations serving households (NPISH)
# Final consumption expenditure by government
# Gross fixed capital formation
# Changes in inventories
# Changes in valuables
# Exports: Total (fob)

###Hybrid
# P3_S13	Final consumption expenditure of general government
# P3_S14	Final consumption expenditure of households
# P3_S15	Final consumption expenditure of non-profit institutions serving households
# P51G	Gross fixed capital formation
# P5M	Changes in inventories and acquisition less disposals of valuables

###INITIALISATION###
exio_hybrid_update = icio_hybrid_update = figaro_hybrid_update = T


#The user should ensure that the selected year is available in all three MRIOTs.
year = 2020

###DOWNLOAD CORRESPONDING DATA###
#Ensure MRIOTs version are up to date : 'link' or 'links_icio' parameters

folder_figaro = figaro_fetcher(year)

folder_icio = icio_fetcher(year)

folder_exio = exiobase_fetcher(year)

###MRIOTS HYBRIDIZATION###


if(exio_hybrid_update) exiobase_hybridization(folder_figaro = folder_figaro,folder_exio = folder_exio,verbose = verbose)


if(icio_hybrid_update) icio_hybridization(folder_icio = folder_icio,verbose = verbose)


if(figaro_hybrid_update) figaro_hybridization(folder_figaro = folder_figaro,verbose = verbose)

###COMPARE ALIGNED MRIOTS###

compare_hybrid_eemriot()

