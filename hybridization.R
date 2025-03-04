exiobase_hybridization = function(folder_hybrid,
                                  folder_figaro, #EXIOBASE Hybridization requires ad hoc external weightings. Figaro is used
                                  folder_exio,
                                  verbose = T)
{

  if(verbose) print("Aligning EXIOBASE classification with a hybrid system: 44 countries × 44 ISIC4 sectors – Initialization")

  if(missing(folder_hybrid)) folder_hybrid = get_folder_hybrid()

  folder_hybrid = ifelse(substring(folder_hybrid,nchar(folder_hybrid)) != "/",paste0(folder_hybrid,"/"),folder_hybrid)

  folder_hybrid_exio = paste0(folder_hybrid,"EXIO/")

  dir.create(folder_hybrid_exio,showWarnings = F)

  exio_countries = get_exiobase_hybrid_countries(source_tab_pass)

  exio_figaro = get_exiobase_figaro_correspondance(source_tab_pass)

  source_figaro = paste0(folder_figaro,"/flatfile_eu-ic-io_ind-by-ind_24ed_2020.csv")

  figaro = fread(source_figaro)

  ########EXIOBASE HYBRIDIZATION PROCEDURE#######

  if(verbose) print("EXIOBASE hybrization : Computing FIGARO value-added based weights for sectoral classification alignment...")

  figaro_va_raw = figaro %>% filter(rowIi %in% c("D21X31","OP_RES","OP_NRES","D1","D29X39","B2A3G") & !grepl("P3|P5",colIi)) %>%
    mutate(industry = exio_figaro$industry[match(colIi,exio_figaro$figaro)],
           country = case_when(counterpartArea %in% exio_countries ~ counterpartArea,
                               T ~ 'ROW'))

  figaro_va = figaro_va_raw %>%
    group_by(country,industry) %>%
    summarise(value = sum(obsValue),.groups = 'keep') %>%
    ungroup()

  exio_va_raw = fread(paste0(folder_exio,"/factor_inputs/F.txt"),
                      sep = "\t",
                      header = F) %>%
    t() %>%
    as.data.frame() %>%
    `colnames<-`(.[1,]) %>%
    {.[-1,]} %>%
    mutate(country = case_when(region %in% figaro_va$country ~ region,
                               T ~ "ROW"))

  exio_va = exio_va_raw %>%
    mutate(across(-c(region,country,stressor,sector),as.numeric)) %>%
    mutate(value_added = rowSums(select(.,-c(region,stressor,sector,country)))) %>%
    select(country,sector,value_added) %>%
    group_by(sector,country) %>%
    summarise(value_added = sum(value_added),.groups = 'keep') %>%
    ungroup()

  sto_repartition_key = c()

  for(i in unique(exio_va$country))
  {

    #WHEN ONE EXIO = MULTIPLE IND

    figaro_repartition_key =
      exio_figaro %>%
      select(Lib,industry) %>%
      distinct() %>%
      mutate(value_added = figaro_va$value[match(paste0(industry,i),paste0(figaro_va$industry,figaro_va$country))]) %>%
      group_by(Lib) %>%
      mutate(n = n(),
             share = case_when(n == 1 ~ 1,
                               T ~ value_added / sum(value_added)))
    #WHEN ONE IND = MULTIPLE EXIO

    exio_repartition_key =
      exio_figaro %>%
      select(Lib,industry) %>%
      distinct() %>%
      mutate(value_added = exio_va$value_added[match(paste0(Lib,i),paste0(exio_va$sector,exio_va$country))],
             repartition_key = figaro_repartition_key$share,
             country = i,
             id = paste0(country,"_",industry),
             exio_id = paste0(country,"_",Lib),
             use_id = paste0(country,"_",Lib),
             key_id = paste0(exio_id,'@',id))


    check_repartition = exio_repartition_key %>%
      group_by(industry) %>%
      summarise(value = sum(value_added * repartition_key,na.rm=T),.groups = 'keep')

    if(round(sum(check_repartition$value,na.rm=T)) != round(sum(exio_va %>% filter(country == i) %>% select(value_added)))) stop(paste0('Aggregation error, country : ',i))

    sto_repartition_key = rbind(sto_repartition_key,
                                exio_repartition_key %>% select(country,Lib,industry,repartition_key,id,exio_id,use_id,key_id))

  }

  rm(figaro)

  model_hybrid_vector =
    data.frame(id = apply(expand.grid(unique(figaro_va$country),unique(exio_figaro$industry)), 1, paste0, collapse="_")) %>%
    separate(id,into = c("country","industry"),sep = "_",extra = 'merge',remove = F) %>%
    arrange(country,industry)

  ####EXIOBASE Z HYBRIDIZATION PROCEDURE

  #VROOM AND COLLECT ASSOCIATION IS FAR MORE EFFICIENT FOR HEAVY DATASETS

  if(verbose) print("Read EXIOBASE and collect data structure informations...")

  exio = arrow:::open_dataset(paste0(folder_exio,"/Z.txt"),
                              format = "text",
                              col_names = F,
                              delim = "\t")


  #This vector is the EXIOBASE colnames

  cn_exio = exio[c(1:2),] %>%
    collect() %>%
    summarise(cn = paste0(.[1,],"_",.[2,]),.groups = 'keep')

  #This vector is the EXIOBASE rownames
  rn_exio = exio[,c(1,2)] %>%
    collect() %>%
    unite(rn,c(1,2),sep = "_")

  exio_col_num = 3:ncol(exio)

  exio_row_countries = c("WA","WL","WE","WF","WM","TW")

  file_hybrid_z = paste0(folder_hybrid_exio,"Z.csv")

  #Deleting previous Z
  if(file.exists(file_hybrid_z))
  {
    unlink(file_hybrid_z)
    if(verbose) print('Previous Hybrid Z matrix deleted')
  }

  #Computing Hybrid Z country by country (too heavy to process all at once)

  if(verbose) print("Computing the hybridized EXIOBASE-based MRIOT by chunk (Use Country)...")

  for(i in unique(model_hybrid_vector$country)){

    rn_hybrid = model_hybrid_vector$id[model_hybrid_vector$country == i]

    #This object stores origin EXIOBASE cells and destination hybrid cell

    rk = sto_repartition_key %>%
      filter(id %in% rn_hybrid) %>%
      select(resource_id = key_id,exio_resource_id = exio_id,hybrid_resource_id = id,resource_repartition_key = repartition_key) %>%
      slice(rep(1:n(), each = nrow(sto_repartition_key))) %>%
      group_by(resource_id) %>%
      mutate(use_id = sto_repartition_key$key_id,
             exio_use_id = sto_repartition_key$exio_id,
             hybrid_use_id = sto_repartition_key$id,
             use_repartition_key = sto_repartition_key$repartition_key,
             cell_repartition_key = use_repartition_key * resource_repartition_key) %>% #for additivity
      filter(cell_repartition_key != 0) %>% #there is negatives in exio
      ungroup() %>%
      unite(cell_id,c('exio_use_id','exio_resource_id'),sep = '.',remove = F)

    #This chunk stores EXIOBASE elements related to the object rk

    exio_row_num = which(unlist(rn_exio) %in% rk$exio_resource_id)

    if(i == 'ROW') exio_row_num = which(substr(unlist(rn_exio),1,2) %in% exio_row_countries)

    raw_exio_chunk = exio[exio_row_num,exio_col_num] %>%
      collect() %>%
      as.data.frame() %>%
      `colnames<-`(cn_exio$cn[exio_col_num]) %>%
      `rownames<-`(rn_exio$rn[exio_row_num])

    raw_exio_chunk = raw_exio_chunk %>%
      rownames_to_column("resource_id") %>%
      pivot_longer(-1,names_to = "use_id") %>%
      mutate(value = as.numeric(value),
             use_country = substr(use_id,1,2),
             use_industry = substring(use_id,4),
             use_country = case_when(use_country %in% unique(figaro_va$country) ~ use_country,
                                     T ~ 'ROW')) %>%
      unite(use_id,c('use_country','use_industry'),sep = "_",remove = F)

    if(i == 'ROW')
    {
      raw_exio_chunk = raw_exio_chunk %>%
        mutate(resource_industry = substring(resource_id,4),
               resource_country = 'ROW') %>%  ###REQUIRES TO RENAME Rest of the world DEMAND country (use_country)
        unite(resource_id,c('resource_country','resource_industry'),sep = "_",remove = F)
    }

    exio_chunk = raw_exio_chunk %>%
      unite(cell_id,c('use_id','resource_id'),sep = '.',remove = F) %>%
      group_by(cell_id) %>%
      summarise(value = sum(value),.groups = 'keep')

    rm(raw_exio_chunk)

    #This data.frame stores the EXIOBASE-based values distributed through the hybrid classification

    raw_row_values = rk %>%
      mutate(raw_z = exio_chunk$value[match(cell_id,exio_chunk$cell_id)]) %>%
      mutate(distributed_z = raw_z * cell_repartition_key)

    rm(rk)

    row_values = raw_row_values %>%
      group_by(hybrid_use_id,hybrid_resource_id) %>%
      summarise(value = sum(distributed_z),.groups = 'keep') %>%
      ungroup()

    if(round(sum(row_values$value)) != round(sum(exio_chunk$value))) stop(paste('Aggregation error for country ',i,' products'))


    write.table(row_values,
                file_hybrid_z,
                sep = ";",
                row.names = F,
                col.names = !file.exists(file_hybrid_z),
                fileEncoding = 'latin1',
                append = T)

    rm(row_values)

    if(verbose) print(paste0("Computing the hybridized EXIOBASE-based MRIOT by chunk : ",i))

  }

  if(verbose) print(paste0("Computing the hybridized EXIOBASE-based MRIOT by chunk : Done"))

  ####EXIOBASE VA HYBRIDIZATION PROCEDURE

  if(verbose) print("Computing the hybridized EXIOBASE-based Value-Added data")

  file_hybrid_va = paste0(folder_hybrid_exio,"/VA.csv")

  exio_distributed_va =
    exio_va_raw %>%
    pivot_longer(-c(region,country,stressor,sector),names_to = 'component') %>%
    mutate(value = as.numeric(value),
           hybrid_component = case_when(component == "Taxes less subsidies on products purchased: Total" ~ "TLS",
                                        T ~ "VA")) %>%
    group_by(country,sector,hybrid_component) %>%
    summarise(value = sum(value),.groups = 'keep') %>%
    mutate(exio_id = paste0(country,"_",sector),
           component_exio_id = paste0(exio_id,".",hybrid_component)) %>%
    ungroup()

  hybrid_exio_va =
    sto_repartition_key %>%
    slice(rep(1:n(), each = 2)) %>%
    group_by(country,Lib,id,industry) %>%
    mutate(component = c('TLS','VA')) %>%
    ungroup() %>%
    mutate(component_exio_id = paste0(exio_id,".",component),
           value = exio_distributed_va$value[match(component_exio_id,exio_distributed_va$component_exio_id)]) %>%
    group_by(country,industry,id,component) %>%
    summarise(value = sum(value * repartition_key),.groups = 'keep') %>%
    ungroup()

  write.table(hybrid_exio_va,
              file_hybrid_va,
              sep = ";",
              fileEncoding = 'latin1',
              row.names = F)

  ####EXIOBASE DEMAND HYBRIDIZATION PROCEDURE

  if(verbose) print("Computing the hybridized EXIOBASE-based Final Demand data")

  file_hybrid_d = paste0(folder_hybrid_exio,"/D.csv")

  exio_demand_raw = fread(paste0(folder_exio,"/Y.txt"),
                          sep = "\t",
                          header = F) %>%
    as.data.frame() %>%
    `colnames<-`(c("region",paste0(.[1,-1],"_",.[2,-1]))) %>%
    {.[-c(1:3),]} %>%
    pivot_longer(-c(1:2),names_to = "component") %>%
    mutate(country = case_when(region %in% figaro_va$country ~ region,
                               T ~ "ROW"),
           value = as.numeric(value)) %>%
    filter(value > 0) %>%
    separate(component,into = c('counterpartarea','component'),sep = "_",remove = F) %>%
    mutate(counterpartarea = case_when(counterpartarea %in% figaro_va$country ~ counterpartarea,
                                       T ~ "ROW")) %>%
    rename(exio_industry = "_") %>%
    mutate(hybrid_component = case_when(component == "Final consumption expenditure by government" ~ "P3_S13",
                                        component == "Final consumption expenditure by households" ~ "P3_S14",
                                        component == "Final consumption expenditure by non-profit organisations serving households (NPISH)" ~ "P3_S15",
                                        component == "Gross fixed capital formation" ~ "P51G",
                                        component %in% c("Changes in inventories","Changes in valuables") ~ "P5M")) %>%
    unite(component_exio_id,c("country","exio_industry","hybrid_component","counterpartarea"),sep = ".") %>%
    group_by(component_exio_id) %>%
    summarise(value = sum(value),.groups = 'keep') %>%
    ungroup()

  hybrid_exio_demand =
    sto_repartition_key %>%
    select(country,Lib,id,industry,repartition_key) %>%
    slice(rep(1:n(), each = 5)) %>%
    group_by(country,Lib,id,industry) %>%
    mutate(component = c('P3_S13','P3_S14','P3_S15','P51G','P5M')) %>%
    slice(rep(1:n(), each = length(exio_countries))) %>%
    group_by(country,Lib,id,industry,component) %>%
    mutate(counterpartarea = exio_countries) %>%
    ungroup() %>%
    unite(component_exio_id,c("country","Lib","component","counterpartarea"),sep = ".",remove = F) %>%
    mutate(value = exio_demand_raw$value[match(component_exio_id,exio_demand_raw$component_exio_id)]) %>%
    group_by(country,industry,id,component,counterpartarea) %>%
    summarise(value = sum(value * repartition_key,na.rm=T),.groups = 'keep') %>%
    ungroup()

  write.table(hybrid_exio_demand,
              file_hybrid_d,
              sep = ";",
              fileEncoding = 'latin1',
              row.names = F)

  if(verbose) print("Computing the hybridized EXIOBASE-based GHG Direct Emissions data")

  file_original_unit = paste0(folder_exio,"/air_emissions/unit.txt")

  file_original_c = paste0(folder_exio,"/air_emissions/F.txt")

  #AR5 GWP

  file_hybrid_c = paste0(folder_hybrid_exio,"/emissions.csv")

  units =
    fread(file_original_unit) %>%
    mutate(GWP = case_when(unit == "kg CO2-eq" ~ 1,
                           grepl("CO2",stressor) ~ 1,
                           grepl("CH4",stressor) ~ 28,
                           grepl("N2O",stressor) ~ 265,
                           grepl("SF6",stressor) ~ 23500,
                           T ~ 0))

  emissions_stressor = fread(file_original_c,sep = "\t",header = F) %>%
    `colnames<-`(paste0(.[1,],"_",.[2,])) %>%
    {.[-c(1:3),]} %>%
    pivot_longer(-1,names_to = 'id') %>%
    mutate(value = as.numeric(value),
           GWP = units$GWP[match(region_sector,units$stressor)],
           id = case_when(substr(id,1,2) %in% exio_countries ~ id,
                          T ~ paste0('ROW',substring(id,3)))) %>%
    group_by(id) %>%
    summarise(value = sum(value * GWP),.groups = 'keep')

  exio_ghg = sto_repartition_key %>%
    mutate(emissions = emissions_stressor$value[match(exio_id,emissions_stressor$id)]) %>%
    group_by(id) %>%
    summarise(value = sum(repartition_key * emissions) / 1000,.groups = 'keep') #  to compute tonnes #TOTAL 45.9 really close to FIGARO estimates

  write.table(exio_ghg,
              file_hybrid_c,
              sep = ";",
              fileEncoding = 'latin1',
              row.names = F)

}


icio_hybridization = function(folder_hybrid,
                              folder_icio,
                              verbose = T)
{

  if(verbose) print("Aligning ICIO classification with a hybrid system: 44 countries × 44 ISIC4 sectors – Initialization")

  if(missing(folder_hybrid)) folder_hybrid = get_folder_hybrid()

  folder_hybrid = ifelse(substring(folder_hybrid,nchar(folder_hybrid)) != "/",paste0(folder_hybrid,"/"),folder_hybrid)

  folder_hybrid_icio = paste0(folder_hybrid,"ICIO/")

  dir.create(folder_hybrid_icio,showWarnings = F)

  exio_countries = get_exiobase_hybrid_countries(source_tab_pass)

  exio_figaro = get_exiobase_figaro_correspondance(source_tab_pass)

  source_icio = list.files(folder_icio,full.names = T) %>%
    subset(grepl("SML",list.files(folder_icio)))

  if(verbose) print("Computing the hybridized ICIO-based Intermediate Transactions matrix")

  icio = vroom(source_icio,delim = ",",show_col_types = F) %>%
    pivot_longer(-1,names_to = "use_id")

  Z_icio = icio %>%
    rename(resource_id = V1) %>%
    filter((resource_id %in% c('OUT','VA','TLS') == F) & use_id != 'OUT') %>%
    mutate(use_country = countrycode(substr(use_id,1,3),'iso3c','iso2c',warn = F),
           use_industry = gsub("_","T",substring(use_id,5)),
           resource_country = countrycode(substr(resource_id,1,3),'iso3c','iso2c',warn = F),
           resource_industry = gsub("_","T",substring(resource_id,5))) %>%
    filter(use_industry %in% c("HFCE","NPISH","GGFC","GFCF","INVNT","DPABR") == F) %>%
    mutate(hybrid_use_country = case_when(use_country %in% exio_countries ~ use_country,
                                          T ~ 'ROW'),
           hybrid_use_industry = case_when(substr(use_industry,1,1) %in% c("B","E","F","G","I","K","L","M","N","O","P","Q","R","S","T","U") ~ paste0(substr(use_industry,1,1),"Z"),
                                           use_industry == 'A01_02' ~ 'A01T02',
                                           use_industry == 'C17_18' ~ 'C17T18',
                                           use_industry == 'C31T33' ~ 'CM',
                                           use_industry == 'D' ~ 'D35',
                                           use_industry == 'J58T60' ~ 'JA',
                                           use_industry == 'J62_63' ~ 'JC',
                                           T ~ use_industry),
           hybrid_resource_country = case_when(resource_country %in% exio_countries ~ resource_country,
                                               T ~ 'ROW'),
           hybrid_resource_industry = case_when(substr(resource_industry,1,1) %in% c("B","E","F","G","I","K","L","M","N","O","P","Q","R","S","T","U") ~ paste0(substr(resource_industry,1,1),"Z"),
                                                resource_industry == 'A01_02' ~ 'A01T02',
                                                resource_industry == 'C17_18' ~ 'C17T18',
                                                resource_industry == 'C31T33' ~ 'CM',
                                                resource_industry == 'D' ~ 'D35',
                                                resource_industry == 'J58T60' ~ 'JA',
                                                resource_industry == 'J62_63' ~ 'JC',
                                                T ~ resource_industry)) %>%
    group_by(use_country = hybrid_use_country,
             use_industry = hybrid_use_industry,
             resource_country = hybrid_resource_country,
             resource_industry = hybrid_resource_industry)  %>%
    summarise(value = sum(value),.groups = 'keep') %>%
    ungroup() %>%
    unite("use_id",c("use_country",'use_industry'),remove = F) %>%
    unite("resource_id",c("resource_country",'resource_industry'),remove = F)

  write.table(Z_icio,
              paste0(folder_hybrid_icio,"Z.csv"),
              sep = ";",
              fileEncoding = 'latin1',
              row.names = F)

  if(verbose) print("Computing the hybridized ICIO-based Value-Added data")

  VA_ICIO = icio %>%
    rename(resource_id = V1) %>%
    filter((resource_id %in% c('VA','TLS') == T) & use_id != 'OUT') %>%
    mutate(use_country = countrycode(substr(use_id,1,3),'iso3c','iso2c'),
           use_industry = gsub("_","T",substring(use_id,5))) %>%
    filter(use_industry %in% c("HFCE","NPISH","GGFC","GFCF","INVNT","DPABR") == F) %>%
    mutate(hybrid_use_country = case_when(use_country %in% exio_countries ~ use_country,
                                          T ~ 'ROW'),
           hybrid_use_industry = case_when(substr(use_industry,1,1) %in% c("B","E","F","G","I","K","L","M","N","O","P","Q","R","S","T","U") ~ paste0(substr(use_industry,1,1),"Z"),
                                           use_industry == 'A01_02' ~ 'A01T02',
                                           use_industry == 'C17_18' ~ 'C17T18',
                                           use_industry == 'C31T33' ~ 'CM',
                                           use_industry == 'D' ~ 'D35',
                                           use_industry == 'J58T60' ~ 'JA',
                                           use_industry == 'J62_63' ~ 'JC',
                                           T ~ use_industry)) %>%
    group_by(country = hybrid_use_country,
             industry = hybrid_use_industry,
             component = resource_id) %>%
    summarise(value = sum(value),.groups = 'keep') %>%
    ungroup() %>%
    unite("id",c("country",'industry'),remove = F)

  write.table(VA_ICIO,
              paste0(folder_hybrid_icio,"VA.csv"),
              sep = ";",
              fileEncoding = 'latin1',
              row.names = F)

  if(verbose) print("Computing the hybridized ICIO-based Final Demand data")

  D_ICIO = icio %>%
    rename(resource_id = V1) %>%
    filter((resource_id %in% c('OUT','VA','TLS') == F) & use_id != 'OUT') %>%
    mutate(use_country = countrycode(substr(use_id,1,3),'iso3c','iso2c'),
           use_industry = gsub("_","T",substring(use_id,5)),
           resource_country = countrycode(substr(resource_id,1,3),'iso3c','iso2c'),
           resource_industry = gsub("_","T",substring(resource_id,5))) %>%
    filter(use_industry %in% c("HFCE","NPISH","GGFC","GFCF","INVNT","DPABR") == T) %>%
    mutate(hybrid_use_industry = case_when(use_industry == 'GGFC' ~ 'P3_S13',
                                           use_industry == 'HFCE' ~ 'P3_S14',
                                           use_industry == 'NPISH' ~ 'P3_S15',
                                           use_industry == 'GFCF' ~ 'P51G',
                                           T ~ 'P5M'),
           hybrid_use_country = case_when(use_country %in% exio_countries ~ use_country,
                                          T ~ 'ROW'),
           hybrid_resource_country = case_when(resource_country %in% exio_countries ~ resource_country,
                                               T ~ 'ROW'),
           hybrid_resource_industry = case_when(substr(resource_industry,1,1) %in% c("B","E","F","G","I","K","L","M","N","O","P","Q","R","S","T","U") ~ paste0(substr(resource_industry,1,1),"Z"),
                                                resource_industry == 'A01_02' ~ 'A01T02',
                                                resource_industry == 'C17_18' ~ 'C17T18',
                                                resource_industry == 'C31T33' ~ 'CM',
                                                resource_industry == 'D' ~ 'D35',
                                                resource_industry == 'J58T60' ~ 'JA',
                                                resource_industry == 'J62_63' ~ 'JC',
                                                T ~ resource_industry)) %>%
    group_by(country = hybrid_resource_country,
             industry = hybrid_resource_industry,
             counterpartarea = hybrid_use_country,
             component = hybrid_use_industry) %>%
    summarise(value = sum(value),.groups = 'keep') %>%
    ungroup() %>%
    unite("id",c("country",'industry'),remove = F)

  write.table(D_ICIO,
              paste0(folder_hybrid_icio,"D.csv"),
              sep = ";",
              fileEncoding = 'latin1',
              row.names = F)

  if(verbose) print("Computing the hybridized ICIO-based GHG Direct Emissions data")

  if(!file.exists(paste0(folder_icio,"/DF_SCOPE.csv"))) stop("The GHG data is missing. Folder ICIO must contains DF_SCOPE.csv")

  icio_ghg = fread(paste0(folder_icio,"/DF_SCOPE.csv")) %>%
    filter(TIME_PERIOD == year & EMISSIONS_SCOPE == "S1") %>%
    mutate(hybrid_country = case_when(countrycode(EMISSIONS_ORIGIN_AREA,'iso3c','iso2c') %in% exio_countries ~ countrycode(EMISSIONS_ORIGIN_AREA,'iso3c','iso2c'),
                                      T ~ 'ROW'),
           hybrid_industry = case_when(substr(ACTIVITY,1,1) %in% c("B","E","F","G","I","K","L","M","N","O","P","Q","R","S","T","U") ~ paste0(substr(ACTIVITY,1,1),"Z"),
                                       ACTIVITY == 'A01_02' ~ 'A01T02',
                                       ACTIVITY == 'C17_18' ~ 'C17T18',
                                       ACTIVITY == 'C31T33' ~ 'CM',
                                       ACTIVITY == 'D' ~ 'D35',
                                       ACTIVITY == 'J58T60' ~ 'JA',
                                       ACTIVITY == 'J62_63' ~ 'JC',
                                       T ~ ACTIVITY)) %>%
    unite(id,c("hybrid_country","hybrid_industry"),remove = F) %>%
    group_by(id,country = hybrid_country,industry = hybrid_industry) %>%
    summarise(value = sum(OBS_VALUE) * 1000000,.groups = 'keep') %>%
    ungroup()


  write.table(icio_ghg,
              paste0(folder_hybrid_icio,"emissions.csv"),
              sep = ";",
              fileEncoding = 'latin1',
              row.names = F)

}

figaro_hybridization = function(folder_hybrid,
                                folder_figaro,
                                verbose = T)
{

  if(verbose) print("Aligning FIGARO classification with a hybrid system: 44 countries × 44 ISIC4 sectors – Initialization")

  if(missing(folder_hybrid)) folder_hybrid = get_folder_hybrid()

  folder_hybrid = ifelse(substring(folder_hybrid,nchar(folder_hybrid)) != "/",paste0(folder_hybrid,"/"),folder_hybrid)

  folder_hybrid_figaro = paste0(folder_hybrid,"FIGARO/")

  dir.create(folder_hybrid_figaro,showWarnings = F)

  exio_countries = get_exiobase_hybrid_countries(source_tab_pass)

  exio_figaro = get_exiobase_figaro_correspondance(source_tab_pass)

  source_figaro = list.files(folder_figaro,full.names = T) %>% subset(grepl("ind-by-ind",list.files(folder_figaro)) & grepl("csv",.) & grepl("flatfile",.))

  figaro = fread(source_figaro)

  if(verbose) print("Computing the hybridized FIGARO-based Intermediate Transactions matrix")

  z_figaro = figaro %>%
    filter(colIi %in% exio_figaro$figaro & rowIi %in% exio_figaro$figaro) %>%
    mutate(hybrid_use_industry = exio_figaro$industry[match(colIi,exio_figaro$figaro)],
           hybrid_resource_industry = exio_figaro$industry[match(rowIi,exio_figaro$figaro)],
           hybrid_use_country = case_when(counterpartArea %in% exio_countries ~ counterpartArea,
                                          T ~ 'ROW'),
           hybrid_resource_country = case_when(refArea %in% exio_countries ~ refArea,
                                               T ~ 'ROW')) %>%
    unite(hybrid_use_id,c("hybrid_use_country","hybrid_use_industry"),remove = F) %>%
    unite(hybrid_resource_id,c("hybrid_resource_country","hybrid_resource_industry"),remove = F) %>%
    group_by(use_id = hybrid_use_id,
             use_country = hybrid_use_country,
             use_industry = hybrid_use_industry,
             resource_id = hybrid_resource_id,
             resource_country = hybrid_resource_country,
             resource_industry = hybrid_resource_industry) %>%
    summarise(value = sum(obsValue),.groups = 'keep')

  write.table(z_figaro,
              paste0(folder_hybrid_figaro,"Z.csv"),
              sep = ";",
              fileEncoding = 'latin1',
              row.names = F)

  if(verbose) print("Computing the hybridized FIGARO-based Value-Added data")

  va_figaro = figaro %>%
    filter(colIi %in% exio_figaro$figaro & rowIi %in% c("D21X31","OP_NRES","OP_RES","D1","D29X39","B2A3G")) %>%
    mutate(hybrid_use_country = case_when(counterpartArea %in% exio_countries ~ counterpartArea,
                                          T ~ 'ROW'),
           hybrid_use_industry = exio_figaro$industry[match(colIi,exio_figaro$figaro)],
           hybrid_resource_industry = case_when(rowIi == "D21X31" ~ "TLS",
                                                T ~ "VA")) %>%
    unite(id,c('hybrid_use_country','hybrid_use_industry'),remove = F) %>%
    group_by(id,country = hybrid_use_country,industry = hybrid_use_industry,component = hybrid_resource_industry) %>%
    summarise(value = sum(obsValue),.groups = 'keep')

  write.table(va_figaro,
              paste0(folder_hybrid_figaro,"VA.csv"),
              sep = ";",
              fileEncoding = 'latin1',
              row.names = F)

  if(verbose) print("Computing the hybridized FIGARO-based Final Demand data")

  d_figaro = figaro %>%
    filter(grepl("P3|P5",colIi) & rowIi %in% exio_figaro$figaro) %>%
    mutate(hybrid_resource_industry = exio_figaro$industry[match(rowIi,exio_figaro$figaro)],
           hybrid_resource_country = case_when(refArea %in% exio_countries ~ refArea,
                                               T ~ 'ROW'),
           hybrid_use_country = case_when(counterpartArea %in% exio_countries ~ counterpartArea,
                                          T ~ 'ROW')) %>%
    unite(id,c("hybrid_resource_country","hybrid_resource_industry"),remove = F) %>%
    group_by(id,country = hybrid_resource_country,industry = hybrid_resource_industry,counterpartarea = hybrid_use_country,component = colIi) %>%
    summarise(value = sum(obsValue),.groups = 'keep')

  write.table(d_figaro,
              paste0(folder_hybrid_figaro,"D.csv"),
              sep = ";",
              fileEncoding = 'latin1',
              row.names = F)

  if(verbose) print("Computing the hybridized FIGARO-based GHG Direct Emissions data")

  figaro_ghg_file = list.files(folder_figaro,pattern = "greenhouse",full.names = T)

  if(length(figaro_ghg_file) == 0) stop("The GHG data is missing. Folder FIGARO must contains a file named : greenhouse-gas-footprints_20XX-edition_YYYY-data.csv")

  figaro_ghg = fread(figaro_ghg_file) %>%
    filter(industry %in% exio_figaro$figaro) %>%
    mutate(hybrid_industry = exio_figaro$industry[match(industry,exio_figaro$figaro)],
           hybrid_country = case_when(ref_area %in% exio_countries ~ ref_area,
                                      T ~ 'ROW')) %>%
    unite(id,c('hybrid_country','hybrid_industry'),remove = F) %>%
    group_by(id,country = hybrid_country,industry = hybrid_industry) %>%
    summarise(value = sum(obs_value) * 1000,.groups = 'keep') %>%
    ungroup()

  write.table(figaro_ghg,
              paste0(folder_hybrid_figaro,"emissions.csv"),
              sep = ";",
              fileEncoding = 'latin1',
              row.names = F)
}

###COMPARE ALIGNED MRIOTS###

compare_hybrid_eemriot = function(folder_hybrid,verbose = T)
{

  if(missing(folder_hybrid)) folder_hybrid = get_folder_hybrid()

  list_folder = list.files(folder_hybrid,include.dirs = T,full.names = T)

  sto_z = sto_va = sto_d = sto_c = c()

  for(i in list_folder)
  {

    z = fread(paste0(i,"/Z.csv")) %>%
      select(use_id = matches("use_id"),
             resource_id = matches("resource_id"),
             !!basename(i) := value)

    va = fread(paste0(i,"/VA.csv")) %>%
      select(id = matches("id"),
             component = matches("component"),
             !!basename(i) := value)

    d = fread(paste0(i,"/D.csv")) %>%
      select(id = matches("id"),
             counterpartarea = matches("counterpartarea"),
             component = matches("component"),
             !!basename(i) := value)

    c = fread(paste0(i,"/emissions.csv")) %>%
      select(id = matches("id|emissions"),
             !!basename(i) := value)

    if(grepl('EXIO',i))
    {
      z = z %>% usd_to_eur(year,basename(i))

      va = va %>% usd_to_eur(year,basename(i))

      d = d %>% usd_to_eur(year,basename(i))

    }

    if(length(sto_z) > 0)
    {
      sto_z = merge(sto_z,z,all = T) %>% mutate(across(-c("use_id","resource_id"),.fns = function(x) replace_na(x,replace = 0)))

      sto_va = merge(sto_va,va,all = T) %>% mutate(across(-c("id"),.fns = function(x) replace_na(x,replace = 0)))

      sto_d = merge(sto_d,d,all = T) %>% mutate(across(-c("id","counterpartarea","component"),.fns = function(x) replace_na(x,replace = 0)))

      sto_c = merge(sto_c,c,all = T) %>% mutate(across(-c("id"),.fns = function(x) replace_na(x,replace = 0)))

    }

    if(length(sto_z) == 0)
    {
      sto_z = z

      sto_va = va

      sto_d = d

      sto_c = c
    }


    if(verbose) print(paste0("Treated : ",i))
  }

  dir.create(paste0(folder_hybrid,"/COMPARISON/"),showWarnings = F)

  write.table(sto_z,
              paste0(folder_hybrid,"/COMPARISON/Z.csv"),
              sep = ";",
              fileEncoding = 'latin1',
              row.names = F)

  write.table(sto_va,
              paste0(folder_hybrid,"/COMPARISON/VA.csv"),
              sep = ";",
              fileEncoding = 'latin1',
              row.names = F)

  write.table(sto_d,
              paste0(folder_hybrid,"/COMPARISON/D.csv"),
              sep = ";",
              fileEncoding = 'latin1',
              row.names = F)

  write.table(sto_c,
              paste0(folder_hybrid,"/COMPARISON/emissions.csv"),
              sep = ";",
              fileEncoding = 'latin1',
              row.names = F)

if(verbose) print(paste0("Comparison files stored in : ",folder_hybrid,"/COMPARISON/"))

}
