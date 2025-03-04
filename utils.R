x = c('dplyr','tidyr','tibble','DBI','curl','rvest',"data.table",'readxl','WDI','insee','arrow','countrycode','vroom','rstudioapi')

lapply(x,library,character.only = T)

default_icio = list(`1995` = "https://stats.oecd.org/wbos/fileview2.aspx?IDFile=d26ad811-5b58-4f0c-a4e3-06a1469e475c",
                    `2001` = "https://stats.oecd.org/wbos/fileview2.aspx?IDFile=7cb93dae-e491-4cfd-ac67-889eb7016a4a",
                    `2006` = "https://stats.oecd.org/wbos/fileview2.aspx?IDFile=ea165bfb-3a85-4e0a-afee-6ba8e6c16052",
                    `2011` = "https://stats.oecd.org/wbos/fileview2.aspx?IDFile=1f791bc6-befb-45c5-8b34-668d08a1702a",
                    `2016` = "https://stats.oecd.org/wbos/fileview2.aspx?IDFile=d1ab2315-298c-4e93-9a81-c6f2273139fe",
                    FPT = "https://stats.oecd.org/wbos/fileview2.aspx?IDFile=f76594ba-12e5-485f-847b-707a40596f6a")


exiobase_fetcher = function(year,folder_exio = tempdir(check=T),link = "https://zenodo.org/records/14869924",verbose = T)
{

  full_url =
    read_html(link) %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    subset(grepl("IOT",.) & grepl("ixi",.) & grepl(year,.) & grepl("download",.)) %>%
    unique() %>%
    paste0("https://zenodo.org/",.)

  file_zipped =
    curl_download(full_url,
                  destfile = tempfile(),
                  quiet = !verbose)

  unzip(file_zipped,
        exdir = folder_exio)

  return(folder_exio)
}

icio_fetcher = function(year,
                        folder_icio = tempdir(),
                        links_icio = default_icio,
                        verbose = T)
{

  if(verbose) print("Please make sure that links_icio are correct : https://www.oecd.org/en/data/datasets/inter-country-input-output-tables.html (Scrap forbidden)")

  if(verbose) print("Please make sure that link_ghg is correct : https://data-explorer.oecd.org/?tm=DF_ICIO_GHG_SCOPE_2023 (Scrap forbidden)")

  full_url = links_icio[[which(year >= as.numeric(names(links_icio))) %>% tail(1)]]

  file_zipped =
    curl_download(full_url,
                  destfile = tempfile(),
                  quiet = !verbose)

  unzip(file_zipped,
        files = paste0(year,"_SML.csv"),
        exdir = folder_icio)

  unlink(file_zipped)

  link_ghg = links_icio[["FPT"]]

  file_zipped =
    curl_download(link_ghg,
                  destfile = tempfile(),
                  quiet = !verbose)

  unzip(file_zipped,
        exdir = folder_icio)

  unlink(file_zipped)

  return(folder_icio)

}

figaro_fetcher = function(year,folder_figaro = tempdir(check=T),link = "https://ec.europa.eu/eurostat/fr/web/esa-supply-use-input-tables/database",verbose = T)
{
  full_urls =
    read_html(link) %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    subset(grepl("greenhouse-gas-footprints|flatfile_eu-ic-io_ind-by-ind",.) & grepl(".csv|.zip",.) & grepl(year,.)) %>%
    unique() %>%
    paste0("https://ec.europa.eu/",.)

  file_ghg = paste0(folder_figaro,"/",basename(full_urls %>% subset(grepl("greenhouse",full_urls))))

  unzip_flat =
    curl_download(full_urls %>% subset(grepl("flatfile_eu-ic-io",full_urls)),
                  destfile = tempfile(),
                  quiet = !verbose)

  unzip(unzip_flat,
        exdir = folder_figaro)

  curl_download(full_urls %>% subset(grepl("greenhouse",full_urls)),
                destfile = file_ghg,
                quiet = !verbose)


  return(folder_figaro)
}

get_exiobase_figaro_correspondance = function(source_tab_pass)
{
  exio_figaro = fread(source_tab_pass) %>%
    select(-c(vrai_code,ExioCode,PaysLib)) %>%
    pivot_longer(-c(1:2),names_to = "raw_industry") %>%
    filter(value == 1) %>%
    mutate(raw_industry = case_when(Lib == "Extra-territorial organizations and bodies" ~ 'D99',
                                    T ~ raw_industry)) %>%
    mutate(industry = case_when(raw_industry %in% c("D01T02","D03") ~ gsub("D","A",raw_industry),
                                raw_industry %in% c("D10T12","D13T15","D16","D17T18","D19","D20","D21","D22","D23","D24","D25","D26","D27","D28","D29","D30") ~ gsub("D","C",raw_industry),
                                raw_industry == "D35" ~ "D35",
                                raw_industry %in% c("D49","D50","D51","D52","D53") ~ gsub("D","H",raw_industry),
                                raw_industry == "D61" ~ 'J61',
                                raw_industry %in% c('D05T06','D07T08') ~ 'BZ',
                                raw_industry == 'D31T33' ~ 'CM',
                                raw_industry == 'D36T39' ~ 'EZ',
                                raw_industry == 'D41T43' ~ 'FZ',
                                raw_industry == 'D45T47' ~ 'GZ',
                                raw_industry == 'D55T56' ~ 'IZ',
                                raw_industry == 'D58T60' ~ 'JA',
                                raw_industry == 'D62T63' ~ 'JC',
                                raw_industry == 'D64T66' ~ 'KZ',
                                raw_industry == 'D68' ~ 'LZ',
                                raw_industry == 'D69T75' ~ 'MZ',
                                raw_industry == 'D77T82' ~ 'NZ',
                                raw_industry == 'D84' ~ 'OZ',
                                raw_industry == 'D85' ~ 'PZ',
                                raw_industry == 'D86T88' ~ 'QZ',
                                raw_industry == 'D90T93' ~ 'RZ',
                                raw_industry == 'D94T96' ~ 'SZ',
                                raw_industry == 'D97T98' ~ 'TZ',
                                raw_industry == 'D99' ~ 'UZ')) %>%
    mutate(figaro = case_when(raw_industry == "D01T02" ~ list(c("A01","A02")),
                              raw_industry == "D03" ~ list("A03"),
                              raw_industry %in% c('D05T06','D07T08') ~ list('B'),
                              raw_industry == "D10T12" ~ list("C10T12"),
                              raw_industry == "D13T15" ~ list("C13T15"),
                              raw_industry == "D16" ~ list("C16"),
                              raw_industry == "D17T18" ~ list(c("C17","C18")),
                              raw_industry =="D19" ~ list("C19"),
                              raw_industry == "D20" ~ list("C20"),
                              raw_industry == "D21" ~ list("C21"),
                              raw_industry == "D22" ~ list("C22"),
                              raw_industry == "D23" ~ list("C23"),
                              raw_industry == "D24" ~ list("C24"),
                              raw_industry == "D25" ~ list("C25"),
                              raw_industry == "D26" ~ list("C26"),
                              raw_industry == "D27" ~ list("C27"),
                              raw_industry == "D28" ~ list("C28"),
                              raw_industry == "D29" ~ list("C29"),
                              raw_industry == "D30" ~ list("C30"),
                              raw_industry == 'D31T33' ~ list(c("C31_32","C33")),
                              raw_industry == "D35" ~ list("D35"),
                              raw_industry == 'D36T39' ~ list(c("E36","E37T39")),
                              raw_industry == 'D41T43' ~ list('F'),
                              raw_industry == 'D45T47' ~ list(c("G45","G46","G47")),
                              raw_industry == "D49" ~ list("H49"),
                              raw_industry == "D50" ~ list("H50"),
                              raw_industry == "D51" ~ list("H51"),
                              raw_industry == "D52" ~ list("H52"),
                              raw_industry == "D53" ~ list("H53"),
                              raw_industry == 'D55T56' ~ list('I'),
                              raw_industry == 'D58T60' ~ list(c("J58","J59_60")),
                              raw_industry == 'D61' ~ list('J61'),
                              raw_industry == 'D62T63' ~ list('J62_63'),
                              raw_industry == 'D64T66' ~ list(c("K64","K65","K66")),
                              raw_industry == 'D68' ~ list('L'),
                              raw_industry == 'D69T75' ~ list(c("M69_70","M71","M72","M73","M74_75")),
                              raw_industry == 'D77T82' ~ list(c("N77","N78","N79",'N80T82')),
                              raw_industry == 'D84' ~ list('O84'),
                              raw_industry == 'D85' ~ list('P85'),
                              raw_industry == 'D86T88' ~ list(c("Q86","Q87_88")),
                              raw_industry == 'D90T93' ~ list(c("R90T92","R93")),
                              raw_industry == 'D94T96' ~ list(c("S94","S95","S96")),
                              raw_industry == 'D97T98' ~ list('T'),
                              raw_industry == 'D99' ~ list('U'))) %>%
    select(-Pays) %>%
    distinct() %>%
    unnest(cols = c(figaro))

  cat("The correspondence table between EXIOBASE and FIGARO originates from an INSEE working paper:
https://www.insee.fr/fr/statistiques/7624261 (Bourgeois, Gervois, Lafrogne-Joussier).
It provides a valuable and verifiable resource for aligning EXIOBASE with the ISIC Rev.4 classification.")

  return(exio_figaro)
}

get_exiobase_hybrid_countries = function(source_tab_pass)
{
  exio_countries = fread(source_tab_pass) %>%
    select(Pays) %>%
    mutate(Pays = case_when(Pays %in% c("WA","WL","WE","WF","WM","TW") ~ 'ROW',
                            T ~ Pays)) %>%
    distinct() %>%
    unlist()
}

usd_to_eur = function(data,year,col = "value")
{

  detect_temp = list.files(dirname(tempdir()),recursive = T,full.names = T) %>% subset(grepl(paste0("USD_EUR_EXCHANGE_RATE_OECD_",year),.))

  if(length(detect_temp) > 0) usd_eur = read.csv(detect_temp[1],sep = ";")
  else{
    usd_eur = read.csv("https://sdmx.oecd.org/archive/rest/data/OECD,DF_DP_LIVE,/.EXCH...A?dimensionAtObservation=AllDimensions&format=csvfile") %>%
      filter(TIME_PERIOD == year & LOCATION == "EU27_2020") %>%
      select(OBS_VALUE) %>%
      unlist()

    write.table(usd_eur,tempfile(pattern = paste0("USD_EUR_EXCHANGE_RATE_OECD_",year),fileext = '.csv'),sep = ";")
  }

  data =
    data %>%
    mutate(across(matches(col),function(x) x * unlist(usd_eur)))

  return(data)
}

get_folder_hybrid = function()
{

  file = list.files(tempdir(),recursive = T,full.names = T) %>% subset(grepl("folder_hybrid_comparison",.))

  update = T

  if(length(file) == 1)
  {
    if(abs(difftime(Sys.time(),file.info(file)$ctime,units = 'hours')) < 1) update = F
  }

  if(!update)
  {
    folder_hybrid = readRDS(file)
  }else{

    folder_hybrid = readline("Param 'folder_hybrid' is empty. Please, provide a correct folder path.")

    saveRDS(folder_hybrid,tempfile(pattern = "folder_hybrid_comparison",fileext = '.rdata'))
  }

  return(folder_hybrid)
}

source_tab_pass = paste0(dirname(getSourceEditorContext()$path),"/TabPass Exio3 to FIGARO.csv")

exio_figaro = get_exiobase_figaro_correspondance(source_tab_pass)

source(paste0(dirname(getSourceEditorContext()$path),"/hybridization.R"))
