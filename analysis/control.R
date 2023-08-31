rm(list = ls()); gc()

packages <- c("dplyr", "rvest", "xml2", "stringr")
sapply(packages, library, character.only = T)

sapply(paste0("R/", list.files("R")), source, encoding = "UTF-8")

# description -------------------------------------------------------------------------------------
# webscrape mittag.at
# search for a keyword: schnitzel
# get current prices

# controls ----------------------------------------------------------------------------------------

# region: Österreich meaning to webscrape all federal states
# otherwise individual federal state
region <- "Oberösterreich"
keyword <- "schnitzel"

# regional vector ---------------------------------------------------------------------------------
region_scrape <- getregion(region)

# GET ---------------------------------------------------------------------------------------------
scrape <- function(region, keyword = "schnitzel"){
  keys <- c(paste(toupper(substr(keyword, 1, 1)), substr(keyword, 2, nchar(keyword)), sep=""),
            paste(tolower(substr(keyword, 1, 1)), substr(keyword, 2, nchar(keyword)), sep=""))
  out <- list()
  
  for(i in 1:length(region)){
    
    # reg <- region[i]
    
    url <- paste0("https://www.mittag.at/a/", reg)
    
    page <- read_html(url)
    
    nodes <- page %>% 
      html_elements('body') %>% 
      xml_find_all('//dd')
    
    text <- html_text(nodes)
    contain_key <- which(str_detect(text, keys[1]) | str_detect(text, keys[2]))
    link_index <- contain_key + 1
    
    links <- nodes[link_index] %>% 
      html_nodes('a') %>% 
      html_attr('href')
    links <- links[substr(links, 1, 3) == "/r/"]
    
    url2 <- paste0("https://www.mittag.at", links)
    
    for(j in 1:length(url2)){
      page <- read_html(url2[j])
      
      adr <- page %>% 
        html_elements('body') %>%
        html_nodes('address') %>% 
        html_text()
      adr <- adr[1]
      adr <- gsub("\n", " ", adr)
      adr <- trimws(adr)
      
      pr <- page %>% 
        html_elements('body') %>% 
        html_elements('.prices') %>% 
        html_text()
      pr <- pr[1]
      pr <- gsub("\n", " ", pr)
      pr <- gsub(",", ".", pr)
      pr <- unlist(str_split(pr, " "))
      pr <- suppressWarnings(as.numeric(pr))
      pr <- pr[!is.na(pr)]
      
      out[[reg]][[j]] <-
        list("url" = url2[j],
             "address" = adr,
             "prices" = pr)
      
      print(j)
      Sys.sleep(sample(1:10, 1))
    }
  }
  return(out)
}



loc <- cont %>%
  html_elements('body') %>%
  xml_find_all('//span[contains(@class, "Text-sc-10o2fdq-0 daYHkI")]') %>%
  html_text()