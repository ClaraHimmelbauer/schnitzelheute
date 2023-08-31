# funktion um die url für jene region zu erhalten, die man scrapen will

getregion <- function(region = "Österreich"){
  
  region_list <- list("burgenland" = c("eisenstadt", "neusiedl", "pinkafeld"),
                      "kaernten", "niederoesterreich", "ooe", "salzburg",
                      "steiermark", "tirol", "vorarlberg", "wien")
  bundeslaender <- c("Burgenland", "Kärnten", "Niederösterreich", "Oberösterreich",
                     "Salzburg", "Steiermark", "Tirol", "Vorarlberg", "Wien")
  
  if(!(region %in% c(bundeslaender, "Österreich"))){
    stop("your region is not valid.
         Please use one of the following values:
         Burgenland, Kärnten, Niederösterreich, Obersöterreich, Salzubrg, Steiermark, Tirol, Vorarlberg, Wien, Österreich")
  } 
  
  # region to scrape
  if(region == "Österreich"){
    region_scrape <- unlist(region_list)
  } else {
    x <- which(bundeslaender == region)
    region_scrape <- unlist(region_list[[x]])
  }
  
  return(region_scrape)
}
