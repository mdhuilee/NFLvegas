
#' To extract Vegas lines
#'
#' This function allows you to extract Vegas lines from NFL matches.
#' @param from The beginning year 
#' @param to The end year
#' @param obs Number of observations. Default value is "all", you could put numeric values instead
#' @keywords nfl
#' @export
#' @examples
#' nfl_vegas(from = 2011, to = 2011, obs = 5)

nfl_vegas <- function(from = 2011, to = 2011, obs = "all"){
library(stringr);library(rvest)
timestamp() 
    
outurl <- character(0)

for (year in from:to ) {

    nfl_season <- read_html(paste0("http://www.pro-football-reference.com/years/",year,"/games.htm"))
    test1 <- nfl_season %>% 
        html_nodes("#games a") %>%    
            #use CSS selector '#games a' to select all tags including links in the table on the website
        html_attr("href")   
            #to extract the value of attribute "href", which contains the links we need
    
    test2 <- test1[substr(test1,2,4) == "box"]    
            #to select the link we need
    test3 <- paste0("http://www.pro-football-reference.com",test2)
            #create the new link we need
    outurl <- c(outurl,test3)
    
}

row = length(outurl)

if (obs != "all") {

row = as.numeric(obs)

}
out <- matrix(nrow = row, ncol = 5)

for (i in 1:row ) {
    
    nfltest <- read_html(outurl[i])
    test <- nfltest %>% 
        html_nodes("#game_info td") %>%
        html_text() 
    
    out[i,5] <- str_extract(test[match("Vegas Line",test)+1], "\\s-?\\d*\\.?\\d+\\b")
    out[i,4] <- str_trim(str_replace(test[match("Vegas Line",test)+1], "\\s-?\\d*\\.?\\d+\\b", ""))
    
    year <- as.numeric(substr(outurl[i],49,52))
    out[i,1] <- year
    out[i,3] <- substr(outurl[i],49,60)
    out[i,2] <- ifelse(substr(outurl[i],53,54) %in% c("01", "02"), year-1, year)
    
}

outdata <- as.data.frame(out)
colnames(outdata) <- c("year","season", "gameid", "favorite", "spread")

timestamp() 
outdata
}


 
