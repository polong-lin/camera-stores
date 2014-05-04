#ADENCAMERA.COM
require(stringr)

#Canon lenses
url <- "http://www.adencamera.com/default.asp?Category=7&Manufacturer=125&showall=yes"
library(httr)
html = GET(url)
content <- htmlTreeParse(url, useInternalNodes = T)

#Retrieve the Lens Names from HTML code, under <a class = "productLink">
productLink <- xpathSApply(content, "//a[@class='productLink']", xmlValue)

#Retrieve the Lens Prices from HTML code, under <div class = "ourPrice">
ourPrice <- xpathSApply(content, "//div[@class='ourPrice']", xmlValue)

#Regex out Canon from Product
patternCanon <- "Canon "
rCanon <- regexec(patternCanon, productLink)
matchCanon <- regmatches(productLink, rCanon)
unlist(matchCanon)
productLink <- gsub(" ?Canon ?", "", productLink)
productLink <- gsub(" Lens", "", productLink)
productLink_gsub <- str_trim(gsub("\\*\\* .+\\*\\*", "", productLink))

#Regex out Notes from Product
sale_regmatch <- function(x) {
  if(length(regmatches(x, regexpr("\\*\\* .+\\*\\*", x))) == 0) {
    return("")
  }
  regmatches(x, regexpr("\\*\\* .+\\*\\*", x))
}
notes <- str_trim(gsub("\\*","",unlist(lapply(productLink, sale_regmatch))))


#Regex prices out of HTML form
patternPrice = "[0-9]+[,]?[0-9]+.[0-9][0-9]"
r <- regexec(patternPrice, ourPrice)
regmatch <- regmatches(ourPrice, r)
ourPriceList <- gsub(",","", unlist(regmatch))



d<- data.frame("DateAccess" = rep(Sys.Date(), length(ourPriceList)), 
               "Brand" = rep("Canon", length(ourPriceList)),
               "Product" = productLink_gsub, 
               "Price" = as.numeric(ourPriceList),
               "Notes" = notes)
d <- d[with(d, order(Price, decreasing = TRUE)),]
d


### Next steps:
### Turn into its own column: "EF" and other prefixes
### Turn into its own column: focal length (e.g., 70-200mm)
### Turn into its own column: aperture (e.g., f/3.5-5.6)
### Turn into its own column: Is it L-series?
### Turn into its own column: Does it have IS?
### Turn into its own column: Is it USM or STM?




###############
#Extra: Copy dataframe d to clipboard.
#clip <- pipe("pbcopy", "w")                       
#write.table(d, file=clip)                               
#close(clip)
###############