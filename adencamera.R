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
rownames(d) <- 1:nrow(d)

head(d)
# DateAccess Brand                               Product Price Notes
# 1 2014-05-04 Canon EF 200-400mm f4L IS USM Extender 1.4x 11600  Sale
# 2 2014-05-04 Canon                EF 800mm f/5.6L IS USM 10949  Sale
# 3 2014-05-04 Canon              EF 600mm f4.0L IS II USM 10849  Sale
# 4 2014-05-04 Canon             EF 400mm f/2.8L IS II USM 10175  Sale
# 5 2014-05-04 Canon                EF 500mm f4L IS II USM  8644  Sale
# 6 2014-05-04 Canon              EF 300mm f2.8L IS II USM  6589  Sale



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