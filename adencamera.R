#ADENCAMERA.COM

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

#Regex prices out of HTML form
patternPrice = "[0-9]+[,]?[0-9]+.[0-9][0-9]"
r <- regexec(patternPrice, ourPrice)
regmatch <- regmatches(ourPrice, r)
ourPriceList <- unlist(regmatch)

d<- data.frame("DateAccess" = rep(date(), length(ourPriceList)), 
               "Brand" = rep("Canon", length(ourPriceList)),
               "Product" = productLink, 
               "Price" = ourPriceList)
d

#Extra: Copy dataframe d to clipboard.
#clip <- pipe("pbcopy", "w")                       
#write.table(d, file=clip)                               
#close(clip)
