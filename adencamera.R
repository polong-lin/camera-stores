## Purpose of this script:
## Retrieves df of Canon lenses and their prices from Aden Camera's website
## Mines using XML package, and refines df using regular expressions

#ADENCAMERA.COM
require(stringr)
require(XML)
require(RCurl)

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

#Regex function
data.regmatch <- function(x, pattern) {
  if(length(regmatches(x, regexpr(as.character(pattern), x))) == 0) {
    return("")
  }
  regmatches(x, regexpr(as.character(pattern), x))
}

#Regex out Notes from Product
sale_pattern <- "\\*\\* .+\\*\\*"
notes <- str_trim(gsub("\\*","",unlist(lapply(productLink, data.regmatch, sale_pattern))))

#Regex EF, EF-S, TS-E from productLink
type_pattern <- "EF |EF-S |TS-E|MP-E |EF-M"
lens_type <- str_trim(unlist(lapply(d$Product, data.regmatch, type_pattern)))
#d$Type <- lens_type

#Regex focal length in mm
focallength_pattern <- " ([0-9]*[-]+)?[0-9]+mm"
focal_length <- unlist(lapply(d$Product, data.regmatch, focallength_pattern))
#d["Focal Length"] <- focal_length

#Regex aperture
aperture_pattern <- " [fF](/)?([0-9.]{1,3})?(.)?([0-9.]{1,3})(L)?( )?"
aperture <- gsub("f", "", unlist(lapply(d$Product, data.regmatch, aperture_pattern)))
aperture <- gsub("/", "", aperture)
aperture <- gsub("L", "", aperture)
aperture <- str_trim(aperture)
#d$Aperture <- aperture

#Regex L-series
Lseries_pattern <- "L "
Lseries <- unlist(lapply(d$Product, data.regmatch, Lseries_pattern))
#d$Luxury <- Lseries


#Regex IS
is_pattern <- " IS "
is <- str_trim(unlist(lapply(d$Product, data.regmatch, is_pattern)))
#d$IS <- is

#Regex Mark
mark_pattern <- " II "
mark <- unlist(lapply(d$Product, data.regmatch, mark_pattern))
#d$Mark <- mark

#Regex Motor USM/STM
motor_pattern <- "USM|STM"
motor <- unlist(lapply(d$Product, data.regmatch, motor_pattern))
#d$Motor <- motor

#Regex prices out of HTML form
patternPrice = "[0-9]+[,]?[0-9]+.[0-9][0-9]"
r <- regexec(patternPrice, ourPrice)
regmatch <- regmatches(ourPrice, r)
ourPriceList <- gsub(",","", unlist(regmatch))

#Create df
d<- data.frame("DateAccess" = rep(Sys.Date(), length(ourPriceList)), 
               "Brand" = rep("Canon", length(ourPriceList)),
               "Type" = lens_type,
               "Focal Length" = focal_length,
               "Aperture" = aperture,
               "Luxury" = Lseries,
               "IS" = is,
               "Mark" = mark,
               "Motor" = motor,
               "Price" = as.numeric(ourPriceList),
               "Notes" = notes)

d[complete.cases(d$Price,d$),]

d.ordered <- d[with(d, order(Price, decreasing = TRUE)),]
rownames(d.ordered) <- 1:nrow(d)
#d
head(d.ordered)

d$Product












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