camera-stores
=============
##Data-mining of camera store prices

##Webcrape of names and prices of cameras/lenses of online camera stores


##Purpose:
To create a dataframe of the listed lenses and current prices on the a camera store website for the purposes of easier visualization and comparison.
-To also add columns for any special notes
-Option: Export to Excel file

##Method:
Given a URL, retrive HTML and parse.
Return lens names, prices, notes from HTML.

##Future goals:
-Generate dataframes based on a list of URLs from a single store
-Apply to multiple stores
-Compare prices by store
-Be able to interactively choose a lens, and return a list of stores sorted by prices