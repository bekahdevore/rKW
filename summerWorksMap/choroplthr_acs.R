library(choroplethr)
library(choroplethrZip)
library(RCurl)
library(dplyr)
library(DT)
library(ggplot2)

zip_choropleth_acs("B05001", endyear = 2014, span = 5,  msa_zoom="Louisville/Jefferson County, KY-IN", num_colors = 9) + scale_fill_brewer(palette = 'Reds')

