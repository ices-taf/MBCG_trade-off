library(rmarkdown)

setwd("D:/MBCG_trade-off/5 - Output/Markdown_html")
path <- "D:/MBCG_trade-off/5 - Output/Markdown_html"

rmarkdown::render(input=paste0(path, "/Create_HTML_Baltic_Sea.Rmd"),
                  output_file="HTML_Baltic_Sea.html")
rmarkdown::render(input=paste0(path, "/Create_HTML_Greater_North_Sea.Rmd"),
                  output_file="HTML_Greater_North_Sea.html")
rmarkdown::render(input=paste0(path, "/Create_HTML_Celtic_Seas.Rmd"),
                  output_file="HTML_Celtic_Seas.html")
rmarkdown::render(input=paste0(path, "/Create_HTML_Bay_of_Biscay_and_the_Iberian_Coast.Rmd"),
                  output_file="HTML_BayofBiscay_and_theIberianCoast.html")

