library(knitr)
library(rmarkdown)

knitr::opts_knit$set(base.url = 'https://dl.dropboxusercontent.com/u/860546/',
              base.dir = 'C:/Users/Roman/Dropbox/Public/')
knitr::opts_chunk$set(fig.path="figure/futfutarb06032015")
knitr::opts_chunk$get("fig.path")

knit2html("futfutarb.Rmd", encoding = 'UTF-8')

