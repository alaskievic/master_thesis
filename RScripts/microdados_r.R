#install.packages("devtools")
#devtools::install_github("lucasmation/microdadosBrasil")

library('microdadosBrasil')




# Censo Demográfico 2000
download_sourceData("CENSO", 2000, unzip = T)
d <- read_CENSO('domicilios', 2000)



d <- read_CENSO('pessoas', 2000)





