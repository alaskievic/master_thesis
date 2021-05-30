
library(sp)
library(raster)
library(rgdal)
library(tmap)
library(readxl)



#######################
# Functions           #
#######################

# Parameters:
## input.level: Low or High
## crop: Soy or Maize
## fun.agg: mean, sum (...)
## ibge.data: municipios or estados

amc.Ipea = function()
{
  amc= read_xls("/Users/thiagopereiralobo/Documents/Data/IPEA/AMC/IPEADATA_Municipios1997_X_AMCs.XLS", sheet= "Municípios_X_AMCs")
  amc = amc[c("UFMUNDV", "NEW_CODE_1991_1997")]
  names(amc) = c("CD_MUN","amc")
  return(amc)
}

potential = function(input.level, crop, fun.aggr, ibge.data, df, amc.view)
{
  fao.path = paste0("/Users/thiagopereiralobo/Documents/Data/FAOGAEZ/RainFeed/", input.level, "Input/", crop, "/data.asc")
  if(ibge.data == "municipios")
  {
    ibge.path = paste("/Users/thiagopereiralobo/Documents/Data/IBGE/Mapas/br_municipios_2019")
    ibge.file = paste("BR_Municipios_2019")
  }
  if(ibge.data == "estados")
  {
    ibge.path = paste("/Users/thiagopereiralobo/Documents/Data/IBGE/Mapas/uf_2019")
    ibge.file = paste("BR_UF_2019")
  }
  r =  raster(fao.path)
  p =  readOGR(ibge.path, ibge.file, stringsAsFactors = F)
  if (amc.view == TRUE) 
  {
    amc = amc.Ipea()
    p = merge(p, amc, by = "CD_MUN")
    p= aggregate(p, by = "amc", na.rm= TRUE)
  }
  p = extract(brick(r), p, fun = fun.aggr, na.rm = TRUE, sp = TRUE)
  if (df == TRUE)
    {
    p = data.frame(p)
    if(amc.view== FALSE) {colnames(p) = c("cod_munic", "name", "uf", "area_km2", input.level)}
    if(amc.view== TRUE) {colnames(p) = c("amc", input.level)}
   }  
  return(p)
}

build.data = function(crop.name, fun.agg, dtype, amc.agg)
{
  soy.low = potential(input.level = "Low", crop = crop.name, fun.aggr=fun.agg, ibge.data= dtype, df= TRUE, amc.view = amc.agg)
  soy.high = potential(input.level = "High", crop = crop.name, fun.aggr=fun.agg, ibge.data= dtype, df= TRUE, amc.view = amc.agg)
  if(amc.agg == FALSE){data= merge(soy.low, soy.high[c("cod_munic", "High")], by = "cod_munic")}
  if(amc.agg == TRUE){data= merge(soy.low, soy.high, by = "amc")}
  data$potential_diff = data$High - data$Low
  return(data)
}

#######################
# Main Code           #
#######################
soy.low = potential(input.level = "Low", crop = "Soy", fun.aggr=mean, ibge.data= "municipios", df= TRUE, amc.view = FALSE)
soy.low = potential(input.level = "Low", crop = "Soy", fun.aggr=mean, ibge.data= "municipios", df= TRUE, amc.view = TRUE)


main.data.mean = build.data(crop.name= "Soy", fun.agg= mean, dtype= "municipios", amc.agg= TRUE)

write.csv(main.data.mean, "/Users/thiagopereiralobo/Documents/Python/PhD/Project/StrChange/Input/faogaez_final.csv")



