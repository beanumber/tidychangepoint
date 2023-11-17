library(readxl)
library(xlsx)
archivo_excel <- "Parametros pruebas V01.xlsx"
hoja_excel <- "V01"
matriz_parametros <- read.xlsx(xlsxFile = archivo_excel, sheet = hoja_excel, skipEmptyRows = FALSE,colNames = T)


matriz_parametros <- read_excel("data-raw/Parametros pruebas V01.xlsx")
renglon_parametros <- matriz_parametros[2,]
texto_list_param <- genera_texto_eval_param(renglon_parametros)
eval(parse(text=texto_list_param))
