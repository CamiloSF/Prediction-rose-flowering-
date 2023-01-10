# Cargar paquetes
pacman::p_load(here, dplyr, writexl, lubridate)

# Datos y parametros

programa<-read.table(file.choose(),h=T) # Cargar datos (Archivo "Rosas_Datos.txt" en la carpeta Camilo sf)
programa$ID<-paste(programa[,1],parametros[,2],programa[,3],programa[,4])

parametros<-read.table(file.choose(),h=T) # Cargar datos (Archivo "Parametros rosas.txt" en la carpeta Camilo sf)
parametros$ID<-paste(parametros[,1],parametros[,2],parametros[,3],parametros[,4])

retorno<-read.table(file.choose(),h=T)
retorno$ID<-paste(retorno[,1],retorno[,2],retorno[,3],retorno[,4])

Lista_variedades<-read.table(file.choose(),h=T)
Lista_variedades$ID<-paste(Lista_variedades[,1],Lista_variedades[,2],Lista_variedades[,3],Lista_variedades[,4])

# Run---------------------------------------------------------------


d<-NULL
for (j in seq_along(Lista_variedades$Variedad)) {
 z<-Lista_variedades[j,5]
  
 Tallos_Al<-programa %>% filter(Tipo=="ALINEAMIENTO" & ID==z) %>% 
   mutate(Tallos_p = Tallos * subset(retorno,ID==z)$RET_Alineamiento)
 
 Tallos_Co<- programa %>% filter(Tipo=="CORTE" & ID==z) %>% 
   mutate(Tallos_p = Tallos * subset(retorno,ID==z)$RET_Corte)
 
 x<-rbind(Tallos_Al,Tallos_Co)
 d<-bind_rows(d,x)
 
}

d$Fecha_Corte<-as.Date(d$Fecha_Corte)


jo<-NULL 
for (i in seq_along(d$Tallos)) {
  Var=d[i,9]
  
  Fecha_Cos<-d[i,5]+subset(parametros,ID==Var)$dias_span
  Tallos_Prog<-d$Tallos_p[i]*subset(parametros,ID==Var)$span
  je<-d[i,1:10]
  ji<-cbind(je,Fecha_Cos,Tallos_Prog,row.names = NULL)
  jo<-bind_rows(jo,ji)
  
}

#Exportar a Excel

write_xlsx(jo,'Resultado_simulador_Rosas.xlsx')

# End ---------------------------------------------------------------

