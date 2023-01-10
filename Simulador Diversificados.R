# Cargar paquetes
pacman::p_load(here, dplyr, writexl, lubridate)

# Datos y parametros

Siembras<-read.table(file.choose(),h=T)
Siembras$ID<-paste(Siembras[,1],Siembras[,5],Siembras[,6])

parametros<-read.table(file.choose(),h=T)
parametros$ID<-paste(parametros[,1],parametros[,2],parametros[,3])

Aprov<-read.table(file.choose(),h=T)
Aprov$ID<-paste(Aprov[,1],Aprov[,2],Aprov[,3])

Lista_variedades<-read.table(file.choose(),h=T)
Lista_variedades$ID<-paste(Lista_variedades[,1],Lista_variedades[,2],Lista_variedades[,3])

# Run---------------------------------------------------------------


d<-NULL
for (j in seq_along(Lista_variedades$Variedad)) {
  z<-Lista_variedades[j,4]
  
  Tallos_Al<-Siembras %>% filter(ID==z) %>% 
    mutate(Tallos_p = Plantas_sembradas * subset(Aprov,ID==z)$Aprovechamiento)
  x<-Tallos_Al
  d<-bind_rows(d,x)
  
}

d$Fecha_siembra<-as.Date(d$Fecha_siembra)


jo<-NULL 
for (i in seq_along(d$Plantas_sembradas)) {
  Var=d[i,10]
  
  Fecha_Cos<-d[i,7]+subset(parametros,ID==Var)$span_dias
  Tallos_Prog<-d$Tallos_p[i]*subset(parametros,ID==Var)$span
  je<-d[i,1:11]
  ji<-cbind(je,Fecha_Cos,Tallos_Prog,row.names = NULL)
  jo<-bind_rows(jo,ji)
  
}

#Exportar a Excel

write_xlsx(jo,'Resultado_simulador_Diversificados.xlsx')

# End ---------------------------------------------------------------

