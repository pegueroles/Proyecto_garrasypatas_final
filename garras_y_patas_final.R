"RECETA DE DISEÑO"
"datosJuntos: pagina web -> string & int  "
"Definicion de datosJuntos: ingresar los link de cada perro que se encuentran en 3 pagina 
de la web garrasypatas, y analizar las caracteristicas del can"
"Ejemplo: http://www.garrasypatas.cl/perros/page/1/ ->
http://www.garrasypatas.cl/project/afortunado/ y de este perro nos entrega ->
Nombre - Edad - Raza - Sexo - Tamaño - Se adapta a departamento - Convive con niños
Convive con perros - Convive con gatos - Nivel de energía Esterilizado - Whatsapp" 

#Libreria encargada de leer el html
library(rvest)

# AbriR un csv 
if(file.exists("todoslosperros.cvs")){
  print("todoslosperros.cvs")
  filedatosJuntos <- read.csv(file = "todoslosperros.cvs", header = TRUE)
}

#DataFrame encargado de almacenar la informacion
datosJuntos = data.frame()

for(i in 1:3){
  print(paste("http://www.garrasypatas.cl/perros/page/",i,"/",sep = ""))
  
  web_perros <- read_html(paste("http://www.garrasypatas.cl/perros/page/",i,"/",sep = ""))
  
  los_link_perros <- html_nodes(web_perros, '.cmsms_open_link')
  
  los_links <- html_attr(los_link_perros,"href")
  
  #extraccion de cada canino
  for(elCan in los_links){
    #extraccion cada perro
    print(elCan)
    adopta_perros <- elCan
    
    primer_perro<- read_html(adopta_perros)
    
    nombre <- html_nodes(primer_perro,'.project_features_item_title')
    
    # Extraccion del texto contenido en otra_tabla con la función html_text
    # y transformación a lista de los contenidos con as.list
    limpio <- as.list(html_text(nombre))
    
    otra_tabla <- html_nodes(primer_perro,'.project_features_item_desc' )
    
    # Extraccion del texto contenido en otra_tabla con la función html_text
    # y transformación a lista de los contenidos con as.list
    tabla_limpia <- as.list(html_text(otra_tabla))
    
    # Asignación de nombres a la lista tabla_limpia, que se encuentran
    # almacenados en la variable limpio
    names(tabla_limpia) <- limpio
    
    # Creacio de dataframe a partir de los datos de la variable tabla_limpia
    tablajunta <- as.data.frame(tabla_limpia)
    
    # Union de dataFrames
    datosJuntos <- rbind(datosJuntos,tablajunta)
    
  }
}


#Unificar registros nuevos con los del CSV ya ingresados
if(exists("filedatosJuntos")){
  print("Uniendo los DataFrames")
  datosJuntos <- rbind(datosJuntos,tablajunta)
}

# elimina datos duplicados
datosJuntos <- unique(datosJuntos)

#guardar la informacion en cvs
write.csv(datosJuntos, file = "todoslosperros.cvs")

#Graficar
library('ggplot2')

datos <- read.csv("todoslosperros.cvs")

barra1 <- ggplot(datos,aes(x = Edad))+geom_bar(width=0.6,fill="red", color="black")+ggtitle("Gráfico de barras")
barra2 <- ggplot(datos,aes(x = Raza))+geom_bar(width=0.6,fill="orange", color="black")+ggtitle("Gráfico de barras")
barra3 <- ggplot(datos,aes(x = Sexo))+geom_bar(width=0.6,fill="yellow", color="black")+ggtitle("Gráfico de barras")

barra4 <- ggplot(datos,aes(x = Tamaño))+geom_bar(width=0.6,fill="pink", color="black")+ggtitle("Gráfico de barras")
barra5 <- ggplot(datos,aes(x = Se.adapta.a.departamento))+geom_bar(width=0.6,fill="green", color="black")+ggtitle("Gráfico de barras")
barra6 <- ggplot(datos,aes(x = Convive.con.niños))+geom_bar(width=0.6,fill="gray", color="black")+ggtitle("Gráfico de barras")
barra7 <- ggplot(datos,aes(x = Convive.con.perros))+geom_bar(width=0.6,fill="purple", color="black")+ggtitle("Gráfico de barras")
barra8 <- ggplot(datos,aes(x = Esterilizado))+geom_bar(width=0.6,fill="coral", color="black")+ggtitle("Gráfico de barras")

