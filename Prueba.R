@ -74,11 +74,42 @@ NDCdata.best<-lapply(c(1:pages),function(x){
}
)

#

NDCdata.best<-do.call(rbind,NDCdata.best)
# you can see we get the same object
dim(NDCdata.best)

#Next we need to do this for all countries and all available data in this repository
#let's first begin with more countries
#
#let's explore the database
colnames(NDCdata)
names(NDCdata)
summary(NDCdata)
#get unique values of variable
unique(NDCdata$iso_code3)
#let's do all at once
sapply(NDCdata,unique)
sapply(NDCdata,max)

#let's do this for all countries

#primero tengo que obtenter un vector con las claves de los países
#segundo tengo que estimar cuantas páginas por país hay
#tercero en función de eso programar mi lapply para que haga rbind en todas las tables }
# guardar un csv de cada pais en mi carpeta
# este proceso tiene que ocurrir en todos los paises


#segunda opción jalo la base completa, y el loop corre 2003 .
#guardo el archivo


NamesC<-
  for (i in 1:length(NamesC))
    