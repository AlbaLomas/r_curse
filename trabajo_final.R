install.packages("palmerpenguins")
library(palmerpenguins)
data(package = 'palmerpenguins')

####(1) Número de individuos totales, masculinos y femeninos por especie. 
####La media, desviación estándar, valor mínimo y máximo de la longitud 
####y profundidad del pico, la longitud de la aleta y el tamaño.
library(dplyr)
View(penguins)

#Numero de filas --> numero de individuos totales
n_individuos <- nrow(penguins)
n_individuos
#Numero de individuos masculinos y femeninos por especie
summary(penguins)

species_names <- c("Adelie", "Gentoo", "Chinstrap" )
species_names
genders <- c("female", "male")

colnames(penguins)
for (specie_ in species_names) {
  for(gender in genders){
    n1 <-filter(penguins, sex== gender & species == specie_)
    cat("The number of"  , gender , "individuals in " , specie_, "species is :")
    print(nrow(n1))
  }
  
}

####Media,desviacion standard , valor mínimo y máximo de la longitud 
####y profundidad del pico, la longitud de la aleta y el tamaño.
colnames(penguins)
bill_length_media = median(penguins$bill_length_mm, na.rm = TRUE)
bill_length_media

bill_depth_media = median(penguins$bill_depth_mm, na.rm = TRUE)
bill_depth_media

flipper_length_media = median(penguins$flipper_length_mm, na.rm = TRUE)
flipper_length_media

body_mass_media = median(penguins$body_mass_g, na.rm = TRUE)
body_mass_media

######desviacion standard

bill_length_sd = sd(penguins$bill_length_mm, na.rm = TRUE)
bill_length_sd

bill_depth_sd = sd(penguins$bill_depth_mm, na.rm = TRUE)
bill_depth_sd

flipper_length_sd = sd(penguins$flipper_length_mm, na.rm = TRUE)
flipper_length_sd

body_mass_sd = sd(penguins$body_mass_g, na.rm = TRUE)
body_mass_sd

      
######Obtener mininmos
min(penguins$bill_length_mm, na.rm = TRUE)
min(penguins$bill_depth_mm, na.rm = TRUE)
min(penguins$flipper_length_mm, na.rm = TRUE)
min(penguins$body_mass_g, na.rm = TRUE)

######Obtener máximos
max(penguins$bill_length_mm, na.rm = TRUE)
max(penguins$bill_depth_mm, na.rm = TRUE)
max(penguins$flipper_length_mm, na.rm = TRUE)
max(penguins$body_mass_g, na.rm = TRUE)



#Un gráfico de barras que represente el número de individuos muestreados de cada especie en cada isla, 
#representando las especies en diferentes colores (chinstrap – morado, gentoo – azul, adelie – naranja). 

table<- (penguins$species)
table
bar_plot_graph <- table(penguins$species, penguins$island)
bar_plot_graph
barplot(bar_plot_graph, col =c("orange", "purple","blue" ))

#Contesta a las siguientes preguntas: ¿qué especie se ha muestreado en las tres islas? 
#Adelia
#¿cuántos individuos se han muestreado de la isla Dream?
dream_ind <- table(penguins$island)
dream_ind["Dream"]



###Un gráfico multipanel de cajas y bigotes del tamaño de los pingüinos según su sexo, 
###donde aparezca un panel para cada especie. Contesta a las siguientes preguntas: 
penguins <- penguins[!is.na(penguins$bill_length_mm),]
penguins <- penguins[!is.na(penguins$bill_depth_mm),]
penguins <- penguins[!is.na(penguins$flipper_length_mm),]
penguins <- penguins[!is.na(penguins$body_mass_g),]
penguins <- penguins[!is.na(penguins$sex),]


gentoo <- penguins[penguins$species == "Gentoo", ]
gentoo
adelie <- penguins[penguins$species == "Adelie", ]
adelie
chinstrap <- penguins[penguins$species == "Chinstrap", ]
chinstrap

library(patchwork) 

box_gentoo <- ggplot(data = gentoo) + 
  geom_boxplot(aes(x = sex, y = body_mass_g)) + 
  ggtitle("GENTOO") + xlab("Sex") + ylab("Size")

box_adelie <- ggplot(data = adelie) + 
  geom_boxplot(aes(x = sex, y = body_mass_g)) + 
  ggtitle("ADELIE") + xlab("Sex") + ylab("Size")

box_chinstrap <- ggplot(data = chinstrap) + 
  geom_boxplot(aes(x = sex, y = body_mass_g))+
  ggtitle("CHINSTRAP") + xlab("Sex") + ylab("Size")

box_gentoo + box_adelie +box_chinstrap

###¿qué especie tiene mayor tamaño? 
#Gentoo

###¿en qué especie las hembras y los machos tienen un tamaño más similar?
#Chinstrap