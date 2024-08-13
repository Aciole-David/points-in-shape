# Find points that fall within a shape
# Dr. David Aciole Barbosa
# github.com/Aciole-David
# Laboratory of Structural and Functional Genomics
# Integrated Biotechnology Center
# University of Mogi das Cruzes, Sao Paulo, Brazil
# please cyte the source

# 1 - load libs
#some are optional, test it
library(ggplot2)
library(sf)
library(plyr)
library(dplyr)


# 2 - load shapes

#test it to avoid some errors
#sf_use_s2(FALSE)

shp1 <- st_read("shape1.shp")
shp2<- st_read('shape2.shp')

st_crs(shp1) <- 4269
st_crs(shp2) <- 4269

a=read.delim2('metadata.txt',header = T)

str(a)

# # # # # # # # # # # # # # 
# optional filters
#filter region
a <- filter(a, Country == 'columnname' )

#filter states:
shp1_states<-c("state1", "state2","...", "staten")
a <- a %>%
  filter(State.Province %in% shp1_states)

#filter has info:
a <- a %>%
  filter(a$info == 1)
# # # # # # # # # # # # # # 

##REMOVE EMPTY AND NA IN COORDINATES COLUMNS
a <- a[(!is.na(a$Latitude)), ]
a <- a[(!is.na(a$Longitude)), ]

#remove columns made from previous analyses
#a<-a[,-c(9,10)]

str(a)
#add column to compare intersection
a$index<-as.numeric(rownames(a))

#convert coordinates to custom sf
custom.sf <- st_as_sf(a, coords = c("Longitude","Latitude"))
st_crs(custom.sf) <- 4269



shp3 <- st_read('shape3.shp')
st_crs(shp3) <- 4269

# # # # # # # # # # # # # # 
# optional rename regions
#rename some regions to color as the same
shp3$legenda_1<-gsub(shp3$legenda_1, pattern = ' Place1', replacement = '')
shp3$legenda_1<-gsub(shp3$legenda_1, pattern = ' Place2', replacement = '')
shp3$legenda_1<-gsub(shp3$legenda_1, pattern = ' Place3', replacement = '')
shp3$legenda_1<-gsub(shp3$legenda_1, pattern = ' Place4', replacement = '')
shp3$legenda_1<-gsub(shp3$legenda_1, pattern = ' Place5', replacement = '')
# # # # # # # # # # # # # # 

#legenda_1 is the column with regions
regions<-data.frame(region.name=unique(shp3$legenda_1))


# For each aspect region loop ####
#get region
for (now.region in regions$region.name) {
  print(now.region)

shp3.now<-shp3[shp3$legenda_1==now.region,]

st_crs(custom.sf) <- 4269
st_crs(shp3.now) <- 4269

unique(shp3.now$legenda_1)

#merge multipolygons
shp3.now <- shp3.now %>%
    summarise(geometry = sf::st_union(geometry)) %>%
    ungroup()

# 5 - filtrar coordenadas de interesse com interseção no shape inicial
b=as.data.frame(st_intersects(shp3.now, custom.sf)[[1]])

colnames(b)<-'found'
b$index=b$found

# 6 - unir resultado com tabela de coordenadas de interesse
c<-merge(a, b, by="index", all = T)
c[is.na(c)] <- 0
c$occurence<-ifelse(c$found>0, 'Inside', 'Outside')

c<-c[c$found>0,]


try(c$aspect<-now.region)


write.table(x = c, 
            file = paste0('info-',now.region,'.txt'),
            sep = '\t', quote = F, row.names = F)



