library(httr)
library(rjson)

#ap<-"https://coronavirusapi-france.now.sh/AllDataByDate?date=2020-04-19"
#ceci permet d'obtenir les valeurs pour 19 avril 2020

# pour une date que l'utilisateur choisira :
date<-"2020-04-19" # devra être modifier lors de l'ajout dans l'ui/server
api_date<- paste("https://coronavirusapi-france.now.sh/AllDataByDate?date=", date)
Brut_api_date<-GET(api_date)
rawToChar(Brut_api_date$content)
liste_date<-fromJSON(rawToChar(Brut_api_date$content), method = "C") #je sais j'étudie la question
names(liste_date) #devrait obtenir "code, nom, date, hospitalisation, reanimation etc
#et non allFranceDataByDate : probleme dans GET et dans le format de Rjson


ap<-"https://coronavirusapi-france.now.sh/AllDataByDate?date=2020-08-11"
donneebr<-GET(ap)

liste<-fromJSON(rawToChar(donneebr$content)) #On obtient la liste des infos par dep pour une date précise


dat<-liste$allFranceDataByDate[,c(1,4:7)]

f<-unique(dat[,1])
dat<-dat %>%filter( code== f)


sumna<-function(x){
  sum(x,na.rm = TRUE)
}



sumd<-apply(dat,2,sumna)

select(sumd,reanimation)

attributes(dat)
dat$hospitalises


hos<-liste$allFranceDataByDate$hospitalises%>%sum()
sum(hos$hospitalises)
hos[,4]
liste$allFranceDataByDate[4]%>%unlist()
liste$allFranceDataByDate$hospitalises[4]

liste$allFranceDataByDate[4,4]
paste("https://coronavirusapi-france.now.sh/AllDataByDate?date=","2020-04-19",sep = "")

rawToChar(donneebr$content)
liste<-fromJSON(rawToChar(donneebr$content)) #On obtient la liste des infos par dep pour une date précise


hos<-liste$allFranceDataByDate%>%sapply("[",4)%>%as.data.frame()%>%sum()
#On obtient la liste des infos par dep pour une date précise



apdep<-"https://coronavirusapi-france.now.sh/AllDataByDepartement?Departement=Rhône"
as.character.Date(date())


apdep<-paste("https://coronavirusapi-france.now.sh/AllDataByDepartement?Departement=",
             as.character("Jura"), #changer avec input mais recup liste dep avant
             sep = "")
donneedep<-GET(apdep)
listedep<-fromJSON(rawToChar(donneedep$content))
glimpse(listedep$allDataByDepartement)


paste("https://coronavirusapi-france.now.sh/AllDataByDate?date=","2020-04-19",sep = "")
sel<-function(x){
  s<-x %>% as.data.frame() %>% select_if(is.numeric)
  s
}

sel_date<-function(x){
  s<-x %>% as.data.frame() %>% select(date)
  s
}

dd<-listedep$allDataByDepartement %>% sapply(sel)
head(dd)
dd[c(1:50)]

da<-listedep$allDataByDepartement %>% sapply(sel_date)
da %>% sapply("[") %>% t()
unlist(da)
t(as.data.frame(da[-c(1:34)]))

df<-dd[-c(1:34)]
head(df)
df<-t(df) %>% sapply("[",c(1:6))%>% as.data.frame()%>% t() %>% unlist()

colnames(df)
head(df[,4])

cbind(t(as.data.frame(da[-c(1:34)])),t(df) %>% sapply("[",c(1:6)) %>% as.data.frame() %>% t())

mef_don<-function(x){  #a mettre en reactive dans serveur en ajoutant sel et sel_date
  don<-x %>% sapply(sel)
  dat<-x %>% sapply(sel_date)

  don<-t(don[-c(1:34)]) %>% sapply("[",c(1:6)) %>% as.data.frame() %>% t()
  cname<-colnames(don)
  don<-unlist(don)
  dat<-((dat[-c(1:34)]))
  #les 33 premieres lignes sont pourries et pas standardisees
  #a partir de la ligne 34 toutes les lignes ont la meme forme donc plus simple
  donnee<-don %>% matrix(ncol = 6) %>% data.frame(row.names = dat)
  colnames(donnee)<- cname

  seqD<-seq.Date(from=as.Date("2020-03-29"),to=as.Date("2020-04-01"),by=1)
  donnee[c(as.character( seqD)),]
}

mf<-mef_don(listedep$allDataByDepartement)
glimpse(mf)

head(mf)

seqD<-seq.Date(from=as.Date("2020-03-29"),to=as.Date("2020-04-01"),by=1)

mf[c(as.character( seqD)),]

as.Date("2020-03-29")


mef_don2<-function(x,date_depart,date_fin){  #a mettre en reactive dans serveur en ajoutant sel et sel_date
  don<-x %>% sapply(sel)
  dat<-x %>% sapply(sel_date)

  don<-t(don[-c(1:34)]) %>% sapply("[",c(1:6)) %>% as.data.frame() %>% t()
  cname<-colnames(don)
  don<-unlist(don)
  dat<-dat[-c(1:34)]
  #les 33 premieres lignes sont pourries et pas standardisees
  #a partir de la ligne 34 toutes les lignes ont la meme forme donc plus simple
  donnee<-don %>% matrix(ncol = 6) %>% data.frame(row.names = dat)
  colnames(donnee)<- cname

  seqD<-seq.Date(from=as.Date(date_depart),to=as.Date(date_fin),by=1)
  d<-donnee[c(as.character( seqD)),]
  glimpse(d)
  d
}

try<-mef_don2(listedep$allDataByDepartement,"2020-03-28","2021-08-12")

x<-listedep$allDataByDepartement
date_depart<-"2020-03-29"
date_fin<-"2020-04-01"


mef_don_dep<-function(x,date_depart,date_fin){
  x<-x[-c(1:34),c(1,10,8,7,11,12,13)]
  donnee<-x %>% data.frame(row.names = x$date) %>% select(-date)

  seqD<-seq.Date(from=as.Date(date_depart),to=as.Date(date_fin),by=1)
  d<-donnee[c(as.character( seqD)),]
  glimpse(d)
  d
}

ess<-mef_don_dep(listedep$allDataByDepartement,"2020-03-28","2021-08-12")
cg<-as.Date(rownames(ess))
class(cg)















apdep<-paste("https://coronavirusapi-france.now.sh/AllDataByDepartement?Departement=",
             as.character("France"), #changer avec input mais recup liste dep avant
             sep = "")
donneedep<-GET(apdep)
listedep<-fromJSON(rawToChar(donneedep$content))
glimpse(listedep$allDataByDepartement)

listedep$allDataByDepartement<-listedep$allDataByDepartement[-c(1:100),]

mef_don_dep<-function(x,date_depart,date_fin,source){
  x<-x %>% as.data.frame() %>% filter(sourceType == source) %>% select(date,
                                                 hospitalises,
                                                 reanimation,
                                                 deces,
                                                 gueris,
                                                 nouvellesHospitalisations,
                                                 nouvellesReanimations)
  donnee<-x %>% data.frame(row.names = x$date) %>% select(-date)

  seqD<-seq.Date(from=as.Date(date_depart),to=as.Date(date_fin),by=1)
  d<-donnee[c(as.character( seqD)),]
  glimpse(d)
  d
}

source<-ifelse("France"=="Frace","ministere-sante","sante-publique-france-data")

listedep$allDataByDepartement<-ifelse("France"=="France",
                                      listedep$allDataByDepartement[-c(1:100),],
                                      listedep$allDataByDepartement)

listedep$allDataByDepartement

ess<-mef_don_dep(listedep$allDataByDepartement,"2021-07-12","2021-08-12", "ministere-sante")
as.Date(rownames(ess))

listedep$allDataByDepartement %>% select(deces)

## CARTOGRAPHIE
library(leaflet)
library(sf)
library(png)
France<- st_read(here::here("Dash_COVID/departements-20180101.shp"), quiet=TRUE)
dep <- France %>%
  dplyr::filter(nom %in% "Martinique")
#outrmer<- readPNG("Dash_COVID/Departements-d-Outre-Mer-122.png")

if (dep$nom == "Mayotte"){
  long <-45.1181; lat<--12.825 ; z=20
} else if (dep$nom == "Guyane"){lat = 3.965; long = -53.02 ;z=3
}  else if(dep$nom =="La Réunion"){ lat=-21.124; long= 55.5386 ; z=22
} else if (dep$nom =="Martinique"){ lat = 14.6409; long= -60.9988 ; z=20
}  else {long<-3 ; lat<-47 : z=5.05}


leaflet() %>%
  setView(long, lat, zoom=z)%>% #"Esri.WorldTerrain" "OpenTopoMap" Esri.WorldPhysical
  addProviderTiles("Esri.WorldPhysical")  %>% #Esri.WorldImagery
  addPolygons(data=dep, weight = 2, color="orange",fillOpacity=0.55) %>%
  addPolylines(data = France, color="black", fillOpacity = 0, weight = 1, opacity = 1) %>%
  addMiniMap(width = 75, height = 75, zoomLevelOffset = -7)


#france_json <- geojsonio::geojson_read("departements-avec-outre-mer.geojson", what = "sp")
#france_json<-st_transform(france_json)
#spdf <- rmapshaper::ms_simplify(france_json, keep = 0.1)
#pal <- colorNumeric("Blues", domain = spdf$nom)
#epsg2163 <- leafletCRS(
#  crsClass = "L.Proj.CRS",
#  code = "EPSG:2163",
#  proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997
#  +b=6370997 +units=m +no_defs",
#  resolutions = 2^(16:7))

#leaflet(spdf, options = leafletOptions(crs = epsg2163)) %>%
#  addPolygons(weight = 1, color = "#444444", opacity = 1, fillOpacity = 0.7,
#              smoothFactor = 0.5, labelOptions = labelOptions(direction = "auto")
#             )



library(rworldmap)
library(rgeos)

# get world map
wmap <- getMap(resolution="high")

# get centroids
centroids <- gCentroid(wmap, byid=TRUE)

# get a data.frame with centroids
centroid_df <- as.data.frame(centroids)


# affichage des premières lignes
head(centroid_df)
