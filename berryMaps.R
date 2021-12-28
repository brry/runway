# osm running tracks for given regions on interactive map
# Berry Boessenkool, May 2020, berry-b@gmx.de

message("Defining functions and loading packages...")

# 0. Packages ------------------------------------------------------------------
library(leaflet) # leafletOutput, leaflet, addTiles, fitBounds, addMeasure, addPolylines, addPolygons
library(leaflet.extras) # addControlGPS, gpsOptions
library(osmdata) # opq, add_osm_feature, osmdata_sf


# Locations --------------------------------------------------------------------

loc <- read.table(header=TRUE, sep=",", text="
n, y     ,  x     , zm, t    , l    , b    , r    ,sel,Ort
1, 53.248,  12.652, 15, 53.29, 12.73, 53.21, 12.58,T,Sewekow
2, 52.4  ,  13.05 , 13, 52.46, 13.17, 52.33, 12.90,F,Potsdam
3, 52.545,  14.08 , 15, 52.56, 14.02, 52.52, 14.12,F,Waldsieversdorf
4, 53.21 ,  13.32 , 13, 53.24, 13.24, 53.17, 13.42,F,Lychen
5, 48.67 ,  10.70 , 15, 48.69, 10.65, 48.63, 10.76,F,Tapfheim
6, 43.45 , -80.45 , 15, 43.50,-80.54, 43.42,-80.37,F,Kitchener
7, 44.072, -81.753, 15, 44.19,-81.77, 43.91,-81.63,F,Cottage
8, 45.51 , -78.72 , 13, 46.05,-79.10, 45.34,-77.81,F,Algonquin
9, 46.21 , -80.78 , 14, 46.35,-81.65, 45.88,-80.47,F,Killarney
10,51.595,  10.542, 15, 51.62, 10.50, 51.57, 10.58,F,BadSachsa
")

startview <- 1
if(F){
bnd <- loc[startview,c("l","t","r","b")]
leaflet() %>% addTiles() %>% 
                fitBounds(bnd$l,bnd$t,bnd$r,bnd$b) %>% 
                addMarkers(lng=unlist(loc[startview,c("l","l","r","r")]), 
                           lat=unlist(loc[startview,c("b","t","b","t")]))
rm(bnd)
}


# 1. Functions -----------------------------------------------------------------
# generate popup displays from df rows:
popleaf2 <- function(df) # see also berryFunctions - popleaf
  {
  df <- as.data.frame(df)
  df <- df[,colnames(df)!="geometry"] # apparently doesn't work for sf
  sel <- colnames(df)
  hw <- which(sel=="highway")
  if(length(hw)==1) sel <- c(sel[hw], sel[-hw])
  apply(df, MARGIN=1, function(x)
    {
    nna <- !is.na(x[sel])
    paste(sub("highway", "type", sel[nna]), ": ", x[sel[nna]], collapse="<br>")
    })
  }

# download streets and tracks for certain region
downloadTracks <- function(bbox)
  {
  bbox <- unlist(bbox, use.names=FALSE)
  # get OSM streets for bbox:
  streets <- opq(bbox=c(bbox[4], bbox[3], bbox[2], bbox[1])) %>% 
             add_osm_feature(key="highway") %>% osmdata_sf()
  # prepare output:
  tracks <- streets$osm_lines
  polygs <- streets$osm_polygons

  # geometry error on each first run in fresh R session: 
  # attr(obj, "sf_column") does not point to a geometry column
  tt1 <- try(sf::st_geometry(tracks), silent=TRUE) # having this prevents the geometry error! WTF...
  names(tracks$geometry) <- NULL # https://github.com/ropensci/osmdata/issues/100
  names(polygs$geometry) <- NULL
  tt <- try(sf::st_geometry(tracks), silent=TRUE) # the error would otherwise occur later in addPolylines
  if(inherits(tt, "try-error")) stop(paste0("Something went wrong, ",
    "probably in creating the geometry column of the data.\n",
    "Simply try again, that should normally work fine.\n\n",
    "Debugging info: ", tt))
  
  tracks$highway[is.na(tracks$highway)] <- "NA"
  polygs$highway[is.na(polygs$highway)] <- "NA"
  
  bigstrt <- tracks$highway %in% c("primary", "secondary", "tertiary", "motorway", "motorway_link")
  residen <- tracks$highway %in% c("residential", "service", "footway")
  private <- tracks$access  %in% c("no","private")
  if(is.null(tracks$access)) private <- FALSE
  
  rtracks <- tracks[!bigstrt & !residen & !private,]
  bigstrt <- tracks[ bigstrt & !residen & !private,]
  residen <- tracks[residen & !private,]
  private <- tracks[private,]
  
  polygns <- polygs
  
  rtracks$popup <- popleaf2(rtracks)
  bigstrt$popup <- popleaf2(bigstrt)
  residen$popup <- popleaf2(residen)
  private$popup <- popleaf2(private)
  polygns$popup <- popleaf2(polygns)
 
  list(bbox=bbox, rtracks=rtracks, bigstrt=bigstrt, residen=residen, private=private, polygns=polygns)
  }



addGroups <- function(map, ct)
  {
  if(!is.null(ct$rtracks)) map <- addPolylines(map, data=ct$rtracks, popup=~popup, col="blue",   group="tracks")
  if(!is.null(ct$residen)) map <- addPolylines(map, data=ct$residen, popup=~popup, col="green",  group="residential")
  if(!is.null(ct$private)) map <- addPolylines(map, data=ct$private, popup=~popup, col="red",    group="private")
  if(!is.null(ct$bigstrt)) map <- addPolylines(map, data=ct$bigstrt, popup=~popup, col="orange", group="large roads") ;  
  if(!is.null(ct$polygns)) map <- addPolygons( map, data=ct$polygns, popup=~popup, col="blue",   group="tracks")
  map
  }



# 2. Get data ------------------------------------------------------------------


message("Downloading tracks for ", sum(loc$sel,na.rm=T), " region",if(sum(loc$sel,na.rm=T)>1)"s", "...")
ct <- list()
for(i in which(loc$sel)) ct[[i]] <- downloadTracks(loc[i,c("t","l","b","r")])


# 3. Create map ----------------------------------------------------------------

#leaflet() %>% addTiles() %>% addPolylines(data=tracks, popup=~popup)
message("Creating map...")
  {
  # create map, add controls:
  rmap <- leaflet() %>% 
  addSearchOSM(options=searchOptions(autoCollapse=TRUE, minLength=2, hideMarkerOnCollapse=TRUE, zoom=16)) %>% 
  addControlGPS(options=gpsOptions(position="topleft", 
                activate=TRUE, autoCenter=TRUE, maxZoom=16, setView=TRUE)) %>% 
  addMeasure(primaryLengthUnit="kilometers", primaryAreaUnit="hectares",
            activeColor="#3D535D", completedColor="#7D4479", position="topleft") %>% 
  addScaleBar(position="topleft") %>% 
  addFullscreenControl()
  rmap <- setView(rmap, loc[startview,"x"], loc[startview,"y"], zoom=loc[startview,"zm"])
  # add grouped data:
  for(i in which(loc$sel)) rmap <- addGroups(rmap, ct[[i]])
  # add background map layer options:
  prov <- c(OSM="OpenStreetMap", Sat="Esri.WorldImagery", Topo="OpenTopoMap") # mapview::mapviewGetOption("basemaps")
  for(pr in names(prov)) rmap <- rmap %>% addProviderTiles(unname(prov[pr]), group=pr, 
                                          options=providerTileOptions(maxZoom=20))
  rmap <- rmap %>% addLayersControl(baseGroups=names(prov),
      overlayGroups=c("tracks","residential", "private", "large roads"),
      options=layersControlOptions(collapsed=FALSE)) %>% 
  hideGroup(c("private"))
  rmap
  }

# Export:
if(T){
message("Exporting map as html...")
htmlwidgets::saveWidget(rmap, "index.html", selfcontained=TRUE)
message("Changing html header...")
# HTML head for mobile devices:
# https://stackoverflow.com/questions/42702394/make-leaflet-map-mobile-responsive
map_h <- readLines("index.html")
map_h <- sub('<title>leaflet</title>', x=map_h,
 '<meta name="viewport" content="width=device-width, initial-scale=1.0"/>\n<title>brry.github.io/runway</title>')
writeLines(map_h, "index.html") ; rm(map_h)
}  

openFile("index.html")
