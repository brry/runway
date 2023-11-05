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
1, 53.248,  12.652, 15, 53.29, 12.58, 53.21, 12.73,T,Sewekow
2, 52.375,  13.125, 14, 0    ,     0,     0,     0,T,Potsdam
3, 52.545,  14.08 , 15, 52.56, 14.02, 52.52, 14.12,F,Waldsieversdorf
4, 53.21 ,  13.32 , 13, 53.24, 13.24, 53.17, 13.42,F,Lychen
5, 48.67 ,  10.70 , 15, 48.69, 10.65, 48.63, 10.76,F,Tapfheim
6, 43.45 , -80.45 , 15, 43.50,-80.54, 43.42,-80.37,F,Kitchener
7, 44.072, -81.753, 15, 44.19,-81.77, 43.91,-81.63,F,Cottage
8, 45.51 , -78.72 , 13, 46.05,-79.10, 45.34,-77.81,F,Algonquin
9, 46.21 , -80.78 , 14, 46.35,-81.65, 45.88,-80.47,F,Killarney
10,51.595,  10.542, 15, 51.62, 10.50, 51.57, 10.58,F,BadSachsa
11,35.357,  24.271, 15, 35.49, 24.11, 35.00, 24.41,F,Kreta
12,51.642,  13.708, 15, 51.70, 13.65, 51.59, 13.76,F,Finsterwalde
13,52.309,  13.446, 15, 0    ,     0,     0,     0,F,Rangsdorf
14,52.410,  12.975, 15, 0    ,     0,     0,     0,F,Golm
15,49.924,  11.585, 14, 0    ,     0,     0,     0,F,Bayreuth
16,28.967, -13.670, 15, 0    ,     0,     0,     0,F,Lanzarote
17,56.372,  15.517, 15, 0    ,     0,     0,     0,F,Stensjoe
18,54.440,  12.683, 15, 0.   ,     0,     0,     0,F,Zingst
")
loc$t[loc$t==0] <- loc$y[loc$t==0]+0.05
loc$b[loc$b==0] <- loc$y[loc$b==0]-0.05 # loc$b <- ifelse(loc$b==0, loc$y-0.05, loc$b)
# for(i in 1:nrow(loc)) 
loc$l[loc$l==0] <- loc$x[loc$l==0]-0.08
loc$r[loc$r==0] <- loc$x[loc$r==0]+0.08


startview <- 2
if(F){
bnd <- loc[startview,c("l","t","r","b")]
leaflet() %>% addTiles() %>% 
                fitBounds(bnd$l,bnd$t,bnd$r,bnd$b) %>% 
                addMarkers(lng=unlist(loc[startview,c("l","l","r","r")]), 
                           lat=unlist(loc[startview,c("b","t","b","t")]))%>% print()
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
  
  bigstrt <- tracks$highway %in% c("primary", "secondary", "tertiary", "motorway", "motorway_link", "trunk", "trunk_link")
  if(!is.null(tracks$motorroad)) bigstrt <- bigstrt | sapply(tracks$motorroad=="yes", isTRUE)
  residen <- tracks$highway %in% c("residential", "service", "living_street")#, "footway")
  if(!is.null(tracks$footway)) residen <- residen | sapply(tracks$footway=="sidewalk", isTRUE)
  private <- tracks$access  %in% c("no","private")
  if(is.null(tracks$access)) private <- FALSE
  
  rtracks <- tracks[!bigstrt & !residen & !private,]
  bigstrt <- tracks[ bigstrt & !residen & !private,]
  residen <- tracks[residen & !private,]
  private <- tracks[private,]
  
  polygns <- polygs[!polygs$access %in% c("no","private"), ]
  polygpr <- polygs[ polygs$access %in% c("no","private"), ]
  
  rtracks$popup <- popleaf2(rtracks)
  bigstrt$popup <- popleaf2(bigstrt)
  residen$popup <- popleaf2(residen)
  private$popup <- popleaf2(private)
  polygns$popup <- popleaf2(polygns)
  polygpr$popup <- popleaf2(polygpr)
 
  list(bbox=bbox, rtracks=rtracks, bigstrt=bigstrt, residen=residen, private=private, polygns=polygns, polygpr=polygpr)
  }



addGroups <- function(map, ct)
  {
  if(!is.null(ct$rtracks)) map <- addPolylines(map, data=ct$rtracks, popup=~popup, col="blue",   group="tracks")
  if(!is.null(ct$residen)) map <- addPolylines(map, data=ct$residen, popup=~popup, col="green",  group="residential")
  if(!is.null(ct$private)) map <- addPolylines(map, data=ct$private, popup=~popup, col="red",    group="private")
  if(!is.null(ct$bigstrt)) map <- addPolylines(map, data=ct$bigstrt, popup=~popup, col="orange", group="large roads") ;  
  if(!is.null(ct$polygns)) map <- addPolygons( map, data=ct$polygns, popup=~popup, col="blue",   group="tracks")
  if(!is.null(ct$polygpr)) map <- addPolygons( map, data=ct$polygpr, popup=~popup, col="red",    group="private")
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
  rmap <- berryFunctions::bmap(loc[startview,"x"], loc[startview,"y"], loc[startview,"zm"])
  # add grouped data:
  for(i in which(loc$sel)) rmap <- addGroups(rmap, ct[[i]])
  rmap <- removeLayersControl(rmap)
  prov <- sapply(rmap$x$calls, function(cl) cl$method) == "addProviderTiles"
  prov <- sapply(rmap$x$calls[prov], function(cl) cl$args[[3]])
  rmap <- rmap %>% addLayersControl(baseGroups=prov,
      overlayGroups=c("tracks","residential", "private", "large roads"),
      options=layersControlOptions(collapsed=FALSE)) %>% 
  hideGroup(c("private"))
  print(rmap)
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
berryFunctions::openFile("index.html")
}  

