# osm running tracks on interactive map
# Berry Boessenkool, Dec 2019, berry-b@gmx.de

library(shiny) # fluidPage, p, actionButton, checkboxInput, eventReactive, showNotification, shinyApp
library(leaflet) # leafletOutput, leaflet, addTiles, fitBounds, addMeasure, addPolylines, addPolygons
library(leaflet.extras) # addControlGPS, gpsOptions
library(osmdata) # opq, add_osm_feature, osmdata_sf

# function to generate popup displays from df rows:
popleaf2 <- function(df) # see also berryFunctions::popleaf
  {
  df <- as.data.frame(df)
  df <- df[,colnames(df)!="geometry"] # apparently doesn't work for sf
  sel <- colnames(df)
  hw <- which(sel=="highway")
  sel <- c(sel[hw], sel[-hw])
  apply(df, MARGIN=1, function(x)
    {
    nna <- !is.na(x[sel])
    paste(sub("highway", "type", sel[nna]), ": ", x[sel[nna]], collapse="<br>")
    })
}

  
ui <- fillPage(
  leafletOutput("runwaymap", width="100%", height="95%"),
  fillRow(flex=c(0.1,3,3), width="100%",
  p(" "),
  p(HTML('App by Berry Boessenkool, 2019. <a href="mailto:berry-b@gmx.de">Email</a>, <a href="https://brry.github.io">Homepage</a>')),
  actionButton("get_tracks", "get runnning tracks in this area")
  )
)

server <- function(input, output, session) {
tracks <- eventReactive(input$get_tracks, {
  # current bounding box:
  bbox <- input$runwaymap_bounds
  if(is.null(bbox)) return() # for first run of the app
  bbox <- unlist(bbox, use.names=FALSE)
  # warn about too large region: toDo
  # get OSM streets for bbox:
  streets <- opq(bbox=c(bbox[4], bbox[3], bbox[2], bbox[1])) %>% 
             add_osm_feature(key="highway") %>% osmdata_sf()
  # prepare output:
  tracks <- streets$osm_lines
  polygs <- streets$osm_polygons
  # exclude big roads:
  exclude <-  c("primary", "secondary", "tertiary", "motorway", "motorway_link")
  tracks <- tracks[!tracks$highway %in% exclude,]
  names(tracks$geometry) <- NULL # https://github.com/ropensci/osmdata/issues/100
  names(polygs$geometry) <- NULL
  list(bbox=bbox, tracks=tracks, polygs=polygs)
  }, ignoreNULL=FALSE)

output$runwaymap <- renderLeaflet({
  ct <- tracks() # ct: current tracks
  first <- is.null(ct$bbox)
  hastracks <- !is.null(ct$tracks)
  haspolygs <- !is.null(ct$polygs)
  # Warning for no-data bboxes:
  if(!first & !hastracks) showNotification(paste("No tracks found in bbox", 
    toString(ct$bbox)), duration=NULL, id="notification_bbox", type="warning")
  # subsets of tracks:
  if(hastracks){
    ct$tracks$highway[is.na(ct$tracks$highway)] <- "NA"
    ct$tracks$popup <- popleaf2(ct$tracks)
    residen <- ct$tracks$highway %in% c("residential", "service")#, "footway")
    private <- ct$tracks$access  %in% c("no","private")
    tracks  <- ct$tracks[!residen & !private,]
    residen <- ct$tracks[residen,]
    private <- ct$tracks[private,]
    }
  if(haspolygs){
    ct$polygs$highway[is.na(ct$polygs$highway)] <- "NA"
    ct$polygs$popup <- popleaf2(ct$polygs)
    } 
  # select map bounds (default on first app run   or   current):
  bnd <- if(first) c(52.41, 13.04, 52.40, 13.03) else 
                   unlist(input$runwaymap_bounds, use.names=FALSE)
  # create map, add controls:
  rmap <- leaflet() %>% addTiles() %>% fitBounds(bnd[2],bnd[1],bnd[4],bnd[3]) %>% 
    addMeasure(primaryLengthUnit="kilometers", primaryAreaUnit="hectares",
               activeColor="#3D535D", completedColor="#7D4479") %>% 
    addControlGPS(options=gpsOptions(position="topleft", 
                  activate=TRUE, autoCenter=TRUE, maxZoom=14, setView=TRUE))
  # add grouped data:
  if(hastracks) {
    rmap <- addPolylines(rmap, data=tracks,  popup=~popup, group="tracks")
    rmap <- addPolylines(rmap, data=residen, popup=~popup, group="residential")
    rmap <- addPolylines(rmap, data=private, popup=~popup, group="private")
  }
  if(haspolygs) 
    rmap <- addPolygons( rmap, data=ct$polygs, popup=~popup, group="tracks")
  # add background map layer options:
  prov <- c("OpenStreetMap", "CartoDB.Positron", "CartoDB.DarkMatter", 
          "Esri.WorldImagery", "OpenTopoMap") # mapview::mapviewGetOption("basemaps")
  for(pr in prov) rmap <- rmap %>% addProviderTiles(pr, group=pr)
  rmap <- rmap %>% addLayersControl(baseGroups=prov,
      overlayGroups=c("tracks","residential", "private"),
      options=layersControlOptions(collapsed=FALSE))
  rmap
  })
}
shinyApp(ui, server)
