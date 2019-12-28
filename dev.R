# osm running tracks on interactive map
# Berry Boessenkool, Dec 2019, berry-b@gmx.de

library(shiny) # fluidPage, p, actionButton, checkboxInput, eventReactive, showNotification, shinyApp
library(leaflet) # leafletOutput, leaflet, addTiles, fitBounds, addMeasure, addPolylines, addPolygons
library(leaflet.extras) # addControlGPS, gpsOptions
library(osmdata) # opq, add_osm_feature, osmdata_sf

ui <- fillPage(
  leafletOutput("runwaymap", width="100%", height="95%"),
  fillRow(flex=c(0.1,3,4,2,2,2), width="100%",
  p(" "),
  p(HTML('App by Berry Boessenkool, 2019. <a href="mailto:berry-b@gmx.de">Email</a>, <a href="https://brry.github.io">Homepage</a>')),
  actionButton("get_tracks", "get runnning tracks in this area"),
  checkboxInput("show_tracks", "tracks", value=TRUE),
  checkboxInput("show_residential", "residential", value=TRUE),
  checkboxInput("show_private", "private", value=FALSE)
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
  names(tracks$geometry) <- NULL # https://github.com/ropensci/osmdata/issues/100
  names(polygs$geometry) <- NULL
  list(bbox=bbox, tracks=tracks, polygs=polygs)
  }, ignoreNULL=FALSE)

output$runwaymap <- renderLeaflet({
  ct <- tracks() # ct: current tracks
  first <- is.null(ct$bbox)
  hastracks <- !is.null(ct$tracks) & input$show_tracks
  haspolygs <- !is.null(ct$polygs) & input$show_tracks
  # generate popup displays:
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
  # select only streets that seem suitable for running:
  if(hastracks){
    exclude <-  c("primary", "secondary", "tertiary", "motorway", "motorway_link")
    if(!input$show_residential)
      exclude <- c(exclude, "residential", "service")#, "footway")
    ct$tracks <- ct$tracks[!ct$tracks$highway %in% exclude,]
    if(!input$show_private)
      ct$tracks <- ct$tracks[!ct$tracks$access %in% c("no","private"),]
    ct$tracks$popup <- popleaf2(ct$tracks)
    hastracks <- nrow(ct$tracks) > 0
    }
  if(haspolygs) ct$polygs$popup <- popleaf2(ct$polygs)
  # select map bounds (default on first app run   or   current):
  bnd <- if(first) c(52.41, 13.04, 52.40, 13.03) else 
                   unlist(input$runwaymap_bounds, use.names=FALSE)
  #
  if(hastracks) ct$tracks$highway[is.na(ct$tracks$highway)] <- "NA"
  if(haspolygs) ct$polygs$highway[is.na(ct$polygs$highway)] <- "NA"
  # create map:
  rmap <- leaflet() %>% addTiles() %>% fitBounds(bnd[2],bnd[1],bnd[4],bnd[3]) %>% 
    addMeasure(primaryLengthUnit="kilometers", primaryAreaUnit="hectares",
               activeColor="#3D535D", completedColor="#7D4479") %>% 
    addControlGPS(options=gpsOptions(position="topleft", 
                  activate=TRUE, autoCenter=TRUE, maxZoom=14, setView=TRUE))
  if(hastracks) rmap <- addPolylines(rmap, data=ct$tracks, popup=~popup)
  if(haspolygs) rmap <- addPolygons( rmap, data=ct$polygs, popup=~popup)
  if(!first & !hastracks) showNotification(paste("No tracks found in bbox", 
    toString(ct$bbox)), duration=NULL, id="notification_bbox", type="warning")
  rmap
  })
}
shinyApp(ui, server)
