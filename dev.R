# osm running tracks on interactive map
# Berry Boessenkool, Dec 2019, berry-b@gmx.de

# toDo: 
# - pan/zoom to layer group
# - publish app
# - link to in readme
# - add example GPS track
# - reduce number of files in repo
# - rename dev.R script to app.R
# - trace occasional error: attr(obj, "sf_column") does not point to a geometry column
# - figure out why it shows only residential tracks for Berlinchen
# - figure out why the app reloads map frantically after get tracks, then zoom in, then pan


# 0. Packages ------------------------------------------------------------------
library(shiny) # fluidPage, p, actionButton, checkboxInput, eventReactive, showNotification, shinyApp
library(leaflet) # leafletOutput, leaflet, addTiles, fitBounds, addMeasure, addPolylines, addPolygons
library(leaflet.extras) # addControlGPS, gpsOptions
library(osmdata) # opq, add_osm_feature, osmdata_sf

default_map_area <- c(53.0,  12.0,  53.1,  12.1) # largebox >0.1 Test
default_map_area <- c(52.41, 13.04, 52.40, 13.03) # Potsdam
default_map_area <- c(53.22, 12.61, 53.20, 12.57) # Berlinchen
if(F) {bnd <- default_map_area ; leaflet() %>% addTiles() %>% 
  fitBounds(bnd[2],bnd[1],bnd[4],bnd[3]) %>% 
  addMarkers(lng=bnd[c(2,2,4,4)], lat=bnd[c(1,3,1,3)])}

# 1. Functions -----------------------------------------------------------------
# generate popup displays from df rows:
popleaf2 <- function(df) # see also berryFunctions::popleaf
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

# function to add GPX record to map
readGPX <- function(gpxfile) # see also github.com/brry/visGPX
  {
  df <- try(plotKML::readGPX(gpxfile), silent=TRUE)
  if(inherits(df,"try-error")) stop("Reading the GPX file failed. Error message by ",
                                    "'plotKML::readGPX':\n", df)
  df <- df$tracks[[1]][[1]]
  # check for columns:
  if(!is.data.frame(df)) stop("The GPX file was not read as a data.frame, but a ", class(df))
  cn <- c("lon","lat","ele","time")
  present <- cn %in% colnames(df)
  if(any(!present)) stop("The GPX df does not contain the following column(s): ", 
                         toString(cn[!present]),".")
  # format columns, add distance:
  df$ele <- round(as.numeric(df$ele),1)
  df$time <- strptime(df$time, format="%Y-%m-%dT%H:%M:%SZ")
  df$dist <- OSMscale::earthDist("lat", "lon", data=df, along=TRUE)
  df$dist <- cumsum(df$dist) # path length in km
  # create km markers:
  kms <- max(round(df$dist))
  if(kms>=1)
    {
    kms <- seq(1,kms,1)
    kmark <- sapply(kms, function(x) which.min(abs(df$dist-x)))
    } else
    kmark <- NULL
  df$dist <- round(df$dist,3)
  df$display <- popleaf2(df)
  list(df=df, kmark=kmark, kms=kms)
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
  # exclude big roads:
  exclude <-  c("primary", "secondary", "tertiary", "motorway", "motorway_link")
  tracks <- tracks[!tracks$highway %in% exclude,]
  names(tracks$geometry) <- NULL # https://github.com/ropensci/osmdata/issues/100
  names(polygs$geometry) <- NULL
  list(bbox=bbox, tracks=tracks, polygs=polygs)
  }


# warning for large bboxes:
checkZoom <- function(zoom) # no output, side effect conditionally warning / error
  {
  if(zoom<10) stop(safeError(paste0("You've zoomed out too far (level ",zoom,
     "). I won't request OSM tracks for such a large region. ",
     "Copy the source code at github.com/brry/runway, ", 
     "remove this error and run the app locally if you really want to.")))
  if(zoom<12)
  showModal(modalDialog(title=paste("Large data request. Current zoom level:",zoom),
    p("The current map area is very large and will contain many OSM tracks."), 
    p("The download may take a while..."), 
    fade=FALSE, footer=modalButton("OK")))
  }


# 2. App layout ----------------------------------------------------------------  
ui <- fillPage(
  leafletOutput("runwaymap", width="100%", height="92%"),
  fillRow(
  p(HTML('App by Berry Boessenkool, 2019. <a href="mailto:berry-b@gmx.de">Email</a>, <a href="https://brry.github.io">Homepage</a>')),
  actionButton("get_tracks", "get runnning tracks in this area"),
  fileInput("import_gpx", NULL, buttonLabel="import GPX", accept="gpx")
  )
)


# 3. App inputs ----------------------------------------------------------------
server <- function(input, output, session) {
tracks <- eventReactive(input$get_tracks, {
  bbox <- input$runwaymap_bounds
  if(is.null(bbox)) return() # for first run of the app
  checkZoom(input$runwaymap_zoom)
  downloadTracks(bbox)
  }, ignoreNULL=FALSE)

gpx <- eventReactive(input$import_gpx, {
  if(is.null(input$import_gpx)) return() # if no file is imported
  readGPX(input$import_gpx$datapath)
  }, ignoreNULL=FALSE)


# 4. Computation ---------------------------------------------------------------
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

# 5. Create map ----------------------------------------------------------------
  # select map bounds (default on first app run   or   current):
  bnd <- if(first) default_map_area else 
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
      overlayGroups=c("tracks","residential", "private", "gpx"),
      options=layersControlOptions(collapsed=FALSE))
  cg <- gpx()
  if(!is.null(cg$df)) rmap <- rmap %>% addCircleMarkers(~lon, ~lat, data=cg$df, 
      popup=~display, stroke=FALSE, color="black", radius=3, group="gpx")
  if(!is.null(cg$kmark)) rmap <- rmap %>% addLabelOnlyMarkers(
      lng=cg$df$lon[cg$kmark], lat=cg$df$lat[cg$kmark], label=paste(cg$kms,"km"), 
      labelOptions=labelOptions(noHide=TRUE,textsize="14px",textOnly=TRUE), group="gpx")
  rmap
  })
}
shinyApp(ui, server)
