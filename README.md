# runway
suggest running tracks on an interactive map

<https://brry.shinyapps.io/runway>

### usage
Zoom / pan to the area of interest, then click on `get running tracks`.  
The app will download OSM "highway" elements, remove large roads, 
categorize the remaining tracks and display them on the leaflet map.
Each segment can be clicked for more information.

Hopefully, this will also aid others to find runnning tracks, as the OSM maps 
will only display those at high zoom levels, 
where they also look very similar to administrative boundaries.


### further features
Besides displaying tracks, GPX tracks can be imported and displayed, 
along with popup information for each recorded coordinate and a km-counter.  
This has only been tested for GPX files exported from OSMtracker for Android;
Let me know if this feature needs more development.

A measurement tool and a selection of base maps for the background are available as well.
Both work best without any tracks downloaded.

This repository contains the app
[source code](https://github.com/brry/runway/blob/master/app.R)
along with a
[GPX example file](https://github.com/brry/runway/blob/master/example.gpx).  
The app is entirely written in R (a language mainly for data analysis, visualisation and statistics)
and heavily relies on the `leaflet` and `shiny` packages.


### background information
This app was created in December 2019 by [Berry Boessenkool](mailto:berry-b@gmx.de).
Any feedback is welcome.  
If you like my work, feel free to recommend me as [R trainer and consultant](https://brry.github.io).  
This project is licenced under [CC-BY](https://creativecommons.org/licenses/by/4.0), 
so use the source (Luke) as you like, but please do cite me.
