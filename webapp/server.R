shinyServer(function(input, output, session) {

  # Data selectors
  output$raster_subcategory <- renderUI({
    choices <- names(rasters[[input$raster_category]])
    if (!is.null(choices)) {
      selected <- NULL
      if (! is.null(input$raster)) {
        if (input$raster %in% choices) {
          selected <- input$raster
        }
      }
      selectInput("raster", label=NULL, choices=choices, width="100%", selected=selected)
    }
  })

  output$poly_subcategory <- renderUI({
    choices <- names(polygons[[input$poly_category]])
    if (!is.null(choices)) {
      selected <- NULL
      if (! is.null(input$poly)) {
        if (input$raster %in% choices) {
          selected <- input$poly
        }
      }
      selectInput("poly", label=NULL, choices=choices, width="100%", selected=selected)
    }
  })

  # Data extractors
  raster_layer <- reactive({
    if (!is.null(input$raster)) {
      rasters[[input$raster_category]][[input$raster]]
    } else {
      NULL
    }
  })

  poly_layer <- reactive({
    if (!is.null(input$poly)) {
      polygons[[input$poly_category]][[input$poly]]
    } else {
      NULL
    }
  })

  output$map <- renderLeaflet({

    # provider_url <- switch(input$tiles,
    #   "grey"="Esri.WorldGrayCanvas",
    #   "dark"="CartoDB.DarkMatterNoLabels",
    #   "watercolor"="Stamen.Watercolor"
    # )

    leaflet(options=leafletOptions(minZoom=3, maxZoom=8, attributionControl=F, zoomSnap=0.25)) %>%
      # See https://leaflet-extras.github.io/leaflet-providers/preview/
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      # addProviderTiles(provider_url) %>%
      fitBounds(lng1=-5.4, lat1=32, lng2=36.3, lat2=48)
      # setView(lng=15.45, lat=38, zoom=5)
  })

  # Define the color palette depending on the input
  colorpal <- reactive({
    x <- raster_layer()
    vals <- values(x)
    if (input$raster_category == "Bathymetry") {
      range <- range(vals, na.rm=T)
      pal <- colorNumeric(blues, domain=range, na.color="transparent")

    } else if (input$raster_category == "Frontiers congruence") {
      range <- range(vals, na.rm=T)
      pal <- colorNumeric(BuGnYl, domain=range, na.color="transparent")

    } else if (input$raster_category == "Threats (Micheli et al 2013)") {
      range <- range(vals, na.rm=T)
      pal <- colorNumeric(RdYlGn, domain=range, na.color="transparent")

    } else if (input$raster_category == "Protection areas (Micheli et al 2013)") {
      range <- range(vals, na.rm=T)
      pal <- colorNumeric(brewer_colors(7, "RdYlGn", rev=F)[4:7], domain=range, na.color="transparent")

    } else {
      levels <- sort(unique(vals))
      range <- NULL
      pal <- colorFactor(clr[1:length(levels)], domain=levels, na.color="transparent")
    }
    return(list(pal=pal, range=range))
  })

  # Add base raster layer
  observe({
    # get base raster layer
    x <- raster_layer()

    # remove the current one
    leafletProxy("map") %>%
      # remove previous layer
      removeImage(layerId="base")

    # if one is selected, add it
    if (!is.null(x)) {
      leafletProxy("map") %>%
        addRasterImage(x, layerId="base", colors=colorpal()$pal, project=F)
    }
  })

  # Add regions frontiers
  observe({
    # frontiers layer
    x <- poly_layer()
    if (is.null(x)) {
      # when nothing is selected, remove potentially existing frontiers
      leafletProxy("map") %>%
        clearGroup("frontiers")
    } else {
      # otherwise, remove previous frontiers and add new ones
      leafletProxy("map") %>%
        clearGroup("frontiers") %>%
        addPolygons(data=x, fill=F, group="frontiers", color="black", weight=2, opacity=0.5, smoothFactor=0)
    }
  })

  # Add consensus regions and frontiers
  observe({
    # consensus regions
    if ("regions" %in% input$consensus) {
      leafletProxy("map") %>%
        clearGroup("regions") %>%
        addPolygons(data=regions, fill=F, group="regions", color=input$colour, weight=2, opacity=0.8, smoothFactor=0.5)
    } else {
      leafletProxy("map") %>%
        clearGroup("regions")
    }
    # consensus frontiers
    if ("frontiers" %in% input$consensus) {
      leafletProxy("map") %>%
        clearGroup("ridges") %>%
        addPolylines(data=ridges, group="ridges", color=input$colour, weight=3, opacity=0.8, dashArray="2,4", smoothFactor=0.5)
    } else {
      leafletProxy("map") %>%
        clearGroup("ridges")
    }
  })

})
