shinyServer(function(input, output, session) {

  # Data selectors
  output$raster_subcategory <- renderUI({
    choices <- names(rasters[[input$raster_category]])
    if (!is.null(choices)) {
      selectInput("raster", label="", choices=choices)
    }
  })

  output$poly_subcategory <- renderUI({
    choices <- names(polygons[[input$poly_category]])
    if (!is.null(choices)) {
      selectInput("poly", label="", choices=choices)
    }
  })

  # Data extractors
  raster_layer <- reactive({
    if (!is.null(input$raster)) {
      rasters[[input$raster_category]][[input$raster]]
    }
  })

  poly_layer <- reactive({
    if (!is.null(input$poly)) {
      polygons[[input$poly_category]][[input$poly]]
    }
  })

  output$map <- renderLeaflet({
    # TODO restrict max zoom
    leaflet() %>%
      # See https://leaflet-extras.github.io/leaflet-providers/preview/
      # Funky
      # addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") %>%
      # addProviderTiles("Stamen.Watercolor") %>%
      # Terrain
      # addProviderTiles("Esri.WorldPhysical") %>%
      # addProviderTiles("Esri.WorldTerrain") %>%
      # addProviderTiles("Esri.OceanBaseMap") %>%
      # addProviderTiles("Acetate.terrain") %>%
      # Neutral gray
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      # addProviderTiles("Acetate.basemap") %>%
      # addProviderTiles("CartoDB.PositronNoLabels") %>%
      # Dark
      # addProviderTiles("CartoDB.DarkMatterNoLabels") %>%
      #
      fitBounds(lng1=-5.4, lat1=30, lng2=36.3, lat2=46)
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
    x <- raster_layer()
    # NB: take care of the fact that before the page is fully loaded, nothing is selected
    if (!is.null(x)) {
      # get an appropriate colour palette
      pal <- colorpal()
      # add raster layer
      leafletProxy("map") %>%
        # remove previous layer
        removeImage(layerId="base") %>%
        # add new one
        addRasterImage(x, layerId="base", colors=pal$pal, project=F)
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
