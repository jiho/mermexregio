shinyUI(
  bootstrapPage(
    tags$style(type = "text/css", HTML("
    html, body {
      width:100%;
      height:100%;
    }
    .controls {
      font-size: 90%;
      background: rgba(255,255,255,0.5);
      border-radius: 0px 0px 5px 5px;
      padding: 0px 10px 8px 10px;
    }
    .controls > * {
      display: inline-block;
    }
    label {
      margin-bottom: 0px
    }
    .form-group {
      margin-bottom: 0px
    }
    .shiny-input-container {
      width: 320px !important;
    }
    .checkbox {
      margin-top: 0px !important;
    }
    .info, .leaflet-control-zoom {
      border-radius: 4px;
      box-shadow: none;
      margin-top: 20px !important;
      border: 1px solid #ccc;
    }
    "
    )),

    leafletOutput("map", width = "100%", height = "100%"),


    absolutePanel(top = 0, left = 50, class="controls",
      # base map layer type
      selectInput("raster_category", label="Map", choices=c("None", names(rasters)), selected="Frontiers congruence"),
      # and subsequent choices
      uiOutput("raster"),

      # overlay layer type
      selectInput("poly_category", label="Frontier overlay", choices=c("None", names(polygons)), selected="None"),
      # and subsequent choices
      uiOutput("poly"),

      br(),

      # consensus layers
      checkboxGroupInput("consensus", label="Consensus", choices=c("regions","frontiers"), selected = c("regions","frontiers"), inline=TRUE),
      # with a chose of colours
      radioButtons("colour", label="Colour", choices=c("white", "black", "red"), inline = TRUE)
    ),

    # advertise MerMex!
    absolutePanel(bottom = 10, left = 10,
      a(href="https://mermex.mio.univ-amu.fr", img(src="mermex.png", height="50px"))
    )

 )
)
