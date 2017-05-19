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
    .popover {
      min-width:100px;
      width:400px;
      max-width:400px;
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
      a(href="https://mermex.mio.univ-amu.fr", img(src="mermex.png", height="50px")),
      tags$button(class="btn btn-default", "Download data",
        `data-toggle`="popover",
        `data-placement`="top",
        # title="Download data",
        `data-content`='
        <dl>
        <dt>Synthesis</dt>
        <dd>Consensus <a href="Concensus_regions.zip">regions</a>, <a href="Concensus_frontiers.zip">frontiers</a></dd>
        <dd>Congruence <a href="Frontiers_congruence_Count.tif">raw</a> or <a href="Frontiers_congruence_Smoothed.tif">smoothed</a></dd>

        <dt>Retained regionalisations</dt>
        <dd>Spalding et al (2007)
            <a href="Retained_regionalisations_Spalding_et_al_2007.tif">cleaned</a>,
            <a href="Raw_regionalisations_Spalding_et_al_2007.tif">raw</a></dd>
        <dd>Mayot et al (2016) Cluster on climato.
            <a href="Retained_regionalisations_Mayot_et_al_2016_Cluster_on_climato.tif">cleaned</a>,
            <a href="Raw_regionalisations_Mayot_et_al_2016_Cluster_on_climato.tif">raw</a></dd>
        <dd>Palmieri (2014)
            <a href="Retained_regionalisations_Palmieri_2014_Chl_tot.tif">cleaned</a>,
            <a href="Raw_regionalisations_Palmieri_2014_Chl_tot.tif">raw</a></dd>
        <dd>Reygondeau et al (2017) Bioregions epipelagic
            <a href="Retained_regionalisations_Reygondeau_et_al_2017_Bioregions_epipelagic.tif">cleaned</a>,
            <a href="Raw_regionalisations_Reygondeau_et_al_2017_Bioregions_epipelagic.tif">raw</a></dd>
        <dd>Berline et al (2014)
            <a href="Retained_regionalisations_Berline_et_al_2014.tif">cleaned</a>,
            <a href="Raw_regionalisations_Berline_et_al_2014">raw</a></dd>
        <dd>Rossi et al (2014)
            <a href="Retained_regionalisations_Rossi_et_al_2014_PLD_30.tif">cleaned</a>,
            <a href="Raw_regionalisations_Rossi_et_al_2014_PLD_30.tif">raw</a></dd>
        <dd>Nieblas et al (2014)
            <a href="Retained_regionalisations_Nieblas_et_al_2014_Full.tif">cleaned</a>,
            <a href="Raw_regionalisations_Nieblas_et_al_2014_Full.tif">raw</a></dd>
        <dd>Reygondeau et al (2014) Ecoregions pelagic
            <a href="Retained_regionalisations_Reygondeau_et_al_2014_Ecoregions_pelagic.tif">cleaned</a>,
            <a href="Raw_regionalisations_Reygondeau_et_al_2014_Ecoregions_pelagic.tif">raw</a></dd>

        <dt>Other regionalisations</dt>
        <dd>D\'Ortenzio and Ribera d\'Alcalà (2009)
            <a href="Any_regionalisation_D_Ortenzio_and_Ribera_d_Alcala_2009.tif">cleaned</a>,
            <a href="Raw_regionalisations_D_Ortenzio_and_Ribera_d_Alcala_2009.tif">raw</a></dd>
        <dd>Mayot et al (2016) Average cluster
            <a href="Any_regionalisation_Mayot_et_al_2016_Average_cluster.tif">cleaned</a>,
            <a href="Any_regionalisations_Mayot_et_al_2016_Average_cluster.tif">raw</a></dd>
        <dd>Palmieri (2014) Chl max
            <a href="Any_regionalisation_Palmieri_2014_Chl_max.tif">cleaned</a>,
            <a href="Raw_regionalisations_Palmieri_2014_Chl_max.tif">raw</a></dd>
        <dd>Palmieri (2014) Chl surf
            <a href="Any_regionalisation_Palmieri_2014_Chl_surf_PISCES.tif">cleaned</a>,
            <a href="Any_regionalisations_Palmieri_2014_Chl_surf_PISCES.tif">raw</a></dd>
        <dd>Reygondeau (2017) Bathypelagic
            <a href="Any_regionalisation_Reygondeau_et_al_2017_Bioregions_bathypelagic.tif">cleaned</a>,
            <a href="Raw_regionalisations_Reygondeau_et_al_2017_Bioregions_bathypelagic.tif">raw</a></dd>
        <dd>Reygondeau (2017) Mesopelagic
            <a href="Any_regionalisation_Reygondeau_et_al_2017_Bioregions_mesopelagic.tif">cleaned</a>,
            <a href="Raw_regionalisations_Reygondeau_et_al_2017_Bioregions_mesopelagic.tif">raw</a></dd>
        <dd>Rossi et al (2014) PLD=60
            <a href="Any_regionalisation_Rossi_et_al_2014_PLD_60.tif">cleaned</a>,
            <a href="Raw_regionalisations_Rossi_et_al_2014_PLD_60.tif">raw</a></dd>
        <dd>Nieblas et al (2014) Classical
            <a href="Any_regionalisation_Nieblas_et_al_2014_Classical.tif">cleaned</a>,
            <a href="Raw_regionalisations_Nieblas_et_al_2014_Classical.tif">raw</a></dd>
        <dd>Nieblas et al (2014) Mesoscale
            <a href="Any_regionalisation_Nieblas_et_al_2014_Mesoscale.tif">cleaned</a>,
            <a href="Raw_regionalisations_Nieblas_et_al_2014_Mesoscale.tif">raw</a></dd>
        <dd>Reygondeau et al (2014) Ecoregions
            <a href="Any_regionalisation_Reygondeau_et_al_2014_Ecoregions.tif">cleaned</a>,
            <a href="Raw_regionalisations_Reygondeau_et_al_2014_Ecoregions.tif">raw</a></dd>
        <dd>Reygondeau et al (2014) Contiguous
            <a href="Any_regionalisation_Reygondeau_et_al_2014_Ecoregions_contig.tif">cleaned</a>,
            <a href="Raw_regionalisations_Reygondeau_et_al_2014_Ecoregions_contig.tif">raw</a></dd>
        </dl>
        '
      ),
      tags$script("$(document).ready(function(){
        $('[data-toggle=\"popover\"]').popover({html:true});;
      });")
    )

 )
)
