shinyUI(
  bootstrapPage(
    tags$style(type = "text/css", HTML("
    html, body {
      width:100%;
      height:100%;
    }

    /* Unify position and appearance between leaflet and custom panels */
    .leaflet-control {
      box-shadow: none;
    }
    .info {
      background: white;
    }
    #settings-panel {
      top: 10px;
      left: 46px;
      background: rgba(255, 255, 255, 0.7);
      border-radius: 4px;
      padding: 0 5px;
    }
    /* interface colors */
    #collapsers .btn,
    .leaflet-control-zoom-in,
    .leaflet-control-zoom-out {
      color: #337ab7 !important;
    }
    #collapsers .btn:hover,
    .leaflet-control-zoom-in:hover,
    .leaflet-control-zoom-out:hover {
      background-color: #f1f0f2 !important;
      color: #23527c !important;
    }

    /* Compress settings section */
    label {
      margin-top: 0;
      margin-bottom: 0px;
    }
    .form-group {
      margin-top: 0;
      margin-bottom: 0;
    }
    .sub  {
      margin-top: -5px
    }
    .shiny-options-group {
      margin-top: -5px !important;
    }
    dl {
      margin-bottom: 0;
    }
    /* make select inputs large enough to fit names */
    .shiny-input-container {
      min-width: 320px !important;
    }
    /* add a bit of space at the bottom to look cleaner */
    #data,
    #info {
      margin-bottom: 10px;
      font-size: 90%;
    }
    /* align options vertically */
    #consensus-settings .shiny-options-group {
      margin-top: 6px !important;
      margin-bottom: 23px;
    }

    #data dd,
    #info p {
      background: white;
      padding-left: 5px;
      padding-right: 5px;
      border-radius: 4px;
    }

    /* Format collapse toggles */
    #collapsers {
    }
    #collapsers .btn {
      padding-top: 3px;
      padding-bottom: 3px;
      border: 0;
    }
    #collapsers a:after{
      font-family: 'Glyphicons Halflings';
      font-size: 70%;
      content: '\\e253';
    }
    #collapsers a.collapsed:after{
      content: '\\e252';
    }
    /* move the first one flush with the container */
    #settings-collapser {
      margin-left: -5px;
    }
    "
    )),

    leafletOutput("map", width = "100%", height = "100%"),

    absolutePanel(id="settings-panel",
      fluidRow(id="collapsers",
        div(class="col-md-12",
          a("Settings ", class="btn btn-default", id="settings-collapser", href="#settings", `data-toggle`="collapse"),
          a("Download ", class="btn btn-default collapsed", id="data-collapser", href="#data", `data-toggle`="collapse"),
          a("Info ", class="btn btn-default collapsed", id="data-collapser", href="#info", `data-toggle`="collapse")
        )
      ),

      # Settings
      fluidRow(id="settings", class="collapse in",
        div(class="col-md-4", id="raster-settings",
          # base map layer type
          selectInput("raster_category", label="Background map", choices=c("None", names(rasters)), selected="Frontiers congruence", width="100%"),
          # and subsequent choices
          uiOutput("raster_subcategory", class="sub")
        ),
        div(class="col-md-4", id="frontiers-settings",
          # overlay layer type
          selectInput("poly_category", label="Overlay frontiers of", choices=c("None", names(polygons)), selected="None", width="100%"),
          # and subsequent choices
          uiOutput("poly_subcategory", class="sub")
        ),
        div(class="col-md-4", id="consensus-settings",
          checkboxGroupInput("consensus", label="Overlay consensus", choices=c("regions","frontiers"), selected = c("regions","frontiers"), inline=TRUE),
          # with a choice of colours
          div(class="sub", radioButtons("colour", label=NULL, choices=c("white", "black", "red"), inline = TRUE))
        )
      ),

      # Data
      fluidRow(id="data", class="collapse",
        div(class="col-md-4",
          HTML('
            <dl>
              <dt>Synthesis</dt>
              <dd>Consensus <a href="Consensus_regions.zip">regions</a>, <a href="Consensus_frontiers.zip">frontiers</a></dd>
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
            </dl>'
          )
        ),
        div(class="col-md-4",
          HTML('
            <dl>
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
          ')
        ),
        div(class="col-md-4",
          HTML('
            <dl>
            <dt>Threats</dt>
            <dd>Micheli et al. (2013) <a href="https://doi.org/10.1371/journal.pone.0079889">Cumulative Human Impacts on Mediterranean and Black Sea Marine Ecosystems: Assessing Current Pressures and Opportunities</a>. PLoS ONE 8(12):e79889. <a href="Threats_Micheli_et_al_2013.zip">data layers</a> (NB: resolution reduced compared to original data)</dd>

            <dt>Protection plans</dt>
            <dd>Micheli et al. (2013) <a href="https://doi.org/10.1371/journal.pone.0059038">Setting Priorities for Regional Conservation Planning in the Mediterranean Sea</a>. PLoS ONE 8(4):e59038. <a href="Protection_areas_Micheli_et_al_2013.zip">data layers</a> (NB: resolution reduced compared to original data)</dd>
            <dt>Bathymetry</dt>
            <dd>Data from <a href="http://dx.doi.org/10.7289/V5C8276M">ETOPO1</a>. <a href="Bathymetry_1min.tif">1 min</a>, <a href="Bathymetry_4min.tif">4 min</a></dd>
            </dl>
            ')
          )
      ),

      # Info
      fluidRow(id="info", class="collapse",
        div(class="col-md-4",
          p(HTML('This page displays the result of: Ayata SD, Irisson J-O, Aubert A, Berline L, Dutay JC, Mayot N, Nierblas A-E, d\'Ortenzio F, Palmiéri J, Reygondeau G, Rossi V, Guieu C (2017) <em>Regionalisation of the Mediterranean basin, a MERMEX synthesis</em>, Progress in Oceanography x(x):xx-xx. and should be cited as such.')),
          a(href="https://mermex.mio.univ-amu.fr", img(src="mermex.png", height="50px"))
        ),
        div(class="col-md-4",
          HTML('<p>This study was conducted as part of the WP5 <a href="https://mermex.mio.univ-amu.fr">MERMEX</a>/<a href=http://www.mistrals-home.org>MISTRALS</a> project and is a contribution to the international <a href="http://www.solas-int.org" title="SOLAS INT">SOLAS</a>, <a href="http://imber.info/index.php/" title="Home - IMBER">IMBER</a> and <a href="https://www.futureearthcoasts.org">LOICZ</a> programs. The lead authors are grateful to Pr. Philippe Koubbi (MNHN) for initiating (eco)regionalisation studies of the Mediterranean Sea at the <a href="http://lov.obs-vlfr.fr">Laboratoire d’Océanographie de Villefranche-sur-mer</a> (LOV, UPMC/CNRS). Some initial thoughts that have led to this synthesis were also supported by the PlankMed action of WP5 <a href="https://mermex.mio.univ-amu.fr">MERMEX</a> and by the EC FP7 <a href="http://www.perseus-net.eu" title="Homepage | perseus">PERSEUS Project</a>. The authors are grateful to F Micheli and N Levin for openly sharing their data regarding cumulative impacts and conservation schemes in the Mediterranean Sea.</p>')
        ),
        div(class="col-md-4",
          HTML('<p>This application is powered by <a href="https://shiny.rstudio.com">shiny</a> and <a href="http://leafletjs.com" title="Leaflet - a JavaScript library for interactive maps">leaflet</a>. Colours are extended from <a href="http://colorbrewer2.org/" title="ColorBrewer: Color Advice for Maps">Harrower and Brewer (2003)</a>. The source code for it, as well as for the whole data analysis pipeline, by <a href="http://obs-vlfr.fr/~irisson/">Jean-Olivier Irisson</a>, is freely available at <a href="https://github.com/jiho/mermexregio">github</a>, under the <a href="https://www.gnu.org/licenses/gpl-3.0.en.html">GPL v3 license</a>.</p>')
        )
      )
    )
  )
)
