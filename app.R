library(htmltools)
library(shiny)
library(echarts4r)
library(bslib)
library(leaflet)
library(reactable)
library(fst)
library(tidyverse)
library(sf)
library(profvis)

# save(list = c('underdiagnosis_drug_chart',
#               'underdiagnosis_deprivation_chart',
#               'diabetes_undiagnosed_deprivation_chart',
#               'af_map',
#               'deprivation_map',
#               'statins_map',
#               'hypertension_undiagnosed_deprivation_chart',
#               'af_undiagnosed_deprivation_chart',
#               'ckd_undiagnosed_deprivation_chart',
#               'master_data_table',
#               'master_data'
#                   ),
#      file = 'file.RData'
#      )

load('file.RData')



# source('./data_model.R')
# source('./ckd.R')
# source('./af.R')
# source('./diabetes.R')
# source('./hypertension.R')
# source('./drugs_underdiagnosis.R')
# source('./master_data_table.R')

# load(".RData")

# rm(past_populations,
#    instantiate_base_pop,
#    default_fracture4_female,
#    th,
#    test_poopulation,
#    teset_population,stroke_incidence,
#    population_w_established_prevalence,new_year_pop,wrapping_examples_in_function)
# rm(list=ls())

# source('./components/footer.R')
source('modules/startup_overlay/startup_overlay_div.R')
source('modules/pivot_module/pivottable.R')
source('modules/pivot_module/pivottable_module.R')

# source('sppg_div.R')

# load('sppg2.RData')

print(environment())
lapply(ls(environment()), function(x){
  y <<- x
  obj <- get(x)
  print(x)
  if('echarts4r' %in% class(obj) ){
    print('T')
    
    if(!is.null(obj$x$theme) & obj$x$theme != ''){
      print(obj$x$theme)
      # print(x)
      # print(obj$x$theme)
      # obj$dependencies[[1]]$src$file <- system.file("htmlwidgets/lib/echarts-6.0.0/themes", package = "echarts4r")#htmltools::htmlDependency(name = obj$x$theme, version = "1.0.0",
      #                         src = system.file("htmlwidgets/lib/echarts-6.0.0/themes", package = "echarts4r"),
      #                        script = paste0(obj$x$theme, ".js")
      # )
      
      obj$dependencies[[1]] <-  htmltools::htmlDependency(name = obj$x$theme, version = "1.0.0", 
                                                          src = system.file("htmlwidgets/lib/echarts-6.0.0/themes", package = "echarts4r"),
                                                          script = paste0(obj$x$theme, ".js")
      )
      
      
      # x<<-obj
      # print (obj)
      assign(x, obj, envir = globalenv())
    }}
  
})

master_data <- master_data %>% 
   select(-c(X, Y))

graph_wrapper <- function(..., header =NULL){
   
   div(class = "grid-item grid-item--graph theme-green",
       div(class = "grid-item-content text-white",
           div(class = "chart-card",
               if(!is.null(header)){
                  div(class = "card-header text-white",# style = "font-size: 0.5em;",
                      header
                  )},
               ...
           )
       )
   )
}

# theme_x <- readLines('theme.json')

ui <- page_fluid( id = 'main-content',
                  theme = bs_theme(version = 5,
                                   bootswatch = 'darkly', #bootswatch = 'lux',
                                   font_scale = 0.7,
                                   # primary='white',
                                   #primary = 'cyan',
                                   #fg = 'white',
                                   fg='white',
                                   bg = 'rgb(31,31,31)',
                                   #primary = 'black'
                  ),
                  
                  startup_overlay_div(8000,11000),
                  includeCSS('./www/progress_bar.css'),
                  tags$head(
                     # Include external dependencies
                     
                     # Packery CSS and JS from CDN (no Draggabilly needed)
                     # tags$script(src = "https://unpkg.com/packery@2/dist/packery.pkgd.min.js"),
                     
                     HTML('<script>
                        window.FontAwesomeConfig = {
                          searchPseudoElements: true
                        }
                      </script>'),
                     # tags$script(src = "roma.js"),
                     #includeScript("roma.js"),
                     HTML('<style>
.dashboard-nav{
--primary-navy: #1e3a8a;
  --primary-blue: #3b82f6;
  --secondary-blue: #60a5fa;
  --accent-orange: #f97316;
  --success-green: #10b981;
  --warning-yellow: #f59e0b;
  --danger-red: #ef4444;
  --neutral-gray: #6b7280;
  --light-gray: #f8fafc;
  --medium-gray: #e2e8f0;
  --dark-gray: #374151;
  --white: #ffffff;
  --glass-bg: rgba(255, 255, 255, 0.85);
--glass-border: rgba(255, 255, 255, 0.2);
--shadow-light: 0 1px 3px rgba(0, 0, 0, 0.1);
--shadow-medium: 0 4px 6px rgba(158, 123, 123, 0.07);
--shadow-heavy: 0 10px 25px rgba(0, 0, 0, 0.1);
--border-radius: 12px;
--border-radius-lg: 16px;
--transition: all 0.2s cubic-bezier(0.4, 0, 0.2, 1);
--fa-style-family-brands: "Font Awesome 6 Brands";
--fa-font-brands: normal 400 1em/1 "Font Awesome 6 Brands";
--fa-font-regular: normal 400 1em/1 "Font Awesome 6 Free";
--fa-style-family-classic: "Font Awesome 6 Free";
--fa-font-solid: normal 900 1em/1 "Font Awesome 6 Free";
color: var(--dark-gray);
line-height: 1.6;
margin: 0;
box-sizing: border-box;
animation-duration: 0.01ms !important;
animation-iteration-count: 1 !important;
transition-duration: 0.01ms !important;
background: var(--white);
border-bottom: 1px solid var(--medium-gray);
padding: 1rem 2rem;
margin: 4rem 0rem;

box-shadow: var(--shadow-light);
}
</style>'),
                     # Custom CSS styling
                     tags$style("
* { box-sizing: border-box; }

html {
  scroll-behavior: smooth;
  font-size:1.0rem;
  color:white;
}

body {
  /*font-family: sans-serif;  */
  margin: 0;
  padding: 0;
   color:white;
}

#pivot-section {
   color:black;
}

p,h*,span{    color:white;}

/* ---- Layout ---- */

.main-container {
  display: flex;
  min-height: 100vh;
}

.sidebar {
  width: 250px;
  background: linear-gradient(180deg, #2c3e50 0%, #34495e 100%);
  color: white;
  position: fixed;
  height: 100vh;
  left: 0;
  top: 0;
  overflow-y: auto;
  box-shadow: 2px 0 10px rgba(0,0,0,0.1);
  z-index: 1000;
}

.sidebar-header {
  padding: 20px;
  border-bottom: 1px solid rgba(255,255,255,0.1);
  text-align: center;
}

.sidebar-title {
  font-size: 1.4em;
  font-weight: bold;
  margin: 0;
}

.nav-section {
  padding: 15px 0;
}

.nav-section-title {
  padding: 10px 5px;
  font-size: 1em;
  
  /*text-transform: uppercase;*/
  letter-spacing: 1px;
  margin: 0;
}

.nav-item {
  
  padding: 3px 3px;
  margin: 2px 0px;
  cursor: pointer;
  transition: all 0.3s ease;
  border-left: 3px solid transparent;
}

.nav-item:hover {
  background: rgba(var(--bs-primary-rgb), 0.5)
  border-radius:10px;

}

.nav-item.active {
  /*background: rgba(52, 152, 219, 0.5);
  color:white; */
  font-weight: bold;
  border-radius:10px;

}

.nav-iten.active{
  background-color: white;
  color: black !important;
  font-weight: normal;
  opacity:1;
  background-opacity:1;

}

a.nav-iten:hover{

 background-color: grey;
 color:black !important;
 opacity:1;
 background-opacity:1;

}


.nav-icon {
  margin-right: 10px;
  width: 16px;
  display: inline-block;
}


.content-area {
  margin:auto;
  flex: 1;
  /*padding: 20px;*/
}


/* ---- grid ---- */
.grid {
  padding: 10px;
  border-radius: 40px;
  max-width: 70vw;
  min-height: 100vh;
}


/* clear fix */
.grid:after {
  content: '';
  display: block;
  clear: both;
}


/* ---- .grid-item ---- */

.grid-item {
  /*float: left;*/
  border-radius:20px;
  box-shadow: 0 1px 3px rgba(100, 0, 0, 0.01);
  border: 1px solid hsla(0, 0%, 100%, 0.5);
  transition: all 0.3s ease;
  cursor: pointer;
  overflow: hidden;
  margin-right: 2px;
  margin-bottom: 2px;
}

.grid-item-content {
  padding: 20px;
  border-radius: 20px 20px 0px 0px;
  width: 100%;
  height: 100%;
  transition: all 0.4s ease;
}

.grid-item--width2 { width: 150px; }
.grid-item--height2 { height: 150px; }
.grid-item--small { width: 150px; height: 150px; }
.grid-item--graph { width: 700px; height: 600px; }

/* Expanded state */
.grid-item.is-expanded {
  z-index: 100;
}


.grid-item.is-expanded.grid-item--small {
  width: 400px;
  height: 300px;
}

.grid-item.is-expanded.grid-item--graph {
  width: 800px;
  height: 500px;
  padding:1rem;
}

.grid-item.is-expanded .grid-item-content {
  transform: scale(1.02);
}

.grid-item:hover {
 /*
  margin-right: 0px;
  margin-bottom: 0px;
  */
  transform: translate(5px 5px);
  z-index:1000;
}

.grid-item:hover .grid-item-content {
  background: rgba( 10,10,10, 0.5);
}

/* Click indicator */
.grid-item::after {
  position: absolute;
  top: 10px;
  right: 10px;
  opacity: 0;
  transition: opacity 0.3s ease;
  font-size: 12px;
  background: rgba(0, 0, 0, 0.3);
  color: white;
  padding: 4px 6px;
  border-radius: 4px;
}

.grid-item.is-expanded::after {
  content: '\f16c';
    /* >> Name of the FA free font (mandatory), e.g.:
               - 'Font Awesome 5 Free' for Regular and Solid symbols;
               - 'Font Awesome 5 Pro' for Regular and Solid symbols (Professional License);
               - 'Font Awesome 5 Brand' for Brands symbols. */
    font-family: 'Font Awesome 5 Free';
    /* >> Weight of the font (mandatory):
               - 400 for Regular and Brands symbols;
               - 900 for Solid symbols;
               - 300 for Light symbols. */
    font-weight: 400;
    opacity:1;
}

/* Metric Cards */
.metric-card {
  margin:auto;
  text-align: center;
  height: 100%;
  width: 100px;
  display: flex;
  flex-direction: column;
  justify-content: center;
}

.handle{
  position:relative;
  top:1px;
  left:15px;
}

.metric-value {
  font-size: 2.1em;
  font-weight: bold;
  margin-bottom: 10px;
}

.metric-label {
  font-size: 1em;
  color: #6c757d;
  margin-bottom: 15px;
}

.metric-change {
  font-size: 0.8em;
  font-weight: 600;
  padding: 5px 15px;
  border-radius: 20px;
  display: inline-block;
}

.metric-change.positive {
  background: rgba(40, 167, 69, 0.1);
  color: #28a745;
}

.metric-change.negative {
  background: rgba(220, 53, 69, 0.1);
  color: #dc3545;
}

/* Chart Cards */
.chart-card {
  padding: 15px;
  height: 100%;
  width: 100%;
  display: flex;
  flex-direction: column;
}

.chart-card .echarts4r {
  flex: 1;
  min-height: 0;
  width: 100% !important;
  height: 100% !important;
}

.card-header {
  font-weight: bold;
  margin-bottom: 15px;
  color: #495057;
  border-bottom: 2px solid #e9ecef;
  padding-bottom: 10px;
}

/* Control Panel */
.control-panel {
  background: white;
  padding: 20px;
  border-radius: 12px;
  margin-bottom: 20px;
  box-shadow: 0 2px 10px rgba(0,0,0,0.1);
}

/* Navigation Cards*/
.nav-card {
  color: black;
  opacity:0.7;
  display: flex;
  flex-direction: column;
  justify-content: center;
  cursor: pointer;
  margin:0.5rem;
  padding:0.5rem;
  border-radius:0.5rem;
  transition:all 0.5s ease;
}

.nav-card:hover {
    opacity:0.9;
  transform: scale(1.02);
  z-index:10;
  transition:all 0.3s ease;
}

.nav-card-icon {
  color:white;
  text-align: right;
  font-size: 1.4em;
  margin-bottom: 0.5rem;
  opacity: 0.9;
}

/* Tab Content Styling */
.tab-content {
  display: block;
  padding: 20px;
  animation: fadeIn 0.3s ease-in-out;
}

.tab-content.active {
  display: block;
}

@keyframes fadeIn {
  from { opacity: 0; }
  to { opacity: 1; }
}

.nav-card-icon i:hover {
  box-shadow: 0 1px 1px rgba(255,255,255,0.1);
}

.nav-card-title {
  font-size: 1.3em;
  font-weight: bold;
  margin-bottom: 8px;
}

a{
text-decoration:none !important;
}

.nav-card-description {
  font-size: 0.8em;
  opacity: 0.8;
}

.nav-card.reports {
  background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);
}

.nav-card.settings {
  background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%);
}

.nav-card.analytics {
  background: linear-gradient(135deg, #43e97b 0%, #38f9d7 100%);
}

")
                  ),
                  
                  # Main layout container
                  
                  # Control Panel
                  
                  div(class = 'w-100  p-0 fixed-top d-flex border-0 gap-0 flex-column flex-wrap',
                      shiny::tags$nav(
                         #style='position:fixed;top:0px;left:0px',
                         class = " bg-black w-100 p-3 border-bottom", # bg-light mt-5
                         shiny::h1(  span( class = 'h5',"Population Health Model"), span(class = 'text-secondary','Public Health Agency'),
                                     
                                    # div(class= 'float-right', 
                                     HTML('<h7><span class="badge rounded-pill bg-opacity-100  bg-success ">Underdiagnosis</span></h7>'),
                                     HTML('<h7><span class="badge rounded-pill bg-opacity-100  bg-success ">Treatment</span></h7>'),
                                     HTML('<h7><span class="badge rounded-pill bg-opacity-100  bg-success ">GPs</span></h7>'),
                                     
                                     
                                     HTML('<h7><span class="badge rounded-pill bg-opacity-100 text-secondary bg-danger ">Hypertension</span></h7>'),
                                     HTML('<h7><span class="badge rounded-pill bg-opacity-100 text-secondary bg-danger ">Diabetes</span></h7>'),
                                     HTML('<h7><span class="badge rounded-pill bg-opacity-100 text-secondary bg-danger ">Atrial Fibrillation</span></h7>'),
                                     HTML('<div><span class="badge rounded-pill bg-opacity-100 bg-danger "></span></div>'),
                                     # ),
                                     
                                     
                                     
                                     
                                     # span(HTML('<h7><span class="badge rounded-pill text-black bg-opacity-100 bg-primary">PHA', '' ,'</span></h7>')),
                                     # HTML('<h7><span class="badge rounded-pill bg-opacity-75 bg-info ">Population Health Model </span></h7>'),
                                     HTML('<h7 id = "move" style = "float:right;">
                       <!-- <a href="https://apply-for-innovation-funding.service.gov.uk/competition/2186/overview/e51c18bc-21b3-450d-bdbc-2f43dad3b268">
                       <span class="badge rounded-pill bg-light">
                        <i class="fa-solid fa-arrow-up-right-from-square"></i>
                       OPIP bid</span>
                       </a> -->
                       </h7>'),
                                     class = "navbar-brand mb-0 w-100"
                         )
                      ),
                      
                      # div(class='mt-5 pt-5'),##position:fixed;top:0px;
                      HTML('<nav id = "top-top"
                                  style = "width:100%;
                                  margin-inline:-12px;backdrop-filter: blur(5px);background: #00D4FF;
                                  background: linear-gradient(55deg, rgba(var(--bs-warning-rgb)0.7) 35%,var(--bs-primary) 55%, var(--bs-info) 87%);",
                                  class=" w-100 bg-gradient-blue-cyan bg-opacity-50">
                      <div class="text-white">
                      <div id ="top-nav-content" class="d-flex gap-5 justify-content-center align-items-center">
                      <a href="#overview-section" class="nav-item bg-opacity-100 nav-iten p-1 px-4 rounded-pill text-white">
                      <i class="fas fa-tachometer-alt"></i>
                      <span class="fs-6">Overview</span>
                      <i class="ms-2 fas fa-chevron-right"></i>

                      </a>
                      <a href="#evidence-section" class="nav-item bg-opacity-100 nav-iten p-1 px-4 rounded-pill text-white">
                      <i class="fas fa-exclamation-triangle"></i>
                      <span class="fs-6">Evidence</span>
                      <i class="ms-2 fas fa-chevron-right"></i>

                      </a>
                      <a href="#prevalence-section" class="nav-item bg-opacity-100 nav-iten p-1 px-4 rounded-pill text-white">
                      <i class="fas fa-tachometer-alt"></i>
                      <span class="fs-6">Prevalence</span>
                      <i class="ms-2 fas fa-chevron-right"></i>

                      </a>
                      <a href="#incidence-section" class="nav-item bg-opacity-100 nav-iten p-1 px-4 rounded-pill text-white">
                      <i class="fas fa-map-marked-alt"></i>
                      <span class="fs-6">Incidence</span>
                       <i class="ms-2 fas fa-chevron-right"></i>
                      </a>
                       

                      <a href="#downstream-section" class="nav-item bg-opacity-100 nav-iten p-1 m-3 px-4 rounded-pill text-white">
                      <i class="fas fa-users"></i>
                      <span class="fs-6" >Downstream Service Demand</span>
                      <i class="ms-2 fas fa-chevron-right"></i>

                      </a>
                      </div>
                      </div>
                      </nav>')
                      
                  ),
                  
                  # div(style='top:15vh;width:12vw;z-index:1000',class = ' ms-2  p-3 d-flex flex-column display-absolute position-fixed left-0 ',#shadow-sm glass-card
                  #     
                  #     div(class = "nav-section",
                  #         h6(class = "bg-opacity-100 border-5  p-2 rounded-3  text-bg-dark ", "Health Burden"), #text-body-secondary
                  #         div(class = "nav-item",
                  #             span(class = "nav-icon"),
                  #             "Dashboard"
                  #         ) #,
                  #         # div(class = "nav-item active",
                  #         #     span(class = "nav-icon"),
                  #         #     "Analytics"
                  #         # ),
                  #         # div(class = "nav-item",
                  #         #     span(class = "nav-icon"),
                  #         #     "Reports"
                  #         #)
                  #     ),
                  #     
                  #     # div(class = "nav-section",
                  #     #     h6(class = "bg-opacity-100 border-5  p-2 rounded-3  text-bg-dark text-light",
                  #     #        "ADHD"),
                  #     #
                  #     #     div(class = "nav-item",
                  #     #         span(class = "nav-icon"),
                  #     #         "Risk"
                  #     #     )#,
                  #     
                  #     # div(class = "nav-item",
                  #     #     span(class = "nav-icon"),
                  #     #     "Geography"
                  #     # ),
                  #     
                  #     # div(class = "nav-section",
                  #     #     h6(class = "bg-opacity-100  border-5  p-2 rounded-3 text-bg-dark text-light", "Intervention"), #nav-section-title
                  #     #     div(class = "nav-item",
                  #     #         span(class = "nav-icon"),
                  #     #         "Specify"
                  #     #     ),
                  #     #     div(class = "nav-item",
                  #     #         span(class = "nav-icon"),
                  #     #         "Scenarios"
                  #     #     )
                  #     # )
                  # ),
                  
                  div(class = "main-container", `data-bs-spy` = "scroll", `data-bs-target` = "#top-nav-content", `data-bs-offset` = "70",
                      # div(class='mt-5 pt-5')
                      # Main content area
                      
                      div(class = "content-area",
                          # div( class="tab-pane active", id="overview", role="tabpanel", `aria-labelledby`="overview",
                          
                          div(class = "tab-content",
                              
                              # Dashboard Tab Content
                              div(id = "dashboard-tab", class = "tab-pane show active", #
                                  
                                  div(class = "container-fluid",
                                      style = "padding-left: 15%;padding-top:5%;padding-right:15%",
                                      
                                      # sppg_div(),
                                      div(class=" rounded-3 bg-danger bg-op text-white p-4 my-5 opacity-100 shadow-sm",
                                          h5(icon('danger'),icon('warning'),'Experimental'),
                                          span(class='text-white','This analysis is experimental. The difficulty is detecting underdignosis 
                                               is detecting an absence of data. Estimates of rates of underdiagnosis are taken from other UK constituencies and applied to Northern Irelands Population and disease prevalence rate.
                                               The direct underdiagnosis is taken where possible, otherwise the comparison between the extraoplated true prevalence and registered prevalence is used to estimate underdiagnosis.')),
                                      
                                      # Overview Section
                                      
                                      div(id='overview-section',class='min-vh-100 py-5my-5',
                                          
                                          div(class=" rounded-3 bg-black text-white p-4 my-5 opacity-100 shadow-sm",
                                              h5('Overview'),
                                              img(class = 'float-right ', src = 'treatment.png', height = '40px'),
                                              div(class='text-white','Overview of estimated patterns of underdiagnosis. Underdiagnosis tends to track with greater absolute prevalence and greater deprivation.
                                                   Starting with Diabetes we take rates of underdiagnosis from the england health survey and apply them to Northern Irelands Population',
                                                  )
                                              ),
                                          
                                          
                                          div(class = "grid-item",
                                              div(class = "grid-item-content",
                                                  div(class = "chart-card",
                                                      
                                                      div(class = "text-white card-header",# style = "font-size: 0.5em;",
                                                          h5('Underdiagnosis with Drug Items per General Practice' )
                                                      ),
                                                      
                                                      underdiagnosis_drug_chart
                                                      
                                                  )
                                              ),
                                              div(class='p-3 rounded-2 text-bg-secondary',
                                                  'Raw Prevalence of Undiagnosed Conditions with Unweighted by strength, Pharmaceutical Items. 
                                                             Note Hypertension underdiangosis is taken as undertreatment from pharmeceutical status.
                                                             Diabetes is taken from English underdiagnosis estimates rates applied to Northern Ireland.
                                                             CKD and AF are taken from comparison and the difference between extrapolated survey rates applied to NI 
                                                             ')
                                              
                                          
                                          
                                      ),
                                      br(),
                                          
                                      div(class = "grid-item",
                                          div(class = "grid-item-content",
                                              div(class = "chart-card",
                                                  
                                                  div(class = "text-white card-header",# style = "font-size: 0.5em;",
                                                      h5('Prevalence of Undiagnosed Condition with Townsend Deprivation - Higher townsend score is more affluent and lower is deprived!' )
                                                  ),
                                                  
                                                  underdiagnosis_deprivation_chart
                                                  
                                              )
                                          ),
                                          div(class='p-3 rounded-2 text-bg-secondary',
                                              'Underdiagnsosis tends to drop with increasing Deprivation ')
                                  ),
                                  br(),
                                  
                                          div(class = "grid-item",
                                              div(class = "grid-item-content",
                                                  div(class = "chart-card",
                                                      
                                                      div(class = "text-white card-header",# style = "font-size: 0.5em;",
                                                          h5('Prevalence of Undiagnosed Diabetes by age and MDM2017 deprivation quintile' )
                                                      ),
                                                      diabetes_undiagnosed_deprivation_chart,
                                                      
                                                  )
                                              ),
                                              div(class='p-3 rounded-2 text-bg-secondary',
                                                  'Prevalence of Diabetes- undiagnosed. The absence of data in NI collected on underdiagnosis means reliance on other means to infer 
                                                  under diagnosis. Here we apply rates of England underdiagnosis to NI')
                                              
                                          ),
                                          
                              
                                  br(),
                                  
                                  div(class = "grid-item",
                                      div(class = "grid-item-content",
                                          div(class = "chart-card",
                                              
                                              div(class = "text-white card-header",# style = "font-size: 0.5em;",
                                                  h5('Geospatial Analysis' )
                                              ),
                                              
                                              div(class="row g-3" ,
                                                 div(class="col-6" ,
                                                 div(class="p-3 border rounded" ,
                                                     div(class = "card-header text-white",# style = "font-size: 0.5em;",
                                                         'Map of Prevalence of Estimated Undiagnosed Atrial Fibrillation'),
                                                     af_map)
                                                 ),
                                                 
                                                 div(class="col-6" ,
                                                 div(class="p-3 border rounded" , 
                                                     div(class = "card-header text-white",# style = "font-size: 0.5em;",
                                                         'Map of Number of Deprivation disparity between pracitce Deprivation and patients '),
                                                     deprivation_map )
                                                 ),
                                                 
                                                 div(class="col-6" ,
                                                 div(class="p-3 border rounded" ,
                                                     div(class = "card-header text-white",# style = "font-size: 0.5em;",
                                                         'Map of Number of Statins Prescriptions'),
                                                     statins_map)
                                                 ),
                                                 
                                                 # div(class="col-6" ,
                                                 # div(class="p-3 border rounded" ,4)
                                                 # )
                                                 # )
                                          
                                                 ),
                                      ),
                                      div(class='p-3 rounded-2 text-bg-secondary',
                                          ' ')
                                  )
                                   )
                                      ),
                                      
                                      div(id = "evidence-section",class='min-vh-100',
                                          
                                          div(style = "min-height: 400px;",class = 'd-flex flex-row py-5 justify-content-evenly gap-5 flex-wrap',
                                              graph_wrapper(hypertension_undiagnosed_deprivation_chart ,
                                                            header = span(span(class='fs-5','Prevalence of undiagnosed Hypertension') ,
                                                                           span(class='ms-2  badge rounded-pill text-bg-secondary','England Survey'),
                                                                           # span(class='ms-2  badge rounded-pill text-bg-dark','Empirical'),
                                                                           span(class='ms-2 p-1 text-bg-dark',' NI GP recorded')
                                                                        )),
                                              
                                          div(class="bg-white text-dark p-3 rounded-3 fs-5 my-5 opacity-100 shadow-sm",
                                              h5('Hypertension'),
                                              span(class='','
                                                   Hypertension survey data from england has untreated hypertension as a response that we treat as under treatment')
                                              ),
                                              
                                              graph_wrapper(af_undiagnosed_deprivation_chart ,
                                                            header=span(span(class='fs-5','Prevalence of Undiagnosed Atrial Fibrillation') ,
                                                                        span(class='ms-2  badge rounded-pill text-bg-secondary','England Survey'),
                                                                           span(class='ms-2  badge rounded-pill text-bg-dark',''),
                                                                           span(class='ms-2 p-1 text-bg-light','NI  GP recorded'))),
                                              
                                          div(class="bg-white text-dark p-3 rounded-3 fs-5 my-5 opacity-100 shadow-sm",
                                              h5('Atrial Fibrillation'),
                                              span(class='','AF underdiagnosis is extrapolated true prevalence data against recorded Prevalence data.')),
                                          
                                          graph_wrapper(ckd_undiagnosed_deprivation_chart ,
                                                        header=span(span(class='fs-5','Prevalence of Undiagnosed Chronic Kidney Disease ') ,
                                                                    span(class='ms-2  badge rounded-pill text-bg-secondary','England Survey'),
                                                                    span(class='ms-2  badge rounded-pill text-bg-dark',''),
                                                                    span(class='ms-2 p-1 text-bg-light','NI  GP recorded'))),
                                          
                                          div(class="bg-white text-dark p-3 rounded-3 fs-5 my-5 opacity-100 shadow-sm",
                                              h5('Chronic Kidney Disease'),
                                              span(class='','CKD underdiagnosis is extrapolated true prevalence data against recorded Prevalence data.')),
                                          
                                          
                                          ),
                                          
                                          div(class='p-3 m-3 rounded-3 text-bg-info',
                                              'The above analysis was made possible by applying English Survey ', tags$cite("Rates"), 'against Northern Irelands populations',
                                              ),
                                          div(class='mx-2 fs-6 badge rounded-pill bg-primary', 'Health Survey Data ',icon(class = 'ms-3',"arrow-up-right-from-square"))
                                          
                                      ), # End evidence section # End incidence section
                                      
                                      
                                      
                                      # Prevalence Section
                                      
                                      div(id = "prevalence-section",
                                          class='min-vh-100 overflow-hidden',
                                          
                                          div(class="text-white bg-dark fs-5 p-3 rounded-2 my-5 opacity-100 shadow-sm", 
                                              h5(' Master Table' ), 
                                              span(class='','The Built Data Model from which results are derived')),
                                          
                                          div(style = "min-height: 400px;",class = 'd-flex flex-row py-5 justify-content-evenly gap-5 flex-wrap',
                                              
                                          # div( class = 'overflow-scroll w-50',
                                          # div(class= 'overflow-scroll w-75 text-bg-black ',
                                          #reactable(class = 'text-bg-default',
                                          
                                          reactable(static = F , width = 700,
                                                    class = 'text-bg-default overflow-hidden p-2 rounded-2',
                                                    master_data_table)                                          
                                          # )
                                          # ),
                                          
                                          # div(class=" bg-white text-dark p-4 my-5 opacity-100 shadow-sm",
                                          #     h5('ADHD Estimate in the population'),
                                          #     span(class='','Comparison of approaches for prevalence and incidence')),
                                          # 
                                          # div(class='d-flex flex-row py-5 justify-content-evenly gap-5 flex-wrap',#grid
                                          #     
                                          #     graph_wrapper(af_undiagnosed_deprivation_chart ,header=span(span(class='fs-5','ADHD Prevalence') ,
                                          #                                  span(class='m-1  badge rounded-pill text-bg-secondary','Rates'), div(class='text-muted','Estimated Actual'))),
                                          #     
                                          #     graph_wrapper(x ,header=span(span(class='fs-5','Prevalence by age') ,span(class='ms-2  badge rounded-pill text-bg-secondary','Counts'),
                                          #                                  span(class='ms-2 p-1 text-bg-secondary','Estimated Actual'))),
                                          #     
                                          #     graph_wrapper(x ,header=span(span(class='fs-5','Prevalence') ,span(class='ms-2  badge rounded-pill text-bg-secondary','Counts'),
                                          #                                  span(class='ms-2  badge rounded-pill text-bg-dark','Simulation'),
                                          #                                  span(class='ms-2 p-1 text-bg-dark','Estimated GP recorded'))),
                                          #     
                                          #     graph_wrapper(x ,header=span(span(class='fs-5','Prevalence by HSCT') ,span(class='ms-2  badge rounded-pill text-bg-secondary','Counts'),
                                          #                                  span(class='ms-2  badge rounded-pill text-bg-dark','Simulation'),
                                          #                                  span(class='ms-2 p-1 text-bg-dark','Estimated GP recorded'))),
                                          #     
                                          # ),
                                          )
                                      ), # End prevalence section
                                      
                                      # Evidence Section (placeholder)
                                      
                                      # Incidence Section
                                              #####################
                                      
                                          )
                                      ),
                                      
                                      div(id = "incidence-section",class='min-vh-100',
                                          div(class="text-white bg-dark fs-5 p-3 rounded-2 my-5 opacity-100 shadow-sm", h5('Enhanced Interrogation' ), span(class='','Build reports from the joint Distribution of variables')),
                                          div(class='d-flex flex-row gap-5 py-5 justify-content-evenly min-vh-100 flex-wrap',#grid

                                              #####################
                                              div(class = "container-fluid rounded-5", id = 'pivot-section', style = "width:100%;padding:3%;background:lightblue;",
                                                  h3("Population Health Data - Interactive Pivot Analysis"),
                                                  p(class='lead',"Drag columns to create custom analysis. Use dimensions for grouping, measures for aggregation."),
                                                  div(style = "font-size: 0.6rem !important;",
                                                      # Pivot module UI
                                                      pivot_module_ui("pivot_reports",
                                                                      data_names = c("sex", "age_risk", "county", "hsct", "bmi",
                                                                                     "Urban_status", "mdm_quintile_soa_name", "ethnicity",
                                                                                     "stroke", "chd", "diabetes", "dementia", "heart_failure"),
                                                                      fun_names = c("sum","mean","median","min","max","count","n_distinct")),
                                                      
                                                  )
                                              )
                                      # Downstream Section
                                      # div(id = "downstream-section",class='min-vh-100',
                                      #     div(class="text-white bg-primary fs-5 p-3 rounded-2 my-5 opacity-100 shadow-sm", h5('Referrals'), span(class='','Adults, Children and Trusts')),
                                      #     div(class='d-flex flex-row gap-5 py-5 justify-content-evenly min-vh-100  flex-wrap',#grid
                                      #
                                      #         graph_wrapper(adult_referrals_e ,header=span('Adult Referrals' ,span(class='ms-2 text-bg-secondary badge rounded-pill','Counts'),span(class='ms-2 p-1 text-muted','Estimated Actual')),
                                      #         ),
                                      #         graph_wrapper(accepted_adult_referrals_e ,header=span('Accepted Adult Referrals' ,span(class='ms-2  badge rounded-pill text-bg-secondary','Counts'),span(class='ms-2 p-1 text-muted','Cumulative')),
                                      #         ),
                                      #
                                      #         graph_wrapper(header=span('Adult Referals by HSCT' ,span(class='ms-2  badge rounded-pill text-bg-secondary','Counts'),span(class='ms-2 p-1 text-muted','Cumulative')),
                                      #                       div(style = 'font-size: 11px;', reactable(select(adult_referrals_tbl,Year = year, HSCT, Count = n) ))),
                                      #
                                      #         graph_wrapper(child_referrals_e ,header=span('Child Referrals' ,span(class='ms-2  badge rounded-pill text-bg-secondary','Counts'),span(class='ms-2 p-1 text-muted','Cumulative')),
                                      #                       div(class='p-3 rounded-2 text-bg-secondary','0.1% of prevalence per year assumed referral rate applied to estimated actual prevalence')),
                                      #         graph_wrapper(accepted_child_referrals_e ,header=span('Accepted Child Referrals' ,span(class='ms-2  badge rounded-pill text-bg-secondary','Counts'),span(class='ms-2 p-1 text-muted','Cumulative')),
                                      #                       div(class='p-3 rounded-2 text-bg-secondary','70% acceptance rate per referral applied')
                                      #
                                      #
                                      #         ) # End prevalence section
                                      #     ),
                                      #     div(class='ms-2 fs-6  badge rounded-pill bg-primary', '0.1% of prevalence was choosen for the total referrals ',icon("warning")),
                                      #     div(class='ms-2 fs-6  badge rounded-pill bg-primary', '70% of referrals were assumed to be accepted  ',icon("warning"))
                                      # ),
                                      
                                      # div(id = "Links",class='min-vh-100 pt-5 mt-5',
                                      #     div(class="text-dark bg-white fs-5 p-3 rounded-2 my-5 opacity-100 shadow-sm", h5('Links'), span(class='','')),
                                      #     div(class='justify-content-evenly min-vh-50 flex-wrap',#grid
                                      #         # d-flex flex-row gap-2 py-5
                                      #
                                      #         div(class = "grid-item nav-card float-left analytics bg-opacity-50",
                                      #             div(onclick = "window.open('https://cks.nice.org.uk/topics/attention-deficit-hyperactivity-disorder/background-information/prevalence','_blank')",
                                      #                 class = "nav-card-icon",
                                      #                 icon("arrow-up-right-from-square")),
                                      #             div(class = "nav-card-title", "NICE"),
                                      #             div(class = "nav-card-description text-wrap ", "NICE estimates,
                                      #                    used by ADHD UK and NHS England for ADHD real prevalence estiamtes"),
                                      #         ),
                                      #         div(class = "grid-item nav-card float-left settings bg-opacity-50",
                                      #             div(onclick = "window.open('https://digital.nhs.uk/data-and-information/publications/statistical/adult-psychiatric-morbidity-survey/survey-of-mental-health-and-wellbeing-england-2023-24/attention-deficit-hyperactivity-disorder','_blank')",
                                      #                 class = "nav-card-icon",
                                      #                 icon("arrow-up-right-from-square")),
                                      #             div(class = "nav-card-title", "NHS England"),
                                      #             div(class = "nav-card-description", "Experimental Mental Health Statistics"),
                                      #         ),
                                      #
                                      #         div(class = "grid-item nav-card float-left reports bg-opacity-50",
                                      #             div(onclick = "window.open('https://pubmed.ncbi.nlm.nih.gov/37455585','_blank')",
                                      #                 class = "nav-card-icon",
                                      #                 icon("arrow-up-right-from-square")),
                                      #             div(class = "nav-card-title", "Douglas G J McKechnie"),
                                      #             div(class = "nav-card-description", "Attention-deficit hyperactivity disorder diagnoses and prescriptions in UK primary care, 2000-2018: population-based cohort study"),
                                      #         ),
                                      #
                                      #         div(class = "grid-item nav-card float-left analytics bg-opacity-50",
                                      #             div(onclick = "window.open('https://adhduk.org/adhd-statistics/','_blank')",
                                      #                 class = "nav-card-icon",
                                      #                 icon("arrow-up-right-from-square")),
                                      #             div(class = "nav-card-title", "ADHD UK"),
                                      #             div(class = "nav-card-description text-wrap ", "Rudimentary estimates of devolved nations, adult and children adhd prevalence"),
                                      #
                                      #         ),
                                      #
                                      #         div(class = "grid-item nav-card float-left settings bg-opacity-50",
                                      #             div(onclick = "window.open('https://digital.nhs.uk/data-and-information/publications/statistical/mi-adhd/may-2025','_blank')",
                                      #                 class = "nav-card-icon",
                                      #                 icon("arrow-up-right-from-square")),
                                      #             div(class = "nav-card-title", "NHS England"),
                                      #             div(class = "nav-card-description", "Experimental Mental Health Statistics"),
                                      #         )
                                      #     ))
                                  
                                  ################################
                                
                                  ########################################
                                  )
                                  
                                  
                              ) # End of dashboard grid
                              #, # End of dashboard tab content
                          ),
                 
                          
                          # Analytics Tab Content
                          # div(id = "analytics-tab", class = "tab-pane active show ", #style = "width:100%",
                          #     div(style = 'position:absolute;top:0;left:0;margin-inline:-12px;',#class = "container-fluid",
                          #         div(
                          #             #leafletOutput("mymap", width = '100vw', height = '100vh'),
                          #         ),
                          #     ),
                          #
                          #     div(class = 'glass-card p-3 me-3',
                          #         style = 'position:absolute;top:12vh;right:0;height:75vh; overflow:scroll; background: rgba(255, 255, 255, 0.64)!important; /* 0.14 */',#class = "container-fluid",
                          #         # style='top:15vh;width:15vw;z-index:1000',class = ' ms-3 p-4 d-flex flex-column display-absolute position-fixed left-0 shadow-sm glass-card'
                          #         div(class = 'd-flex flex-column gap-3 justify-content-between',
                          #             div( span(class = '','BMI'),
                          #                  div(style = 'height:150px;width:200px;')
                          #             ),
                          #             div( span(class = '','Age'),
                          #                  div(style = 'height:150px;width:200px;')
                          #             ),
                          #             div(span(class = '','Sex'),
                          #                 div(style = 'height:150px;width:200px;')
                          #             ),
                          #             div(span(class = '','Deprivation'),
                          #                 div(style = 'height:150px;width:200px;')
                          #             ),
                          #             div(span(class = '','Risk'),
                          #                 div(style = 'height:250px;width:200px;')
                          #             ),
                          #             div(span(class = '','Risk'),
                          #                 div(style = 'height:200px;width:200px;')
                          #             ),
                          #
                          #             span( class='ms-4',
                          #                   h6('Population:',textOutput('headline_count'))
                          #             ),
                          #             span( class='ms-4',
                          #                   h6('Qrisk Score:',textOutput('qrisk_average'))
                          #             )
                          #         )
                          #     )
                          #
                          # ),
                          
                          # Reports Tab Content
                          # div(id = "reports-tab", class = " tab-pane active show",# style = "display: none;",
                    
                          # ),
                          
                          # Orders Tab Content
                          # div(id = "ObesityRisk-tab", class = "  tab-pane show active ", style = "",
                          #     div( style = "padding-left: 17%;",
                          #          h3("Obesity Contributions to Obesity"),
                          #          p(class = 'lead', "Factors leading to the cause of Obesity and often accompany it ")
                          # ),
                          
                          # div(id = "geography-tab", class = "tab-pane show active", style = " ",
                          #     div(class = "container-fluid",   style = "padding-left: 14%", #;width:100%
                          #         h3("Geography Analytics"),
                          #         p(class = 'lead',"A supplement to the <a onclick= alert('hello')>analytics</a> showing the heirarchical ")
                          # )
                          # ),
                          
                          
                          # div(id = "population-tab", class = "tab-pane show active ", style = " ",
                          #     div(class = "container-fluid",   style = "padding-left: 17%;",
                          #         h3(" Obesity Analytics"),
                          #
                          #         p(class = 'lead', "Analysis of Obesity dynamic in the Population"),
                          #
                          #     )
                          # ),
                          
                          
                          # div(id = "lifestyle-tab", class = "  tab-pane show active ", style = " ",
                          #     div(style = "padding-left: 17%;",
                          #         h3("Lifestyle Dashboard"),
                          #         p(class = 'lead',"Deprivation content will be displayed here when the Users nav item is clicked."),
                          #         h6('Health Burden Attributable to obesity'),
                          #
                          #
                          #     )
                          # ),
                          
                          # Users Tab Content
                          # div(id = "deprivation-tab", class = "  tab-pane active show", style = " ",
                          #     div(class = "container-fluid",   style = "padding-left: 15%;",
                          #
                          # ),
                          
                          # div(id = "society-tab", class = "tab-pane show active", style = " ",
                          #     div(class = "container-fluid",   style = "padding-left: 17%;",
                          #
                          #     )
                          # ),
                          
                      ) # End of tab-content-container
                      
                  ), # Close content-area
                  
                  # Initialize Packery with Click and Expand JavaScript
                  tags$script(HTML("
$(document).ready(function() {
  // Initialize Packery when the page loads
  setTimeout(function() {
    var $grid = $('.grid').packery({
      itemSelector: '.grid-item',
      columnWidth: 80,
      gutter:10
    });

    // Handle click events on grid items
    $grid.on('doubleclick', '.grid-item-content', function(event) {
      event.preventDefault();
      event.stopPropagation();

      var itemElem = event.currentTarget.parentNode;
      var $item = $(itemElem);
      var isExpanded = $item.hasClass('is-expanded');

      // Toggle the expanded class
      $item.toggleClass('is-expanded');

      // Force a layout update after the CSS transition
      setTimeout(function() {
        if (isExpanded) {
          // If contracting, use shiftLayout to compact everything
          $grid.packery();
        } else {
          // If expanding, first layout normally, then fit the expanded item
          $grid.packery('shiftLayout');
          setTimeout(function() {
            $grid.packery('shiftLayout', itemElem);
          }, 50);
        }
      }, 50); // Small delay to let CSS transition start

      // Also trigger layout after CSS transition completes
      setTimeout(function() {
        $grid.packery();
      }, 150); // Match CSS transition duration (0.4s + buffer)
    });

    $('.chart-card').on('click', function() {
      console.log('resize');
      // Trigger ECharts resize
      setTimeout(function() {
        $('.echarts4r').each(function() {

          //if (this.echartsInstance) {
          console.log('resizeIn');
          console.log(this);
          echarts.getInstanceByDom(this).resize();
            //this.echartsInstance.resize();
          //}
        });
      }, 300);
    });
    
      // Run on page/window resize
$(window).on('resize', function () {
  $('.echarts4r').each(function () {
    const inst = echarts.getInstanceByDom(this);
    if (inst) {
      inst.resize();
    }
  });
});


    // Tab Navigation Functionality
    $('.nav-item').on('click', function() {
      // Remove active class from all nav items
      $('.nav-item').removeClass('active');
      // Add active class to clicked item
      $(this).addClass('active');

      // Hide all tab content
      $('.tab-pane').removeClass('active show')

      // Show corresponding tab content based on nav item text
      var navText = $(this).text().trim().toLowerCase();
      var tabId = '';

      switch(navText) {
        case 'dashboard':
          tabId = 'dashboard-tab';
          break;
        case 'analytics':
          tabId = 'analytics-tab';
          break;
        case 'reports':
          tabId = 'reports-tab';
          break;
        case 'geography':
          tabId = 'geography-tab';
          break;
        case 'deprivation':
          tabId = 'deprivation-tab';
          break;
        case 'population':
          tabId = 'population-tab';
          break;
        case 'intervention':
          tabId = 'intervention-tab';
          break;
        case 'specify':
          tabId = 'specify-tab';
          break;
        case 'scenarios':
          tabId = 'scenarios-tab';
          break;
        case 'obesity risk':
          tabId = 'ObesityRisk-tab';
          break;
        case 'northern trust':
          tabId = 'NorthernTrust-tab';
          break;
        case 'lifestyle':
          tabId = 'lifestyle-tab';
          break;
         case 'society':
          tabId = 'society-tab';
          break;
        default:
          tabId = 'dashboard-tab';
      }

      $('#' + tabId).addClass('active show')

      // Re-initialize packery if dashboard tab is shown
      if (tabId === 'dashboard-tab') {
        setTimeout(function() {
          $('.grid').packery();
        }, 100);
      }

      // Initialize pivot module if reports tab is shown
      if (tabId === 'reports-tab') {
        setTimeout(function() {
          // Trigger pivot module column update
          if (window.Shiny && window.Shiny.setInputValue) {
            window.Shiny.setInputValue('pivot_reports_tab_shown', Math.random());
          }

          // Re-trigger any drag-drop initialization if needed
          if (typeof window.setupDropzone === 'function') {
            console.log('Re-initializing pivot drag-drop zones');
            window.setupDropzone('#column-pool');
            window.setupDropzoneCat('#groups-drop');
            window.setupDropzoneCat('#wide-by-drop');
            window.setupDropzone('#values-drop');
            window.setupDropzoneValueFunc('#value-func-drop');
          }
        }, 200);
      }
    });


    // setTimeout(function() {
    //  $('.tab-pane').removeClass('active show')
    //  $('#' + 'specify-tab').addClass('active show')
    // }, 1000);

      setTimeout(function() {
          $('.grid').packery();
        }, 100);

  }, 500); // Small delay to ensure DOM is ready

  // Initialize Bootstrap Scrollspy
  var scrollspyEl = document.querySelector('[data-bs-spy=\"scroll\"]');
  if (scrollspyEl) {
    var scrollspy = new bootstrap.ScrollSpy(scrollspyEl, {
      target: '#top-nav-content',
      offset: 80
    });
  }

});
"
                                   
                  ), # End tags$script
                  echarts4rOutput("dummy", height = "0px")
                  ), # End main-container
                  
                  # footer()
)

# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {
   
   
   x <- pivot_module_server('pivot_reports', data = shiny::reactive(as.data.frame(master_data)))
   
   
   output$dummy <- renderEcharts4r({
      e_charts(mtcars ,cyl) %>%
         e_line(mpg) %>%
         e_theme('roma')
   }
   )
   
   output$prevalence_rates_graph <- renderPlot(prevalence_rates_graph)
   output$prevalent_counts_graph <- renderPlot(prevalent_counts_graph)
   output$prevalence_my_sim_graph <- renderPlot(prevalence_my_sim_graph)
   output$yearly_incidence_graph <- renderPlot(yearly_incidence_graph)
   output$cumulative_incidence_graph <- renderPlot(cumulative_incidence_graph)
   output$new_adult_transitions <- renderPlot(new_adult_transitions)
   output$new_adult_transitions <- renderPlot(new_adult_transitions)
   output$adult_referals <- renderPlot(adult_referals)
   output$accepted_adult_referals <- renderPlot(accepted_adult_referals)
   output$child_referals <- renderPlot(child_referals)
   output$accepted_child_referals <- renderPlot(accepted_child_referals)
   
   
}


shinyApp(ui = ui, server = server)


