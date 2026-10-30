library(shiny)
library(echarts4r)
library(jsonlite)
library(dplyr)
library(purrr)
#' 
# load('prep_interactive_model_run.RData')
#' 
#' # save(file = 'run_requirements.RData',
#' #      initial_time_zero_population,
#' #      prevalence_hsct,
#' #      prevalence_hsct_new,
#' #      test_specification,
#' #      trusts,
#' #      morbidities,
#' #      apply_qmortality_mortality,
#' #      apply_age_sex_death,
#' #      lifetables,
#' #      qmortality_risk,
#' #      qmortality_female_risk,
#' #      qmortality_male_risk,dead_population,
#' #      apply_chd_risk,
#' #      transform_10y_probability_to_1y,
#' #      transform_probability_to_1y)
#' #
#' # rm(list=ls())
#' #
#' # load(file = 'run_requirements.RData')
#' 
#' #' initial_time_zero_population <- read.csv('initial_time_zero_population.csv')
#' source('main_utils_2_4.R')

source('./modules/chart_update_module/prep_interactive_model_run.R')

# load("prep_interactive_model_run.RData")

# Module UI

chartUpdateModuleUI <- function(id) {
  ns <- NS(id)
  echart_id <- (ns('target_echart')) #demo_chart-target_echart
  tagList(
    # Include custom JavaScript
    tags$head(tags$script(HTML( paste0("
    
    setTimeout(function() {
    console.log('running add message handler');
    Shiny.addCustomMessageHandler('updateChart', function(seriesList) {
        console.log('inmessage handler');

        console.log(seriesList);
  const chartEl = document.querySelector('#",echart_id," > .echarts4r'); 
  if (!chartEl) {
    console.warn('Chart container not found');
    return;
  }
  
  //chart.resize();

  const chart = echarts.getInstanceByDom(chartEl);
  if (!chart) {
    console.warn('ECharts instance not ready');
    return;
  }

  const legendData = seriesList.map(s => s.name);
  const seriesConfig = seriesList.map(s => ({
    name: s.name,
    type: 'line',
    data: s.data
  }));

  chart.setOption({
    xAxis: { type: 'time', scale: true, max: '2030-01-01' },
    yAxis: { type: 'value', scale: true },
    tooltip: { trigger: 'axis' },
    legend: { data: legendData },
    series: seriesConfig
  });
});
    }, 500);")))
    ),
    
    # Chart container
  #  div(class = "card",
     #   div(class = "card-header",
   #         h4("Real-time Simulation Results")
    #    ),
        div(class = "d-flex align-items-center justify-content-center",
            # actionButton(ns("start_simulation"), "Start Simulation", 
            #              class = "btn btn-primary", icon = icon("play")),
            div(id = ns('target_echart'), 
                class = 'shadow-sm rounded-5 d-flex align-items-center justify-content-center',
                style = "height: 80%;width:60%;",
                
                # Initial empty chart
                #e_charts()
                data.frame(year = 2026, incidence = 100000) |>
                  mutate(year = as.character(year)) |>
                  e_charts(year,width = 500) |> #,width = '100rem'
                  e_theme('walden') |> 
                  e_axis_labels(x='Year',y='Incidence') |> 
                e_line(incidence, name = "Waiting for Input")
        #    )
        )
    ),
    
    # Status display
    # div(class = "mt-3",
    #     verbatimTextOutput(ns("simulation_status"))
    # )
  
  )
}

# Module Server
chartUpdateModuleServer <- function(id, runButton = reactive(run)) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for simulation state
    simulation_active <- reactiveVal(FALSE)
    simulation_results <- reactiveVal(data.frame())
    
    # Status output
    # output$simulation_status <- renderText({
    #   if (simulation_active()) {
    #     "Simulation is running..."
    #   } else {
    #     "Simulation stopped."
    #   }
    # })
    
    # Start simulation
    observeEvent(runButton(), { #input$start_simulation, {
      print('start')
      simulation_active(TRUE)
      simulation_results(data.frame())
      
      # Run simulation in background using reactiveTimer or observe
      run_simulation_async()
    })
    
    # Stop simulation
    observeEvent(input$stop_simulation, {
      simulation_active(FALSE)
    })
    
    # Async simulation function
    run_simulation_async <- function() {
      
      # Initialize simulation parameters
      num_runs <- 10 #input$num_runs
      duration <- 10 #input$duration
      
      past_populations <- data.frame() #initial_time_zero_population)
      
      for(run in 1:(num_runs)) {
        
        cat(paste('################################### \n run : ', run, ' \n###################################### \n'))
        
        #reinstate the same initial time zero population for each new run some they are comparable
        #current_population <- initial_time_zero_population |> 
        #  mutate(run = {{run}})
        
        population_w_established_prevalence <- reduce2(
          .x = rep(trusts,length(morbidities)),
          .y = rep(morbidities,each = length(trusts)),
          .init = initial_time_zero_population,
          .f = function(pop, trust, morbidity) {
            
            assign_year_minus_one_prevalence(
              input_population = pop,
              trust = trust,
              morbidity = morbidity,
              #year = 2017,
              prevalence_df = prevalence_hsct_new,
              configuration = test_specification
            )
          }
        )
        
        current_population <- population_w_established_prevalence |> 
          mutate(run = {{run}})
        
        current_population <- current_population |> mutate(bern_trial = runif(n()))
        
        for (time in 1:duration){
          
          cat(paste('###################################### \n Time, t : ', time, '\n Run, r:', run,'\n###################################### \n'))
          
          print('Adding the current population to the past populations data structure')
          #current_population <- current_population |> select(-bern_trial)
          past_populations <- bind_rows(past_populations, current_population)
          
          current_population <- current_population |>
            mutate(age = age + 1) |> 
            mutate(
              age20 = cut(age,include.lowest = T,
                          breaks = seq(0,120,20),
                          labels = c('0-20',
                                     '20-40',
                                     '40-60',
                                     '60-80',
                                     '80-100',
                                     '100-120')
              )
            )
          
          current_population <- current_population |>
            mutate(year = year + 1)
          
          print('Apply and Partition deaths')
          
          current_population <- current_population %>% 
            apply_age_sex_death(apply_death = F) |> 
            apply_qmortality_mortality(apply_death = T)
          #uses data.table - now converts back to data frame in function
          
          current_population_who_died <- current_population |> 
            filter( !is.na(death) & !is.null(death) & !death==0 )
          
          dead_population <- bind_rows(dead_population, current_population_who_died)
          
          current_population_alive <- current_population |> 
            filter(is.na(death)| is.null(death)| death==0)
          
          current_population <- current_population_alive
          
          if (FALSE){  
            #print('Shouldnt enter')
            
          }else{ # baseline always runs
            
            print('entered non intervention loop')
            
            current_population <- current_population |> 
              calculate_risk_of_morbidity()
            
            print('Applying absolute morbidity onset')
            print(paste('population df run',max(current_population$run)))
            
            current_population <- current_population |> 
              declare_absolute_incident_morbidity(morbidity = "stroke") |> 
              declare_absolute_incident_morbidity(morbidity = "chd") |> 
              declare_absolute_incident_morbidity(morbidity = "diabetes") |> 
              declare_absolute_incident_morbidity(morbidity = "dementia") |> 
              declare_absolute_incident_morbidity(morbidity = "heart_failure") |> 
              declare_absolute_incident_morbidity(morbidity = "atrial_fibrillation") |> 
              declare_absolute_incident_morbidity(morbidity = "hypertension") |> 
              declare_absolute_incident_morbidity(morbidity = "chronic_kidney_disease") |> 
              declare_absolute_incident_morbidity(morbidity = "lung_cancer") 
            
            print('Calculating the incidence of stroke')
            df <- count(past_populations, stroke  = (stroke!=0), run, year) |>
              filter(stroke ==TRUE) |> 
              mutate(n = n * test_specification$population$scale_down_factor)
            print(df)
            
            df_json <- jsonlite::toJSON(
              unname(as.list(df[c('year','n')])),  # prevent named keys like "year", "incidence"
              dataframe = "columns",
              auto_unbox = TRUE
            )

            print(df_json)
            
            print('Calculating the json')
            
            series_list <- df %>%
              group_by(run) %>%
              summarise(data = list(map2(as.character(year), n, ~ list(.x, .y)))) %>%
              mutate(name = paste0("run ", run)) %>%
              transmute(name, data) %>%
              jsonlite::toJSON(auto_unbox = TRUE)
            
            print(series_list)
            print('calculating the series list')
            
            session$sendCustomMessage("updateChart", series_list)
            
          }
        }
      }
    }

    
    # Return reactive for external use
    return(list(
      past_populations = reactive(past_populations),
      active = reactive(simulation_active()),
      results = reactive(simulation_results())
    ))
  })
}

# shinyApp(
#   ui = fluidPage(
#     chartUpdateModuleUI("chart1"),
#     actionButton("run", "Run Simulation")
#   ),
#   server = function(input, output, session) {
#     chartUpdateModuleServer("chart1", runButton = reactive(input$run))
#   }
# )

