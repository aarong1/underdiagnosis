library(tidyverse)
library(data.table)

load('./modules/chart_update_module/prep_interactive_model_run.RData')
pop <- read.fst('./populations/k5_population.fst')
initial_time_zero_population$year <- 2022
#test_population <- initial_time_zero_population
# test_specification <- model_specification


run_model <- function() {
  
  past_populations <- data.frame()#initial_time_zero_population)
  
  for(run in 1:(test_specification$model$number_of_runs)) {
    
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
    
    for (time in 1:test_specification$model$duration){
      
      cat(paste('###################################### \n Time, t : ', time, '\n Run, r:', run,'\n###################################### \n'))
      
      print('Adding the current population to the past populations data structure')
      #current_population <- current_population |> select(-bern_trial)
      past_populations <- rbind(past_populations, current_population)
      
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
      #uses data.table - not converts back to data frame in function
      
      current_population_who_died <- current_population |> 
        filter( !is.na(death) & !is.null(death) & !death==0 )
      
      dead_population <- rbind(dead_population, current_population_who_died)
      
      current_population_alive <- current_population |> 
        filter(is.na(death)| is.null(death)| death==0)
      
      current_population <- current_population_alive
      
      
      if (FALSE){  
        print('Shouldnt enter')
        
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
        
        dummy(series_list)
        
      }
      
    }
    
    
  }
  return(past_populations)

}
