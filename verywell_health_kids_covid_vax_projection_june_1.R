library(httr)
library(dplyr)
library(readr)
library(rlang)
library(purrr)

## Function to update datawrapper charts
republish_chart <- function(API_KEY, chart_id, data, subtitle = NULL, title = NULL, notes) {
  
  data_refresh <- PUT(url = paste0("https://api.datawrapper.de/v3/charts/", 
                                   chart_id, "/data"),
                      add_headers(authorization = paste("Bearer", 
                                                        API_KEY, 
                                                        sep = " ")),
                      body = format_csv(data))
  
  call_back <- list(metadata = list())
  
  if (!is.null(title)) {
    call_back$title <- title
  }
  
  if (!is.null(subtitle)) {
    call_back$metadata$describe$intro <- subtitle   
  }
  
  call_back$metadata$annotate$notes <- notes
  
  notes_res <- PATCH(url = paste0("https://api.datawrapper.de/v3/charts/", 
                                  chart_id),
                     add_headers(authorization = paste("Bearer", API_KEY, 
                                                       sep = " ")),
                     body = call_back,
                     encode = "json")
  
  publish_res <- POST(
    url = paste0("https://api.datawrapper.de/v3/charts/", 
                 chart_id, "/publish"),
    add_headers(authorization = paste("Bearer", 
                                      API_KEY, 
                                      sep = " "))
  )
  
  list(data_refresh, notes_res, publish_res) -> res_list
  
  if (any(map_lgl(res_list, http_error))) {
    which(map_lgl(res_list, http_error))[1] -> error_idx
    
    stop_for_status(res_list[[error_idx]], task = paste0("update step ",
                                                         error_idx, 
                                                         " of chart ", 
                                                         chart_id))
    
  } else {
    message(paste0("Chart ", chart_id, " updated successfully"))
  }
  
}

# Importing datawrapper api key and socrata password environment secrets
DW_API <- Sys.getenv("DW_API_KEY")
SCT_PW <- Sys.getenv("SCT_PW")

# Creating reference dataframe from base R state name and abbreviation vectors
state_ref <- tibble(
  full_name = c(state.name, "District of Columbia", 
                "Puerto Rico", "United States"),
  abbv = c(state.abb, "DC", "PR", "US")
)

# Importing population of various age groups by state
## TODO: Update this with new ACS 2020 5-year data
state_pops <- read_csv("./data/state_pops.csv", col_names = T, col_types = "ciiiii")

# Getting CDC vaccine data by state from the data.cdc.gov site via the
# socrata api: 
# https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-Jurisdi/unsk-b7fc

cdc_fetch <- tryCatch(
  {
    res <- GET(url = "https://data.cdc.gov/resource/unsk-b7fc.csv?$limit=2000",
               authenticate(user = "anesta@dotdash.com", 
                            password = SCT_PW))
    
    stop_for_status(res)
    
    content(res, encoding = "UTF-8", type = "text/csv",
            col_types = cols(.default = col_character())) %>% 
      mutate(date = base::as.Date(date)) -> covid_vax_by_state
    
  }, 
  error = function(cond) {
    e_full <- error_cnd(class = "cdc_fetch", 
                        message = paste(
                          "An error occured with fetching the CDC Data:", 
                          cond, 
                          "on", Sys.Date(), "\n"))
    
    message(e_full[["message"]])
    
    return(e_full)
    
  }
  
)

Sys.sleep(3)

if (class(cdc_fetch)[2] != "rlang_error") {
 
  # Creating Datawrapper subtitle note
  dw_subtitle <- paste0("COVID-19 vaccination data from the CDC as of ", 
                        format(max(covid_vax_by_state$date), "%m/%d/%Y"),
                        ". Idaho excluded due to lack of age breakout in data.")
  
  covid_vax_by_state %>% 
    select(date, location, series_complete_yes, series_complete_12plus, series_complete_18plus, series_complete_pop_pct) %>% 
    mutate(across(starts_with("series_complete"), as.numeric)) %>% 
    mutate(series_complete_12_17 = series_complete_12plus - series_complete_18plus,
           series_complete_5_11 = series_complete_yes - series_complete_12plus) %>% 
    inner_join(state_ref, by = c("location" = "abbv")) %>% 
    inner_join(state_pops, by = c("full_name" = "state")) -> covid_w_kids
  
  
  # Getting the growth rate of the last 4 weeks of 5 to 11 yr olds
  vax_delta_5_11_last_four <- covid_w_kids %>% 
    filter(date %in% c(max(date), max(date) - 28), location != "ID") %>% 
    arrange(location, desc(date)) %>% 
    mutate(vax_delta_5_11 = series_complete_5_11 - lead(series_complete_5_11, n = 1)) %>% 
    filter(date == max(date)) %>% 
    mutate(growth_rate_5_11_last_four = vax_delta_5_11 / pop_5_11) %>% 
    select(location, growth_rate_5_11_last_four, vax_delta_5_11)
  
  
  # Getting the growth rate of the last 4 weeks of 12 to 17 yr olds
  vax_delta_12_17_last_four <- covid_w_kids %>% 
    filter(date %in% c(max(date), max(date) - 28), location != "ID") %>% 
    arrange(location, desc(date)) %>% 
    mutate(vax_delta_12_17 = series_complete_12_17 - lead(series_complete_12_17, n = 1)) %>% 
    filter(date == max(date)) %>% 
    mutate(growth_rate_12_17_last_four = vax_delta_12_17 / pop_12_17) %>% 
    select(location, growth_rate_12_17_last_four, vax_delta_12_17)
  
  # Getting the growth rate of 18+ in last 4 weeks  
  vax_delta_18_plus_last_four <- covid_w_kids %>% 
    filter(date %in% c(max(date), max(date) - 28), location != "ID") %>% 
    arrange(location, desc(date)) %>% 
    mutate(vax_delta_18plus = series_complete_18plus - lead(series_complete_18plus, n = 1)) %>% 
    filter(date == max(date)) %>% 
    mutate(growth_rate_18_plus_last_four = vax_delta_18plus / pop_18_plus) %>% 
    select(location, growth_rate_18_plus_last_four, vax_delta_18plus)
  
  # Getting the latest days' kids vaccination data
  covid_w_kids_latest <- covid_w_kids %>% 
    filter(date == max(date))
  
  # The "jun1_last_4_weeks_rate" variables are created by taking the daily number of new vaccinations
  # (by dividing the delta from the number for the latest day and the number 28 days ago by 28) 
  # for each group, multiplying that number by the number of days between the date of the latest data
  # and June 1st,and then adding it to the latest total raw number of vaccinations for that group
  
  # proj_total_pop_vax_jun1 is adding up all the raw projected vax totals by age (5-11, 12-17, 18+)
  # and dividing that number by the total population
  
  # vax_pct_5_11_jun1_w_last_four_week_rate & vax_pct_12_17_jun1_w_last_four_week_rate are
  # just taking the projected raw number of said age group vaccinated on jun 1 and dividing by that
  # age group population
  
  covid_w_kids_latest %>% 
    inner_join(vax_delta_5_11_last_four, by = "location") %>% 
    inner_join(vax_delta_12_17_last_four, by = "location") %>% 
    inner_join(vax_delta_18_plus_last_four, by = "location") %>% 
    mutate(num_5_11_jun1_last_4_weeks_rate = round(
      ((vax_delta_5_11 / 28) * (as.integer(as.Date("2022-06-01")) - as.integer(max(date))))) + series_complete_5_11,
      num_12_17_jun1_last_4_weeks_rate = round(
        ((vax_delta_12_17 / 28) * (as.integer(as.Date("2022-06-01")) - as.integer(max(date))))) + series_complete_12_17,
      num_18_plus_jun1_last_4_weeks_rate = round(
        ((vax_delta_18plus / 28) * (as.integer(as.Date("2022-06-01")) - as.integer(max(date))))) + series_complete_18plus,
      proj_total_pop_vax_jun1 = if_else(
        ((num_5_11_jun1_last_4_weeks_rate + num_12_17_jun1_last_4_weeks_rate + num_18_plus_jun1_last_4_weeks_rate) / total_pop) > 1, 
        100,
        round(
          ((num_5_11_jun1_last_4_weeks_rate + num_12_17_jun1_last_4_weeks_rate + num_18_plus_jun1_last_4_weeks_rate) / 
             total_pop) * 100, 1)
      ),
      vax_pct_5_11_jun1_w_last_four_week_rate = if_else((num_5_11_jun1_last_4_weeks_rate / pop_5_11) > 1,
                                                        100,
                                                        round((num_5_11_jun1_last_4_weeks_rate / pop_5_11) * 100, 1)),
      vax_pct_12_17_jun1_w_last_four_week_rate = if_else((num_12_17_jun1_last_4_weeks_rate / pop_12_17) > 1,
                                                         100,
                                                         round((num_12_17_jun1_last_4_weeks_rate / pop_12_17) * 100, 1))
    ) %>% 
    select(full_name,
           series_complete_pop_pct,
           vax_pct_5_11_jun1_w_last_four_week_rate,
           vax_pct_12_17_jun1_w_last_four_week_rate,
           proj_total_pop_vax_jun1
    ) %>% 
    arrange(desc(proj_total_pop_vax_jun1)) -> tot_proj
  
  states_proj <- tot_proj %>% 
    filter(full_name != "United States")
  
  natl_proj <- tot_proj %>% 
    filter(full_name == "United States")
  
  full_proj <- bind_rows(natl_proj, states_proj) %>% 
    mutate(across(2:5, ~paste0(as.character(.x), "%"))) %>% 
    rename(`State/Territory/Federal District` = full_name,
           `Percent of total population fully vaccinated` = series_complete_pop_pct,
           `Projected percent of 5-11 vaccinated by June 1` = vax_pct_5_11_jun1_w_last_four_week_rate,
           `Projected percent of 12-17 vaccinated by June 1` = vax_pct_12_17_jun1_w_last_four_week_rate,
           `Projected total population vaccinated by June 1` = proj_total_pop_vax_jun1)
  
  write_csv(full_proj, "./visualizations/kids_vax_projection_20220601.csv")
  
  Sys.sleep(2)
  # Updating Datawrapper chart
  republish_chart(API_KEY = DW_API,
                  chart_id = "SmQgS",
                  data = full_proj,
                  notes = dw_subtitle
  ) 
}
