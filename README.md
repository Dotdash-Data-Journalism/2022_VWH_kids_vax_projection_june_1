# Verywell Health Kids COVID-19 Vaccination Projection for June 1st, 2022

This repository contains an R script `verywell_health_kids_covid_projection_june_1.R` that is run via 
a [GitHub Action](https://docs.github.com/en/actions) every day at approximated 2:47am UTC. 
The script fetches the latest [COVID-19 vaccination](https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-Jurisdi/unsk-b7fc)
data by US state from the CDC. It then creates a CSV `kids_vax_projection_20220601.csv` in the 
`visualizations` folder of the projected proportion of 5 - 11 year olds, 12 - 17 year olds, and all residents that will be fully vaccinated
as of the latest day's vaccination data US state & Washington DC except Idaho (due to lack of age breakout data). 
The CSV is then sent via [API](https://developer.datawrapper.de/reference/introduction) to update a [Datawrapper](https://www.datawrapper.de/) 
table of the COVID-19 vaccination rates projects by state and age group for [Verywell health](https://www.verywellhealth.com/).