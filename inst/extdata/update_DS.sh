#!/bin/bash

#curl -L -O https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv

JHUurl="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/"
TSdomain="csse_covid_19_time_series"
AGGdomain="csse_covid_19_daily_reports"
TSfiles="time_series_covid19_confirmed_global.csv time_series_covid19_deaths_global.csv time_series_covid19_recovered_global.csv 
	time_series_covid19_confirmed_US.csv time_series_covid19_deaths_US.csv"

### Time Series data
for file in `echo ${TSfiles}`; do
	target=${JHUurl}/${TSdomain}/${file} ;
	echo $target ;
	curl -L -O  $target ;
done

### Aggregated data

echo "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-27-2020.csv"

dailyReport=${JHUurl}/${AGGdomain}/`date -v-1d +"%m-%d-%Y"`".csv"
echo $dailyReport
curl -L -O $dailyReport
