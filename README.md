# COVID19

## Introduction
The covid19 package allows users to obtain live data from the
novel Coronavirus COVID-19 as published by the JHU CCSE repository [1].

The goal of this package is to make the latest data quickly available
for researchers and the scientific community.


### Data Accesibility
The `covid19()` function can obtain data from the JHU's CCSE repository,
aggregating the data in different categories "confirmed"/"deaths"/"recovered"
cases reported daily per country/region/city.
By using this function, the data will be pulled directly from the JHU repository
with the latest updates.


### Analitcal & Graphical Indicators
In addition to the data availability the package includes some basics functions
to estimate totals per regions/country/cities, growth rates and daily changes in
the reported number of cases.


Function  | description
--------	 | -----------
`tots.per.location`  |  compute totals per region and plot time series for that specific region/country
`growth.rate`  |  compute changes and growth rates per region and plot time series for that specific region/country
-----

 
## Installation
For using the "covi19" package, first you will need to install it.

The stable version can be downloaded from the CRAN repository:
```
install.packages("covid19")
```

To obtain the development version you can get it from the github repository, i.e.
```
# need devtools for installing from the github repo
install.packages("devtools")

# install bioC.logs
devtools::install_github("mponce0/covid19")
```

For using the package, either the stable or developmemnt version, just load it using the library function:
```
# load covid19
library(covid19)
```


## Examples

### Reading data
```
# obtain all the records combined for "confirmed", "deaths" and "recovered" cases
 covid19.data.ALLcases <- covid19()

# obtain records combined for "confirmed" cases
 covid19.confirmed.cases <- covid19("confirmed")

# obtain records combined for "deaths" cases
 covid19.deaths.cases <- covid19("deaths")

# obtain records combined for "recovered" cases
 covid19.recovered.cases <- covid19("recovered")
```

### Some basic analysis
#### Totals per Country/Region/Province
```
# totals for confirmed cases for "Ontario"
tots.per.location(covid19.confirmed.cases,geo.loc="Ontario")

# total for confirmed cases for "Canada"
tots.per.location(covid19.confirmed.cases,geo.loc="Canada")

# total nbr of deaths for "Mainland China"
tots.per.location(covid19.deaths.cases,geo.loc="Mainland China")

# total for death cases for "ALL" the regions
tots.per.location(covid19.deaths.cases)
```

## Further Resources
* blog.ephorie.de/epidemiology-how-contagious-is-novel-coronavirus-2019-ncov


## References
[1] 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE
https://github.com/CSSEGISandData/COVID-19
