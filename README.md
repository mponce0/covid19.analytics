# COVID19

## Introduction
The covid19 package allows users to obtain live data from the
novel Coronavirus COVID-19 as published by the JHU CCSE repository [1].
The goal of this pakcage is to make the latest data quickly avaialble
for reserachers and the scientific community.

### Data Accesibility
The `covid19()` function can obtain data from the JHU's CCSE repository,
aggregating the data in different categories "confirmed"/"deaths"/"recovered"
cases reported daily per country/region/city.
By using this function, the data will be pulled directly from the JHU repository
with the latest updates.

### Analitcal Estimates
In addition to the data availability we procide some basics functions to estimate
totals per regions/country/cities, growth rates and daily changes in the reported
number of cases.

 
## Installation
```
# development version
devtools::install_github("mponce0/covid19")
```

```
# stable version from CRAN
install.packages("covid19")
```

After installating the package, just load it to use it:
```
library(covid19)
```

## Examples

## References
[1] 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE
https://github.com/CSSEGISandData/COVID-19
