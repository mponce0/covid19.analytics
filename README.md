# COVID19.Analytics

<!-- ~*~ -->
<!-- badges: start -->
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version-last-release/covid19.analytics)](https://cran.r-project.org/package=covid19.analytics)
[![CRAN checks](https://cranchecks.info/badges/worst/covid19.analytics)](https://cranchecks.info/pkgs/covid19.analytics)
[![Downloads](https://cranlogs.r-pkg.org/badges/covid19.analytics)](https://cran.r-project.org/package=covid19.analytics)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/covid19.analytics)](https://cran.r-project.org/package=covid19.analytics)
[![R-CMD-check](https://github.com/mponce0/covid19.analytics/actions/workflows/r.yml/badge.svg)](https://github.com/mponce0/covid19.analytics/actions/workflows/r.yml)
[![Build Status](https://travis-ci.org/mponce0/covid19.analytics.svg?branch=master)](https://travis-ci.org/mponce0/covid19.analytics)
[![codecov](https://codecov.io/gh/mponce0/covid19.analytics/branch/master/graph/badge.svg)](https://codecov.io/gh/mponce0/covid19.analytics)
[![DOI](https://zenodo.org/badge/246323140.svg)](https://zenodo.org/badge/latestdoi/246323140)	<!--[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4640307.svg)](https://doi.org/10.5281/zenodo.4640307)-->
[![DOI](https://joss.theoj.org/papers/10.21105/joss.02995/status.svg)](https://doi.org/10.21105/joss.02995)	<!-- [![JOSSstatus](https://joss.theoj.org/papers/43bab096ad574f4510a7258c20a1153d/status.svg)](https://joss.theoj.org/papers/43bab096ad574f4510a7258c20a1153d) -->
<!-- badges: end -->
<!-- ~*~ -->


<!-------------  TOC  ----------------->
# Table of Contents
<!--
<details>
    <summary>Click to Expand/Collapse</summary>
-->
1. [Introduction](#introduction)
2. [covid19.analytics Main Features](#packageFeatures)
    1. [Data Accessibility](#dataaccess)
        1. [Data Structure](#datastructure)
        2. [Data Intregrity and Checks](#dataintegrity)
        3. [Genomics Data](#genomicsdata)
    2. [Analytical & Graphical Indicators](#functionalities)
3. [Installation](#installation)
4. [Examples](#examples)
5. [About](#about)
	1. [Media & Press](#media)
	2. [References and Citation](#references)
	    1. [Citing covid19.analytics](#citation)
<!-- </details> -->
<!-------------  TOC  ----------------->



<!-- <div><object data=".travis.yml"></object></div> -->



## Introduction <a name="introduction"></a>
The "covid19.analytics" R package allows users to obtain live\* worldwide data from the
*novel Coronavirus Disease* originally reported in 2019, COVID-19.

One of the main goals of this package is to make the latest data about the COVID-19 pandemic
promptly available to researchers and the scientific community.

The "covid19.analytics" package also provides basic analysis tools and functions to
investigate these datasets.

<!--
<div>
<object data="{{ site.url }}{{ site.baseurl }}/man/figures/livemap.html" width="105%" height="525"></object>
</div>
-->
<object data="man/figures/livemap.html" width="105%" height="525"></object>
<!--
<object data="https://raw.githubusercontent.com/mponce0/covid19.reports/master/reports/livemap.html" width="105%" height="525"></object>
.
<object data="https://github.com/mponce0/covid19.reports/blob/master/reports/livemap.html" width="105%" height="525">
</object>
-->


The following sections briefly describe some of the covid19.analytics package main features, we strongly recomend users to read our paper ["covid19.analytics: An R Package to Obtain, Analyze and Visualize Data from the Coronavirus Disease Pandemic"](https://arxiv.org/abs/2009.01091) (https://arxiv.org/abs/2009.01091) where further details about the package are presented and discussed.



## covid19.analytics Main Features  <a name="packageFeatures"></a>
The `covid19.analytics` package is an open source tool, which its main implementation and API
is the R package.
In addition to this, the package has a few more adds-on:

 * a central GitHub repository, https://github.com/mponce0/covid19.analytics
 where the latest development version and source code of the package are available.
 Users can also submit tickets for bugs, suggestions or comments using the "issues" tab.
 
 * a rendered version with live examples and documentation also hosted at GitHub pages,
 https://mponce0.github.io/covid19.analytics/
 
 * a dashboard for interactive usage of the package with extended capabilities
 for users without any coding expertise, https://covid19analytics.scinet.utoronto.ca

   The dashboard can also be deployed locally using the `covid19Explorer()` function which
 is part of the `covid19.analytics` package.
 
 * a *backup* data repository hosted at GitHub,
 https://github.com/mponce0/covid19analytics.datasets --
        where replicas of the live datasets are stored for redundancy and
 robust accesibility sake.
 


### Data Sources <a name="dataSrcs"></a>
The "covid19.analytics" package provides access to the following open-access data sources:

 * **[1]** <a name="JHUrepo"></a> 2019 Novel CoronaVirus COVID-19 (2019-nCoV) Data Repository by
Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)
https://github.com/CSSEGISandData/COVID-19

 * **[2]** <a name="TORdata"></a> COVID-19: Status of Cases in Toronto -- City of Toronto
https://www.toronto.ca/home/covid-19/covid-19-latest-city-of-toronto-news/covid-19-status-of-cases-in-toronto/

 * **[3]** <a name="ODtor"></a> COVID-19: Open Data Toronto
https://open.toronto.ca/dataset/covid-19-cases-in-toronto/

 * **[4]** <a name="HealthCan"></a> COVID-19: Health Canada
https://health-infobase.canada.ca/covid-19/
<!--
https://health-infobase.canada.ca/src/data/covidLive
https://health-infobase.canada.ca/src/data/covidLive/covid19-download.csv
-->

 * **[5]** <a name="NCBIgenome"></a> Severe acute respiratory syndrome coronavirus 2 isolate Wuhan-Hu-1, complete genome
NCBI Reference Sequence: NC_045512.2
https://www.ncbi.nlm.nih.gov/nuccore/NC_045512.2

 * **[6]** <a name="OWIDvaccination"></a> COVID-19 Vaccination and Testing records from "Our World In Data" (OWID)
 https://github.com/owid/
 
 * **[7]** <a name="VCpandemics"></a> Pandemics historical records from Visual Capitalist (and sources within)
 https://www.visualcapitalist.com/history-of-pandemics-deadliest/
 https://www.visualcapitalist.com/the-race-to-save-lives-comparing-vaccine-development-timelines/
 

<details>
    <summary>Click to Expand/Collapse</summary>

### Data Accessibility <a name="dataaccess"></a>
<details>
    <summary>Click to Expand/Collapse</summary>

The `covid19.data()` function allows users to obtain realtime data about the COVID-19 reported cases
from the JHU's CCSE repository, in the following modalities:
* "aggregated" data for the latest day, with a great 'granularity' of geographical regions (ie. cities, provinces, states, countries)
* "time series" data for larger accumulated geographical regions (provinces/countries)

* "deprecated": we also include the original data style in which these datasets were reported initially.

The datasets also include information about the different categories (status) "confirmed"/"deaths"/"recovered"
of the cases reported daily per country/region/city.

This data-acquisition function, will first attempt to retrieve the data directly
from the JHU repository with the latest updates.
If for what ever reason this fails (eg. problems with the connection) the package
will load a preserved "image" of the data which is **not** the latest one but it
will still allow the user to explore this older dataset.
In this way, the package offers a more robust and resilient approach to the quite
dynamical situation with respect to data availability and integrity.


#### Data retrieval options
<!--
 |    argument    |   description  |
 |----------------|----------------|
 | `aggregated`   |  latest number of cases *aggregated* by country |
 **Time Series data**
 | `ts-confirmed` |  time series data of confirmed cases |
 | `ts-deaths`    |  time series data of fatal cases |
 | `ts-recovered` |  time series data of recovered cases |
 | `ts-ALL`       |  all time series data combined |
 | `ts-confirmed-us` |  time series data of confirmed cases for the US detailed per state |
 | `ts-deaths-us`    |  time series data of fatal cases for the US detailed per state |
**Deprecated data formats**
 | `ts-dep-confirmed` | time series data of confirmed cases as originally reported (deprecated) |
 | `ts-dep-deaths`    | time series data of deaths as originally reported (deprecated) |
 | `ts-dep-recovered` | time series data of recovered cases as originally reported (deprecated)|
 **Combined**
 | `ALL`          | all of the above |
-->

<!------- TABLE ------>
<table style="width:100%">
  <tr>
    <th>argument</th>
    <th>description</th> 
  </tr>
  <tr>
    <td> <code>aggregated</code> </td>
    <td> latest number of cases <i>aggregated</i> by country </td>
  </tr>
  <tr>
    <th colspan="2"> <b>Time Series data</b> </th>
  </tr>
  <tr>
    <td> <code>ts-confirmed</code> </td>
    <td> time series data of confirmed cases </td> 
  </tr>
  <tr>
    <td> <code>ts-deaths</code> </td>
    <td> time series data of fatal cases </td> 
  </tr>
  <tr>
    <td> <code>ts-recovered</code> </td>
    <td> time series data of recovered cases </td>
  </tr>
  <tr>
    <td> <code>ts-ALL</code> </td>
    <td> all time series data combined </td>
  </tr>
  <tr>
     <th colspan="2"> <b>Deprecated data formats</b> </th>
  </tr>
  <tr>
   <td> <code>ts-dep-confirmed</code> </td>
   <td> time series data of confirmed cases as originally reported (deprecated) </td>
  </tr>
  <tr>
   <td> <code>ts-dep-deaths</code> </td>
   <td> time series data of deaths as originally reported (deprecated) </td>
 </tr>
 <tr>
   <td> <code>ts-dep-recovered</code> </td>
   <td> time series data of recovered cases as originally reported (deprecated) </td>
 </tr>
 <tr>
  <th colspan="2"> <b>Combined</b> </th>
 </tr>
 <tr>
  <td> <code>ALL</code> </td>
  <td> all of the above </td>
 </tr>
   <tr>
    <th colspan="2"> Time Series data for <i>specific locations</i> </th>
  </tr>
   <tr>
    <td> <code>ts-Toronto</code> </td>
    <td> time series data of confirmed cases for the city of Toronto, ON - Canada </td> 
  </tr>
  <tr>
    <td> <code>ts-confirmed-US</code> </td>
    <td> time series data of confirmed cases for the US detailed per state </td> 
  </tr>
  <tr>
    <td> <code>ts-deaths-US</code> </td>
    <td> time series data of fatal cases for the US detailed per state </td> 
  </tr>
</table>
<!------- TABLE ------>


### Data Structure <a name="datastructure"></a>
The *TimeSeries* data is organized in an specific manner with a given set of fields or columns,
which resembles the following structure:

<table>
 <tr>
  <td>"Province.State"</td>  <td>"Country.Region"</td>  <td>"Lat"</td>  <td>"Long"</td>  <td>...</td><td>seq of dates</td><td>...</td> 
 </tr>
</table>


#### Using your own data and/or importing new data sets
If you have data structured in a data.frame organized as described above, then most of the functions provided by the "covid19.analytics" package for analyzing *TimeSeries* data will work with your data.
In this way it is possible to add new data sets to the ones that can be loaded using the repositories predefined in this package and extend the analysis capabilities to these new datasets.

Be sure also to check the compatibility of these datasets using the `Data Integrity and Consistency Checks` functions described in the following section.


### Data Integrity and Consistency Checks <a name="dataintegrity"></a>
Due to the ongoing and rapid changing situation with the COVID-19 pandemic, sometimes the reported data has been detected to change its internal format or even show some "anomalies" or "inconsistencies" (see https://github.com/CSSEGISandData/COVID-19/issues/).

For instance, in some cumulative quantities reported in time series datasets, it has been observed that these quantities instead of continuously increase sometimes they decrease their values which is something that should not happen, (see for instance, https://github.com/CSSEGISandData/COVID-19/issues/2165). We refer to this as inconsistency of **"type II"**.

Some negative values have been reported as well in the data, which also is not possible or valid; we call this inconsistency of **"type I"**.

When this occurs, it happens at the level of the origin of the dataset, in our case, the one obtained from the JHU/CCESGIS repository [[1](#JHUrepo)].
In order to make the user aware of this, we implemented two consistency and integrity checking functions:

* `consistency.check()`, this function attempts to determine whether there are consistency issues within the data, such as, negative reported value (inconsistency of "type I") or anomalies in the cumulative quantities of the data (inconsistency of "type II")

* `integrity.check()`, this determines whether there are integrity issues within the datasets or changes to the structure of the data

Alternatively we provide a `data.checks()` function that will run both functions on an specified dataset.

#### Data Integrity
It is highly unlikely that you would face a situation where the internal structure of the data, or its actual integrity may be compromised but if you think that this is the case or the `integrity.check()` function reports this, please we urge you to contact the developer of this package (https://github.com/mponce0/covid19.analytics/issues).

#### Data Consistency
Data consistency issues and/or anomalies in the data have been reported several times, see https://github.com/CSSEGISandData/COVID-19/issues/.
These are claimed, in most of the cases, to be missreported data and usually are just an insignificant number of the total cases.
Having said that, we believe that the user should be aware of these situations and we recommend using the `consistency.check()` function to verify the dataset you will be working with.

#### Nullifying Spurious Data
In order to deal with the different scenarios arising from incomplete, inconsistent
or missreported data, we provide the `nullify.data()` function, which will
remove any potential entry in the data that can be suspected of these incongruencies.
In addition ot that, the function accepts an optional argument `stringent=TRUE`,
which will also prune any incomplete cases (e.g. with NAs present).


### Genomics Data <a name="genomicsdata"></a>
Similarly to the rapid developments and updates in the reported cases of the disease,
the genetic sequencing of the virus is moving almost at equal pace.
That's why the covid19.analytics package provides access to a good number of the genomics
data currently available.

The `covid19.genomic.data()` function allows users to obtain the COVID-19's
genomics data from NCBI's databases [[5](#NCBIgenome)].
The type of genomics data accessible from the package is described in
the following table.

<table>
 <tr>
  <td>type</td>  <td>description</td>  <td>source</td>
 </tr>
 <tr>
     <td>genomic</td>
     <td>a composite list containing different indicators and elements of the SARS-CoV-2 genomic information</td>
     <td>https://www.ncbi.nlm.nih.gov/sars-cov-2/</td>
 </tr>
 <tr>
     <td>genome</td>
     <td>genetic composition of the reference sequence of the SARS-CoV-2 from GenBank</td>
     <td>https://www.ncbi.nlm.nih.gov/nuccore/NC_045512</td>
 </tr>
 <tr>
     <td>fasta</td>
     <td>genetic composition of the reference sequence of the SARS-CoV-2 from a fasta file</td>
     <td>https://www.ncbi.nlm.nih.gov/nuccore/NC_045512.2?report=fasta</td>
 </tr>
 <tr>
     <td>ptree</td>
     <td>phylogenetic tree as produced by NCBI data servers</td>
     <td>https://www.ncbi.nlm.nih.gov/labs/virus/vssi/#/precomptree</td>
 </tr>
 <tr>
     <td>nucleotide / protein</td>
     <td>list and composition of nucleotides/proteins from the SARS-CoV-2 virus</td>
     <!-- <td>https://www.ncbi.nlm.nih.gov/labs/virus/vssi/#/virus?SeqType_s=Genome&VirusLineage_ss=SARS-CoV-2,%20taxid:2697049</td> -->
     <td> https://www.ncbi.nlm.nih.gov/labs/virus/vssi/#/ </td>
 </tr>
 <tr>
     <td>nucleotide-fasta / protein-fasta</td>
     <td>FASTA sequences files for nucleotides, proteins and coding regions</td>
     <!-- <td>https://www.ncbi.nlm.nih.gov/labs/virus/vssi/#/virus?SeqType_s=Genome&VirusLineage_ss=SARS-CoV-2,%20taxid:2697049</td> -->
     <td> https://www.ncbi.nlm.nih.gov/labs/virus/vssi/#/ </td>
 </tr>
</table>


Although the package attempts to provide the latest available genomic data, there are
a few important details and differences with respect to the reported cases data.
For starting, the amount of genomic information available is way larger than
the data reporting the number of cases which adds some additional constraints
when retrieving this data.
In addition to that, the hosting servers for the genomic databases impose
certain limits on the rate and amounts of downloads.

In order to mitigate these factors, the covid19.analytics package employs a couple of
different strategies as summarized below:
* most of the data will be attempted to be retrieved live from NCBI databases
        -- same as using `src='livedata'`
* if that is not possible, the package keeps a local version of
some of the largest datasets (i.e. genomes, nucleotides and proteins) which
might not be up-to-date
        -- same as using `src='repo'`.
* the package will attempt to obtain the data from a mirror server
with the datasets updated on a regular basis but not necessarily with the
latest updates
        -- same as using `src='local'`.


</details>


### Analytical & Graphical Indicators <a name="functionalities"></a>
<details>
    <summary>Click to Expand/Collapse</summary>

In addition to the access and retrieval of the data, the package includes some
basics functions to estimate totals per regions/country/cities, growth rates
and daily changes in the reported number of cases.


### Overview of the Main Functions from the "covid19.analytics" Package
<!--
 | Function  | description |
 | --------	 | ----------- |
 **Data Acquisition**
 | `covid19.data` |  obtain live\* worldwide data for covid19 virus, from the JHU's CCSE repository [1]
 **Analysis**
 | `report.summary`  |  summarize the current situation, will download the latest data and summarize different quantities
 | `tots.per.location`  |  compute totals per region and plot time series for that specific region/country
 | `growth.rate`  |  compute changes and growth rates per region and plot time series for that specific region/country
 **Graphics**
 | `totals.plt`   |  plots in a static and interactive plot total number of cases per day
 | `live.map`     |  generates an interactive map displaying cases around the world
 **Modelling**
 | `generate.SIR.model`  |  generates a SIR (Susceptible-Infected-Recovered) model
-->

<!------- TABLE ------>
<table style="width:100%">
  <tr>
    <th> Function </th>
    <th> Description </th>
    <th> Main Type of Output</th>
  </tr>
  <tr>
   <th colspan="3"> <b>Data Acquisition</b> </th>
  </tr>
  <tr>
    <td> <code>covid19.data</code> </td>
    <td> obtain live* worldwide data for COVID-19 virus, from the JHU's CCSE repository [<a href="#JHUrepo">1</a>] </td>
    <td> return dataframes/list with the collected data </td>
  </tr>
   <tr>
    <td> <code>covid19.Toronto.data</code> </td>
    <td> obtain live* data for COVID-19 cases in the city of Toronto, ON Canada, from the City of Toronto reports [<a href="#TORdata">2</a>] --or-- Open Data Toronto [<a href="#ODtor">3</a>] </td>
    <td> return dataframe/list with the collected data </td>
  </tr>
   <tr>
    <td> <code>covid19.Canada.data</code> </td>
    <td> obtain live* Canada specific data for COVID-19 cases, from Health Canada [<a href="#HealthCan">4</a>] </td>
    <td> return dataframe/list with the collected data </td>
  </tr>
   <tr>
    <td> <code>covid19.US.data</code> </td>
    <td> obtain live* US specific data for COVID-19 virus, from the JHU's CCSE repository [<a href="#JHUrepo">1</a>] </td>
    <td> return dataframe with the collected data </td>
  </tr>

   <tr>
    <td> <code>covid19.vaccination</code> </td>
    <td> obtain up-to-date COVID-19 vaccination records from [<a href="#OWIDvaccination">5</a>] </td>
    <td> return dataframe/list with the collected data </td>
  </tr>

   <tr>
    <td> <code>covid19.testing.data</code> </td>
    <td> obtain up-to-date COVID-19 testing records from [<a href="#OWIDvaccination">5</a>] </td>
    <td> return dataframe with the testing data or testing data details </td>
  </tr>

   <tr>
    <td> <code>pandemics.data</code> </td>
    <td> obtain pandemics and pandemics vaccination *historical* records from [<a href="#VCpandemics">6</a>] </td>
    <td> return dataframe with the collected data </td>
  </tr>


<tr>
  <td> <code>covid19.genomic.data  c19.refGenome.data  c19.fasta.data  c19.ptree.data  c19.NPs.data  c19.NP_fasta.data</code> </td>
    <td> obtain covid19's genomic sequencing data from NCBI [<a href="#NCBIgenome">5</a>] </td>
  <td> list, with the RNA seq data in the <code>"$NC_045512.2"</code> entry </td>
 </tr>
   <tr>
   <th colspan="3"> <b>Data Quality Assessment</b> </th>
  </tr>
  <tr>
    <td> <code>data.checks</code> </td>
    <td> run integrity and consistency checks on a given dataset </td>
    <td> diagnostics about the dataset integrity and consistency </td>
  </tr>
  <tr>
    <td> <code>consistency.check</code> </td>
    <td> run consistency checks on a given dataset </td>
    <td> diagnostics about the dataset consistency </td>
  </tr>
  <tr>
    <td> <code>integrity.check</code> </td>
    <td> run integrity checks on a given dataset </td>
    <td> diagnostics about the dataset integrity </td>
  </tr>
  <tr>
    <td> <code>nullify.data</code> </td>
    <td> remove inconsistent/incomplete entries in the original datasets </td>
    <td> original dataset (dataframe) without "suspicious" entries </td>
  </tr>
 <tr>
   <th colspan="3"> <b>Analysis</b> </th>
  </tr>
  <tr>
    <td> <code>report.summary</code> </td>
    <td> summarize the current situation, will download the latest data and summarize different quantities </td>
    <td> on screen table and static plots (pie and bar plots) with reported information, can also output the tables into a text file</td>
  </tr>
  <tr>
   <td> <code>tots.per.location</code> </td>
   <td> compute totals per region and plot time series for that specific region/country </td>
   <td> static plots: data + models (exp/linear, Poisson, Gamma), mosaic and histograms when more than one location are selected </td>
  </tr>
  <tr>
   <td> <code>growth.rate</code> </td>
   <td> compute changes and growth rates per region and plot time series for that specific region/country </td>
   <td> static plots: data + models (linear,Poisson,Exp), mosaic and histograms when more than one location are selected </td>
  </tr>
  <tr>
   <td> <code>single.trend</code> <br> <code>mtrends</code> </td>
   <td> visualize different indicators of the "trends" in daily changes for a single or mutliple locations </td>
   <td> compose of static plots: total number of cases vs time, daily changes vs total changes in different representations</td>
  </tr>
  <tr>
   <td> <code>estimateRRs</code> </td>
   <td> compute estimates for fatality and recovery rates on a rolling-window interval </td>
   <td> list with values for the estimates (mean and sd) of reported cases and recovery and fatality rates </td>
  </tr>
<tr>
   <th colspan="3">Graphics and Visualization</th>
 </tr>
  <tr>
   <td> <code>total.plts</code> </td>
   <td> plots in a static and interactive plot total number of cases per day, the user can specify multiple locations or global totoals </td>
   <td> static and interactive plot </td>
 </tr>
   <tr>
   <td> <code>itrends</code> </td>
   <td> generates an interactive plot of daily changes vs total changes in a log-log plot, for the indicated regions </td>
   <td> interactive plot </td>
 </tr>
  <tr>
   <td> <code>live.map</code> </td>
   <td> generates an interactive map displaying cases around the world </td>
   <td> static and interactive plot </td>
  </tr>
  <tr>
   <th colspan="3">Modelling</th>
 </tr>
  <tr>
   <td> <code>generate.SIR.model</code> </td>
   <td> generates a SIR (Susceptible-Infected-Recovered) model </td>
   <td> list containing the fits for the SIR model </td>
 </tr>
  <tr>
   <td> <code>plt.SIR.model</code> </td>
   <td> plot the results from the SIR model </td>
   <td> static and interactive plots </td>
 </tr>
  <tr>
   <td> <code>sweep.SIR.model</code> </td>
   <td> generate multiple SIR models by varying parameters used to select the actual data </td>
   <td> list containing the values  parameters, $\beta, \gamma$ and $R_0$ </td>
 </tr>
 <tr>
   <th colspan="3">Data Exploration</th>
 </tr>
  <tr>
   <td> <code>covid19Explorer</code> </td>
   <td> launches a dashboard interface to explore the datasets provided by covid19.analytics</td>
   <td> web-based dashboard </td>
 </tr>
  <tr>
   <th colspan="3">Auxiliary functions</th>
 </tr>
  <tr>
   <td> <code>geographicalRegions</code> </td>
   <td> determines which countries compose a given continent </td>
   <td> list of countries </td>
 </tr>
</table>
<!------- TABLE ------>


---


<!-- ~*~ -->
### API Documentation
Documentation of the functions available in the `covid19.analytics` package can be found at
https://cran.r-project.org/web/packages/covid19.analytics/covid19.analytics.pdf
<!-- ~*~ -->

### Details and Specifications of the Analytical & Visualization Functions
<details>
    <summary>Click to Expand/Collapse</summary>

#### Reports
The `report.summary()` generates an overall report summarizing the different datasets.
It can summarize the "Time Series" data (`cases.to.process="TS"`), the "aggregated" data (`cases.to.process="AGG"`) or both (`cases.to.process="ALL"`).
It will display the top 10 entries in each category, or the number indicated in the `Nentries` argument, for displaying all the records set `Nentries=0`.

The function can also target specific geographical location(s) using the `geo.loc` argument.
When a geographical location is indicated, the report will include an additional "Rel.Perc" column for the confirmed cases indicating the *relative* percentage among the locations indicated.
Similarly the totals displayed at the end of the report will be for the selected locations.

In each case ("TS" or/and "AGG") will present tables ordered by the different cases included, i.e.
confirmed infected, deaths, recovered and active cases.

The dates when the report is generated and the date of the recorded data will be included at the beginning of each table.

It will also compute the totals, averages, standard deviations and percentages of various quantities:
* it will determine the number of *unique* locations processed within the dataset
* it will compute the total number of cases per case

* Percentages: percentages are computed as follow:
  - for the "Confirmed" cases, as the ratio between the corresponding number of cases and the total number of cases, i.e. a sort of *"global percentage"* indicating the percentage of infected cases wrt the rest of the world
  - for "Confirmed" cases, when geographical locations are specified, a *"Relative percentage"* is given as the ratio of the confirmed cases over the total of the selected locations
  
  - for the other categories, "Deaths"/"Recovered"/"Active", the percentage of a given category is computed as the ratio between the number of cases in the corresponding category divided by the "Confirmed" number of cases, i.e. a *relative percentage* with respect to the number of confirmed infected cases in the given region

* For "Time Series" data:
  - it will show the *delta* (change or variation) in the last day, daily changes day before that (t-2), three days ago (t-3), a week ago (t-7), two weeks ago (t-14) and a month ago (t-30)
  - when possible, it will also display the percentage of "Recovered" and "Deaths" with respect to the "Confirmed" number of cases
  - The column "GlobalPerc" is computed as the ratio between the number of cases for a given country over the total of cases reported
  - The *"Global Perc. Average (SD: standard deviation)"* is computed as the average (standard deviation) of the number of cases among all the records in the data
  - The *"Global Perc. Average (SD: standard deviation) in top X"* is computed as the average (standard deviation) of the number of cases among the top *X* records


Typical structure of a `summary.report()` output for the Time Series data:
```
################################################################################ 
  ##### TS-CONFIRMED Cases  -- Data dated:  2020-04-12  ::  2020-04-13 12:02:27 
################################################################################ 
  Number of Countries/Regions reported:  185 
  Number of Cities/Provinces reported:  83 
  Unique number of geographical locations combined: 264 
-------------------------------------------------------------------------------- 
  Worldwide  ts-confirmed  Totals: 1846679 
-------------------------------------------------------------------------------- 
   Country.Region Province.State Totals GlobalPerc LastDayChange   t-2   t-3   t-7  t-14 t-30
1              US                555313      30.07         28917 29861 35098 29595 20922  548
2           Spain                166831       9.03          3804  4754  5051  5029  7846 1159
3           Italy                156363       8.47          4092  4694  3951  3599  4050 3497
4          France                132591       7.18          2937  4785  7120  5171  4376  808
5         Germany                127854       6.92          2946  2737  3990  3251  4790  910
.
.
.
-------------------------------------------------------------------------------- 
  Global Perc. Average:  0.38 (sd: 2.13) 
  Global Perc. Average in top  10 :  7.85 (sd: 8.18) 
-------------------------------------------------------------------------------- 

******************************************************************************** 
********************************  OVERALL SUMMARY******************************** 
******************************************************************************** 
  ****  Time Series TOTS **** 
  	 ts-confirmed	 ts-deaths	 ts-recovered 
  	 1846679	      114091	    421722 
               			6.18% 		   22.84% 
  ****  Time Series AVGS **** 
  	 ts-confirmed	 ts-deaths	 ts-recovered 
  	 6995	         432.16	    1686.89 
  			             6.18% 		   24.12% 
  ****  Time Series SDS **** 
  	 ts-confirmed	 ts-deaths	 ts-recovered 
  	 39320.05	     2399.5	    8088.55 
  			             6.1% 		    20.57% 

 * Statistical estimators computed considering 250 independent reported entries 
******************************************************************************** 
```

Typical structure of a `summary.report()` output for the *Aggregated* data:
```
################################################################################################################################# 
  ##### AGGREGATED Data  -- ORDERED BY  CONFIRMED Cases  -- Data dated:  2020-04-12  ::  2020-04-13 12:02:29 
################################################################################################################################# 
  Number of Countries/Regions reported: 185 
  Number of Cities/Provinces reported: 138 
  Unique number of geographical locations combined: 2989 
--------------------------------------------------------------------------------------------------------------------------------- 
                      Location Confirmed Perc.Confirmed Deaths Perc.Deaths Recovered Perc.Recovered Active Perc.Active
1                        Spain    166831           9.03  17209       10.32     62391          37.40  87231       52.29
2                        Italy    156363           8.47  19899       12.73     34211          21.88 102253       65.39
3                       France    132591           7.18  14393       10.86     27186          20.50  91012       68.64
4                      Germany    127854           6.92   3022        2.36     60300          47.16  64532       50.47
5  New York City, New York, US    103208           5.59   6898        6.68         0           0.00  96310       93.32
.
.
.
=================================================================================================================================
  	 Confirmed	 Deaths	  Recovered 	Active 
  Totals 
  	 1846680  	 114090	  421722    	1310868 
  Average 
  	 617.83	    38.17.  	141.09    	438.56 
  Standard Deviation 
  	 6426.31	   613.69	  2381.22 	  4272.19 
  
 * Statistical estimators computed considering 2989 independent reported entries
```

In both cases an overall summary of the reported cases is presented by the end, displaying totals, average and standard devitation of the computed quantities.

A full example of this report for today can be seen 
 <a href="https://github.com/mponce0/covid19.analytics/blob/master/man/figures/covid19-SummaryReport.txt" target="_blank">here</a>
<!--<a href="https://github.com/mponce0/covid19.reports/blob/master/reports/covid19-SummaryReport.txt" target="_blank">here</a>-->
(updated twice a day, daily).

In addition to this, the function will also generate some graphical outputs, including pie and bar charts representing the top regions in each category.


#### Totals per Location & Growth Rate
It is possible to dive deeper into a particular location by using the `tots.per.location()` and `growth.rate()` functions.
Theses functions are capable of processing different types of data, as far as these are "Time Series" data.
It can either focus in one category (eg. "TS-confirmed","TS-recovered","TS-deaths",) or all ("TS-all").
When these functions detect different type of categories, each category will be processed separatedly.
Similarly the functions can take multiple locations, ie. just one, several ones or even "all" the locations within the data.
The locations can either be countries, regions, provinces or cities. If an specified location includes multiple entries, eg. a country that has several cities reported, the functions will group them and process all these regions as the location requested.


##### Totals per Location
This function will plot the number of cases as a function of time for the given locations and type of categories, in two plots: a log-scale scatter one a linear scale bar plot one.

When the function is run with multiple locations or all the locations, the figures will be adjusted to display multiple plots in one figure in a mosaic type layout.

Additionally, the function will attempt to generate different fits to match the data:
* an exponential model using a Linear Regression method
* a Poisson model using a General Linear Regression method
* a Gamma model using a General Linear Regression method
The function will plot and add the values of the coefficients for the models to the plots and display a summary of the results in screen.

It is possible to instruct the function to draw a "confidence band" based on a *moving average*, so that the trend is also displayed including a region of higher confidence based on the mean value and standard deviation computed considering a time interval set to equally dividing the total range of time over 10 equally spaced intervals.

The function will return a list combining the results for the totals for the different locations as a function of time.


##### Growth Rate
The `growth.rate()` function allows to compute *daily changes* and the *growth rate* defined as the ratio of the daily changes between two consecutive dates.

The `growth.rate()` shares all the features of the `tots.per.location()` function, i.e. can process the different types of cases and multiple locations.

The graphical output will display two plots per location:
* a scatter plot with the number of changes between consecutive dates as a function of time, both in linear scale (left vertical axis) and log-scale (right vertical axis) combined
* a bar plot displaying the growth rate for the particular region as a function of time.

When the function is run with multiple locations or all the locations, the figures will be adjusted to display multiple plots in one figure in a mosaic type layout.
In addition to that, when there is more than one location the function will also generate two different styles of heatmaps comparing the changes per day and growth rate among the different locations (vertical axis) and time (horizontal axis).

The function will return a list combining the results for the "changes per day" and the "growth rate" as a function of time.


#### Trends in Daily Changes
We provide three different functions to visualize the *trends* in daily changes of reported cases from time series data.

* <code>single.trend</code>, allows to inspect one single location, this could be used with the worldwide data sliced by the corresponding location, the Toronto data or the user's own data formatted as "Time Series" data.

* <code>mtrends</code>, similar to single.trend function, but accepts multiple or single locations generating one plot per location requested

* <code>itrends</code>, function to generate an interactive plot of the trend in daily changes representing changes in number of cases vs total number of cases in log-scale using **splines** techniques to smooth the abrupt variations in the data


The first two functions will generate "static" plots in a compose with different insets:
- the main plot represents daily changes as a function of time
- the inset figures in the top, from left to right:
   - total number of cases (in linear and semi-log scales),
   - changes in number of cases vs total number of cases
   - changes in number of cases vs total number of cases in log-scale
- the second row of insets, represent the "growth rate" (as defined above) and the "normalized" growth rate defined as the growth rate divided by the maximum growth rate reported for this location



#### Plotting Totals
The function `totals.plt()` will generate plots of the total number of cases as a function of time.
It can be used for the total data or for an specific or multiple locations.
The function can generate static plots and/or interactive ones, as well, as linear and/or semi-log plots.


#### Plotting Cases in the World
The function `live.map()` will display the different cases in each corresponding location all around the world in an interactive map of the world.
It can be used with time series data or aggregated data, aggregated data offers a much more detailed information about the geographical distribution.


### Experimental: Modelling the evolution of the Virus spread
We are working in the development of *modelling* capabilities.
A preliminary prototype has been included and can be accessed using the `generate.SIR.model` function, which implements a simple SIR (*Susceptible-Infected-Recovered*) ODE model using the actual data of the virus.

This function will try to identify the data points where the onset of the epidemy began and consider the following data points to generate a proper guess for the two parameters describing the SIR ODE system.
After that, it will solve the different equations and provide details about the solutions as well as plot them in a static and interactive plot.

#### Sweeping models...
For exploring the parameter space of the SIR model, it is possible to produce a
series of models by varying the conditions, i.e. range of dates considered for
optimizing the parameters of the SIR equation, which will effectively *sweep*
a range for the parameters $\beta, \gamma$ and $R_0$.
This is implemented in the function `sweep.SIR.models()`, which takes a
range of dates to be used as starting points for the number of cases used to
feed into the `generate.SIR.model()` producing as many models as different
ranges of dates are indicated.
One could even use this in combination to other resampling or Monte Carlo
techniques to estimate statistical variability of the parameters from the
model.

</details>


### Further Features
We will continue working on adding and developing new features to the package,
in particular modelling and predictive capabilities.

Please contact us if you think of a functionality or feature that could be useful to add.
</details>

</details>



## Installation  <a name="installation"></a>
<details>
    <summary>Click to Expand/Collapse</summary>

For using the "covi19.analytics" package, first you will need to install it.

The stable version can be downloaded from the CRAN repository:
```R
install.packages("covid19.analytics")
```

To obtain the development version you can get it from the github repository, i.e.
```R
# need devtools for installing from the github repo
install.packages("devtools")

# install covid19.analytics from github
devtools::install_github("mponce0/covid19.analytics")
```

For using the package, either the stable or development version, just load it using the library function:
```R
# load "covid19.analytics"
library(covid19.analytics)
```
</details>



## Examples  <a name="examples"></a>

In this section, we include basic examples of the main features of the `covid19.analytics` package.

  * We strongly recommend users to check further examples and details about the `covid19.analytics` package available in our manuscript,
https://arxiv.org/abs/2009.01091 

  * Code/scripts with examples and tutorials are available at https://github.com/mponce0/covid19.analytics/tree/literature/tutorial 

<details>
    <summary>Click to Expand/Collapse</summary>

### Reading data
```R
# obtain all the records combined for "confirmed", "deaths" and "recovered" cases -- *aggregated* data
 covid19.data.ALLcases <- covid19.data()

# obtain time series data for "confirmed" cases
 covid19.confirmed.cases <- covid19.data("ts-confirmed")

# reads all possible datasets, returning a list
 covid19.all.datasets <- covid19.data("ALL")

# reads the latest aggregated data
 covid19.ALL.agg.cases <- covid19.data("aggregated")

# reads time series data for casualties
 covid19.TS.deaths <- covid19.data("ts-deaths")

# reads testing data
 testing.data <- covid19.testing.data()
```

Read covid19's genomic data 
```R
# obtain covid19's genomic data
 covid19.gen.seq <- covid19.genomic.data()

# display the actual RNA seq
 covid19.gen.seq$NC_045512.2
```

Obtaining Pandemics data
```R
# Pandemic historical records
 pnds <- pandemics.data(tgt="pandemics")

# Pandemics vaccines development times
 pnds.vacs <- pandemics.data(tgt="pandemics_vaccines")
```


### Some basic analysis
#### Summary Report
```R
# a quick function to overview top cases per region for time series and aggregated records
report.summary()
```

<!--
<p>
  <img src="{{ site.url }}{{ site.baseurl }}/man/figures/report-summ-agg.pdf" width="45%" />
  <img src="{{ site.url }}{{ site.baseurl }}/man/figures/report-summ-TSconfirmed.pdf" width="45%" />
</p>
-->
<p>
  <img src="man/figures/report-summ-agg.pdf" width="45%" />
  <img src="man/figures/report-summ-TSconfirmed.pdf" width="45%" />
</p>


```R
# save the tables into a text file named 'covid19-SummaryReport_CURRENTDATE.txt'
# where *CURRRENTDATE* is the actual date
report.summary(saveReport=TRUE)
```

<object data="man/figures/covid19-SummaryReport.pdf " type="application/pdf" width="100%" height="500px">
 <embed src="man/figures/covid19-SummaryReport.pdf ">
 <p> 
  E.g. today's report is available <a href="https://github.com/mponce0/covid19.analytics/blob/master/man/figures/covid19-SummaryReport.txt">here</a> 
 </p>
 </embed>
</object>


```R
# summary report for an specific location with default number of entries
report.summary(geo.loc="Canada")

# summary report for an specific location with top 5
report.summary(Nentries=5, geo.loc="Canada")

# it can combine several locations
report.summary(Nentries=30, geo.loc=c("Canada","US","Italy","Uruguay","Argentina"))
```



#### Totals per Country/Region/Province
```R
# totals for confirmed cases for "Ontario"
tots.per.location(covid19.confirmed.cases,geo.loc="Ontario")

# total for confirmed cases for "Canada"
tots.per.location(covid19.confirmed.cases,geo.loc="Canada")

# total nbr of deaths for "Mainland China"
tots.per.location(covid19.TS.deaths,geo.loc="China")

# total nbr of confirmed cases in Hubei including a confidence band based on moving average
tots.per.location(covid19.confirmed.cases,geo.loc="Hubei", confBnd=TRUE)
```


<object data="man/figures/Hubei_totals.pdf" type="application/pdf" width="450px">
 <embed src="https://github.com/mponce0/covid19.analytics/blob/master/man/figures/Hubei_totals.pdf">
 </embed>
</object>
<object data="man/figures/Italy_totals.pdf" type="application/pdf" width="450px">
 <embed src="https://github.com/mponce0/covid19.analytics/blob/master/man/figures/Italy_totals.pdf"> 
 </embed>
</object>
<object data="man/figures/Germany_totals.pdf" type="application/pdf" width="450px">
 <embed src="https://github.com/mponce0/covid19.analytics/blob/master/man/figures/Germany_totals.pdf">
 </embed>
</object>
<object data="man/figures/Ontario_totals.pdf" type="application/pdf" width="450px">
 <embed src="https://github.com/mponce0/covid19.analytics/blob/master/man/figures/Ontario_totals.pdf">
<!--
<p>
  <img src="man/figures/Hubei_totals.png" width="24%" >
  <img src="man/figures/Italy_totals.png" width="24%" >
  <img src="man/figures/Germany_totals.png" width="24%" >
  <img src="man/figures/Ontario_totals.png" width="24%" >
</p>
-->
  <p>
  Images available <a href="https://github.com/mponce0/covid19.analytics/tree/master/man/figures/">here</a> 
 </p>
 </embed>
</object>

<!--
<p>
  <img src="man/figures/Hubei_totals.pdf" width="24%" />
  <img src="man/figures/Italy_totals.pdf" width="24%" />
  <img src="man/figures/Germany_totals.pdf" width="24%" />
  <img src="man/figures/Ontario_totals.pdf" width="24%" />
</p>
-->


The figures show the total number of cases for different cities (provinces/regions) and countries:
one the upper plot in log-scale with a linear fit to an exponential law and in linear scale in the bottom panel.
Details about the models are included in the plot, in particular the *growth rate* which in several cases appears to be around 1.2+ as predicted by some models.
Notice that in the case of Hubei, the values is closer to 1, as the dispersion of the virus has reached its logistic asymptote while in other cases (e.g. Germany and Italy --for the presented dates--) is still well above 1, indicating its exponential growth.


**IMPORTANT** Please notice that the "linear exponential" modelling function implements a *simple (naive)* and straight-forward linear regression model, which is **not** optimal for exponential fits.
The reason is that the errors for large values of the dependent variable weight much more than those for small values when apply the exponential function to go back to the original model.
Nevertheless for the sake of a quick interpretation is OK, but one should bare in mind the implications of this simplification.


We also provide two additional models, as shown in the figures above, using the Generalized Linear Model `glm()` function, using a *Poisson* and *Gamma* family function.
In particular, the `tots.per.location` function will determine when is possible to automatically generate each model and display the information in the plot as well as details of the models in the console.


```R
# read the time series data for all the cases
all.data <- covid19.data('ts-ALL')

# run on all the cases
tots.per.location(all.data,"Japan")
```
<p>
  <img src="man/figures/Japan_confirmed.pdf" width="32.5%" />
  <img src="man/figures/Japan_recovered.pdf" width="32.5%" />
  <img src="man/figures/Japan_deaths.pdf" width="32.5%" />
</p>


It is also possible to run the `tots.per.location` (and `growth.rate`) functions,
on the whole data set, for which a quite large but complete mosaic figure will
be generated, e.g.
```R
# total for death cases for "ALL" the regions
tots.per.location(covid19.TS.deaths)

# or just
tots.per.location(covid19.data("ts-confirmed"))
```



#### Growth Rate
```R
# read time series data for confirmed cases
TS.data <- covid19.data("ts-confirmed")

# compute changes and growth rates per location for all the countries
growth.rate(TS.data)

# compute changes and growth rates per location for 'Italy'
growth.rate(TS.data,geo.loc="Italy")

# compute changes and growth rates per location for 'Italy' and 'Germany'
growth.rate(TS.data,geo.loc=c("Italy","Germany"))
```

<p>
  <img src="man/figures/gr-changes_Hubei.pdf" width="24%" />
  <img src="man/figures/gr-changes_Italy.pdf" width="24%" />
  <img src="man/figures/gr-changes_Germany.pdf" width="24%" />
  <img src="man/figures/gr-changes_Canada.pdf" width="24%" />
</p>

The previous figures show on the upper panel the number of changes on a daily basis in linear scale (thin line, left y-axis) and log scale (thicker line, right y-axis), while the bottom panel displays the growth rate for the given country/region/city.


Combining multiple geographical locations:
```R
# obtain Time Series data
TSconfirmed <- covid19.data("ts-confirmed")

# explore different combinations of regions/cities/countries
# when combining different locations, heatmaps will also be generated comparing the trends among these locations
growth.rate(TSconfirmed,geo.loc=c("Italy","Canada","Ontario","Quebec","Uruguay"))

growth.rate(TSconfirmed,geo.loc=c("Hubei","Italy","Spain","United States","Canada","Ontario","Quebec","Uruguay"))

growth.rate(TSconfirmed,geo.loc=c("Hubei","Italy","Spain","US","Canada","Ontario","Quebec","Uruguay"))
```

<p>
  <img src="man/figures/changes-per-day-1.pdf" width="22.5%" />
  <img src="man/figures/changes-per-day-2.pdf" width="22.5%" />
  <img src="man/figures/heatmap-changes.pdf" width="22.5%" />
  <img src="man/figures/heatmap-growthRate.pdf" width="22.5%" />
</p>


#### Trends
```R
# single location trend, in this case using data from the City of Tornto
tor.data <- covid19.Toronto.data()
single.trend(tor.data[tor.data$status=="Active Cases",])

# or data from the province of Ontario
ts.data <- covid19.data("ts-confirmed")
ont.data <- ts.data[ ts.data$Province.State == "Ontario",]
single.trend(ont.data)

# or from Italy
single.trend(ts.data[ ts.data$Country.Region=="Italy",])


# multiple locations
ts.data <- covid19.data("ts-confirmed")
mtrends(ts.data, geo.loc=c("Canada","Ontario","Uruguay","Italy")

# multiple cases
mtrends(tor.data)


# interactive plot of trends
# for all locations and all type of cases
itrends(covid19.data("ts-ALL"),geo.loc="ALL")

# or just for confirmed cases and some specific locations, saving the result in an HTML file named "itrends_ex.html"
itrends(covid19.data("ts-confirmed"), geo.loc=c("Uruguay","Argentina","Ontario","US","Italy","Hubei"), fileName="itrends_ex")

# interactive trend for Toronto cases
itrends(tor.data[,-ncol(tor.data)])
```

<p>
  <img src="man/figures/trendTor.pdf" width="40%" />
  <object data="man/figures/itrends_ex.html" width="58.5%" height="525"></object>
</p>



#### Visualization Tools
```R
# retrieve time series data
TS.data <- covid19.data("ts-ALL")

# static and interactive plot 
totals.plt(TS.data)
```
<object data="man/figures/totals.html" width="80%" height="525">
</object>

```R
# totals for Ontario and Canada, without displaying totals and one plot per page
totals.plt(TS.data, c("Canada","Ontario"), with.totals=FALSE,one.plt.per.page=TRUE)

# totals for Ontario, Canada, Italy and Uruguay; including global totals with the linear and semi-log plots arranged one next to the other
totals.plt(TS.data, c("Canada","Ontario","Italy","Uruguay"), with.totals=TRUE,one.plt.per.page=FALSE)

# totals for all the locations reported on the dataset, interactive plot will be saved as "totals-all.html"
totals.plt(TS.data, "ALL", fileName="totals-all")
```

<object data="man/figures/totals-all.html" width="100%" height="600">
</object>


```R
# retrieve aggregated data
data <- covid19.data("aggregated")

# interactive map of aggregated cases -- with more spatial resolution
live.map(data)

# or
live.map()

# interactive map of the time series data of the confirmed cases with less spatial resolution, ie. aggregated by country
live.map(covid19.data("ts-confirmed"))

```
<p>
Interactive examples can be seen at
    <a href="https://mponce0.github.io/covid19.analytics/">https://mponce0.github.io/covid19.analytics/</a>
</p>


#### Simulating the Virus spread
```R
# read time series data for confirmed cases
data <- covid19.data("ts-confirmed")

# run a SIR model for a given geographical location
generate.SIR.model(data,"Hubei", t0=1,t1=15)
generate.SIR.model(data,"Germany",tot.population=83149300)
generate.SIR.model(data,"Uruguay", tot.population=3500000)
generate.SIR.model(data,"Ontario",tot.population=14570000)

# the function will aggregate data for a geographical location, like a country with multiple entries
generate.SIR.model(data,"Canada",tot.population=37590000)
```

<p>
  <img src="man/figures/SIR-model-Ontario.pdf" width="32.5%" />
  <img src="man/figures/SIR-model-Canada.pdf" width="32.5%" />
  <img src="man/figures/SIR-model-Germany.pdf" width="32.5%" />
</p>


```R
# modelling the spread for the whole world, storing the model and generating an interactive visualization
world.SIR.model <- generate.SIR.model(data,"ALL", t0=1,t1=15, tot.population=7.8e9, staticPlt=FALSE)
# plotting and visualizing the model
plt.SIR.model(world.SIR.model,"World",interactiveFig=TRUE,fileName="world.SIR.model")
```

<object data="man/figures/world.SIR.model.html" width="105%" height="525"></object>

</details>



## About  <a name="about"></a>
### Authors
* *Marcelo Ponce*: creator, author, mantainer and main developer of the package
* *Amit Sandhel*: contributor, main developer of the covid19.Explorer dashboard
* Community contributions are welcome and can be done via pull-requests


### Media and Press <a name="media"></a>

#### in the news
<details>
    <summary>Click to Expand/Collapse</summary>


##### University Resources
* The Bulletin Brief -- University of Toronto (UofT):
  https://mailchi.mp/9cea706971a2/bulletinbrief-april6-2020?e=caa3066921
* UofT Libraries:
	- Tutorials 
		https://mdl.library.utoronto.ca/covid-19/tutorials
	- Data & Statistical Sources 
		  https://mdl.library.utoronto.ca/covid-19/data
* Department of Statistics, Warwick University (UK):
  https://warwick.ac.uk/fac/sci/statistics/courses/offerholders-post-2020/welcome2020/package1
* ResCOVID-19 (FR):
  http://rescovid19.fr/db/outils.html

##### Compute Ontario, Compute Canada
<!--
* https://computeontario.ca/in-conversation-with-marcelo-ponce-about-his-covid19-analytics-r-package/
-->

##### Social media
* https://twitter.com/ComputeOntario/status/1245825891562917888
* https://twitter.com/ComputeOntario/status/1270736806724632576?s=20
* https://twitter.com/ComputeCanada/status/1246123408418426880
* https://twitter.com/paulchenz/status/1244799016736624640?s=20
* https://twitter.com/JamesBradley002/status/1247139312245899264?s=20
* https://twitter.com/hauselin/status/1247209180492169218?s=20
* https://twitter.com/Ssiamba/status/1271794279510409217?s=20
* https://m.facebook.com/nexacu/photos/a.133550136841673/1407169096146431/?type=3                                                                             


</details>


#### used in ...
<details>
    <summary>Click to Expand/Collapse</summary>

##### Publications
* C.M.Yeşilkanat, *"Spatio-temporal estimation of the daily cases of COVID-19 in worldwide using random forest machine learning algorithm"*, Chaos, Solitons & Fractals (2020); 140(110210) -- https://doi.org/10.1016/j.chaos.2020.110210
* M.Deldar et al., *"SIR Model for Estimations of the Coronavirus Epidemic Dynamics in Iran"*, Journal of Biostatistics and Epidemiology (2020); 6(2):101-106  --
https://doi.org/10.18502/jbe.v6i2.4872
* Hackenberger BK, *"From apparent to true - from frequency to distributions (II)"*, Croat Med J. (2020); 61(4):381-385  -- https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7480748/
* D.Mercatelli et al., *"Web tools to fight pandemics: the COVID-19 experience"*, Briefings in Bioinformatics (2020)  --
https://doi.org/10.1093/bib/bbaa261
* N.Hussain, B.Li, *"Using R-studio to examine the COVID-19 Patients in Pakistan Implementation of SIR Model on Cases"*, Int Journal of  Scientific Research in Multidisciplinary Studies (2020); 6(8):54-59  --
https://www.isroset.org/pub_paper/IJSRMS/9-IJSRMS-04417.pdf
* List of <a href="https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=%22covid19.analytics%22&btnG=" target="_blank">all publications using "covid19.analytics" (from Google Scholar)</a>


##### RECON: R Epidemics Consortium - COVID19 Challenge
* https://tasks.repidemicsconsortium.org/
<!-- https://www.repidemicsconsortium.org/2020-06-09-covid-challenge/ -->

##### CoronaWhy datasets
<!-- ~*~ -->
* http://datasets.coronawhy.org/dataset.xhtml?persistentId=doi:10.5072/FK2/MYQFTR
<!-- ~*~ -->

##### Dashboards
* https://shiny.cliffweaver.com/covid/  --  https://shiny.cliffweaver.com/covid/#section-about
* https://shiny.cliffweaver.com/covid_mobility/  --  https://shiny.cliffweaver.com/covid_mobility/#section-about
* https://covid19analytics.scinet.utoronto.ca


##### Other publications and studies
* Yadav et al., *"Analyzing the Current Status of India in Global Scenario with Reference to COVID-19 Pandemic"*,
Preprints (2020) --
 https://doi.org/10.20944/preprints202007.0001.v1
* M.Murali and R.Srinivasan, "Forecasting COVID-19 Confirmned Cases in India with Snaive, ETS, ARIMA Methods"*, (2020) --
  http://bulletinmonumental.com/gallery/4-sep2020.pdf
* https://www.researchgate.net/publication/341832722_An_Evaluation_of_the_Frameworks_for_Predicting_COVID-19_in_Nigeria_using_Data_Analytics
* Annex I -- RDA COVID-19 Epidemiology WG, *"Sharing COVID-19 Epidemiology Data"*, Research Data Alliance (2020) --
  https://doi.org/10.15497/rda00049
  * A.Claire C.et al., *"COVID-19 Surveillance Data and Models: Review and Analysis, Part 1"*, SSRN (Sept.2020) --
    http://dx.doi.org/10.2139/ssrn.3695335


##### Community & Tutorials
* Featured on "R-bloggers" - Top 40 CRAN packages (April 2020):
	https://www.r-bloggers.com/2020/05/april-2020-top-40-new-cran-packages/amp/                                                                                 
* Featured on "Eye on AI" papers review:
        https://www.eye-on.ai/ai-research-watch-papers/2020/9/7/202097-society-papers

* https://youtu.be/n-sCFsUw0yg

* https://www.kaggle.com/nishantbhadauria/r-covid19-analytics-tutorial-sir-model-maps-glms
* https://rstudio-pubs-static.s3.amazonaws.com/627247_4a5e9d5780844ca2bcddfdd13733cb67.html
* https://people.math.carleton.ca/~davecampbell/datasets/2020/07/24/covid-data-covid-19-govenrmne-of-canada-and-jhu-csse/
* https://theactuarialclub.com/2020/05/15/covid-19-analysis-modelling-visualization-using-r/
* https://stackoverflow.com/questions/63822239/get-r-data-frame-in-python-using-rpy2
* https://stackoverflow.com/questions/65155336/how-do-you-add-the-live-map-function-from-covid19-analytics-to-a-shiny-app
* http://bactra.org/weblog/1176.html
* https://rpubs.com/alexvezeau
* https://rpubs.com/MirzaWAhmed/689518
* https://rpubs.com/pawan_thapa
* https://rpubs.com/Drshunya
* https://rpubs.com/vegatroz
* https://forums.futura-sciences.com/programmation-langages-algorithmique/883064-r-data-science-analyse-de-donnees-covid19.html                               
* https://www.europeanvalley.es/noticias/analizamos-datos-del-covid19-con-r/
* https://medium.com/r-tutorials/how-to-get-daily-covid-19-data-using-r-25bde150df5e
* https://medium.com/analytics-vidhya/corona-19-visualizations-with-r-and-tableau-595296894ca7                                                                
* http://www.sanaitics.com/research-paper.aspx?id=112

<!--
</details>
-->


##### Stats
<!--
<details>
    <summary>Click to Expand/Collapse</summary>
-->
<!-- badges: start -->
<!-- CRAN stats -->
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version-last-release/covid19.analytics)](https://cran.r-project.org/package=covid19.analytics)
[![CRAN checks](https://cranchecks.info/badges/worst/covid19.analytics)](https://cranchecks.info/pkgs/covid19.analytics)
[![Downloads last.mnth](https://cranlogs.r-pkg.org/badges/covid19.analytics)](https://cran.r-project.org/package=covid19.analytics)
[![Downloads last.week](https://cranlogs.r-pkg.org/badges/last-week/covid19.analytics)](https://cran.r-project.org/package=covid19.analytics)
[![Downloads last.day](https://cranlogs.r-pkg.org/badges/last-day/covid19.analytics)](https://cran.r-project.org/package=covid19.analytics)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/covid19.analytics)](https://cran.r-project.org/package=covid19.analytics)
<!-- GitHub badges stats -->
![R language](https://img.shields.io/badge/R-276DC3?style=plastic&logo=r&logoColor=white)
![Maintained](https://img.shields.io/badge/Maintained%3F-yes-green.svg)
![License](https://img.shields.io/github/license/mponce0/covid19.analytics.svg)
![Release](https://img.shields.io/github/release/mponce0/covid19.analytics.svg)
![Commits](https://img.shields.io/github/commits-since/mponce0/covid19.analytics/latest.svg)
![GitHub Downloads](https://img.shields.io/github/downloads/mponce0/covid19.analytics/total?color=blue&label=GitHub%20downloads)
![GitHub All releases](https://img.shields.io/github/downloads/mponce0/covid19.analytics/total?color=blue&style=plastic)
![GitHub release (latest by date including pre-releases)](https://img.shields.io/github/downloads-pre/mponce0/covid19.analytics/latest/total?color=blue&style=plastic)
![GitHub release (latest by SemVer)](https://img.shields.io/github/downloads/mponce0/covid19.analytics/latest/total?color=blue&style=plastic)
![Downloads](https://img.shields.io/github/downloads/mponce0/covid19.analytics/total.svg)
<!--
![](https://starchart.cc/mponce0/covid19.analytics.svg)
![GHusrStats](https://github-readme-stats.vercel.app/api?username=mponce0&theme=blue-green)
![GHusrStats](https://github-readme-stats.vercel.app/api/top-langs/?username=mponce0&theme=blue-green)
-->
<!-- badges: end -->

<!--
<p align="center">
	<img src="https://github.com/mponce0/R.pckgs.stats/blob/master/DWNLDS_covid19.analytics.png" width="65%" alt="Download stats"/>
-->	<!-- <img src="https://github.com/mponce0/R.pckgs.stats/blob/master/DWNLDS_covid19.analytics.pdf" width="65%" alt="Download stats"/> -->
<!--	<figcaption>"Live" download stats, figure generated using "Visualize.CRAN.Downloads"</figcaption>
</p>
-->

<!--
<img src="https://github.com/mponce0/R.pckgs.stats/blob/master/DWNLDS_covid19.analytics.png" width="800" />
![](https://github.com/mponce0/R.pckgs.stats/blob/master/DWNLDS_covid19.analytics.png)

<img align="left" src="https://github.com/mponce0/R.pckgs.stats/blob/master/DWNLDS_covid19.analytics.png" width="200" />
![](https://github.com/mponce0/R.pckgs.stats/blob/master/DWNLDS_covid19.analytics.png)

<p align='center'>
<img align="left" src="https://github.com/mponce0/R.pckgs.stats/blob/master/DWNLDS_covid19.analytics.png" width="40%" />
<img align="left" src="https://starchart.cc/mponce0/covid19.analytics.svg" width="50%" />
</p>
-->

<!--
<object data="https://github.com/mponce0/R.pckgs.stats/raw/master/DWNLDS_covid19.analytics.pdf" type="application/pdf" width="100%" height="500px">
 <embed src="https://github.com/mponce0/R.pckgs.stats/raw/master/DWNLDS_covid19.analytics.pdf">
 <p> 
    <figcaption>"Live" download stats, figure generated using "Visualize.CRAN.Downloads"</figcaption>
 </p>
 </embed>
</object>
-->
<!--
<p>
<object data="https://github.com/mponce0/R.pckgs.stats/blob/master/DWNLDS_covid19.analytics.pdf" type="application/pdf" width="700px" height="700px">
</p>
-->


</details>




## References  <a name="references"></a>
(\*) Data can be upto 24 hs delayed wrt the latest updates.

<details>
    <summary>Click to Expand/Collapse</summary>

### Data Sources <a name="dataSrcs"></a>
[1] <a name="JHUrepo"></a> 2019 Novel CoronaVirus COVID-19 (2019-nCoV) Data Repository by
Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)
https://github.com/CSSEGISandData/COVID-19

[2] <a name="TORdata"></a> COVID-19: Status of Cases in Toronto -- City of Toronto
https://www.toronto.ca/home/covid-19/covid-19-latest-city-of-toronto-news/covid-19-status-of-cases-in-toronto/

[3] <a name="ODtor"></a> COVID-19: Open Data Toronto
https://open.toronto.ca/dataset/covid-19-cases-in-toronto/

[4] <a name="HealthCan"></a> COVID-19: Health Canada
https://health-infobase.canada.ca/covid-19/
<!--
 https://health-infobase.canada.ca/src/data/covidLive
 https://health-infobase.canada.ca/src/data/covidLive/covid19-download.csv
-->

[5] <a name="NCBIgenome"></a> Severe acute respiratory syndrome coronavirus 2 isolate Wuhan-Hu-1, complete genome
NCBI Reference Sequence: NC_045512.2
https://www.ncbi.nlm.nih.gov/nuccore/NC_045512.2

[6] <a name="OWIDvaccination"></a> COVID-19 Vaccination and Testing records from "Our World In Data" (OWID)
https://github.com/owid/
 
[7] <a name="VCpandemics"></a> Pandemics historical records from Visual Capitalist (and sources within)
 https://www.visualcapitalist.com/history-of-pandemics-deadliest/
 https://www.visualcapitalist.com/the-race-to-save-lives-comparing-vaccine-development-timelines/

</details>


### How to Cite this Package  <a name="citation"></a>
If you are using this package please cite our main publication about the `covid19.analytics` package:

Ponce et al., (2021).
*"covid19.analytics: An R Package to Obtain, Analyze and Visualize Data from the 2019 Coronavirus Disease Pandemic."*
**Journal of Open Source Software, 6(59), 2995.**
  https://doi.org/10.21105/joss.02995


You can also ask for this citation information in R:
```R
> citation("covid19.analytics")

To cite covid19.analytics in publications use:

  Ponce et al., (2021). covid19.analytics: An R Package to Obtain,
  Analyze and Visualize Data from the 2019 Coronavirus Disease Pandemic.
  Journal of Open Source Software, 6(59), 2995.
  https://doi.org/10.21105/joss.02995


A BibTeX entry for LaTeX users is

  @Article{covid19analytics_JOSS,
    title = "{covid19.analytics: An R Package to Obtain, Analyze and Visualize Data from the Coronavirus Disease Pandemic}",
    author = {Marcelo Ponce and Amit Sandhel},
    journal = "{Journal of Open Source Software}",
    year = {2021},
    vol = {6},
    doi = {10.21105/joss.02995}
  }
```


Examples and tutorials are available at,
```R

  Marcelo Ponce, Amit Sandhel (2020). covid19.analytics: An R Package
  to Obtain, Analyze and Visualize Data from the Coronavirus Disease
  Pandemic. URL https://arxiv.org/abs/2009.01091

A BibTeX entry for LaTeX users is

  @Article{covid19analytics_RefManual,
    title = "{covid19.analytics: An R Package to Obtain, Analyze and Visualize Data from the Coronavirus Disease Pandemic}",
    author = {Marcelo Ponce and Amit Sandhel},
    journal = {pre-print},
    year = {2020},
    url = {https://arxiv.org/abs/2009.01091},
  }
```




## Further Resources
<details>
    <summary>Click to Expand/Collapse</summary>

<p aling="center">
 <img src="https://phil.cdc.gov//PHIL_Images/2871/2871_lores.jpg" width="75%">
 <br>
 <a href="https://phil.cdc.gov/Details.aspx?pid=2871" target="_blank">Source-Credit: CDC/ Alissa Eckert, MS; Dan Higgins, MAMS</a>
</p>


### Other References
* Delamater PL, Street EJ, Leslie TF, Yang Y, Jacobsen KH.
Complexity of the Basic Reproduction Number (R0).
Emerg Infect Dis. 2019;25(1):1-4.
https://dx.doi.org/10.3201/eid2501.171901
https://wwwnc.cdc.gov/eid/article/25/1/17-1901_article


### More R Resources
* The R Epidemics Consortium (RECON): https://www.repidemicsconsortium.org/
* SIR model: https://blog.ephorie.de/epidemiology-how-contagious-is-novel-coronavirus-2019-ncov
* EpiModel: https://rviews.rstudio.com/2020/03/19/simulating-covid-19-interventions-with-r/
* https://rviews.rstudio.com/2020/03/05/covid-19-epidemiology-with-r/

<!--
### Explanatory Videos
* Exponential Growth and Epidemics, by 3Blue1Brown:  https://www.youtube.com/watch?v=Kas0tIxDvrg
* Simulating an epidemic, by 3Blue1Brown:  https://www.youtube.com/watch?v=gxAaO2rsdIs
-->

### Dashboards
* https://covidgraph.com
<!-- * https://ici.radio-canada.ca/info/2020/coronavirus-covid-19-pandemie-cas-carte-maladie-symptomes-propagation/ -->
* https://ici.radio-canada.ca/info/2020/coronavirus-covid-19-pandemie-cas-carte-maladie-symptomes-propagation/index-en.html
* https://resources-covid19canada.hub.arcgis.com/
* https://aatishb.com/covidtrends/
* https://nextstrain.org/ncov
* http://gabgoh.github.io/COVID/index.html
* https://coronavirus.jhu.edu/map.html
* https://coronavirus.jhu.edu/data/new-cases
<!-- * https://schulich.yorku.ca/covid-19-dynamics/ -->

### Vaccines
* https://ici.radio-canada.ca/info/2021/03/vaccination-variants-covid-19-arn-pfizer-moderna-spicule-infection-immunite/en
* https://www.youtube.com/watch?v=K3odScka55A
* https://www.youtube.com/watch?v=mvA9gs5gxNY

### Models
* https://www.youtube.com/watch?v=wKOslhIFt6U

</details>
