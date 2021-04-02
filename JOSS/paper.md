---
title: 'covid19.analytics: An R Package to Obtain, Analyze and Visualize Data from the 2019 Coronavirus Disease Pandemic'
tags:
  - R
  - CRAN
  - coronavirus
  - data access
authors:
  - name: Marcelo Ponce^[corresponding author]
    orcid: 0000-0001-5850-7240
    affiliation: "1" # (Multiple affiliations must be quoted)
  - name: Amit Sandhel
    affiliation: 2
affiliations:
 - name: SciNet HPC Consortium, University of Toronto, Canada
   index: 1
 - name: Independent Researcher
   index: 2
date: 21 January 2021
bibliography: references.bib
---

# Summary
With the emergence of a new pandemic worldwide, novel strategies attempting to understand its evolution and dynamics,
empowered by computational techniques such as data analysis and computational modelling, have also emerged.
Several initiatives under the umbrella of *open science* are contributing to
tackle this unprecedented situation.
In particular, the *R Language* [@citeR; @ihaka:1996] could be an excellent tool and ecosystem for
approaches focusing on open science and reproducible research.
Hence it is not surprising that with the onset of the pandemic, a large number
of R packages and resources were made available for researchers working in the
pandemic.

In this paper, we present the `covid19.analytics` R package which allows users
to access and analyze worldwide data from publicly available resources.

The `covid19.analytics` R package allows users to obtain
live^[The data usually is accessible from the repositories with a 24
hours delay.] worldwide data of reported cases from the novel *Coronavirus
Disease* (COVID-19), as well as related datasets, such as, genomics data,
testing and vaccinations records and historical pandemics records.
It does this by accessing and retrieving data publicly available and
published by several sources, such as:

- The "COVID-19 Data Repository by the Center for Systems Science and
  Engineering (CSSE) at Johns Hopkins University" [@JHUCSSErepo] for the
worldwide and US data
- Health Canada [@HealthCanada], for Canada specific data
- The city of Toronto for Toronto data [@TorontoData]
- Open Data Toronto for Toronto data [@OpenDataToronto]
- Our World In Data for testing and vaccination data [@OWIDvaccination]
- NCBI servers for genomics datasets [@NCBIdatabases]
- Visual Capitalist infographics for historical pandemic records [@VCpandemics]

The package also provides basic analysis and visualization tools and functions
to investigate these datasets and other ones structured in a similar fashion.



# Statement of need
One important element to consider in the case of an ongoing and unfolding pandemic,
is the rapid data accessibility and its prompt availability.
One of the main goals of the `covid19.analytics` package is to make the latest data
related to the COVID19 pandemic promptly available to researchers and the scientific
community.

As of today, there are a few dozen other packages also in the CRAN repository that
allow users to gain access to different datasets of the COVID-19 pandemic.
In some cases, some packages just provide access to data from specific
geographical locations or the approach to the data structure in which the
data is presented is different from the one presented here.
Nevertheless, having a variety of packages from which users can try and
probably combine, is an important and crucial element in data analysis.
Moreover, it has been reported that different cases of data misuse/misinterpretation
due to different issues, such as, erroneous metadata or data formats
[@Ledford_2020] and in some cases ending in articles' retractions [@Schriml:2020aa].
Therefore providing additional functionalities to check the integrity and
consistency of the data, as our `covid19.analytics` package does is paramount.
This is especially true in a situation where the unfolding of events and data
availability is flowing so fast that sometimes is even hard to keep track of
all the changes.

As of today and to the best of our knowledge, there isn't any other R package
that provides direct access to the COVID19 genomic data of the virus.
The `covid19.analytics` package is indeed capable of retrieving genomics data,
and it does that by incorporating a novel, more reliable and robust way of accessing and
designing different pathways to the data sources.

Moreover, the `covid19.analytics` package offers a modular and versatile approach to
the data by allowing users to input their own data for which most of the functions in the
package can be applied when the data is structured using a
*time series* format as described in the package documentation.



# Functionalities and Main Features

The `covid19.analytics` package is an open source tool, which its main goal is to offer
users prompt and quick access to different data sources related to the unfolding pandemics.
Its main implementation and API is the R package [@covid19analytics] which is available on CRAN [@cran].


In addition to this, the package has a few more add-ons:

- A central GitHub repository, <https://github.com/mponce0/covid19.analytics>
  where the latest development version and source code of the package are
available.
Users can also submit tickets for bugs, suggestions or comments using the "issues" tab from this repository.

- A rendered version with live examples and documentation also hosted at GitHub
  pages, <https://mponce0.github.io/covid19.analytics/>

- A dashboard for interactive usage of the package with extended capabilities
  for users without any coding expertise,
<https://covid19analytics.scinet.utoronto.ca>

- A backup data repository hosted at GitHub,
  <https://github.com/mponce0/covid19analytics.datasets> -- where replicas of
the live datasets are stored for redundancy and robust accessibility (see \autoref{fig:dataSrcs}).


![Schematic of the data acquisition flows between the `covid19.analytics` package and the different sources of data. Dark and solid/dashed lines represent API functions provided by the package accessible to the users. Dotted lines are "internal" mechanisms employed by the package to synchronize and update replicas of the data. \label{fig:dataSrcs}](./covid19-data-sources.pdf)


The package also incorporates a dashboard to facilitate the access to its
functionalities to less experienced users (see \autoref{fig:dashboard}).
This can be accessed interactively in a local session or through the
public deployed dashboard at the URL described above.

![Screenshot of a `covid19.analytics` dashboard implementation -- the dashboard can be used through a web-interface or deployed locally in the users' machine.\label{fig:dashboard}](./dashboard-mosaic.png)


Another unique feature of this package is the ability to incorporate models
to estimate the disease spread by using the actual data.
The `covid19.analytics` package allows users to model the dispersion of the disease by
implementing a simple *Susceptible-Infected-Recovered* (SIR) model
[@kermack1927contribution; @smith2004sir].
The package can also generate multiple SIR models by varying some of the parameters
used to select the actual data to be used in the model and some additional visualizations tools.

There are several plotting and visualization functions within the `covid19.analytics` package,
both generating static and interactive visualizations.


Further details, documentation, examples and tutorials of the `covid19.analytics` package can be found on the [package repository](https://github.com/mponce0/covid19.analytics) and [@ponce2020covid19analytics].


Users can submit bug reports, improvements requests or seek for support using the
["issues tracker"](https://github.com/mponce0/covid19.analytics/issues) tab from the GitHub repository.
Contributions from users and the community are welcome and can be done using
["pull requests"](https://github.com/mponce0/covid19.analytics/pulls) from the repository as well.
We will continue working on adding and developing new features to the package,
a glimpse and brief description about these can be found on the repository's
[Projects area](https://github.com/mponce0/covid19.analytics/projects).


# Acknowledgments
MP wants to thank all his colleagues at SciNet, especially Daniel Gruner for
his continuous and unconditional support, and Marco Saldarriaga who helped us
setting up the VM for installing the shiny server hosting the covid19.analytics
dashboard web interface.



# References
