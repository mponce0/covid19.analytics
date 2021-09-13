# covid19.analytics Dashboard implementation
#
# A.Sandhel/M.Ponce


#######################################################################



covid19dashboard <- function(locn=NULL) {
#' covid19.analytics explorer dashboard
#'
#' @param  locn geographical location to use as default
#'
#' @return  list with shinyApp UI and server
#'
#' @export
#'
#' @importFrom  shiny  h1 h2 h3 h4 h5 hr br tags HTML NS
#' @importFrom  shiny  selectInput checkboxInput numericInput sliderInput fluidRow column
#' @importFrom  shiny  radioButtons downloadButton downloadHandler icon tabPanel conditionalPanel verbatimTextOutput renderText
#' @importFrom  shiny  reactive callModule shinyApp
#' @importFrom  shinydashboard  renderValueBox
#' @importFrom  shinydashboard  sidebarMenu valueBox valueBoxOutput
#' @importFrom  shinycssloaders  withSpinner
#' @importFrom  shinydashboard  box menuItem tabBox tabItem tabItems dashboardBody dashboardHeader dashboardPage dashboardSidebar
#' @importFrom  plotly  renderPlotly plotlyOutput
#' @importFrom  DT  dataTableOutput renderDataTable
#' @importFrom  collapsibleTree  collapsibleTreeOutput collapsibleTreeSummary renderCollapsibleTree
#


#######

#Load packages 
loadLibrary('shiny')
loadLibrary('shinydashboard')
# for including spinners when waiting...
loadLibrary('shinycssloaders')

loadLibrary('plotly')

#datatable library
loadLibrary('DT')
#to do mutate and difference function on dataframes
loadLibrary('dplyr')

#phylogenetic tree library
loadLibrary('ape')
#loadLibrary('phylocanvas')
loadLibrary('collapsibleTree')
loadLibrary('colorspace')

########

capitalize <- function(txt) {
# function to capitalize words

  # break text into words
  words <- strsplit(txt, " ")[[1]]

  return(
	paste(
		# process first letter
		toupper(substring(words, 1,1)),
		# process remaining letters
		tolower(substring(words, 2)),
		sep="", collapse=" ")
	)
}

CapitTxt <- function(txt) {
# function to capitalize words from a sentence

	return(sapply(txt, capitalize))
}


#########


#list of unique countries
category_list <- list("confirmed"="ts-confirmed", "recovered"="ts-recovered", "deaths"="ts-deaths", "all"="ts-ALL")
#get unique list of all lists of the country
ts.data <- covid19.data("ts-ALL")
TGTs.LOCns <- (unique(ts.data[pmatch("Country",names(ts.data))]))
#Add All option manually to dropdown to display all countries data to the end of the dropdown
continents <- c("Africa","America","SouthAmerica","NorthAmerica","CentralAmerica","Asia","Europe","Oceania")
TGTs.LOCns <- list(Countries=TGTs.LOCns[[1]], ALL=list("ALL"))
#print(typeof(TGTs.LOCns))

#TGTs.LOCns <- list(Continents=continents, Countries= TGTs.LOCns[[1]])
#TGTs.LOCns <- list(Continents=continents, Countries=c("ALL",TGTs.LOCns[[1]]))
#print (TGTs.LOCns)

#default setting value for the location drodpwon
if (is.null(locn)) {
	TGT.LOC <- "Ontario"
	TGT.RGN <- "Canada"
} else {
	TGT.RGN <- CapitTxt(checkGeoLoc(ts.data,locn))
}

#Add a continent dropdown this is a manual createion of all continents in the world ie the planet Earth
#continent_list <- list("North Amertica"=northAmerica, "south america"=southAmerica, "europe"="eur", "asia"="as", "ALL"="all")
continent_list <- continents

#header section
header <- dashboardHeader(title="COVID19.ANALYTICS Dashboard Explorer")#, titleWidth=450)

#sidebar section
sidebar <- dashboardSidebar(
		sidebarMenu(id="sidebar",
				menuItem("INDICATORS", tabName = "Dashboard", icon = icon("dashboard")),

				menuItem("GENOMIC", tabName = "menu_genome", badgeLabel = "experimental", badgeColor = "maroon" ),

				menuItem('MODELS', tabName = "menu_models", startExpanded = FALSE, icon=icon("calculator"),
					menuItem("SIR Model", tabName = "menu_sir_model", icon=icon("chart-line")),
					menuItem("Hospital PPE", tabName = "menu_ppe", icon=icon("notes-medical"))	#, badgeLabel = "experimental", badgeColor = "maroon" )
					),			
				menuItem('DATASETS & REPORTS', tabName = "menu_datasets", icon=icon("database"), startExpanded = FALSE,
					menuItem("World Data", tabName = "tbl_world_data", icon=icon("globe")),
					menuItem("Toronto Data", tabName = "menu_toronto", icon=icon("table")),
					menuItem("Data Integrity", tabName="data_integrity_an", icon=icon("search"), badgeLabel = "beta", badgeColor = "black" ),
					menuItem("Report", tabName = "menu_report", icon=icon("file"))
				),
				menuItem('REFERENCES', tabName = "menu_references",startExpanded = FALSE,
					menuItem("covid19.analytics repo", icon = icon("github"), href = "https://github.com/mponce0/covid19.analytics"),
					menuItem("Documentation", icon = icon("file-code-o"), href = "https://mponce0.github.io/covid19.analytics/"),
					menuItem("Paper", icon = icon("file"), href = "https://arxiv.org/abs/2009.01091"),
					#
					menuItem("Data Sources", icon=icon("file-code-o"),startExpanded = FALSE,
						menuItem("JHU CCSE data repository", icon = icon("file-code-o"), href = "https://github.com/CSSEGISandData/COVID-19"),
#						menuItem("City of Toronto", icon = icon("file-code-o"), href = "https://www.toronto.ca/home/covid-19/covid-19-latest-city-of-toronto-news/covid-19-status-of-cases-in-toronto/"),
						menuItem("City of Toronto", icon = icon("file-code-o"), href = "ihttps://www.toronto.ca/home/covid-19/covid-19-pandemic-data/"),
						menuItem("NCBI datasets", icon = icon("file-code-o"), href = "https://www.ncbi.nlm.nih.gov/nuccore/NC_045512.2")
					),
					menuItem("External Dashboards", icon=icon("file-code-o"),startExpanded = FALSE, 
						menuItem("Canada Only", icon = icon("file-code-o"), href = "https://art-bd.shinyapps.io/covid19canada/"),
						menuItem("Vaccine Progress Tracker", icon = icon("file-code-o"), href = "https://vac-lshtm.shinyapps.io/ncov_vaccine_landscape/"),
						menuItem("Healthcare in Ontario", icon = icon("file-code-o"), href = "https://www.covid-19-mc.ca/interactive-model"),
						menuItem("Dash2-petolau", icon = icon("file-code-o"), href = "https://petolau.shinyapps.io/coronadash/"),
						menuItem("Dash3-ncov", icon = icon("file-code-o"), href = "https://vac-lshtm.shinyapps.io/ncov_tracker/"),
						menuItem("John Hopkins", icon = icon("file-code-o"), href = "https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6")
					),
					menuItem("Resources", icon=icon("file-code-o"),
						menuItem("REddit", icon = icon("file-code-o"), href = "https://www.reddit.com/r/ID_News/comments/fedwpp/list_of_covid19_trackers_and_maps/"),
						menuItem("list of internationa ldashboard repo", icon = icon("file-code-o"), href = "https://github.com/CSSEGISandData/COVID-19/issues/576")
					)
				),
				menuItem('ABOUT US', tabName = "menu_aboutus")
			)
	)


#####UI MODULES ######

dashboardUI <- function(id) {
	ns <- NS(id)
	box(h1("Indicators"), width=12,
		"The visualizations below display different indicators of the CoVid19 pandemic.", br(),
		"Select the locations and categories of interest.", br(),
		fluidRow(
			column(3, selectInput(ns("geo_loc_select2"), h4("Locations"), choices=TGTs.LOCns, selected=TGT.RGN, multiple=TRUE)),
			#box(width=3, title="Locations", solidHeader=FALSE, collapsible=FALSE,
			#		selectInput(ns("geo_loc_select2"), h4(""), choices=TGTs.LOCns, selected=TGT.RGN, multiple=TRUE)),
			#column(3, selectInput(ns("continent_list"), h4("Continent"), choices=continent_list, selected=NULL, multiple=TRUE)),
			box(width=3, title="Continent", solidHeader=TRUE, collapsible=TRUE, collapsed=TRUE,
					selectInput(ns("continent_list"),h4(""), choices=continent_list, selected=NULL, multiple=TRUE)),
			#commented out we can delete this no need for this line
			#column(3, selectInput(ns("geo_loc_select2"), h4("Locations"), choices=TGTs.LOCns, selected=TGT.RGN, multiple=TRUE)),
			column(3, selectInput(ns("category_list2"), h4("Category"), choices=category_list)),
			column(3, checkboxInput(ns("with_totals_checkbox_totalplot2"), h4("Display Total"), FALSE))
		),
		tabBox(width=12,  
			tabPanel("ITrend Plot",
						"The plot represents the normalized trend in number of cases",
						"Daily changes vs total changes in a log plot, for the indicated regions", br(),br(), 
						withSpinner(
							plotlyOutput(ns("itrend_plot"),  height = "100%", width = '100%'),
							type=6,size=0.35 )
						),
			tabPanel("Total Plot",
						"Total number of cases per day", br(),br(),
						withSpinner(plotlyOutput(ns("total_plot"),  height = "100%", width = '100%'), type=6,size=0.35 )  ),
			tabPanel("Growth Rate",
						"Daily Changes and Growth Rate", br(), br(),
						withSpinner(plotlyOutput(ns("growthrate_plot"),  height = "100%", width = '100%'), type=6,size=0.35 )  ),
			tabPanel("Live Map", 
					"Live map is an interactive map displaying cases around the world", br(),
					column(4, radioButtons('plot_type', '', choices=c('Time Series', 'Aggregated', 'Both'), inline=TRUE)),
					column(4, checkboxInput(ns("sel_projection"), "Display Projection", FALSE)),
					column(4, checkboxInput(ns("sel_legend"), "Hide Legend", FALSE)),

					conditionalPanel(
						condition="input.plot_type == 'Time Series'",
						box(width=12, collapsible=TRUE, withSpinner(plotlyOutput(ns("ts_livemap"), height = "100%", width = '100%'), type=7,size=0.35 ) ) 
					),
					conditionalPanel(
						condition="input.plot_type == 'Aggregated'",
						box(width=12, collapsible=TRUE, withSpinner(plotlyOutput(ns("agg_livemap"), height = "100%", width = '100%'), type=7,size=0.35)  )
					),
					conditionalPanel(
						condition="input.plot_type == 'Both' ",
							column(4, radioButtons('layout_type', label='', choices=c('Horizontal', 'Vertical'), inline=TRUE)),
							conditionalPanel(
								condition="input.layout_type=='Horizontal'",
								box( width=12, 	
									box(width=6, collapsible=TRUE, withSpinner(plotlyOutput(ns("ts2_livemap"), height = "100%", width = '100%'), type=7,size=0.35 )  ),
									box(width=6, collapsible=TRUE, withSpinner(plotlyOutput(ns("agg2_livemap"), height = "100%", width = '100%'), type=7,size=0.35 )  )
								)
							),
							conditionalPanel(
								condition="input.layout_type=='Vertical'",
								box( width=12, 	
									box(width=12, collapsible=TRUE, withSpinner(plotlyOutput(ns("ts3_livemap"), height = "100%", width = '100%')),type=7,size=0.35  ),
									box(width=12, collapsible=TRUE, withSpinner(plotlyOutput(ns("agg3_livemap"), height = "100%", width = '100%')),type=7,size=0.35  )
								)
							)
					)
			),
			tabPanel("Mosaic",
				"Combined visualization of trends, totals and map distribution of cases",br(),
				box(width=6, collapsible=TRUE, withSpinner(plotlyOutput(ns("itrend_plot2"), height = "100%", width = '100%'),type=7,size=0.35 )  ),
				box(width=6, collapsible=TRUE, withSpinner(plotlyOutput(ns("total_plot2"), height = "100%", width = '100%'),type=7,size=0.35 )  ),					
				box(width=6, collapsible=TRUE, withSpinner(plotlyOutput(ns("ts4_livemap"), height = "100%", width = '100%'),type=7,size=0.35 )  ),
				box(width=6, collapsible=TRUE, withSpinner(plotlyOutput(ns("agg4_livemap"), height = "100%", width = '100%'),type=7,size=0.35 )  )
			)
		),
				tags$head(tags$style(HTML('
    				/* body  */
    				/*body of the background*/
                    .content-wrapper, .right-side { background-color: #2d3741  }

                    /*box body background color for boxes */
					.box-body {background-color: #f8f8f8}

					.nav-tabs {background: ;}
					                .nav-tabs-custom  {background-color: ;
					                                   border-color: ;
					                                   font-weight:bold; color: black                                                    
					                                   }
			        /* logo */
			        .skin-blue .main-header .logo {background-color: #343e48;}

			        /* logo when hovered */
			        .skin-blue .main-header .logo:hover {background-color: #343e48;}

			        /* navbar (rest of the header) */
			        .skin-blue .main-header .navbar {background-color: #343e48;}        

			        /* main sidebar */
			        .skin-blue .main-sidebar {background-color: #343e48;}

			        /* active selected tab in the sidebarmenu */
			        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{background-color: #343e48; color: white}


			        /* other links in the sidebarmenu */
			        .skin-blue .main-sidebar .sidebar .sidebar-menu a{background-color: #46505a; color: white;}

			        /* other links in the sidebarmenu when hovered */
			        .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{ background-color: #e6e6e6;}
			        /* toggle button when hovered  */                    
			         .skin-blue .main-header .navbar .sidebar-toggle:hover{background-color:  #e6e6e6;}

			        '
		        ))),
	)
}


SIRModelUI <- function(id) {
	ns <- NS(id)
	box(width=12,
		h1('SIR Model'),
		"Generate a SIR (Susceptible-Infected-Recovered) model", br(), 
		"",
		fluidRow(
			column(3, selectInput(ns("geo_loc_select3"), label=h4("Country"), choices=TGTs.LOCns, selected=TGT.RGN, multiple=FALSE)),
			#since the SIR model starts with Canada manually adding the default popuplation of Canada as the starting point
			column(3, numericInput(ns("txtbox_pop_num"), label = h4("Population"), min=100000, value = 375900000)),
			column(3, numericInput(ns("txtbox_t0"), label = h4("Start Date"), min=1, max=31, value = 1)),
			column(3, numericInput(ns("txtbox_t1"), label = h4("End Date"), min=1, max=31, value = 15)),
		),
		tabPanel("Box Plot", withSpinner(plotlyOutput(ns("plot_sir_model"), height = "100%", width = '100%')) ),
		tabPanel("sdfdsfsd")
	)
}



tableWorldDataUI <- function(id) {
	ns <- NS(id)
	box(width=12,
		h1('WORLD TABLE'), h4('World data of all covid cases across the globe'),
		column(4, selectInput(ns("category_list3"), label=h4("Category"), choices=category_list)),		
		column(4, downloadButton(ns('downloadData'), "Download")),
		withSpinner(dataTableOutput(ns("table_contents")))
	)
}

reportUI <- function(id) {
	ns <- NS(id)
	box(width=12, 
		h2("Report"),
		"This section can generate reports for the users based on the country and timeseries type selected by the user", br(), 
		"Clicking the download button will save the report displayed as a text file.",
		fluidRow(
				column(3, selectInput(ns("geo_loc_select6"), label=h4("Country"), choices=TGTs.LOCns, selected=TGT.RGN, multiple=TRUE)),
				column(3, selectInput(ns("ddl_TS"), label=h4("Time Series type"), choices=c("time series"="TS", "aggregate"="AGG", "all"="ALL"), selected="TS", multiple=FALSE)),
				column(3, numericInput(ns("txtbox_Nentries"), label = h4("Num of Entries"), min=1, max=183, value = 10)),
			),
		downloadButton(ns('downloadReport'), "Download Report"),
			withSpinner(verbatimTextOutput(ns("report_output_default"))),
	)
}

torontoUI <- function(id) {
	ns <- NS(id)
	box(width=12,
		h2('Toronto Latest Data Set'),
		valueBoxOutput(ns("vbox_d1"), width=3),
		valueBoxOutput(ns("vbox_d2"), width=3),
		valueBoxOutput(ns("vbox_d3"), width=3),
		valueBoxOutput(ns("vbox_total"), width=3),
		fluidRow(
			column(4, downloadButton(ns('download_Toronto_Data'), "Download")),
			box(width=12, withSpinner(dataTableOutput(ns("table_toronto"))) )
		)
	)

}


datareportUI <- function(id) {
        ns <- NS(id)
        box(width=12,
                h2("Data Integrity Report"),
                "Check the integrity and consistency of datasets", br(),
		"Type I inconsistency: when negative values are reported.", br(),
		"Type II inconsistency: when cumulative quantities decrease.", br(),
                "Clicking the download button will save the report displayed as a text file.",
                fluidRow(
				column(3, selectInput(ns("ddl_TS"), label=h4("Time Series type"),
					choices=c('Global ALL'="ts-ALL", 'Global Confirmed'="ts-confirmed", 'Global Recovered'="ts-recovered", 'Global Deaths'="ts-deaths", 'US Confirmed'="ts-confirmed-US",'US Deaths'="ts-deaths-US",'Toronto'="ts-Toronto"),
					selected="ts-ALL", multiple=FALSE)),
			),
		downloadButton(ns('downloadDataReport'), "Download Report"),
		withSpinner(verbatimTextOutput(ns("datareport_output_default"))),
        )
}


ppeUI <- function(id) {
	ns<- NS(id)
	box(width=12,
		h2('Personal Protective Equipment (PPE) Calculator'),
		#h3('Please notice that this is an EXPERIMENTAL feature currently under active development!',
		#	style = "font-family: 'Courier', serif; font-weight: 500; font-size: 500; text-shadow: 3px 3px 3px #aaa; line-height: 1;  color: #FF0000;"
		#),
		h5("This section is capable of calculating the amount of ",tags$i("Personal Protective Equipment (PPE)")," needed by a hospital to safely treat CoViD19 patients."),
		h5("This calculation is based on CDC's calculation [1]."),
		h5("Note that the CDC calculation was originally designed for Ebola but here it is converted to be used for CoViD19"),
		#h4("References:"),
		#h5("[1] ",tags$a(href="https://www.cdc.gov/vhf/ebola/healthcare-us/ppe/calculator.html", "CDC's Ebola PPE Calculator", target="_blank")),
		#h5(tags$a(href="https://www.cdc.gov/coronavirus/2019-ncov/hcp/ppe-strategy/burn-calculator.html","CDC's n-CoVid19 PPE Burn Calculator",target="_blank")),
		#h5("This Reference below the Healthcare Needs in Ontario is an interactive model to predict healthcare resources specifically in Ontario made and desinged by the University of Toronto and SunnyBrook Hopsital"),
		#h5(tags$a(href="https://www.covid-19-mc.ca/interactive-model","Healthcare Needs in Ontario", target="_blank")),
		hr(),
		br(),
		tabBox(width=12,  
			tabPanel("PPE", br(),
				box(width=12,
					h5("This panel breaks down the various amount of PPE needed for a single CoViD19 patient 
						based on various hospital staff and hospitalization duration as displayed in a stacked bar chart"),
					column(3, valueBoxOutput(ns("ppe_total"), width=12) ),
					column(3, numericInput(ns("num_doctor"), label = h4("Doctor"), min=1, max=20, value = 4)),
					column(3, numericInput(ns("num_nurse"), label = h4("Nurse"), min=1, max=20, value = 4)),
					column(3, numericInput(ns("num_observer"), label = h4("Trained Observer"), min=1, max=20, value = 4)),
					column(3, numericInput(ns("num_environ"), label = h4("Enivron"), min=1, max=20, value = 2)),
					column(3, numericInput(ns("num_proj_days"), label = h4("Hospitalization Duration"), min=1, max=100, value = 12)),
					box(width=12, collapsible=TRUE, withSpinner(plotlyOutput(ns("plot_stacked2"), height = "100%", width = '100%')) )
				)
			),
			tabPanel("PPE Advanced Settings", "", br(), 
				h5("This is the advanced setting pages to change each of the various settings. Estimated amount of PPE needed by Role per Shift."),
				h5("To use the advanced setting adjust the slder for each of the various roles to the desired required of PPE needed."),
				br(), br(),
				box(width=12, collapsible=TRUE, title='NURSES', solidHeader=TRUE,
					column(2, sliderInput(ns("num_gown"), label = "Gown", min=1, max=20, value = 2)),
					column(2, sliderInput(ns("num_coverall"), label = "Coverall", min=1, max=20, value = 2)),
					column(2, sliderInput(ns("num_glove"), label = "Glove", min=1, max=100, value = 12)),  
					column(2, sliderInput(ns("num_gloveexam"), label = "Glove Exam", min=1, max=100, value = 4)),
					column(2, sliderInput(ns("num_bootcover"), label = "Boot cover", min=1, max=100, value = 4)),  
					column(2, sliderInput(ns("num_apron"), label = "Apron", min=1, max=100, value = 2)),  
					column(2, sliderInput(ns("num_n95"), label = "N95 Mask", min=1, max=100, value = 2)),   
					column(2, sliderInput(ns("num_faceshield"), label = "Face Shield", min=1, max=100, value = 2)),  
					column(2, sliderInput(ns("num_papr"), label = "PAPR", min=1, max=100, value = 2)),  
					column(2, sliderInput(ns("num_paprbat"), label = "PAPR Battery", min=1, max=100, value = 2)),  
					column(2, sliderInput(ns("num_paprfilter"), label = "PAPR Filter", min=1, max=100, value = 2)) 
				),
				box(width=12, collapsible=TRUE, title='DOCTORS', solidHeader=TRUE,
					column(2, sliderInput(ns("num_docgown"), "Gown", min=1, max=100, value = 1)),
					column(2, sliderInput(ns("num_doccoverall"), label = "Coverall", min=1, max=100, value = 1)),
					column(2, sliderInput(ns("num_docglove"), label = "Glove", min=1, max=100, value = 2)),  
					column(2, sliderInput(ns("num_docgloveexam"), label = "Glove Exam", min=1, max=100, value = 2)),
					column(2, sliderInput(ns("num_docbootcover"), label = "Boot cover", min=1, max=100, value = 2)),  
					column(2, sliderInput(ns("num_docapron"), label = "Apron", min=1, max=100, value = 1)),  
					column(2, sliderInput(ns("num_docn95"), label = "N95 Mask", min=1, max=100, value = 1)),   
					column(2, sliderInput(ns("num_docfaceshield"), label = "Face Shield", min=1, max=100, value = 1)),  
					column(2, sliderInput(ns("num_docpapr"), label = "PAPR", min=1, max=100, value = 1)),  
					column(2, sliderInput(ns("num_docpaprbat"), label = "PAPR Battery", min=1, max=100, value = 1)),  
					column(2, sliderInput(ns("num_docpaprfilter"), label = "PAPR Filter", min=1, max=100, value = 1)) 
				),
				box(width=12, collapsible=TRUE, title='TRAINED OBSERVER', solidHeader=TRUE,
					column(2, sliderInput(ns("num_obsgown"), label = "Gown", min=1, max=100, value = 2)),
					column(2, sliderInput(ns("num_obscoverall"), label = "Coverall", min=0, max=100, value = 0)),
					column(2, sliderInput(ns("num_obsglove"), label = "Glove", min=1, max=100, value = 2)),  
					column(2, sliderInput(ns("num_obsgloveexam"), label = "Glove Exam", min=1, max=100, value = 2)),
					column(2, sliderInput(ns("num_obsbootcover"), label = "Boot cover", min=1, max=100, value = 2)),  
					column(2, sliderInput(ns("num_obsapron"), label = "Apron", min=0, max=100, value = 0)),  
					column(2, sliderInput(ns("num_obsn95"), label = "N95 Mask", min=0, max=100, value = 0)),   
					column(2, sliderInput(ns("num_obsfaceshield"), label = "Face Shield", min=1, max=100, value = 2)),  
					column(2, sliderInput(ns("num_obspapr"), label = "PAPR", min=0, max=100, value = 0)),  
					column(2, sliderInput(ns("num_obspaprbat"), label = "PAPR Battery", min=0, max=100, value = 0)),  
					column(2, sliderInput(ns("num_obspaprfilter"), label = "PAPR Filter", min=0, max=100, value = 0)) 
				),
				box(width=12, collapsible=TRUE, title='ENVIRONMENT SERVICES', solidHeader=TRUE,
					column(2, sliderInput(ns("num_envgown"), label = "Gown", min=1, max=100, value = 2)),
					column(2, sliderInput(ns("num_envcoverall"), label = "Coverall", min=1, max=100, value = 2)),
					column(2, sliderInput(ns("num_envglove"), label = "Glove", min=1, max=100, value = 12)),  
					column(2, sliderInput(ns("num_envgloveexam"), label = "Glove Exam", min=1, max=100, value = 12)),
					column(2, sliderInput(ns("num_envbootcover"), label = "Boot cover", min=1, max=100, value = 12)),  
					column(2, sliderInput(ns("num_envapron"), label = "Apron", min=1, max=100, value = 2)),  
					column(2, sliderInput(ns("num_envn95"), label = "N95 Mask", min=1, max=100, value = 2)),   
					column(2, sliderInput(ns("num_envfaceshield"), label = "Face Shield", min=1, max=100, value = 2)),  
					column(2, sliderInput(ns("num_envpapr"), label = "PAPR", min=1, max=100, value = 2)),  
					column(2, sliderInput(ns("num_envpaprbat"), label = "PAPR Battery", min=1, max=100, value = 4)),  
					column(2, sliderInput(ns("num_envpaprfilter"), label = "PAPR Filter", min=1, max=100, value = 2)) 
				)
			),
			tabPanel("Burn Rate Analysis", br(),
					h5("This section calculates the weekly 7 day ",tags$i("burn rate")," [2] for PPE for a hospital to determine how fast your facility uses PPE."),
					h5("NOTE if you receive a resupply of PPE do not add it to the calcuator as it will disrupt the calculation. Continue following oriingla supply entered in day 1."),
					h5("For restock enter a new calculation"),
					h5("To input the data click on the Advanced Setting tab and click the sections and add the data accordingly"),
					br(), br(),
				tabBox(width=12,
					tabPanel("Daily Usage", 
						h4("Number of boxes of PPE used per day"),
						box(width=12, collapsible=TRUE, withSpinner(plotlyOutput(ns("plot_burn_rate"), height = "100%", width = '100%')) )
					),
					tabPanel("remaining supply",
						h4("Number of days supply remaining"),
						box(width=12, collapsible=TRUE, withSpinner(plotlyOutput(ns("plot_remaining_supply"), height = "100%", width = '100%')) )
					),
					tabPanel("ppe per patient",
						h4("Boxes of PPE used per patient per day"),
						box(width=12, collapsible=TRUE, withSpinner(plotlyOutput(ns("plot_ppe_per_patient"), height = "100%", width = '100%')) )
					),
					tabPanel("Advanced Settings",
						box(width=12, collapsible=TRUE, title='Number of Covid19 Patients', solidHeader=TRUE,
							column(1, numericInput(ns("num_d1patients"), label = h4("Day1"), min=1, max=1000000, value = 20)),
							column(1, numericInput(ns("num_d2patients"), label = h4("Day2"), min=1, max=1000000, value = 20)),
							column(1, numericInput(ns("num_d3patients"), label = h4("Day3"), min=1, max=1000000, value = 28)),
							column(1, numericInput(ns("num_d4patients"), label = h4("Day4"), min=1, max=1000000, value = 26)),
							column(1, numericInput(ns("num_d5patients"), label = h4("Day5"), min=1, max=1000000, value = 35)),
							column(1, numericInput(ns("num_d6patients"), label = h4("Day6"), min=1, max=1000000, value = 0)),
							column(1, numericInput(ns("num_d7patients"), label = h4("Day7"), min=1, max=1000000, value = 0))
						),
						box(width=12, collapsible=TRUE, title='Gloves', solidHeader=TRUE, collapsed=TRUE,
							column(2, numericInput(ns("num_d1glove"), label = h4("Day1"), min=1, max=1000000, value = 1500)),
							column(2, numericInput(ns("num_d2glove"), label = h4("Day2"), min=1, max=1000000, value = 1300)),
							column(2, numericInput(ns("num_d3glove"), label = h4("Day3"), min=1, max=1000000, value = 1000)),
							column(2, numericInput(ns("num_d4glove"), label = h4("Day4"), min=1, max=1000000, value = 0)),
							column(2, numericInput(ns("num_d5glove"), label = h4("Day5"), min=1, max=1000000, value = 0)),
							column(2, numericInput(ns("num_d6glove"), label = h4("Day6"), min=1, max=1000000, value = 0)),
							column(2, numericInput(ns("num_d7glove"), label = h4("Day7"), min=1, max=1000000, value = 0))
						),
						box(width=12, collapsible=TRUE, title='N95 Mask', solidHeader=TRUE, collapsed=TRUE,
							column(2, numericInput(ns("num_d1n95"), label = h4("Day1"), min=1, max=1000000, value = 500)),
							column(2, numericInput(ns("num_d2n95"), label = h4("Day2"), min=1, max=1000000, value = 475)),
							column(2, numericInput(ns("num_d3n95"), label = h4("Day3"), min=1, max=1000000, value = 400)),
							column(2, numericInput(ns("num_d4n95"), label = h4("Day4"), min=1, max=1000000, value = 0)),
							column(2, numericInput(ns("num_d5n95"), label = h4("Day5"), min=1, max=1000000, value = 0)),
							column(2, numericInput(ns("num_d6n95"), label = h4("Day6"), min=1, max=1000000, value = 0)),
							column(2, numericInput(ns("num_d7n95"), label = h4("Day7"), min=1, max=1000000, value = 0))
						),
						box(width=12, collapsible=TRUE, title='Regular Mask', solidHeader=TRUE, collapsed=TRUE, 
							column(2, numericInput(ns("num_d1mask"), label = h4("Day1"), min=1, max=1000000, value = 500)),
							column(2, numericInput(ns("num_d2mask"), label = h4("Day2"), min=1, max=1000000, value = 450)),
							column(2, numericInput(ns("num_d3mask"), label = h4("Day3"), min=1, max=1000000, value = 400)),
							column(2, numericInput(ns("num_d4mask"), label = h4("Day4"), min=1, max=1000000, value = 0)),
							column(2, numericInput(ns("num_d5mask"), label = h4("Day5"), min=1, max=1000000, value = 0)),
							column(2, numericInput(ns("num_d6mask"), label = h4("Day6"), min=1, max=1000000, value = 0)),
							column(2, numericInput(ns("num_d7mask"), label = h4("Day7"), min=1, max=1000000, value = 0))
						),
						box(width=12, collapsible=TRUE, title='Gowns', solidHeader=TRUE,  collapsed=TRUE,
							column(2, numericInput(ns("num_d1gowns"), label = h4("Day1"), min=1, max=1000000, value = 500)),
							column(2, numericInput(ns("num_d2gowns"), label = h4("Day2"), min=1, max=1000000, value = 400)),
							column(2, numericInput(ns("num_d3gowns"), label = h4("Day3"), min=1, max=1000000, value = 380)),
							column(2, numericInput(ns("num_d4gowns"), label = h4("Day4"), min=1, max=1000000, value = 0)),
							column(2, numericInput(ns("num_d5gowns"), label = h4("Day5"), min=1, max=1000000, value = 0)),
							column(2, numericInput(ns("num_d6gowns"), label = h4("Day6"), min=1, max=1000000, value = 0)),
							column(2, numericInput(ns("num_d7gowns"), label = h4("Day7"), min=1, max=1000000, value = 0))
						),
						box(width=12, collapsible=TRUE, title='Respirator', solidHeader=TRUE,  collapsed=TRUE,
							column(2, numericInput(ns("num_d1resp"), label = h4("Day1"), min=1, max=1000000, value = 300)),
							column(2, numericInput(ns("num_d2resp"), label = h4("Day2"), min=1, max=1000000, value = 280)),
							column(2, numericInput(ns("num_d3resp"), label = h4("Day3"), min=1, max=1000000, value = 260)),
							column(2, numericInput(ns("num_d4resp"), label = h4("Day4"), min=1, max=1000000, value = 0)),
							column(2, numericInput(ns("num_d5resp"), label = h4("Day5"), min=1, max=1000000, value = 0)),
							column(2, numericInput(ns("num_d6resp"), label = h4("Day6"), min=1, max=1000000, value = 0)),
							column(2, numericInput(ns("num_d7resp"), label = h4("Day7"), min=1, max=1000000, value = 0))
						)
					)
				)	
			)
		),

		###################

                h4("References:"),
                h5("[1] ",tags$a(href="https://www.cdc.gov/vhf/ebola/healthcare-us/ppe/calculator.html", "CDC's Ebola PPE Calculator", target="_blank")),
                h5("[2] ",tags$a(href="https://www.cdc.gov/coronavirus/2019-ncov/hcp/ppe-strategy/burn-calculator.html","CDC's n-CoVid19 PPE Burn Calculator",target="_blank")),
                h5("The following reference to the ",tags$i("Healthcare Needs in Ontario")," is an interactive model to predict healthcare resources specifically in Ontario made and desinged by the University of Toronto and SunnyBrook Hospital"),
                h5(tags$a(href="https://www.covid-19-mc.ca/interactive-model","Healthcare Needs in Ontario", target="_blank"))

		###################
	)
}


GenomeUI <- function(id) {
        ns <- NS(id)
        box(width=12,
            h2("CoViD19 Grouping Tree"),
            h4("This page displays various trees of the Covid19 genome TODO need to add more descriptions "),
            column(6, selectInput(ns("ddl_genome"), label = "Nucleotide Length", c("29600 < nucleotides > 31000" ="2", "29600 < human hosted > 31000" = "3", "Host Type"="1" ), selected="2")),
	        withSpinner(collapsibleTreeOutput(ns("tree_plot"), height = "500px"), type=6,size=0.35)
        )
}


##############  S E R V E R M O D U L E S  #############
#function to dynmaically enerate title of plot box
DashboardServer <- function(input, output, session, result) {
	#this is a hardcoded implmentation of how the continent could adjust the 
	#country dropdown. Currently the country dropdown is hardcoded and changed manually
	#but mainly there is code to filter the country list depending on the continent
	#list selected
	output$itrend_plot <- output$itrend_plot2  <- renderPlotly({
		#itrend plot/chart
		#get selected drodown values
		#get the continent information
		clist <- input$continent_list
		#get the location dropdown if need be
		geo_loc <- input$geo_loc_select2
		#get the category list only one option can be selected
		category <- input$category_list2
		#get whether the total need to be displayed or not
		with_totals <- input$with_totals_checkbox_totalplot2
		itrends(covid19.data(category),geo.loc=unique(c(geo_loc, clist)), with.totals=with_totals, interactive.display=FALSE)
	})

	output$total_plot <- output$total_plot2 <- renderPlotly({
		#total plot
		#get the continent information
		clist <- input$continent_list
		geo_loc <- input$geo_loc_select2
		#get the category list only one option can be selected
		category <- input$category_list2
		#get whether the total need to be displayed or not
		with_totals <- input$with_totals_checkbox_totalplot2
		#get the raw data of the ts-ALL data
		TS.data <- covid19.data("ts-ALL")
		#plot the chart
		totals.plt(TS.data, geo.loc0=unique(c(geo_loc,clist)), with.totals=with_totals, interactive.display=FALSE)
	})

    output$growthrate_plot <- renderPlotly({
        	#growth rate chart
    		#get the continent information
			clist <- input$continent_list
            geo_loc <- input$geo_loc_select2
            #get the category list only one option can be selected
            category <- input$category_list2
            #get whether the total need to be displayed or not
            with_totals <- input$with_totals_checkbox_totalplot2
            #get the raw data of the ts-ALL data
            TS.data <- covid19.data("ts-confirmed")
            #plot th echart
            growth.rate(TS.data, unique(c(geo_loc, clist)), staticPlt=FALSE, interactiveFig=TRUE, interactive.display=FALSE)
    }) 

	################################33
	#livemap plot charts on the three possible commbinations
	output$ts_livemap  <- output$ts2_livemap <- output$ts3_livemap <- output$ts4_livemap <- renderPlotly({
		legend <- input$sel_legend
		projections <- input$sel_projection
		live.map(covid19.data("ts-confirmed"), interactive.display=FALSE, no.legend=legend, select.projctn=projections)
	}) 

	output$agg_livemap  <- output$agg2_livemap <- output$agg3_livemap <- output$agg4_livemap <- renderPlotly({
		legend <- input$sel_legend
		projections <- input$sel_projection
		live.map(covid19.data("aggregated"), interactive.display=FALSE, no.legend=legend, select.projctn=projections)
	}) 
	#for hte mosaic livemaps we are going to lock the legend and prjoection statically
	output$ts4_livemap <- renderPlotly({
		legend <- input$sel_legend
		projections <- input$sel_projection
		live.map(covid19.data("ts-confirmed"), interactive.display=FALSE, no.legend=TRUE, select.projctn=FALSE)
	}) 

	output$agg4_livemap <- renderPlotly({
		legend <- input$sel_legend
		projections <- input$sel_projection
		live.map(covid19.data("aggregated"), interactive.display=FALSE, no.legend=TRUE, select.projctn=FALSE)
	}) 
	 
	#########################
	
}


SIRModelServer <- function(input, output, session, result) {
	output$plot_sir_model <- renderPlotly({
		#read and extract the inputs for each piece
		country <- input$geo_loc_select3
		pop_num <- input$txtbox_pop_num
		t0 <- input$txtbox_t0
		t1 <- input$txtbox_t1
		data <- covid19.data("ts-confirmed")
		x <- generate.SIR.model(data, country, tot.population=pop_num)
		plt.SIR.model(x, interactive.display=FALSE, interactiveFig=TRUE, add.extras=TRUE)
	})
}

tableWorldDataServer <- function(input, output, session, result) {
#' @importFrom  utils  write.csv

	#render the world data table
	output$table_contents <- renderDataTable({
		#get the category list only one option can be selected
		category <- input$category_list3
		#add the category input selectd to the function cal
		ts.data <- covid19.data(category)
	},options = list(scrollX = TRUE) )

	data <- reactive({
		category <- input$category_list3
		ts.data <- covid19.data(category)
		return (ts.data)
	})

    output$downloadData <- downloadHandler(
	    filename = function() {
	      paste("data-", Sys.Date(), ".csv", sep = "")
	    },
	    content = function(file) {
	      write.csv(data(), file)
	    }
	  )
}


datareportServer <- function(input, output, session, result) {
	output$datareport_output_default <- renderText({
		ts <- input$ddl_TS
		datareport <- capture.output(details <- capture.output(data.checks(data=covid19.data(ts),datasetName=ts)),type='message')
		c(datareport,'\n\n\n',details)
		}, sep='\n')

	datareport <- reactive({
		ts <- input$ddl_TS
		datareport <- capture.output( details <- capture.output(data.checks(data=covid19.data(ts),datasetName=ts)),type='message')
		datareport <- c(datareport,'\n\n\n',details)
		return(datareport)
	})

	output$downloadDataReport <- downloadHandler(
            filename = function() {
              paste("DataIntegrityReport-", Sys.Date(),".txt", sep = "")
            },
            content = function(file) {
              writeLines(paste(datareport() ), file)
            }
        )
}


reportServer <- function(input, output, session, result) {
	output$report_output_default <- renderText({
		#extract the vairables of the inputs
		nentries <- input$txtbox_Nentries
		geo_loc <- input$geo_loc_select6
		ts <- input$ddl_TS
		capture.output(report.summary(graphical.output=FALSE, Nentries=nentries, geo.loc=geo_loc, cases.to.process=ts))}
		, sep='\n')

	report <- reactive({
		nentries <- input$txtbox_Nentries
		geo_loc <- input$geo_loc_select6
		ts <- input$ddl_TS
		report <- capture.output(report.summary(graphical.output=FALSE, Nentries=nentries, geo.loc=geo_loc, cases.to.process=ts))
		return (report)
	})
	
    output$downloadReport <- downloadHandler(
	    filename = function() {
	      paste("Report-", Sys.Date(),".txt", sep = "")
	    },
	    content = function(file) {
	      writeLines(paste(report() ), file)
	    }
	)
}


torontoServer <- function(input, output, session, result) {
#' @importFrom  utils  write.csv

	output$table_toronto <- renderDataTable({
		ts.data <- covid19.Toronto.data()
		# will reverse the order of the dates, so the latest values (dates) show up first
		#datesCols <- grep("-",names(ts.data))
		ts.data <- ts.data[with(ts.data,#order(c(1:4,length(ts.data),rev(datesCols))))]
						rev(1:length(ts.data)))]
		# add new cases row
		ts.data[4,2:(length(ts.data)-5)] <- -diff(as.numeric(ts.data[3,2:(length(ts.data)-4)]))
		ts.data[4,1] <- "New cases"
		ts.data[4,(length(ts.data)-4):length(ts.data)] <- ts.data[1,(length(ts.data)-4):length(ts.data)]
		ts.data <- as.data.frame(ts.data) 
	}, options = list(scrollX = TRUE, autoWidth = TRUE)  )

	data <- reactive({
		ts.data <- covid19.Toronto.data()
		return (ts.data)
	})

    output$download_Toronto_Data <- downloadHandler(
	    filename = function() {
	      paste("Toronto-", Sys.Date(), ".csv", sep = "")
	    },
	    content = function(file) {
	      write.csv(data(), file)
	    }
	)
	
	getx <- reactive({
		lenx <- length(data() )
		labelx <- data()[lenx-1]
		newlist <- list(labelx)
		b <-( as.list(sort(unlist(newlist))) )
		return(b)
	})

	output$vbox_d1 <- renderValueBox({
		b <- getx()
		#tags$p("90k", style = "font-size: 150%;")
	    valueBox(value=tags$p(paste('Death ', b[1], sep="\n"), style="font-size: 16pt"), width = 3, subtitle ='', color = 'red')
	})
	output$vbox_d2 <- renderValueBox({
		b <- getx()
	    valueBox(value=tags$p(paste('Active ', b[2], sep="\n"), style="font-size: 16pt"), width = 3, subtitle ='', color = 'yellow')
	})
	output$vbox_d3 <- renderValueBox({
		b <- getx()
	    valueBox(value=tags$p(paste('Recovered ', b[3], sep="\n"), style="font-size: 16pt"), width = 3, subtitle ='', color = 'green')
	})

	output$vbox_total <- renderValueBox({
		b <- getx()
		lav <- Reduce("+", b)
	    valueBox(value=tags$p(paste('Total Count ', lav, sep="\n"), style="font-size: 16pt"), width = 3, subtitle ='', color = 'blue')
	})
}


ppeServer <- function(input, output, session, result) {
#'
#' @importFrom  stats  lag
#' @importFrom  dplyr  mutate first

	#function to get the number of projected days user selected
	projdays <- reactive({
		days <- input$num_proj_days
		return (days)
	})

	#function to create dataset from default tab panel
	dataset <- reactive({
		data <- data.frame(
		      Name = c("gowns",
		               "coverall",
		               "glove",
		               "gloveexam",
		               "bootcover",
		               "apron",
		               "n95",
		               "faceshield",
		               "papr",
		               "paprbat",
		               "paprfilter"),
		      nurse = as.character(c(input$num_gown,
		                             input$num_coverall,
		                             input$num_glove,
		                             input$num_gloveexam,
		                             input$num_bootcover,
		                             input$num_apron,
		                             input$num_n95,
		                             input$num_faceshield,
		                             input$num_papr,
		                             input$num_paprbat,
		                             input$num_paprfilter
		                             )),
		      doctor = as.character(c(input$num_docgown,
		                             input$num_doccoverall,
		                             input$num_docglove,
		                             input$num_docgloveexam,
		                             input$num_docbootcover,
		                             input$num_docapron,
		                             input$num_docn95,
		                             input$num_docfaceshield,
		                             input$num_docpapr,
		                             input$num_docpaprbat,
		                             input$num_docpaprfilter
		                             )),
		      trained = as.character(c(input$num_obsgown,
		                             input$num_obscoverall,
		                             input$num_obsglove,
		                             input$num_obsgloveexam,
		                             input$num_obsbootcover,
		                             input$num_obsapron,
		                             input$num_obsn95,
		                             input$num_obsfaceshield,
		                             input$num_obspapr,
		                             input$num_obspaprbat,
		                             input$num_obspaprfilter
		                             )),
		      env = as.character(c(input$num_envgown,
		                             input$num_envcoverall,
		                             input$num_envglove,
		                             input$num_envgloveexam,
		                             input$num_envbootcover,
		                             input$num_envapron,
		                             input$num_envn95,
		                             input$num_envfaceshield,
		                             input$num_envpapr,
		                             input$num_envpaprbat,
		                             input$num_envpaprfilter
		                             )),
		      stringsAsFactors = FALSE)
		data$nurse = as.numeric(as.character(data$nurse))
		data$doctor = as.numeric(as.character(data$doctor))
		data$trained = as.numeric(as.character(data$trained))
		data$env = as.numeric(as.character(data$env))
		return (data)
	})

	

	#update the dataframe plot based on the input widget controls
	calc <- reactive({
		a <- projdays()
		plotDataFrame2 <- dataset()
		doctor <- as.numeric(input$num_doctor)
		nurse <- as.numeric(input$num_nurse)
		observ <- as.numeric(input$num_observer)
		environ <- as.numeric(input$num_environ)
		plotDataFrame2$nurse <- plotDataFrame2$nurse*as.numeric(a)*nurse
		plotDataFrame2$doctor <- plotDataFrame2$doctor*as.numeric(a)*doctor
		plotDataFrame2$trained <- plotDataFrame2$trained*as.numeric(a)*observ
		plotDataFrame2$env <- plotDataFrame2$env*as.numeric(a)*environ

		#print (plotDataFrame2)
		return(plotDataFrame2)
	})

	#render the chart using the data frme with the traces
	output$plot_stacked2 <- renderPlotly({
		fig2 <- plot_ly(data=calc(), x = ~Name, y = ~nurse, name='nurse', type='bar', marker = list(color = 'aqua') )
		fig2 <- fig2 %>% add_trace(y = ~doctor, name="doctor", marker = list(color = '#FF7F50'))
		fig2 <- fig2 %>% add_trace(y = ~trained, name="trained env", marker = list(color = '#7FFFD4'))
		fig2 <- fig2 %>% add_trace(y = ~env, name="env servies", marker = list(color = '#FFB6C1'))
		fig2 <- fig2 %>% layout(title = "consumption of material",
		            xaxis = list(title = "equipment Type"),
		            yaxis = list(title = "Amount NEeded"),
		            barmode='stack')	
		fig2
	})

	#function to output the total team in the valuebox
	output$ppe_total <- renderValueBox({
		doctor <- as.numeric(input$num_doctor)
		nurse <- as.numeric(input$num_nurse)
		observ <- as.numeric(input$num_observer)
		environ <- as.numeric(input$num_environ)
		total <- doctor + nurse + observ + environ

	    valueBox(value=tags$p(paste('Total Team: ', total, sep="\n"), style="font-size: 14pt"), width = 3, subtitle ='', color = 'aqua')
	})

	#function to create dataset from default tab panel
	dataset_burn_rate <- reactive({
		data <- data.frame(
		      Name = c("day1",
		               "day2",
		               "day3",
		               "day4",
		               "day5",
		               "day6",
		               "day7"),
		      patient = as.numeric(c(input$num_d1patients,
		      						 input$num_d2patients,
		      						 input$num_d3patients,
		      						 input$num_d4patients,
		      						 input$num_d5patients,
		      						 input$num_d6patients,
		      						 input$num_d7patients
		                             )),
		      n95 = as.numeric(c(input$num_d1n95,
		      						 input$num_d2n95,
		      						 input$num_d3n95,
		      						 input$num_d4n95,
		      						 input$num_d5n95,
		      						 input$num_d6n95,
		      						 input$num_d7n95
		                             )),
		      mask = as.numeric(c(input$num_d1mask,
		      						 input$num_d2mask,
		      						 input$num_d3mask,
		      						 input$num_d4mask,
		      						 input$num_d5mask,
		      						 input$num_d6mask,
		     						 input$num_d7mask
		                             )),
		      gowns = as.numeric(c(input$num_d1gowns,
		      						 input$num_d2gowns,
		      						 input$num_d3gowns,
		      						 input$num_d4gowns,
		      						 input$num_d5gowns,
		      						 input$num_d6gowns,
		     						 input$num_d7gowns
		                             )),
		      resp = as.numeric(c(input$num_d1resp,
		      						 input$num_d2resp,
		      						 input$num_d3resp,
		      						 input$num_d4resp,
		      						 input$num_d5resp,
		      						 input$num_d6resp,
		     						 input$num_d7resp
		                             )),
		      gloves = as.numeric(c(input$num_d1glove,
		      						 input$num_d2glove,
		      						 input$num_d3glove,
		      						 input$num_d4glove,
		      						 input$num_d5glove,
		      						 input$num_d6glove,
		     						 input$num_d7glove
		                             )),
		      stringsAsFactors = FALSE)
		return (data)
	})

	diff_dataset <- reactive ({
		#function to create the diff data frame
		d <- dataset_burn_rate()
		#convert all 0 to NA so that they dn't get graphed
		d[d == 0] <- NA 
		#print(d)
		#recreate the dataframe for charting libraries
		#this tells us the difference the amount of ppe used per day
		xxx <- data.frame(d$Name) %>% 
			mutate(glovediff = (d$gloves - lag(d$gloves, default = first(d$gloves)))*-1 ) %>%
			mutate(n95diff = (d$n95 - lag(d$n95, default = first(d$n95)))*-1 ) %>%
			mutate(maskdiff = (d$mask - lag(d$mask, default = first(d$mask)))*-1 ) %>%
			mutate(gownsdiff = (d$gowns - lag(d$gowns, default = first(d$gowns)))*-1 ) %>%
			mutate(respdiff = (d$resp - lag(d$resp, default = first(d$resp)))*-1 )
		
		#calculate the dataframe mean of each diff note tis will be the total with the zero included
		#aVG PPE/DAY
		#get the total number of values length in a column NOT NA
		col_length <- colSums(!is.na(xxx))
		#get the total mean
		gloves_total_mean <- mean(xxx$glovediff, na.rm=TRUE)
		n95_total_mean <- mean(xxx$n95diff, na.rm=TRUE)
		mask_total_mean <- mean(xxx$maskdiff, na.rm=TRUE)
		gown_total_mean <- mean(xxx$gownsdiff, na.rm=TRUE)
		resp_total_mean <- mean(xxx$respdiff, na.rm=TRUE)
		
		#get the true mean value
		glovemean <- (gloves_total_mean*col_length[2])/(col_length[2]-1)
		n95mean <- (n95_total_mean*col_length[3])/(col_length[3]-1)
		maskmean <- (mask_total_mean*col_length[4])/(col_length[4]-1)
		gownmean <- (gown_total_mean*col_length[5])/(col_length[5]-1)
		respmean <- (resp_total_mean*col_length[6])/(col_length[6]-1)

		#add a new mutation to the dataset of remaining equipment in days
		#get the amount of ppe left and remaining
		xxx <- mutate(xxx, gloveremaining = (d$gloves/glovemean))
		xxx <- mutate(xxx, n95remaining = (d$n95/n95mean))
		xxx <- mutate(xxx, maskremaining = (d$mask/maskmean))
		xxx <- mutate(xxx, gownremaining = (d$gowns/gownmean))
		xxx <- mutate(xxx, respremaining = (d$resp/respmean))

		xxx <- mutate(xxx, glove_per_patient = (xxx$glovediff/d$patient) )
		xxx <- mutate(xxx, n95_per_patient = (xxx$n95diff/d$patient) )
		xxx <- mutate(xxx, mask_per_patient = (xxx$maskdiff/d$patient) )
		xxx <- mutate(xxx, gown_per_patient = (xxx$gownsdiff/d$patient) )
		xxx <- mutate(xxx, resp_per_patient = (xxx$respdiff/d$patient) )

		#print (xxx)
		#print (col_length)

		return (xxx)
	})

	output$plot_burn_rate <- renderPlotly({
		xxx <- diff_dataset()
		fig2 <- plot_ly(data=xxx, x = ~d.Name, y = ~n95diff, name='N95', type='scatter', mode='lines')
		fig2 <- fig2 %>% add_trace(y = ~glovediff, name = 'Gloves', mode = 'lines')
		fig2 <- fig2 %>% add_trace(y = ~maskdiff, name = 'Masks', mode = 'lines') 
		fig2 <- fig2 %>% add_trace(y = ~gownsdiff, name = 'Gowns', mode = 'lines')
		fig2 <- fig2 %>% add_trace(y = ~respdiff, name = 'Respirator', mode = 'lines')
		#add labels, titles and information to the chart
		fig2 <- fig2 %>% layout(title = "Amount of PPE Equipment used per Day",
		            xaxis = list(title = "Day"), yaxis = list(title = "Amount Used")
		        )
		fig2
	})

	output$plot_remaining_supply <- renderPlotly({
		xxx <- diff_dataset()
		fig3 <- plot_ly(data=xxx, x = ~d.Name, y = ~n95remaining, name='N95', type='scatter', mode='lines')
		fig3 <- fig3 %>% add_trace(y = ~gloveremaining, name = 'Gloves', mode = 'lines')
		fig3 <- fig3 %>% add_trace(y = ~maskremaining, name = 'Masks', mode = 'lines') 
		fig3 <- fig3 %>% add_trace(y = ~gownremaining, name = 'Gowns', mode = 'lines')
		fig3 <- fig3 %>% add_trace(y = ~respremaining, name = 'Respirator', mode = 'lines')
		#add labels, titles and information to the chart
		fig3 <- fig3 %>% layout(title = "Calculated Number of Days Supply Remaining",
		            xaxis = list(title = "Day"), yaxis = list(title = "Amount Used")
		        )
		fig3
	})

		output$plot_ppe_per_patient <- renderPlotly({
		xxx <- diff_dataset()
		fig4 <- plot_ly(data=xxx, x = ~d.Name, y = ~n95_per_patient, name='N95', type='scatter', mode='lines')
		fig4 <- fig4 %>% add_trace(y = ~glove_per_patient, name = 'Gloves', mode = 'lines') 
		fig4 <- fig4 %>% add_trace(y = ~mask_per_patient, name = 'Masks', mode = 'lines') 
		fig4 <- fig4 %>% add_trace(y = ~gown_per_patient, name = 'Gowns', mode = 'lines')
		fig4 <- fig4 %>% add_trace(y = ~resp_per_patient, name = 'Respirator', mode = 'lines')
		#add labels, titles and information to the chart
		fig4 <- fig4 %>% layout(title = "Boxes of PPE used per Patient",
		            xaxis = list(title = "Day"), yaxis = list(title = "Amount Used")
		        )
		fig4
	})


}


GenomeServer <- function(input, output, session, result) {
  	#output$phylocanvas_tree <- renderPhylocanvas({
  		#tree <- covid19.genomic.data(type='ptree')
        # plot it using phylocanvas
        #print (tree)
        #phycanv <- phylocanvas(tree, treetype = "radial", alignlabels = TRUE,
        #	showscalebar=FALSE, showlabels=TRUE, 
        #	showhistory=FALSE, textsize=12)
		#phycanv
	#})
	output$tree_plot <- renderCollapsibleTree({
		print ('function ran')

		ddlgenome_select <- input$ddl_genome
		print (ddlgenome_select)

		# retrieve the nucleotides data
		nucx <- covid19.genomic.data(type='nucleotide',src='repo')

		# identify specific fields to look at
		len.fld <- "Length"
		acc.fld <- "Accession"
		geoLoc.fld <- "Geo_Location"
		seq.fld <- "Sequence_Type"
		host.fld <- "Host"

		seq.limit1 <- 25000

		seq.limit1 <- 29600
		seq.limit2 <- 31000

		# selection criteria, nucleotides with seq.length between 29600 and 31000
		selec.ctr.1 <- nucx$Length<seq.limit2 & nucx$Length>seq.limit1

		# remove nucletoides without specifying a "host"
		targets <- nucx[selec.ctr.1 & nucx$Host!='',]

		if (ddlgenome_select == "1") {
			#categorization tree based on Host-type, geographical location, ...
			collapsibleTreeSummary(targets,
			                hierarchy=c(host.fld,geoLoc.fld,acc.fld,c(len.fld,seq.fld)),
			                attribute=len.fld,
			                zoomable=FALSE, collapsed=TRUE)

		}
		else if (ddlgenome_select == "2") {
			collapsibleTreeSummary(nucx[!selec.ctr.1,],
            hierarchy=c(host.fld,geoLoc.fld,acc.fld,c(len.fld,seq.fld)),
            attribute=len.fld,
            zoomable=FALSE, collapsed=TRUE)
		}
		else {
			collapsibleTreeSummary(nucx[!selec.ctr.1 &  nucx$Host=='Homo sapiens',],
            hierarchy=c(host.fld,geoLoc.fld,acc.fld,c(len.fld,seq.fld)),
            attribute=len.fld,
            zoomable=FALSE, collapsed=TRUE)
		}

		

	})




			

}





######################################################################

#body of the dashboard 
body <- dashboardBody(
	tabItems(
		tabItem(tabName='Dashboard', 
			dashboardUI('Dashboard_UI')
		),
		tabItem(tabName="menu_sir_model",
			SIRModelUI("sir_model")
		),
		tabItem(tabName="menu_genome", 
			GenomeUI("genome_tree")
		),
		tabItem(tabName="tbl_world_data",
			tableWorldDataUI("world_table")
		),
		tabItem(tabName="menu_report",
			reportUI('report')
		),
		tabItem(tabName="menu_toronto",
			torontoUI("toronto")
		),
		tabItem(tabName="data_integrity_an",
			datareportUI("datareport")
		),
		tabItem(tabName="menu_aboutus", 
			box(h3("COVID19.ANALYTICS Dashboard EXPLORER",
				 style = "font-family: 'Courier', serif; font-weight: 500; font-size: 500; text-shadow: 3px 3px 3px #aaa; line-height: 1;  color: #404040;"
				)
			),

			box(
				h5("Hosted at ",tags$a(href="https://www.scinethpc.ca/research-scinet/","SciNet HPC: Advanced Reasearch Computing", target="_blank")," at the University of Toronto",
					style = "font-family: 'Courier', serif; font-weight: 500; font-size: 500;"
				),
				h5("Analytics and Data powered by the ",tags$a(href="https://mponce0.github.io/covid19.analytics/", "'covid19.analytics' R package", target="_blank"),
					 style = "font-family: 'Courier', serif; font-weight: 500; font-size: 500;"
				)
			),

                        box(
                                h3("Developers", style = "font-family: 'Courier', serif; font-weight: 500; font-size: 500; text-shadow: 3px 3px 3px #aaa; line-height: 1;  color: #404040;"),
				hr(),
                                h4(tags$a(href="https://scholar.google.com/citations?hl=en&user=Rm5d8HQAAAAJ&view_op=list_works&sortby=pubdate", "Amit Sandhel", target="_blank"), "amit.sandhel _AT_ gmail.com ",
					style = "font-family: 'Courier', serif; font-weight: 500; font-size: 500;"
				),
                                #h4(tags$a(href="https://scholar.google.com/citations?hl=en&user=Rm5d8HQAAAAJ&view_op=list_works&sortby=pubdate", "Publication List", target="_blank"),
                                #	style = "font-family: 'Courier', serif; font-weight: 500; font-size: 500;"
                                #),
                                #br(),
                                h4(tags$a(href="https://scholar.google.com/citations?hl=en&user=CKWPbQEAAAAJ&view_op=list_works&sortby=pubdate", "Marcelo Ponce", target="_blank"),
					style = "font-family: 'Courier', serif; font-weight: 500; font-size: 500;"
				)
			),

			box(
				h3("Acknowledgements",  style = "font-family: 'Courier', serif; font-weight: 500; font-size: 500;"),
				hr(),
				h5(tags$a(href="https://www.scinethpc.ca/research-scinet/","SciNet HPC: Advanced Reasearch Computing", target="_blank")," at the University of Toronto",
					 style = "font-family: 'Courier', serif; font-weight: 500; font-size: 500;"
				),
				h5("Marco Saldarriaga (SciNet/UofT), for all the help setting up this server",
					 style = "font-family: 'Courier', serif; font-weight: 500; font-size: 500;"
				)
			),
			box(h3("References",  style = "font-family: 'Courier', serif; font-weight: 500; font-size: 500;"),
				hr(),
				h5(tags$a(href="https://github.com/mponce0/covid19.analytics","covid19.analytics", target="_blank")," Package",
                                         style = "font-family: 'Courier', serif; font-weight: 500; font-size: 500;"
                                ),
				h5(tags$a(href="https://arxiv.org/abs/2009.01091", "covid19.analytics paper", target="_blank"),
					style = "font-family: 'Courier', serif; font-weight: 500; font-size: 500;"
				),
			)
		#	box(h2("DISCLAIMER")
		#		hr(),
		#		h5("The information presetned in this dashboard
		#	)
		),
		tabItem(tabName="menu_ppe",
			ppeUI("ppe_model")
		)
)) 





#ui design of dashboard
ui <- dashboardPage(
	header,
	sidebar,
	body,
)

server <- function(input, output) {
	#server function to call each of hte 
	callModule(DashboardServer, "Dashboard_UI")
	callModule(SIRModelServer, "sir_model")
	callModule(tableWorldDataServer, "world_table")
	callModule(reportServer, "report")
	callModule(torontoServer, "toronto")
	callModule(ppeServer, "ppe_model")
	callModule(datareportServer, "datareport")
	callModule(GenomeServer, "genome_tree")
}


#shinyApp(ui = ui, server = server)


	return(list(ui=ui,server=server))
}
