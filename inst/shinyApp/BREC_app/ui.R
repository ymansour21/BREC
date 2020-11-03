library(shiny)
library(plotly)
# library(Brec)
library(DT)
library(utils)
library(shinydashboard)
library(shinyWidgets) #for prettyradiobuttons .. to ad infoicone .. not working
library(shinyjs)  # for onclick .. not working
library(shinydashboardPlus)
library(markdown)
library(knitr)
library(shinyAce)
library(sendmailR)
library(shinycssloaders)
library(shinyjs)
library(rlist)
library(tictoc)
library(stringr)
library(shinyalert)

mycss <- "
#select ~ .selectize-control .selectize-input {
  max-height: 100px;
  overflow-y: auto;
}
"


# =======================================================================================================================================
# dashboardHeaderdashboardHeaderdashboardHeaderdashboardHeaderdashboardHeaderdashboardHeaderdashboardHeaderdashboardHeaderdashboardHeader

header <- dashboardHeader(title = "BREC")

# ================================================================================================================================
# dashboardSidebardashboardSidebardashboardSidebardashboardSidebardashboardSidebardashboardSidebardashboardSidebardashboardSidebar

sidebar <- dashboardSidebar(

    sidebarMenu(
        menuItem("Home", tabName = "homeTab", icon = icon("home"), selected = TRUE),

        menuItem("Genomic data", tabName = "genomicDataTab", icon = icon("database "),
                 menuSubItem("Download Data files", tabName = "downloadDataTab", icon = icon("file-download ")),
                 menuItem("Dataset details", tabName = "datasetDetailsTab", icon = icon("database "))),

        menuItem("Run BREC", tabName = "runBrecTab", icon = icon("chart-line"),
                 menuSubItem('Heterochromatin boundaries', tabName = 'chromatinBoundariesTab', icon = icon('dna'))
                 # ,menuSubItem("Recombination rate estimator", tabName = "rrEstimatorTab", icon = icon("calculator"))
        )

        # ,menuSubItem('Use existing datasets', tabName = 'chromatinBoundariesTab', icon = icon('database')),
        # menuItem('Use import new dataset', tabName = 'importingDatasetTab', icon = icon('table'))),  #file-csv

        # menuItem("Download BREC package", tabName = "downloadTab", icon = icon("download ")),
        # menuItem("Install BREC locally", tabName = "installTab", icon = icon("code ")),
        # menuItem("Help", tabName = "helpTab", icon = icon("question-circle "))
        # ,menuItem("Contact us", tabName = "contactTab", icon = icon("envelope"))
    )
)

# ==================================================================================================================================
# dashboardBodydashboardBodydashboardBodydashboardBodydashboardBodydashboardBodydashboardBodydashboardBodydashboardBodydashboardBody

body <- dashboardBody(

    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),

    tabItems(
        tabItem(tabName = "homeTab",
                fluidPage(
                    box(title = strong("Welcome to BREC !"), status ="success", solidHeader = T, width = 800
                        ,h4(strong("An R package/Shiny app for automatically identifying heterochromatin boundaries and estimating local recombination rates along chromosomes")), br()
                    )
                    ,box(title = strong("BREC workflow"), status ="success", solidHeader = T, width = 800, height = 800,
                         imageOutput("pipelineImg1")
                    )
                    ,box(status ="success", solidHeader = F, width = 800, height = 800,
                         imageOutput("pipelineImg2")
                    )
                ),
                # infoBoxOutput("out1"),
                box(title = strong("The research project behind BREC"), status ="success", solidHeader = T, width = 800, height = 800,
                    # h4(strong("BREC")), br(),
                    h4(strong("Motivation"), br(), "Meiotic recombination is a vital biological process playing an essential role in genomes structural and functional dynamics. Genomes exhibit highly various recombination profiles along chromosomes associated with several chromatin states. However, eu-heterochromatin boundaries are not available nor easily provided for non-model organisms, especially for newly sequenced ones. Hence, we miss accurate local recombination rates, necessary to address evolutionary questions."), br(),
                    h4(strong("Results"), br(), "We propose an automated computational tool, based on the Marey maps method, allowing to identify heterochromatin boundaries along chromosomes and estimating local recombination rates. Our method, called", strong("BREC"),"(heterochromatin ",strong("B"),"oundaries and ",strong("REC"),"ombination rate estimates) is non-genome-specific, running even on non-model genomes as long as genetic and physical maps are available. BREC is based on pure statistics and is data-driven, implying that good input data quality remains a strong requirement. Therefore, a data pre-processing module (data quality control and cleaning) is provided. Experiments show that BREC handles different markers density and distribution issues. BREC′s heterochromatin boundaries have been validated with cytological equivalents experimentally generated on the fruit fly Drosophila melanogaster genome, for which BREC returns congruent corresponding values. Also, BREC's recombination rates have been compared with previously reported estimates. Based on the promising results, we believe our tool has the potential to help bring data science into the service of genome biology and evolution. We introduce BREC within an R-package and a Shiny web-based user-friendly application yielding a fast, easy-to-use, and broadly accessible resource."), br()
                    ,h3(strong("Preprint on bioRxiv | Bioinformatics"))
                    ,a(href = "https://www.biorxiv.org/content/10.1101/2020.06.29.178095v3", h4(strong("https://www.biorxiv.org/content/10.1101/2020.06.29.178095v3")))
                    ,h4("Manuscript first posted online: June 30th, 2020")
                    ,h4("Manuscript last updated: July 27th, 2020")
                    ,h3(strong("Peer-review at PLOS Computational Biology | Methods"))
                    ,h4("Manuscript first submission: July 27th, 2020")
                )

        ),

        tabItem(tabName = "chromatinBoundariesTab", #------------------------------------------------------------------------------------------
                fluidPage(
                    tabsetPanel( type = "pills",

                                 tabPanel(title = "One chromosome", br(),br(),#-------------------------
                                          fluidRow(
                                              box(title = "Input parameters ",  status ="primary", solidHeader = T,
                                                  prettyRadioButtons(inputId = "radioChooseMode", label = "Choose data source ", choices = c("existing datasets", "import new dataset"), selected = "existing datasets"),
                                                  uiOutput("contents")
                                                  # checkboxInput(inputId = "checkBoxDCQ", label = "Apply data quality control", value = FALSE),
                                              ),

                                              box(title = "Heterochromatin boundaries",status ="primary", solidHeader = T,
                                                  # withSpinner(
                                                  dataTableOutput("hcb1")
                                                  # )
                                                  ,
                                                  downloadButton('downloadResHCB1', 'Download')
                                              )
                                          ),
                                          box(title = "Summarizing plot", status ="primary", solidHeader = T, width = NULL, height = NULL,
                                              # withSpinner(
                                              plotlyOutput("rrplot1")
                                              # )#color="#0dc5c1"
                                              ,
                                              downloadButton('downloadPlot', 'Download plot')
                                          ),

                                          useShinyalert(),  # Set up shinyalert
                                          actionButton("btnAlert_chrType", "chromosomal arm"),

                                          box(title = "General statistics", status ="primary", solidHeader = T,width = NULL,
                                              # withSpinner(
                                              dataTableOutput("generalStats1"),
                                              # ),
                                              downloadButton('downloadStats1', 'Download'))
                                          # , hide(id="General statistics")
                                          # )

                                          # ,hide(id= "Summarizing plot")
                                 )
                                 # ,
                                 # tabPanel(title = "Whole genome", br(),br(),#-------------------------
                                 #      fluidRow(
                                 #          box(title = "Input parameters ",  status ="primary", solidHeader = T,
                                 #              prettyRadioButtons(inputId = "radioChooseModeW", label = "Choose data source ", choices = c("existing datasets", "import new dataset"), selected = "existing datasets"),
                                 #              uiOutput("contentsW")),
                                 #
                                 #          box(title = "General statistics", status ="primary", solidHeader = T,
                                 #              # withSpinner(
                                 #                  dataTableOutput("generalStatsW"),
                                 #                  # ),
                                 #              downloadButton('downloadStatsW', 'Download'))
                                 #          # , hide(id="General statistics")
                                 #          # )
                                 #      ),
                                 #
                                 #      box(title = "Heterochromatin boundaries for all chromosomes",status ="primary", solidHeader = T, width = NULL,
                                 #          # withSpinner(
                                 #              dataTableOutput("hcbW"),
                                 #          # ) ,
                                 #      downloadButton('downloadResHCBW', 'Download')),
                                 #
                                 #      box(title = "Recombination rates for whole genome",status ="primary", solidHeader = T, width = NULL,
                                 #          # withSpinner(
                                 #          dataTableOutput("rrW"),
                                 #          # )  ,
                                 #          downloadButton('downloadResRRW', 'Download'))
                                 # )
                    )
                )
        )
        ,
        tabItem(tabName = "rrEstimatorTab",#--------------------------------------------------------------------------------------------------
                box(title = "Recombination rates fo specific physical positions",status ="primary", solidHeader = T, width = NULL,
                    textInput(inputId = "inputPhysPos", label = "Type physicalposition (bp)", placeholder = "ex.: 1000 = 1Kb"),


                    verbatimTextOutput("getRRfrompp"),
                    downloadButton('downloadRRforPhysPos', 'Download')),
                box(title = "Recombination rates",status ="primary", solidHeader = T, width = NULL,
                    # withSpinner(
                    dataTableOutput("rr1"),
                    # )  ,
                    downloadButton('downloadResRR1', 'Download')),
                # iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii IDEA FOR VISUALLY INPUT POS OR REGION TO GET RR !! iiiiiiiiiiiiiiiiiiiiiii
                fluidRow(
                    column(4,

                           # Copy the line below to make a slider bar
                           sliderInput("slider1", label = h3("Slider"), min = 0,
                                       max = 100, value = 50)
                    ),
                    column(4,

                           # Copy the line below to make a slider range
                           sliderInput("slider2", label = h3("Slider Range"), min = 0,
                                       max = 100, value = c(40, 60))
                    )
                ),

                hr(),

                fluidRow(
                    column(4, verbatimTextOutput("value")),
                    column(4, verbatimTextOutput("range"))
                )
                # iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
                # ,fluidPage(
                #     tabsetPanel( type = "pills",
                #         tabPanel(title = "Choose from existing datasets", br(),br(),#-------------------------
                #               fluidRow(
                #                   box(title = "Input parameters ",  status ="primary", solidHeader = T,
                #                       selectInput(inputId = "genomeNameInput3", label = "Select a dataset",
                #                                   choices = c("Drosophila melanogaster R6", "Aedes aegypti", "Culex pipiens", "Anopheles gambiae", "Apis mellifera", "Homo sapiens"), selected = "Drosophila melanogaster R6"),
                #                       prettyRadioButtons(inputId = "radioChooseMode1", label = "Choose running mode ", choices = c("one chromosome", "whole genome -- !"), selected = "by chromosome"),
                #                       # icon = icon("question-circle "),
                #
                #                       uiOutput("chrOptions3"),
                #                       actionButton("goButton1" , "Go", icon = icon("arrow-circle-right "))
                #                   ),
                #                   box(title = "General statistics", status ="primary", solidHeader = T,
                #                       dataTableOutput("generalStats3") %>% withSpinner(color="#0dc5c1"),
                #                       downloadButton('downloadResults3', 'Download'))
                #               )
                #          )
                #          ,
                #         tabPanel(title = "Whole genome", br(),br()#-------------------------
                #         )
                #     )
                # )
        )
        ,
        tabItem(tabName = "downloadDataTab",#---------------????????---------------------------------------------------------------------------
                # fluidRow(
                sidebarLayout(
                    sidebarPanel(
                        # tags$div( verbatimTextOutput('selectedDataset'), class = "selectedDataset-content" )
                        verbatimTextOutput('selectedDataset')
                        ,
                        # column(3,
                        uiOutput("loadDatasetsList")
                        ,
                        # ),
                        # column(4,
                        downloadButton('downloadSelectedDS', 'Download selected'),
                        downloadButton('downloadAllDSs', 'Download all')
                        # )
                    ),
                    # fluidPage(dataTableOutput("datasetTable"))
                    # ),
                    mainPanel(
                        dataTableOutput("datasetTable")
                    )
                )
        )
        ,
        tabItem(tabName = "datasetDetailsTab",#---------------????????---------------------------------------------------------------------------
                fluidPage(
                    dataTableOutput("dataMetadata")
                )
        )
        ,
        tabItem(tabName = "downloadTab",#---------------????????---------------------------------------------------------------------------
                fluidPage(
                    uiOutput('downloadRmd')
                )
        )
        ,
        tabItem(tabName = "installTab",#---------------????????---------------------------------------------------------------------------
                fluidPage(
                    uiOutput('installRmd')
                )
        )
        ,
        tabItem(tabName = "helpTab",#--------------------------------------------------------------------------------------------------
                box()
                ,box()
        )
        # ,
        # tabItem(tabName = "contactTab",#-------------------------------------------------------------------------------------------------
        #         pageWithSidebar(
        #             headerPanel("Contact us"),
        #             sidebarPanel(
        #                 textInput("from", "From:", value="from@gmail.com"),
        #                 textInput("to", "To:", value="to@gmail.com"),
        #                 textInput("subject", "Subject:", value=""),
        #                 actionButton("send", "Send mail")
        #             ),
        #             mainPanel(
        #                 aceEditor("message", value="Write your message here")
        #             )
        #         )
        # )
    )
)

# ====================================================================================================================================
# uiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiui

ui <-  dashboardPage( header, sidebar, body, skin = "blue") #useShinyjs(debug = TRUE),   Set up shinyjs
