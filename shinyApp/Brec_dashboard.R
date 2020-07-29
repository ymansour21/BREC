library(shiny)
library(plotly)
library(Brec)
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

# ====================================================================================================================================
# serverserverserverserverserverserverserverserverserverserverserverserverserverserverserverserverserverserverserverserverserverserver

server <- function(input, output, session){

  # Global variables ***********************************************************************************************************

    datasetMetadata <- read.csv(paste0(getwd(), "/../datasets/Sackton_dataset_metadata_S13_Table.csv"), header = T, sep = "\t")
    # myds <- load(paste0(getwd(), "../datasets/raw_genetic_maps.RData"), envir = environment())


    allSpeciesData = list()

    datasetPath = paste0(getwd(), "/../data/")

    # read Sackton datasets = 40 genomes
    for (d in datasetMetadata$Abbreviation) {
        dataset <- read.csv(paste0(datasetPath, d, ".csv"), header = T, sep = "\t")
        allSpeciesData = list.append(allSpeciesData, dataset)
    }
    # add aditional genomes = 3 genomes : CpipJ3, AaegL4 and DmelR6
    additionalGenomes = c("DataAeAegypti3cols_OriginalFile", "DataCxQuainquefasciatusData3cols_OriginalFile_2", "Dmel_R6_formatted_v2")
    for(i in 1:3){
        dataset <- read.csv(paste0(datasetPath, additionalGenomes[i], ".csv"), header = T, sep = "\t")
        allSpeciesData = list.prepend(allSpeciesData, dataset)
    }

    choicesList = c("Drosophila melanogaster R6", "Culex pipiens", "Aedes aegypti",as.character(datasetMetadata$Species))

    # sort alphabetically

  # Reactive values ***********************************************************************************************************
    v1 <- reactiveValues(genomeName = "", inputData = NULL, chromosome = NULL, chrID = NULL,  BrecResultsList = NULL, spData = NULL, spIndex = NULL, alertResponse_chrType = NULL )


  # Render UIs ***********************************************************************************************************
    output$contents <- renderUI({

        if(input$radioChooseMode == "existing datasets"){
            list(
                selectInput(inputId = "genomeNameInput1", label = "Select a dataset", width = 400,
                            choices = choicesList, selected = choicesList[1]),
                selectInput(inputId = "physMapUnit" , label = "Physical map unit", choices = c("Mb", "bp"), width = 400, selected ="Mb"),
                selectInput(inputId = "chrIDInput1" , label = "Select chromosome ID", choices = NULL, width = 400),
                numericInput(inputId = "span", label = "Loess span value (min = 5%, max = 100%, step = 5%)", value = 15, min = 5, max = 100, step = 5, width = 400),
                checkboxInput(inputId = "checkBoxDcleaning", label = "Apply data cleaning", value = FALSE),
                actionButton("runButton1" , "Run", icon = icon("arrow-circle-right "))
            )
        }else if(input$radioChooseMode == "import new dataset"){
            list(
                fileInput(inputId = "inputdataFile", width = 400, label = "Choose dataset file (.csv or .txt)",
                          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                          # ,
                          # tags$hr(),
                          # checkboxInput("header", "Header", TRUE)
                          ),
                textInput(inputId = "genomNameInput2", label = "Type dataset name (optional)", placeholder = "ex.: Dmel R6 genome" , width = 400),
                selectInput(inputId = "gui_separator", label = "Select data separator", width = 400, choices = c("\\t", ",", ";"), selected = "\\t"),
                selectInput(inputId = "physMapUnit" , label = "Physical map unit", choices = c("Mb", "bp"), width = 400, selected ="Mb"),
                selectInput(inputId = "chrIDInput2" , label = "Select chromosome ID", choices = NULL, width = 400),
                numericInput(inputId = "span", label = "Loess span value (min = 5%, max = 100%, step = 5%)", value = 15, min = 5, max = 100, step = 5, width = 400),
                checkboxInput(inputId = "checkBoxDcleaning", label = "Apply data cleaning", value = FALSE),
                actionButton("runButton1" , "Run", icon = icon("arrow-circle-right "))
            )
        }
    })

    output$contentsW <- renderUI({    #Brec_genome

        if(input$radioChooseModeW == "existing datasets"){
            list(
                selectInput(inputId = "genomeNameInput1W", label = "Select a dataset",
                            choices = choicesList, selected = choicesList[1]),
                actionButton("runButton1W" , "Run", icon = icon("arrow-circle-right "))
            )
        }else if(input$radioChooseModeW == "import new dataset"){
            list(
                fileInput(inputId = "inputdataFileW", width = 400, label = "Choose dataset file (.csv or .txt)" ,
                          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                          # ,
                          # tags$hr(),
                          # checkboxInput("header", "Header", TRUE)
                ),
                textInput(inputId = "genomNameInput2W", label = "Type dataset name (optional)", placeholder = "ex.: Dmel R6 genome" , width = 400),
                selectInput(inputId = "gui_separatorW", label = "Select data separator", width = 400, choices = c("\\t", ",", ";"), selected = "\\t"),
                selectInput(inputId = "physMapUnit" , label = "Physical map unit", choices = c("Mb", "bp"), width = 400, selected ="Mb"),
                actionButton("runButton1W" , "Run", icon = icon("arrow-circle-right "))
            )
        }
    })

# **************************************************** one chromosme all observeEvent s*************************************************************
    observeEvent(c(input$genomeNameInput1, input$physMapUnit), {

        v1$spIndex = match(input$genomeNameInput1, choicesList)
        v1$spData = allSpeciesData[[v1$spIndex]]


        inputData = v1$spData
        # print(inputData)
         if(input$physMapUnit == "bp"){
              inputData$mb = inputData$mb/10^6  # convert to Mb without rounding
             # inputData$mb = round((inputData$mb / 10^6), 2)  # convert to Mb
         }
        # print(inputData)
         # chrList = get_list_of_chromosomes(inputData)


         # inputData <- read.table(rawDataFile, header = TRUE, sep = separator)
         chrList = get_list_of_chromosomes(inputData)
         # print(chrList)
    # 0000000000000000000000000000000000000000000000
         # inputData = transform_inputData_to_chromosomes(inputData, chrList)
         # chrList = get_list_of_chromosomes(inputData)
         # print("chrList 2 listing .. whole chromosomses")
         # print(chrList)
    # 0000000000000000000000000000000000000000000000
         updateSelectInput(session = session, inputId = "chrIDInput1" , choices = chrList, selected = chrList[1] )
         v1$inputData = inputData
         v1$genomeName = input$genomNameInput1
    })
    # -----------import new dataset-------------------------------------------------------------------------------------------------
     observeEvent( c(input$inputdataFile) ,{ # input$gui_separator, input$genomNameinput2), {
         inFile <- input$inputdataFile

         if (is.null(inFile))
             return(NULL)

         if(input$gui_separator == "\\t"){
             separator = "\t"
         }else{
             separator = input$gui_separator
         }
         inputData <-  read.csv(inFile$datapath, header = TRUE, sep = separator)   # read.table(rawDataFile$name, header = TRUE, sep = separator)

         if(input$physMapUnit == "bp"){
           inputData$mb = inputData$mb/10^6  # convert to Mb without rounding
           # inputData$mb = round((inputData$mb / 10^6), 2)  # convert to Mb
         }

         chrList = get_list_of_chromosomes(inputData)
         # print(chrList)
    # 0000000000000000000000000000000000000000000000
         # inputData = transform_inputData_to_chromosomes(inputData, chrList)
         # chrList = get_list_of_chromosomes(inputData)
         # print("chrList 2 listing .. whole chromosomses")
         # print(chrList)
    # 0000000000000000000000000000000000000000000000
         updateSelectInput(session = session, inputId = "chrIDInput2" , choices = chrList, selected = chrList[1] )
         v1$inputData = inputData
         v1$genomeName = input$genomNameInput2
        # }
    })

# **************************************************** whole_genome all observeEvent s*************************************************************
     observeEvent(input$genomeNameInput1W, {

         v1$spIndex = match(input$genomeNameInput1W, choicesList)
         v1$spData = allSpeciesData[[v1$spIndex]]

         inputData = v1$spData

         # if(physicalMapUnit != "Mb"){
         #     inputData$mb = round((inputData$mb / 10^6), 2)  # convert to Mb
         # }

         chrList = get_list_of_chromosomes(inputData)

         v1$inputData = inputData
         v1$genomeName = input$genomNameInput1W
     })

     observeEvent( c(input$inputdataFileW) ,{ # input$gui_separator, input$genomNameinput2), {

         inFile <- input$inputdataFileW
         if (is.null(inFile))
             return(NULL)

         if(input$gui_separator == "\\t"){
             separator = "\t"
         }else{
             separator = input$gui_separator
         }
         inputData <-  read.csv(inFile$datapath, header = TRUE, sep = separator)   # read.table(rawDataFile$name, header = TRUE, sep = separator)

         if(input$physMapUnit == "bp"){
           inputData$mb = inputData$mb/10^6  # convert to Mb without rounding
           # inputData$mb = round((inputData$mb / 10^6), 2)  # convert to Mb
         }

         chrList = get_list_of_chromosomes(inputData)

         v1$inputData = inputData
         v1$genomeName = input$genomNameInput2W
         # }
     })

# **************************************************** end all observeEvent s*************************************************************

# (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((RUN))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))


# ((((((((((((((((((((((((((  RUN button for BREC one chromosme  ))))))))))))))))))))))))))

 runBrec_reactive1 <- observeEvent(input$runButton1, {
     # tic("|||||||||  Chrono : Run BREC for one chromosome")

     if(input$radioChooseMode == "existing datasets"){
       v1$chrID =  input$chrIDInput1
     }else{
       v1$chrID =  input$chrIDInput2
     }

     chrID = v1$chrID
     v1$chromosome = get_chromosome_from_inputData(v1$inputData, chrID)
     chromosome = v1$chromosome
     goodDataQuality = data_quality_test(chromosome) # assessing data quality is not optional!
     #--- add a box with the DQC results for the user to know

     if(input$checkBoxDcleaning){
          print("from inside  if(input$checkBoxDcleaning)")
       if(goodDataQuality){ # good data quality 1st iteration
          # -----------------
           shinyalert(
             title = "Great! Your data quality is Good!",
             text = "Still, do you want to remove 5% of the potential outliers in your data ?",
             type = "success",
             showCancelButton = TRUE,
             cancelButtonText = "No need",
             confirmButtonCol = 'green',
             confirmButtonText = "OK, why not!",
             animation = TRUE,
             callbackR = function(x) {
               if(x){ # == yes
                 print("from isndide yes 5% clean")
                 cleanedChromosome_5_perCent = clean_5_perCent_chromosome_data(chromosome, genomeName, chrID)
                 chromosome = cleanedChromosome_5_perCent
               }
              print("Cleaning 5% done > see results on the corresponding.") # -> make box
            })
           # -----------------
       }else{ # low data quality 1st iteration
         # -----------------
         shinyalert(
           title = "Oops! Your data quality is not good enough!",
           text = "A data cleaning step is recommanded! Whould you like to remove the potential outliers in your data ?
           \n When the cleaning step is over, the data quality of your cleaned chromosome will be assessed again.",
           type = "error",
           showCancelButton = TRUE,
           cancelButtonText = "No need",
           confirmButtonCol = 'green',
           confirmButtonText = "Yes, let's clean",
           animation = TRUE,
           callbackR = function(x){
             if(x){ # == yes
               cleanedChromosome = clean_chromosome_data(chromosome, genomeName, chrID)
               chromosome = cleanedChromosome
               goodDataQuality = data_quality_test(chromosome)
               if(goodDataQuality){  # good data quality 2nd iteration
                 shinyalert(
                   title = "Great! Your data quality is Good now!",
                   type = "success",
                   confirmButtonCol = 'green',
                   confirmButtonText = "OK, go on to run BREC",
                   animation = TRUE
                 )
               }else{  # low data quality 2nd iteration
                 shinyalert(
                   title = "Oops! Your data quality is still not good enough!",
                   text = "Another data cleaning step is recommanded! Would you like to clean 5% of the potential remaining outliers in your data ?
             \n When the cleaning step is over, the data quality of your cleaned chromosome will be assessed again.",
                   type = "error",
                   showCancelButton = TRUE,
                   cancelButtonText = "No need",
                   confirmButtonCol = 'green',
                   confirmButtonText = "Yes, let's clean again",
                   animation = TRUE,
                   callbackR = function(x) {
                     if(x){ # == yes
                       cleanedChromosome_5_perCent = clean_5_perCent_chromosome_data(chromosome, genomeName, chrID)
                       chromosome = cleanedChromosome_5_perCent
                       goodDataQuality = data_quality_test(chromosome)
                       if(goodDataQuality){
                         shinyalert(
                           title = "Great! Your data quality is Good now!",
                           type = "success",
                           confirmButtonCol = 'green',
                           confirmButtonText = "OK, go on to run BREC",
                           animation = TRUE
                         )
                       }else{
                         shinyalert(
                           title = "Oh no! Your data quality is still not good enough!",
                           text = "Sorry, BREC behaviour and results are no longer garanteed. But you could still try :)",
                           type = "warning",
                           confirmButtonCol = 'green',
                           confirmButtonText = "OK, go on to run BREC anyway",
                           animation = TRUE
                         )
                       }
                     }
                     print("Cleaning 5% done > see results on the corresponding.") # -> make box
                  })
               }
             }
           })
         # -----------------
         # if(!goodDataQuality){
         #   using_slidingWindowApproach_for_HCB = FALSE
         # }
       }
     }# else{} # if the checkbox is not checked, don't do anything for now! Later, maybe add a confirmation popup?

     v1$chromosome = chromosome
     # print("calling Brec_chromosome")
     BREC_chromosome_results_list = Brec_chromosome(genomeName = v1$genomeName, chromosome =  v1$chromosome, inputChrID = v1$chrID, span = input$span)# bins = input$binsValueInput #input$chrID_existingInput

     # BREC_chromosome_results_list = Brec_chromosome(genomeName = v1$genomeName, inputData =  v1$inputData, inputChrID = chrID, span = input$span)# bins = input$binsValueInput #input$chrID_existingInput

     auto_chrType_object = BREC_chromosome_results_list$chrType_object
     if(auto_chrType_object$chr_type == 2){ # in case the auto process for chrType returned "Don't know"

         shinyalert(
             title = "Is your chromosome an arm?",
             text = "Unfortunately, BREC was not able to automatically decide on the type of this chromosome! \n Could you please confirm if it's telocenrtic (arm)? Otherwise, BREC will process it as metacentric?",
             type = "warning",
             showCancelButton = TRUE,
             cancelButtonText = "No, it's not",
             confirmButtonCol = 'green',
             confirmButtonText = "Yes, it's an arm",
             animation = TRUE,
             callbackR = function(x) {
               if(!x){ # == FALSE
                 chr_type = 1
                 chr_sub_type = "Atelocentric"
                 print("chr_type is Atelocentric (not a chromosomal arm)")
               }else{ # ==TRUE
                 chr_type = 0
                 chr_sub_type = "Telocentric"
                 print("chr_type is telocentric (chromosomal arm)")
               }
               v1$alertResponse_chrType <- chr_type
               v1$alertResponse_chr_sub_type <- chr_sub_type
               # print("in runBrec_tive1 => inputData before ")
               # print(inputData)
               #
               # if(input$physMapUnit == "bp"){
               #     inputData$mb = round((inputData$mb / 10^6), 2)  # convert to Mb
               #     print("from inside if(..=='bp'..")
               # }
               # print("in runBrec_tive1 => inputData after ")
               # print(v1$inputData)

               user_chrType_object = data.frame(chr_type = v1$alertResponse_chrType, chr_sub_type = v1$alertResponse_chr_sub_type)
               print("calling Brec_chromosome_part_2 -- from inside shinyalert")
               v1$BrecResultsList = Brec_chromosome_part_2(BREC_chromosome_results_list, user_chrType_object)
               # print(v1$BrecResultsList[[1]])    ## this displays plot in Rstudio so I can expand it in the navigator

               output$rrplot1 <- renderPlotly({
                   v1$BrecResultsList[[1]]
               })

               output$rr1 <- renderDataTable({
                   RR_output = v1$BrecResultsList[[2]]
                   datatable(RR_output, options = list(paging = TRUE, searching = FALSE))
                   # RR_file = paste0(getwd(),"/../results/drosophila_r6-Chr2_RRestimates_Brec.txt")
                   # RR_output <- read.table(RR_file, header = TRUE, sep = "\t")
                   # datatable(RR_output, options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
               })

               output$hcb1 <- renderDataTable({
                   chrType = v1$BrecResultsList[[5]]
                   HCB_output = v1$BrecResultsList[[3]]
                   telo_output = v1$BrecResultsList[[4]]

                   if(chrType == 1){   # whole chromosome
                       hcbrslTable = cbind(HCB_output[ , c(1,3)], telo_output[ , c(2,4)]) # ========== rename cols
                   }else{ # chrType == 0 # chromosomal arm
                       print("cbind(HCB_output[ , 3], telo_output[ , 2]")
                       print(cbind(HCB_output[ , 3], telo_output[ , 2]))
                       hcbrslTable = cbind( telo_output[ , 2], HCB_output[ , 3]) # ========== rename cols
                   }
                   datatable(hcbrslTable, rownames = FALSE , options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
                   # HCB_file = paste0(getwd(),"/../results/DrosophilamelanogasterR6-Chr2_chromatinBoundaries_Brec.txt")
                   # HCB_output <- read.table(HCB_file, header = TRUE, sep = "\t")
                   # DT::datatable(HCB_output, options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
               })

               output$generalStats1 <- renderDataTable({   ## not ready yet -- come back and fix this later
                   gs_output = data.frame(v1$BrecResultsList[[5]])
                   # gs_output[ , c(2,4)]
                   datatable(gs_output, rownames = FALSE, options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
                   # gs_file = paste0(getwd(),"/../results/drosophila_r6-Chr2_HCB_Brec2.txt")
                   # gs_output <- read.table(gs_file, header = TRUE, sep = "\t")
                   # DT::datatable(gs_output, options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
               })

               }# end callbackR
             )# end shinyalert

       }else{ # in case the user  is not asked to precise chrType
           print("calling Brec_chromosome_part_2 -- from outside shinyalert")
           v1$BrecResultsList = Brec_chromosome_part_2(BREC_chromosome_results_list, auto_chrType_object)
           # print(v1$BrecResultsList[[1]])    ## this displays plot in Rstudio so I can expand it in the navigator

           output$rrplot1 <- renderPlotly({
             v1$BrecResultsList[[1]]
           })

           output$rr1 <- renderDataTable({
             RR_output = v1$BrecResultsList[[2]]
             datatable(RR_output, options = list(paging = TRUE, searching = FALSE))
             # RR_file = paste0(getwd(),"/../results/drosophila_r6-Chr2_RRestimates_Brec.txt")
             # RR_output <- read.table(RR_file, header = TRUE, sep = "\t")
             # datatable(RR_output, options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
           })

           output$hcb1 <- renderDataTable({
             chrType = v1$BrecResultsList[[5]]
             HCB_output = v1$BrecResultsList[[3]]
             telo_output = v1$BrecResultsList[[4]]

             if(chrType == 1){   # whole chromosome
               hcbrslTable = cbind(HCB_output[ , c(1,3)], telo_output[ , c(2,4)]) # ========== rename cols
             }else{ # chrType == 0 # chromosomal arm
               print("cbind(HCB_output[ , 3], telo_output[ , 2]")
               print(cbind(HCB_output[ , 3], telo_output[ , 2]))
               hcbrslTable = cbind( telo_output[ , 2], HCB_output[ , 3]) # ========== rename cols
             }
             datatable(hcbrslTable, rownames = FALSE , options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
             # HCB_file = paste0(getwd(),"/../results/DrosophilamelanogasterR6-Chr2_chromatinBoundaries_Brec.txt")
             # HCB_output <- read.table(HCB_file, header = TRUE, sep = "\t")
             # DT::datatable(HCB_output, options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
           })

           output$generalStats1 <- renderDataTable({   ## not ready yet -- come back and fix this later
             gs_output = data.frame(v1$BrecResultsList[[5]])
             # gs_output[ , c(2,4)]
             datatable(gs_output, rownames = FALSE, options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
             # gs_file = paste0(getwd(),"/../results/drosophila_r6-Chr2_HCB_Brec2.txt")
             # gs_output <- read.table(gs_file, header = TRUE, sep = "\t")
             # DT::datatable(gs_output, options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
           })
         }# end else


       toc()
     })# end runBrec_reactive1 <- observeEvent(input$runButton1, {

 # ((((((((((((((((((((((((((  RUN button for BREC whole genome  ))))))))))))))))))))))))))

 # runBrec_reactive1 <- observeEvent(input$runButton1W, {
 #
 #     tic("|||||||||  Chrono : Run Brec for one chromosome")
 #
 #     print("calling Brec for whole genome")
 #     v1$BrecResultsList = Brec_genome(genomeName = v1$genomeName, inputData =  v1$inputData )# bins = input$binsValueInput #input$chrID_existingInput
 #
 #     output$rrW <- renderDataTable({  # whole_genome -- #not yet
 #         RR_output = v1$BrecResultsList[[2]]
 #         datatable(RR_output, options = list(paging = TRUE, searching = FALSE))
 #         # RR_file = paste0(getwd(),"/../results/drosophila_r6-Chr2_RRestimates_Brec.txt")
 #         # RR_output <- read.table(RR_file, header = TRUE, sep = "\t")
 #         # datatable(RR_output, options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
 #     })
 #
 #     output$hcbW <- renderDataTable({ # whole_genome -- #not yet
 #         HCB_output = v1$BrecResultsList[[3]]
 #         datatable(HCB_output[ , c(1,3)], rownames = FALSE , options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
 #         # HCB_file = paste0(getwd(),"/../results/DrosophilamelanogasterR6-Chr2_chromatinBoundaries_Brec.txt")
 #         # HCB_output <- read.table(HCB_file, header = TRUE, sep = "\t")
 #         # DT::datatable(HCB_output, options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
 #     })
 #     output$generalStatsW <- renderDataTable({ # whole_genome -- #not yet
 #         gs_output = v1$BrecResultsList[[4]]
 #         datatable(gs_output[ , c(2,4)], rownames = FALSE, options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
 #         # gs_file = paste0(getwd(),"/../results/drosophila_r6-Chr2_HCB_Brec2.txt")
 #         # gs_output <- read.table(gs_file, header = TRUE, sep = "\t")
 #         # DT::datatable(gs_output, options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
 #     })
 #
 #     toc()
 # })


# 5555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555

# ((((((((((((((((((((((((((  RUN button for RREstimator  ))))))))))))))))))))))))))

 go_reactive1 <- eventReactive(input$goButton1 ,  {
     get_recombinationRate_from_physPosition(chromosome, physPos)
 })

 output$getRRfrompp <- renderPrint({
     go_reactive1()
     print(" result here")
 })

 output$dataMetadata <- renderDataTable({
     # print(datasetMetadata$Species[1])
     datatable(datasetMetadata[,-2]  , options = list( pageLength = 50,scroller = TRUE, scrollX = TRUE) )
 })

 output$loadDatasetsList <- renderUI({
     selectInput('selectDatasetsList', 'Select a genome to display the corresponding data',
                 choices = choicesList,
                 multiple=TRUE,
                 selectize=FALSE,
                 selected = choicesList[1])
 })

 pp <- eventReactive(input$selectDatasetsList ,{
     v1$spIndex = match(input$selectDatasetsList, choicesList)
     v1$spData = allSpeciesData[[v1$spIndex]]
     datatable(v1$spData, options = list( searching = FALSE, pageLength = 15, language = list(info = "") ) )
 })

 output$datasetTable <- renderDataTable({
     pp()
 })


    # output$out1 <- renderInfoBox({
    #
    #     infoBox("Start here",
    #             a("Run Brec", onclick = "openTab('chromatinBoundariesTab')", href="#"),
    #             icon = icon("chart-line"), color = "blue"
    #     )
    # })

    # output$link2results <- renderInfoBox({
    #
    #     infoBox("Detailes results",
    #             a("Here", onclick = "openTab('resultsTab')", href="#"),
    #             icon = icon("chart-line"), color = "blue"
    #     )
    # })

# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

# ((((((((((((((((((((((((((  dipaly home page BREC workflow  ))))))))))))))))))))))))))
      output$pipelineImg1 <- renderImage({
        filename1 = paste0(getwd(), '/www/new_pipeline_p1_v6_for_GUI.jpg')
        outputArgs = return(list(
                    src = filename1,
                    contentType =  "image/jpg", #"image/svg+xml",
                    alt = "BREC workflow figure 1"
        ))
        }, deleteFile = FALSE
      )

      output$pipelineImg2 <- renderImage({
        filename2 = paste0(getwd(), '/www/new_pipeline_p2_v6_for_GUI.jpg')
        outputArgs = return(list(
            src = filename2,
            contentType =  "image/jpg", #"image/svg+xml",
            alt = "BREC workflow figure 2"
        ))
        }, deleteFile = FALSE
      )

    output$installRmd <- renderUI({
        HTML(markdownToHTML(knit(paste0(getwd(),'/install.rmd'), quiet = TRUE)))
    })

    output$downloadRmd <- renderUI({
        # refere the user to github and bioC
        # HTML(markdownToHTML(knit( paste0(getwd(),'/README.rmd'), quiet = TRUE)))
    })


 # All downloadHandler s (for all -10- download buttons)***************************************************************************************************


    output$downloadSelectedDS <- downloadHandler( #1
        filename = function(){
            paste0(choicesList[v1$spIndex], "_", Sys.Date(), ".csv" )
        },
        content = function(filename){
            write.csv(v1$spData, file = filename, row.names = F ) #
        }
    )

    output$downloadAllDSs <- downloadHandler( #2
        filename = function(){
            paste0("","")
        },
        content = function(filename){
            # write all csv files for each dataset
            mapply(write.csv, allSpeciesData, file = paste0(getwd(), "/../data/downloaded_data/", str_replace_all(string=choicesList, pattern=" ", repl=""), ".csv") )
            print(paste0(" All data files are saved in : ", getwd(), "/../data/downloaded_data/"))
        }
    )

    output$downloadStats1 <- downloadHandler(  #3
        filename = function() {
            paste0('resultsStatst1-', Sys.Date(), '.csv')
        },
        content = function(con) {
            print("from downHandler stats1")
            write.csv(data, con)
        }
    )
    output$downloadStatsW <- downloadHandler(  #4   ## whole-genome
        filename = function() {
            paste0('resultsStats2-', Sys.Date(), '.csv')
        },
        content = function(con) {
            print("from downHandler stats2")
            write.csv(data, con)
        }
    )

    output$downloadPlot <- downloadHandler(  #5
        filename = function() {
            paste0('plot-', Sys.Date(), '.png')
        },
        content = function(con) {
            print("from downHandler plot ")
            save_plot_as_png(RR_plot, plots_path, genomeName, chrID)
            write.csv(data, con)
        }
    )

    output$downloadResHCB1 <- downloadHandler(  #6
        filename = function() {
            paste0('resultsHCB1-', Sys.Date(), '.csv')
        },
        content = function(con) {
            print("from downHandler hcb1")
            chrType = v1$BrecResultsList[[5]]
            HCB_fileContent = generate_HCB_output_data_file(genomeName, chrID, chromosome, heteroChromatinBoundaries, telomeres_boundaries, output_path, chrType)
            cat(" \n *-*-*-*-*||  2 outputs are saved as txt files in this directory :", output_path,"  ||*-*-*-*-* \n ")
            write.csv(data, con)
        }
    )

    output$downloadResHCBW<- downloadHandler(  #7 ## whole-genome
        filename = function() {
            paste0('resultsHCB2-', Sys.Date(), '.csv')
        },
        content = function(con) {
            print("from downHandler hcb2")
            chrType = v1$BrecResultsList[[5]]
            HCB_fileContent = generate_HCB_output_data_file(genomeName, chrID, chromosome, heteroChromatinBoundaries, telomeres_boundaries, output_path, chrType)
            cat(" \n *-*-*-*-*||  2 outputs are saved as txt files in this directory :", output_path,"  ||*-*-*-*-* \n ")
            write.csv(data, con)
        }
    )

    output$downloadResRR1 <- downloadHandler(  #8
        filename = function() {
            paste0('resultsRR1-', Sys.Date(), '.csv')
        },
        content = function(con) {
            print("from downHandler rr1")
            RR_fileContent  = generate_RR_output_data_file(genomeName, chrID, chromosome, RR_object, output_path)
            write.csv(data, con)
        }
    )

    output$downloadResRRW <- downloadHandler( #9
          filename = function() {
            paste0('resultsRR2-', Sys.Date(), '.csv')
          },
          content = function(con) {
              print("from downHandler rr2")
              RR_fileContent  = generate_RR_output_data_file(genomeName, chrID, chromosome, RR_object, output_path)
            write.csv(data, con)
          }
        )

    output$downloadRRforPhysPos <- downloadHandler( #10

      # # You can access the value of the widget with input$slider1, e.g.
      # output$value <- renderPrint({ input$slider1 })
      #
      # # You can access the values of the second widget with input$slider2, e.g.
      # output$range <- renderPrint({ input$slider2 })


          filename = function() {
            paste0('resultsRRForPhysPos-', Sys.Date(), '.csv')
          },
          content = function(con) {
              print("from downHandler rrformphyspos ")
            write.csv(data, con)
          }
        )

 # Contact us form handler ***************************************************************************************************

    # observe({  ## send mail for contact us page
    #     if(is.null(input$send) || input$send == 0) return(NULL)
    #     from <- isolate(input$from)
    #     to <- isolate(input$to)
    #     subject <- isolate(input$subject)
    #     msg <- isolate(input$message)
    #     sendmail(from, to, subject, msg)
    # })
}

shinyApp(ui, server)
