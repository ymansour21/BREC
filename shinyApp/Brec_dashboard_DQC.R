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

# =======================================================================================================================================
# dashboardHeaderdashboardHeaderdashboardHeaderdashboardHeaderdashboardHeaderdashboardHeaderdashboardHeaderdashboardHeaderdashboardHeader

header <- dashboardHeader(title = "BREC")

# ================================================================================================================================
# dashboardSidebardashboardSidebardashboardSidebardashboardSidebardashboardSidebardashboardSidebardashboardSidebardashboardSidebar

sidebar <- dashboardSidebar(

    sidebarMenu(
        menuItem("Home", tabName = "homeTab", icon = icon("home")),

        menuItem("Genomic data", tabName = "genomicDataTab", icon = icon("database "),
                 menuSubItem("Download Data files", tabName = "downloadDataTab", icon = icon("file-download ")),
                 menuItem("Database details", tabName = "datasetDetailsTab", icon = icon("database "))),

        menuItem("Run Brec", tabName = "runBrecTab", icon = icon("chart-line"),
                 menuSubItem('Chromatin boundaries', tabName = 'chromatinBoundariesTab', icon = icon('dna'), selected = TRUE),
                 menuSubItem("Recombination rate estimator", tabName = "rrEstimatorTab", icon = icon("calculator "))),

        # menuSubItem('Use existing datasets', tabName = 'chromatinBoundariesTab', icon = icon('database')),
        # menuItem('Use imported datasets', tabName = 'importingDatasetTab', icon = icon('table'))),  #file-csv

        menuItem("Download BREC package", tabName = "downloadTab", icon = icon("download ")),
        menuItem("Install BREC locally", tabName = "installTab", icon = icon("code ")),
        menuItem("Help", tabName = "helpTab", icon = icon("question-circle ")),
        menuItem("Contact us", tabName = "contactTab", icon = icon("envelope"))
    )
)

# ==================================================================================================================================
# dashboardBodydashboardBodydashboardBodydashboardBodydashboardBodydashboardBodydashboardBodydashboardBodydashboardBodydashboardBody

body <- dashboardBody(

    tabItems(
        tabItem(tabName = "homeTab",
                fluidPage(
                    box(title = "Welcome to BREC !", status ="success", solidHeader = T, width = NULL,
                        "BREC is an automated and non-genome-specific solution based on the Marey maps method in order to provide local recombination rate estimates. Then, identify the chromatin boundaries along chromosomes. This functionality allows determinig the location of the peri/centromeric and telomeric regions known to present a reduced recombination rate in most genomes."
                    )
                ),
                # infoBoxOutput("out1"),
                box(title = " BREC workflow ", status ="primary", solidHeader = T, width = NULL, height = 800,
                    imageOutput("pipelineImg")
                )
        ),

        tabItem(tabName = "chromatinBoundariesTab", #------------------------------------------------------------------------------------------
                fluidPage(
                    tabsetPanel( type = "pills",

                                 tabPanel(title = "One chromosome", br(),br(),#-------------------------
                                          fluidRow(
                                              box(title = "Input parameters ",  status ="primary", solidHeader = T,
                                                  prettyRadioButtons(inputId = "radioChooseMode", label = "Choose running mode ", choices = c("existing datasets", "imported dataset"), selected = "existing datasets"),
                                                  uiOutput("contents"),
                                                  # checkboxInput(inputId = "checkBoxDCQ", label = "Apply data quality control", value = FALSE),
                                                  # checkboxInput(inputId = "checkBoxDcleaning", label = "Apply data cleaning", value = FALSE),
                                                  actionButton("runButton1" , "Run", icon = icon("arrow-circle-right "))
                                              ),

                                              box(title = "Chromatin boundaries",status ="primary", solidHeader = T,
                                                  withSpinner(dataTableOutput("hcb1")) ,
                                                  downloadButton('downloadResHCB1', 'Download')
                                              )
                                          ),
                                          box(title = "Summarizing plot", status ="primary", solidHeader = T, width = NULL, height = NULL,
                                              withSpinner(plotlyOutput("rrplot1")),#color="#0dc5c1"
                                              downloadButton('downloadPlot', 'Download plot')
                                          ),


                                          box(title = "General statistics", status ="primary", solidHeader = T,width = NULL,
                                              # withSpinner(
                                              dataTableOutput("generalStats1"),
                                              # ),
                                              downloadButton('downloadStats1', 'Download'))
                                          # , hide(id="General statistics")
                                          # )

                                          # ,hide(id= "Summarizing plot")
                                 )
                                 ,
                                 tabPanel(title = "Whole genome", br(),br(),#-------------------------
                                          fluidRow(
                                              box(title = "Input parameters ",  status ="primary", solidHeader = T,
                                                  prettyRadioButtons(inputId = "radioChooseModeW", label = "Choose running mode ", choices = c("existing datasets", "imported dataset"), selected = "existing datasets"),
                                                  uiOutput("contentsW")),

                                              box(title = "General statistics", status ="primary", solidHeader = T,
                                                  # withSpinner(
                                                  dataTableOutput("generalStatsW"),
                                                  # ),
                                                  downloadButton('downloadStatsW', 'Download'))
                                              # , hide(id="General statistics")
                                              # )
                                          ),

                                          box(title = "Chromatin boundaries for all chromosomes",status ="primary", solidHeader = T, width = NULL,
                                              # withSpinner(
                                              dataTableOutput("hcbW"),
                                              # ) ,
                                              downloadButton('downloadResHCBW', 'Download')),

                                          box(title = "Recombination rates for whole genome",status ="primary", solidHeader = T, width = NULL,
                                              # withSpinner(
                                              dataTableOutput("rrW"),
                                              # )  ,
                                              downloadButton('downloadResRRW', 'Download'))
                                 )
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
                    downloadButton('downloadResRR1', 'Download'))

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
                        verbatimTextOutput('selectedDataset'),
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
        ,
        tabItem(tabName = "contactTab",#-------------------------------------------------------------------------------------------------
                pageWithSidebar(
                    headerPanel("Contact us"),
                    sidebarPanel(
                        textInput("from", "From:", value=""),
                        textInput(  "to", "To:", value="yasmine.mansour@umontpellier.fr" ),
                        textInput(  "subject", "Subject:", value=""),
                        actionButton( "send",  "Send mail")
                    ),
                    mainPanel(
                        aceEditor( "message", value="You can write your message here :)", theme = "tomorrow"  )
                    )
                )
        )
    )
)

# ====================================================================================================================================
# uiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiuiui

ui <-  dashboardPage( header, sidebar, body, skin = "blue") #useShinyjs(debug = TRUE),   Set up shinyjs

# ====================================================================================================================================
# serverserverserverserverserverserverserverserverserverserverserverserverserverserverserverserverserverserverserverserverserverserver

server <- function(input, output, session){

    # Global variables ***********************************************************************************************************

    options(encoding = "UTF-8")
    # install_needed_pkgs()
    wd = getwd()
    datasetPath = paste0(getwd(), "/../data/")
    plots_path = paste0(wd,"/Brec/plots/")
    output_path = paste0(wd,"/Brec/results/")

    datasetMetadata <- read.csv(paste0(getwd(), "/../datasets/Sackton_dataset_metadata_S13_Table.csv"), header = T, sep = "\t")
    # myds <- load(paste0(getwd(), "../datasets/raw_genetic_maps.RData"), envir = environment())


    #!!!!!!!!!!!!!!!!!! merge dmel + aedes + culex datasets in Sackton's once and for all the remove the next part of code !!!!!!!!

    allSpeciesData = list()

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

    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



    # Reactive values ***********************************************************************************************************
    v1 <- reactiveValues(genomeName = "", inputData = NULL, dataByChrArms = F, BrecResultsList = NULL, spData = NULL, spIndex = NULL )


    # Render UIs ***********************************************************************************************************
    output$contents <- renderUI({

        if(input$radioChooseMode == "existing datasets"){
            list(
                selectInput(inputId = "genomeNameInput1", label = "Select a dataset",
                            choices = choicesList, selected = choicesList[1]),
                selectInput(inputId = "chrIDInput1" , label = "Select chromosome ID", choices = NULL)
            )
        }else if(input$radioChooseMode == "imported dataset"){
            list(
                fileInput(inputId = "inputdataFile", width = 300, label = "Choose dataset file" ,
                          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                          # ,
                          # tags$hr(),
                          # checkboxInput("header", "Header", TRUE)
                ),
                textInput(inputId = "genomNameInput2", label = "Type dataset name (optional)", placeholder = "ex.: Dmel R6 genome" , width = 300),
                selectInput(inputId = "gui_separator", label = "Select data separator", width = 300, choices = c("\\t", ",", ";"), selected = "\\t"),
                selectInput(inputId = "chrIDInput2" , label = "Select chromosome ID", choices = NULL)
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
        }else if(input$radioChooseModeW == "imported dataset"){
            list(
                fileInput(inputId = "inputdataFileW", width = 300, label = "Choose dataset file" ,
                          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                          # ,
                          # tags$hr(),
                          # checkboxInput("header", "Header", TRUE)
                ),
                textInput(inputId = "genomNameInput2W", label = "Type dataset name (optional)", placeholder = "ex.: Dmel R6 genome" , width = 300),
                selectInput(inputId = "gui_separatorW", label = "Select data separator", width = 300, choices = c("\\t", ",", ";"), selected = "\\t"),
                actionButton("runButton1W" , "Run", icon = icon("arrow-circle-right "))
            )
        }
    })

    # **************************************************** one chromosme all observeEvent s*************************************************************
    observeEvent(input$genomeNameInput1, {

        dataByChrArms = F

        v1$spIndex = match(input$genomeNameInput1, choicesList)
        v1$spData = allSpeciesData[[v1$spIndex]]

        if(v1$spIndex == 1){ #DmelR6
            physicalMapUnit = "Mb"
            dataByChrArms = T
        }else{
            physicalMapUnit = "bp"
        }

        #
        #          if(input$genomeNameInput1 == "Drosophila melanogaster R6"){
        #              rawDataFile = paste0(getwd(), "/../data/Dmel_R6_formatted_v2.csv")
        #              physicalMapUnit = "Mb"
        #              dataByChrArms = T
        #
        #          }else if(input$genomeNameInput1 == "Aedes aegypti"){
        #              rawDataFile = paste0(getwd(), "/../data/DataAeAegypti3cols_OriginalFile.csv")
        #              physicalMapUnit = "bp"
        #          }else if(input$genomeNameInput1 == "Culex pipiens"){
        #              rawDataFile = paste0(getwd(), "/../data/DataCxQuainquefasciatusData3cols_OriginalFile_2.csv")
        #              physicalMapUnit = "bp"
        #          }else if(input$genomeNameInput1 == "Homo sapiens"){
        #              rawDataFile = paste0(getwd(), "/../data/hsap.csv")
        #              physicalMapUnit = "Mb"
        #          }else if(input$genomeNameInput1 == "Apis mellifera"){
        #              rawDataFile = paste0(getwd(), "/../data/amel.csv")
        #              physicalMapUnit = "bp"
        #          }else if(input$genomeNameInput1 == "Anopheles gambiae"){
        #              rawDataFile = paste0(getwd(), "/../data/agam.csv")
        #              physicalMapUnit = "bp"
        #          }

        # inputData <- read.table(rawDataFile, header = TRUE, sep = "\t")

        inputData = v1$spData
        # print(inputData)

        # if(physicalMapUnit != "Mb"){
        #     inputData$bp = round((inputData$bp / 10^6), 2)  # convert to Mb
        # }
        # chrList = get_list_of_chromosomes(inputData)

        if (dataByChrArms){ # test if 2 arms are compatible, else, break and display an erroe msg
            chrList = get_list_of_chromosomes(inputData)
            # print(chrList)
            # print(class(chrList))
            inputData = transform_inputData_to_chromosomes(inputData, chrList)
            # print(head(inputData))
            chrList = get_list_of_chromosomes(inputData)
        }else{
            # inputData <- read.table(rawDataFile, header = TRUE, sep = separator)
            chrList = get_list_of_chromosomes(inputData)
        }

        updateSelectInput(session = session, inputId = "chrIDInput1" , choices = chrList, selected = chrList[1] )

        v1$inputData = inputData
        v1$dataByChrArms = dataByChrArms
        v1$genomeName = input$genomNameInput1
    })

    observeEvent( c(input$inputdataFile) ,{ # input$gui_separator, input$genomNameinput2), {

        inFile <- input$inputdataFile

        if (is.null(inFile))
            return(NULL)
        # print(inFile$datapath)

        if(input$gui_separator == "\\t"){
            separator = "\t"
        }else{
            separator = input$gui_separator
        }
        physicalMapUnit = "bp"
        dataByChrArms = F
        inputData <-  read.csv(inFile$datapath, header = TRUE, sep = separator)   # read.table(rawDataFile$name, header = TRUE, sep = separator)
        # if(physicalMapUnit != "Mb"){
        #     inputData$bp = round((inputData$bp / 10^6), 2)  # convert to Mb
        # }

        if (dataByChrArms){ # test if 2 arms are compatible, else, break and display an erroe msg
            chrList = get_list_of_chromosomes(inputData)
            # print(c("chrList from inside if /v2", chrList))
            inputData = transform_inputData_to_chromosomes(inputData, chrList)
            chrList = get_list_of_chromosomes(inputData)
        }else{
            # inputData <- read.csv(inFile$datapath, header = TRUE, sep = separator)
            chrList = get_list_of_chromosomes(inputData)
        }
        print("from oe existing dataset")
        print(head(inputData))
        print(chrList)

        updateSelectInput(session = session, inputId = "chrIDInput2" , choices = chrList, selected = chrList[1] )
        v1$inputData = inputData
        v1$dataByChrArms = dataByChrArms
        v1$genomeName = input$genomNameInput2
        # }
    })

    # **************************************************** whole_genome all observeEvent s*************************************************************
    observeEvent(input$genomeNameInput1W, {

        dataByChrArms = F

        v1$spIndex = match(input$genomeNameInput1W, choicesList)
        v1$spData = allSpeciesData[[v1$spIndex]]

        if(v1$spIndex == 1){ #DmelR6
            physicalMapUnit = "Mb"
            dataByChrArms = T
        }else{
            physicalMapUnit = "bp"
        }

        #
        #          if(input$genomeNameInput1 == "Drosophila melanogaster R6"){
        #              rawDataFile = paste0(getwd(), "/../data/Dmel_R6_formatted_v2.csv")
        #              physicalMapUnit = "Mb"
        #              dataByChrArms = T
        #
        #          }else if(input$genomeNameInput1 == "Aedes aegypti"){
        #              rawDataFile = paste0(getwd(), "/../data/DataAeAegypti3cols_OriginalFile.csv")
        #              physicalMapUnit = "bp"
        #          }else if(input$genomeNameInput1 == "Culex pipiens"){
        #              rawDataFile = paste0(getwd(), "/../data/DataCxQuainquefasciatusData3cols_OriginalFile_2.csv")
        #              physicalMapUnit = "bp"
        #          }else if(input$genomeNameInput1 == "Homo sapiens"){
        #              rawDataFile = paste0(getwd(), "/../data/hsap.csv")
        #              physicalMapUnit = "Mb"
        #          }else if(input$genomeNameInput1 == "Apis mellifera"){
        #              rawDataFile = paste0(getwd(), "/../data/amel.csv")
        #              physicalMapUnit = "bp"
        #          }else if(input$genomeNameInput1 == "Anopheles gambiae"){
        #              rawDataFile = paste0(getwd(), "/../data/agam.csv")
        #              physicalMapUnit = "bp"
        #          }

        # inputData <- read.table(rawDataFile, header = TRUE, sep = "\t")

        inputData = v1$spData
        # print(inputData)

        # if(physicalMapUnit != "Mb"){
        #     inputData$bp = round((inputData$bp / 10^6), 2)  # convert to Mb
        # }
        # chrList = get_list_of_chromosomes(inputData)

        if (dataByChrArms){ # test if 2 arms are compatible, else, break and display an erroe msg
            chrList = get_list_of_chromosomes(inputData)
            # print(chrList)
            # print(class(chrList))
            inputData = transform_inputData_to_chromosomes(inputData, chrList)
            # print(head(inputData))
            chrList = get_list_of_chromosomes(inputData)
        }else{
            # inputData <- read.table(rawDataFile, header = TRUE, sep = separator)
            chrList = get_list_of_chromosomes(inputData)
        }

        print(head(inputData))
        print(chrList)

        v1$inputData = inputData
        v1$dataByChrArms = dataByChrArms
        v1$genomeName = input$genomNameInput1W
    })

    observeEvent( c(input$inputdataFileW) ,{ # input$gui_separator, input$genomNameinput2), {

        inFile <- input$inputdataFileW

        if (is.null(inFile))
            return(NULL)
        # print(inFile$datapath)

        if(input$gui_separator == "\\t"){
            separator = "\t"
        }else{
            separator = input$gui_separator
        }
        physicalMapUnit = "bp"
        dataByChrArms = T
        inputData <-  read.csv(inFile$datapath, header = TRUE, sep = separator)   # read.table(rawDataFile$name, header = TRUE, sep = separator)
        # if(physicalMapUnit != "Mb"){
        #     inputData$bp = round((inputData$bp / 10^6), 2)  # convert to Mb
        # }

        if (dataByChrArms){ # test if 2 arms are compatible, else, break and display an erroe msg
            chrList = get_list_of_chromosomes(inputData)
            # print(c("chrList from inside if /v2", chrList))
            inputData = transform_inputData_to_chromosomes(inputData, chrList)
            chrList = get_list_of_chromosomes(inputData)
        }else{
            # inputData <- read.csv(inFile$datapath, header = TRUE, sep = separator)
            chrList = get_list_of_chromosomes(inputData)
        }
        print("from oe existing dataset W")
        print(head(inputData))
        print(chrList)

        v1$inputData = inputData
        v1$dataByChrArms = dataByChrArms
        v1$genomeName = input$genomNameInput2W
        # }
    })

    # **************************************************** end all observeEvent s*************************************************************

    # (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((RUN))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

    runBrec_reactive1 <- observeEvent(input$runButton1, {

        if(input$radioChooseMode == "existing datasets"){
            chrID =  input$chrIDInput1
        }else{
            chrID =  input$chrIDInput2
        }
        tic("|||||||||  Chrono : Run BREC for one chromosome")



        using_slidingWindowApproach_for_HCB = TRUE

        # genomeName = get_genome_full_name(inputGenomeName)
        # inputData = load_input_data(genomeName,  separator, physicalMapUnit, dataByChrArms) # return listOfChromosomes
        #
        # chrList = get_list_of_chromosomes(inputData)
        # chrID = stringr::str_to_upper(inputChrID)
        # chromosome = get_chromosome_from_inputData(inputData, chrID)
        # cat("\n**/**/**/**/**", "chrId : " , chrID ," --- length of this chr : ", nrow(chromosome),"**/**/**/**/**")

        # **** handling data quality issues ****************
        goodDataQuality = data_quality_test(chromosome)
        if (goodDataQuality) { # good data quality 1st iteration
          cat("\n Your data quality is : Good ! ")
          do_5_perCent_cleaning = ask_user_about_data_cleaning_5_perCent()
          if(do_5_perCent_cleaning){
            cleanedChromosome_5_perCent = clean_5_perCent_chromosome_data(chromosome, genomeName, chrID)
            chromosome = cleanedChromosome_5_perCent
          }
        }else{ # low data quality 1st iteration
          print("Your data quality is : Not good enough !!!")
          do_cleaning = ask_user_about_data_cleaning()
          if(do_cleaning){
        cleanedChromosome = clean_chromosome_data(chromosome, genomeName, chrID)
            chromosome = cleanedChromosome
          }
          goodDataQuality = data_quality_test(chromosome)
          if (goodDataQuality) {  # good data quality 2nd iteration
            print("Your data quality became : Good !")
          }else{  # low data quality 2nd iteration
            print("Your data quality is still : Not good enough !!!")
            do_5_perCent_cleaning = ask_user_about_data_cleaning_5_perCent()
            if(do_5_perCent_cleaning){
              cleanedChromosome_5_perCent = clean_5_perCent_chromosome_data(chromosome, genomeName, chrID)
              chromosome = cleanedChromosome_5_perCent
              goodDataQuality = data_quality_test(chromosome)
            }
          }
          if(!goodDataQuality){
              using_slidingWindowApproach_for_HCB = FALSE
          }
        }
        # ***************************************************

        cat("\n new chr size : " , nrow(chromosome), "\n")
        #**** end of handling data quality issues *************



        print("calling BREC for one chromosome")
        v1$BrecResultsList = Brec_chromosome_DQC(genomeName = v1$genomeName, inputData =  v1$inputData, inputChrID = chrID , dataByChrArms = v1$dataByChrArms)# bins = input$binsValueInput #input$chrID_existingInput
        toc()
    })

    runBrec_reactive1 <- observeEvent(input$runButton1W, {

        tic("|||||||||  Chrono : Run BREC for one chromosome")
        print("calling BREC for whole genome")
        v1$BrecResultsList = Brec_genome(genomeName = v1$genomeName, inputData =  v1$inputData, dataByChrArms = v1$dataByChrArms)# bins = input$binsValueInput #input$chrID_existingInput
        toc()
    })


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
        HCB_output = v1$BrecResultsList[[3]]
        datatable(HCB_output[ , c(1,3)], rownames = FALSE , options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
        # HCB_file = paste0(getwd(),"/../results/DrosophilamelanogasterR6-Chr2_chromatinBoundaries_Brec.txt")
        # HCB_output <- read.table(HCB_file, header = TRUE, sep = "\t")
        # DT::datatable(HCB_output, options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
    })
    output$generalStats1 <- renderDataTable({
        gs_output = v1$BrecResultsList[[4]]
        datatable(gs_output[ , c(2,4)], rownames = FALSE, options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
        # gs_file = paste0(getwd(),"/../results/drosophila_r6-Chr2_HCB_Brec2.txt")
        # gs_output <- read.table(gs_file, header = TRUE, sep = "\t")
        # DT::datatable(gs_output, options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
    })

    output$rrW <- renderDataTable({  # whole_genome -- #not yet
        RR_output = v1$BrecResultsList[[2]]
        datatable(RR_output, options = list(paging = TRUE, searching = FALSE))
        # RR_file = paste0(getwd(),"/../results/drosophila_r6-Chr2_RRestimates_Brec.txt")
        # RR_output <- read.table(RR_file, header = TRUE, sep = "\t")
        # datatable(RR_output, options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
    })

    output$hcbW <- renderDataTable({ # whole_genome -- #not yet
        HCB_output = v1$BrecResultsList[[3]]
        datatable(HCB_output[ , c(1,3)], rownames = FALSE , options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
        # HCB_file = paste0(getwd(),"/../results/DrosophilamelanogasterR6-Chr2_chromatinBoundaries_Brec.txt")
        # HCB_output <- read.table(HCB_file, header = TRUE, sep = "\t")
        # DT::datatable(HCB_output, options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
    })
    output$generalStatsW <- renderDataTable({ # whole_genome -- #not yet
        gs_output = v1$BrecResultsList[[4]]
        datatable(gs_output[ , c(2,4)], rownames = FALSE, options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
        # gs_file = paste0(getwd(),"/../results/drosophila_r6-Chr2_HCB_Brec2.txt")
        # gs_output <- read.table(gs_file, header = TRUE, sep = "\t")
        # DT::datatable(gs_output, options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
    })


    # 5555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555

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
    #             a("Run BREC", onclick = "openTab('chromatinBoundariesTab')", href="#"),
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

    output$pipelineImg <- renderImage({
        filename = paste0(getwd(), '/www/pipeline_merge_v3.jpg')
        outputArgs = return(list(
            src = filename,
            contentType = "jpg",#image/svg+xml
            alt = "BREC workflow"
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
        filename = function() {
            paste0('resultsRRForPhysPos-', Sys.Date(), '.csv')
        },
        content = function(con) {
            print("from downHandler rrformphyspos ")
            write.csv(data, con)
        }
    )

    # Contact us form handler ***************************************************************************************************

    observe({  ## send mail for contact us page  --> tryt with observeEvent
        if(is.null(input$send) || input$send == 0) return(NULL)
        from <- isolate(input$from)
        to <- isolate(input$to)
        subject <- isolate(input$subject)
        msg <- isolate(input$message)
        sendmail(from, to, subject, msg)
    })
}

shinyApp(ui, server)
