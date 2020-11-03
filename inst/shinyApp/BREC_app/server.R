install_github("ymansour21/BREC")

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


# ====================================================================================================================================
# serverserverserverserverserverserverserverserverserverserverserverserverserverserverserverserverserverserverserverserverserverserver

server <- function(input, output, session){

  # Global variables ***********************************************************************************************************
    package_source_path = "/home/yasmine/ProBook/_ISEM/Thesis_work/Recombination_BREC_project_thesis_part_1/DailyUpdates_R_dev/R_projects/My_Brec_project_final/Brec"
    datasetMetadata <- read.csv( paste0(package_source_path, "/datasets/Sackton_dataset_metadata_S13_Table.csv"), header = T, sep = "\t")
    # myds <- load(paste0(package_source_path, "/datasets/raw_genetic_maps.RData"), envir = environment())


    allSpeciesData = list()

    datasetPath =  paste0(package_source_path, "/data/")

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
                   # RR_file = paste0(package_source_path,"/results/drosophila_r6-Chr2_RRestimates_Brec.txt")
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
                   # HCB_file = paste0(package_source_path,"/results/DrosophilamelanogasterR6-Chr2_chromatinBoundaries_Brec.txt")
                   # HCB_output <- read.table(HCB_file, header = TRUE, sep = "\t")
                   # DT::datatable(HCB_output, options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
               })

               output$generalStats1 <- renderDataTable({   ## not ready yet -- come back and fix this later
                   gs_output = data.frame(v1$BrecResultsList[[5]])
                   # gs_output[ , c(2,4)]
                   datatable(gs_output, rownames = FALSE, options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
                   # gs_file = paste0(package_source_path,"/results/drosophila_r6-Chr2_HCB_Brec2.txt")
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
             # RR_file = paste0(package_source_path,"/results/drosophila_r6-Chr2_RRestimates_Brec.txt")
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
             # HCB_file = paste0(package_source_path,"/results/DrosophilamelanogasterR6-Chr2_chromatinBoundaries_Brec.txt")
             # HCB_output <- read.table(HCB_file, header = TRUE, sep = "\t")
             # DT::datatable(HCB_output, options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
           })

           output$generalStats1 <- renderDataTable({   ## not ready yet -- come back and fix this later
             gs_output = data.frame(v1$BrecResultsList[[5]])
             # gs_output[ , c(2,4)]
             datatable(gs_output, rownames = FALSE, options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
             # gs_file = paste0(package_source_path,"/results/drosophila_r6-Chr2_HCB_Brec2.txt")
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
 #         # RR_file = paste0(package_source_path,"/results/drosophila_r6-Chr2_RRestimates_Brec.txt")
 #         # RR_output <- read.table(RR_file, header = TRUE, sep = "\t")
 #         # datatable(RR_output, options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
 #     })
 #
 #     output$hcbW <- renderDataTable({ # whole_genome -- #not yet
 #         HCB_output = v1$BrecResultsList[[3]]
 #         datatable(HCB_output[ , c(1,3)], rownames = FALSE , options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
 #         # HCB_file = paste0(package_source_path,"/results/DrosophilamelanogasterR6-Chr2_chromatinBoundaries_Brec.txt")
 #         # HCB_output <- read.table(HCB_file, header = TRUE, sep = "\t")
 #         # DT::datatable(HCB_output, options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
 #     })
 #     output$generalStatsW <- renderDataTable({ # whole_genome -- #not yet
 #         gs_output = v1$BrecResultsList[[4]]
 #         datatable(gs_output[ , c(2,4)], rownames = FALSE, options = list(paging = FALSE, searching = FALSE, language = list(info = "")))
 #         # gs_file = paste0(package_source_path,"/results/drosophila_r6-Chr2_HCB_Brec2.txt")
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
        filename1 = paste0(package_source_path, '/www/new_pipeline_p1_v6_for_GUI.jpg')
        outputArgs = return(list(
                    src = filename1,
                    contentType =  "image/jpg", #"image/svg+xml",
                    alt = "BREC workflow figure 1"
        ))
        }, deleteFile = FALSE
      )

      output$pipelineImg2 <- renderImage({
        filename2 = paste0(package_source_path, '/www/new_pipeline_p2_v6_for_GUI.jpg')
        outputArgs = return(list(
            src = filename2,
            contentType =  "image/jpg", #"image/svg+xml",
            alt = "BREC workflow figure 2"
        ))
        }, deleteFile = FALSE
      )

    output$installRmd <- renderUI({
        HTML(markdownToHTML(knit(paste0(package_source_path,'/install.rmd'), quiet = TRUE)))
    })

    output$downloadRmd <- renderUI({
        # refere the user to github and bioC
        # HTML(markdownToHTML(knit( paste0(package_source_path,'/README.rmd'), quiet = TRUE)))
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
            mapply(write.csv, allSpeciesData, file = paste0(package_source_path, "/data/downloaded_data/", str_replace_all(string=choicesList, pattern=" ", repl=""), ".csv") )
            print(paste0(" All data files are saved in : ", package_source_path, "/data/downloaded_data/"))
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

# shinyApp(ui, server)
