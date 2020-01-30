#' testing plot_all for paper figures
#'
#' generate interactive plot for the specified chromosome using plot_ly
#'
#' the generated plot is saved as png image and the interactive version will open up in the browser as html page
#'
#' @param chromosome xxx
#' @param RR_object xxx
#' @param genomeName x
#' @param chrID x
#' @param R2DataFrame2D x
#' @param heteroChromatinBoundaries x
#' @param swSize x
#' @param plots_path xx
#'
#' @return plot_ly object
#' @export

plot_steps_all <- function(chromosome, RR_object, genomeName, chrID, R2DataFrame2D, heteroChromatinBoundaries, swSize, plots_path) {

    # to use for paper plots / figures

    min_rr = chromosome$mb[match(min(RR_object$regDr), RR_object$regDr)]

    # p1 = plot_ly(x = chromosome$bp, y= chromosome$cM,
    #             type = 'scatter', mode = 'markers',
    #             yaxis = "y", name = "Experimental data",
    #             marker = list(color = "black"),
    #             hoverinfo = "text",
    #             text = ~paste('Mb:', chromosome$bp, "\n cM:",chromosome$cM)) %>%
    #
    #     layout( #title = paste(genomeName,"- Chr", chrID),
    #            xaxis = list(
    #                showline = TRUE
    #                ,title = paste( "Physical location (Mb)" )
    #                ,color = "black"
    #                ,size = 3
    #                ,zeroline = FALSE
    #                ,ticks = "inside"
    #                ,showgrid = FALSE
    #                ,exponentformat = "power"
    #            )
    #            ,yaxis = list(
    #                showline = TRUE
    #                ,side = "left"
    #                ,overlaying = "y"
    #                ,title = "Genetic location (cM)"
    #                ,color = "black"
    #                ,size = 3
    #                ,zeroline = FALSE
    #                ,anchor = "y"
    #                ,showgrid = FALSE
    #                ,ticks = "inside"
    #                ,exponenformat = "power"
    #            )
    #            ,showlegend = FALSE
    #     )
    # export(p1, file = str_replace_all( paste(plots_path, genomeName,"- Chr", chrID, "_p1.png"), fixed(" "), ""))
    # print(p1)

    #------------------------------------------------------------------
    # p2 =plot_ly(x = chromosome$bp, y= chromosome$cM,
    #             type = 'scatter', mode = 'markers',
    #             yaxis = "y", name = "Experimental data",
    #             marker = list(color = "black"),
    #             hoverinfo = "text",
    #             text = ~paste('Mb:', chromosome$bp, "\n cM:",chromosome$cM)) %>%
    #
    #     add_trace( x = chromosome$bp, y = RR_object$regFn,  #polynomial
    #               type = "scatter", mode = "lines",
    #               line = list(color = "red"),
    #               yaxis = "y2", name = "Interpolation",
    #               inherit = T)  %>%
    #
    #     layout(
    #         xaxis = list(
    #             showline = TRUE
    #             ,title = paste( "Physical location (Mb)" )
    #             ,color = "black"
    #             ,size = 3
    #             ,zeroline = FALSE
    #             ,ticks = "inside"
    #             ,showgrid = FALSE
    #             ,exponentformat = "power"
    #         )
    #         ,yaxis = list(
    #             showline = TRUE
    #             ,side = "left"
    #             ,overlaying = "y"
    #             ,title = "Genetic location (cM)"
    #             ,color = "black"
    #             ,size = 3
    #             ,zeroline = FALSE
    #             ,anchor = "y"
    #             ,showgrid = FALSE
    #             ,ticks = "inside"
    #             ,exponenformat = "power"
    #         )
    #         ,yaxis2 = list(
    #                visible = FALSE
    #            )
    #            ,showlegend = FALSE
    #     )
    # export(p2, file = str_replace_all( paste(plots_path, genomeName,"- Chr", chrID, "_p2.png"), fixed(" "), ""))
    # print(p2)
    # #------------------------------------------------------------------
    # p3 = plot_ly(x = chromosome$bp, y= chromosome$cM,
    #              type = 'scatter', mode = 'markers',
    #              yaxis = "y", name = "Experimental data",
    #              marker = list(color = "black"),
    #              hoverinfo = "text",
    #              text = ~paste('Mb:', chromosome$bp, "\n cM:",chromosome$cM)) %>%
    #
    #     add_trace( x = chromosome$bp, y = RR_object$regFn,  #polynomial
    #                type = "scatter", mode = "lines",
    #                line = list(color = "red"),
    #                yaxis = "y2", name = "Interpolation",
    #                inherit = F)  %>%
    #     add_trace( x = chromosome$bp, y = RR_object$regDr,  #RR
    #               type = "scatter", mode = "lines",
    #               yaxis = "y3",  name = "Recombination rate",
    #               line = list(shape = "linear", width = 6, connectgaps = FALSE, color = "#2CA02C"),
    #               inherit = F)  %>%
    #
    #     layout(
    #         xaxis = list(
    #             showline = TRUE
    #             ,title = paste( "Physical location (Mb)" )
    #             ,color = "black"
    #             ,size = 3
    #             ,zeroline = FALSE
    #             ,ticks = "inside"
    #             ,showgrid = FALSE
    #             ,exponentformat = "power"
    #         )
    #         ,yaxis = list(
    #             showline = TRUE
    #             ,side = "left"
    #             ,overlaying = "y"
    #             ,title = "Genetic location (cM)"
    #             ,color = "black"
    #             ,size = 3
    #             ,zeroline = FALSE
    #             ,anchor = "y"
    #             ,showgrid = FALSE
    #             ,ticks = "inside"
    #             ,exponenformat = "power"
    #         )
    #         ,yaxis2 = list(
    #             visible = FALSE
    #         )
    #            ,yaxis3 = list(
    #                showline = TRUE
    #                ,side = "right"
    #                ,overlaying = "y"
    #                ,title = "Recombination rate (cM/Mb)"
    #                ,color = "#2CA02C"
    #                ,size = 3
    #                ,zeroline = FALSE
    #                ,anchor = "y"
    #                ,showgrid = FALSE
    #                ,ticks = "inside"
    #                ,exponentformat = "power"
    #            )
    #            ,showlegend = FALSE
    #     )
    # export(p3, file = str_replace_all( paste(plots_path, genomeName,"- Chr", chrID, "_p3.png"), fixed(" "), ""))
    # print(p3)
    # #------------------------------------------------------------------
    # p4 =plot_ly(x = chromosome$bp, y= chromosome$cM,
    #             type = 'scatter', mode = 'markers',
    #             yaxis = "y", name = "Experimental data",
    #             marker = list(color = "black"),
    #             hoverinfo = "text",
    #             text = ~paste('Mb:', chromosome$bp, "\n cM:",chromosome$cM)) %>%
    #
    #     add_trace( x = chromosome$bp, y = RR_object$regFn,  #polynomial
    #                type = "scatter", mode = "lines",
    #                line = list(color = "red"),
    #                yaxis = "y2", name = "Interpolation",
    #                inherit = F)  %>%
    #     add_trace( x = chromosome$bp, y = RR_object$regDr,  #RR
    #                type = "scatter", mode = "lines",
    #                yaxis = "y3",  name = "Recombination rate",
    #                line = list(shape = "linear", width = 6, connectgaps = FALSE, color = "#2CA02C"),
    #                inherit = F)  %>%
    #     add_trace( x = chromosome$bp, y = R2DataFrame2D$R2Vect_dir1,
    #               type = "scatter", mode = "lines+markers",
    #               yaxis = "y4",  name = "Rsq forward",
    #               marker = list(color = "#D62728" ),
    #               line = list(color= "#D62728" ),
    #               inherit = F) %>%
    #
    #     layout(
    #         xaxis = list(
    #             showline = TRUE
    #             ,title = paste( "Physical location (Mb)" )
    #             ,color = "black"
    #             ,size = 3
    #             ,zeroline = FALSE
    #             ,ticks = "inside"
    #             ,showgrid = FALSE
    #             ,exponentformat = "power"
    #         )
    #         ,yaxis = list(
    #             showline = TRUE
    #             ,side = "left"
    #             ,overlaying = "y"
    #             ,title = "Genetic location (cM)"
    #             ,color = "black"
    #             ,size = 3
    #             ,zeroline = FALSE
    #             ,anchor = "y"
    #             ,showgrid = FALSE
    #             ,ticks = "inside"
    #             ,exponenformat = "power"
    #         )
    #         ,yaxis2 = list(
    #             visible = FALSE
    #         )
    #         ,yaxis3 = list(
    #             showline = TRUE
    #             ,side = "right"
    #             ,overlaying = "y"
    #             ,title = "Recombination rate (cM/Mb)"
    #             ,color = "#2CA02C"
    #             ,size = 3
    #             ,zeroline = FALSE
    #             ,anchor = "y"
    #             ,showgrid = FALSE
    #             ,ticks = "inside"
    #             ,exponentformat = "power"
    #         ),
    #            yaxis4 = list(
    #                showline = FALSE
    #                ,overlaying = "y"
    #                # ,size = 3
    #                ,zeroline = FALSE
    #                ,anchor = "y"
    #                ,showgrid = FALSE
    #                ,autorange = TRUE
    #                ,showticklabels = FALSE
    #            )
    #            ,showlegend = FALSE
    #     )
    # export(p4, file = str_replace_all( paste(plots_path, genomeName,"- Chr", chrID, "_p4.png"), fixed(" "), ""))
    # print(p4)
    # #------------------------------------------------------------------
    # p5 = plot_ly(x = chromosome$bp, y= chromosome$cM,
    #              type = 'scatter', mode = 'markers',
    #              yaxis = "y", name = "Experimental data",
    #              marker = list(color = "black"),
    #              hoverinfo = "text",
    #              text = ~paste('Mb:', chromosome$bp, "\n cM:",chromosome$cM)) %>%
    #
    #     add_trace( x = chromosome$bp, y = RR_object$regFn,  #polynomial
    #                type = "scatter", mode = "lines",
    #                line = list(color = "red"),
    #                yaxis = "y2", name = "Interpolation",
    #                inherit = F)  %>%
    #     add_trace( x = chromosome$bp, y = RR_object$regDr,  #RR
    #                type = "scatter", mode = "lines",
    #                yaxis = "y3",  name = "Recombination rate",
    #                line = list(shape = "linear", width = 6, connectgaps = FALSE, color = "#2CA02C"),
    #                inherit = F)  %>%
    #     add_trace( x = chromosome$bp, y = R2DataFrame2D$R2Vect_dir1,
    #                type = "scatter", mode = "lines+markers",
    #                yaxis = "y4",  name = "Rsq forward",
    #                marker = list(color = "#D62728" ),
    #                line = list(color= "#D62728" ),
    #                inherit = F) %>%
    #     add_trace( x = chromosome$bp, y = R2DataFrame2D$R2Vect_dir2,
    #               type = "scatter", mode = "lines+markers",
    #               yaxis = "y5",  name = "Rsq backwards" ,
    #               marker = list(color = "#9575D2" ),
    #               line = list(color= "#9575D2" ),
    #               inherit = F) %>%
    #
    #     layout(#title = paste(genomeName,"- Chr", chrID),
    #            xaxis = list(
    #                showline = TRUE
    #                ,title = paste( "Physical location (Mb)" )
    #                ,color = "#0072B2"
    #                ,size = 3
    #                ,zeroline = FALSE
    #                ,ticks = "inside"
    #                ,showgrid = FALSE
    #                ,exponentformat = "power"
    #            )
    #            ,yaxis = list(  # the data points axis is invisible since it matches the one of the polynomial
    #                visible = FALSE
    #            )
    #            ,yaxis2 = list(
    #                showline = TRUE
    #                ,side = "left"
    #                ,overlaying = "y"
    #                ,title = "Genetic location (cM)"
    #                ,color = "0072B2"
    #                ,size = 3
    #                ,zeroline = FALSE
    #                ,anchor = "y"
    #                ,showgrid = FALSE
    #                ,ticks = "inside"
    #                ,exponenformat = "power"
    #                ,showlegend = FALSE
    #            )
    #            ,yaxis3 = list(
    #                showline = TRUE
    #                ,side = "right"
    #                ,overlaying = "y"
    #                ,title = "Recombination rate (cM/Mb)"
    #                ,color = "#2CA02C"
    #                ,size = 3
    #                ,zeroline = FALSE
    #                ,anchor = "y"
    #                ,showgrid = FALSE
    #                ,ticks = "inside"
    #                ,exponentformat = "power"
    #            )
    #            ,yaxis4 = list(
    #                showline = FALSE
    #                ,overlaying = "y"
    #                # ,size = 3
    #                ,zeroline = FALSE
    #                ,anchor = "y"
    #                ,showgrid = FALSE
    #                ,autorange = TRUE
    #                ,showticklabels = FALSE
    #            )
    #            ,yaxis5 = list(
    #                showline = FALSE
    #                ,overlaying = "y"
    #                # ,size = 3
    #                ,zeroline = FALSE
    #                ,anchor = "y"
    #                ,showgrid = FALSE
    #                ,autorange = TRUE
    #                ,showticklabels = FALSE
    #            )
    #     )
    # export(p5, file = str_replace_all( paste(plots_path, genomeName,"- Chr", chrID, "_p5.png"), fixed(" "), ""))
    # print(p5)
    # #------------------------------------------------------------------
    # p6 = plot_ly(x = chromosome$bp, y= chromosome$cM,
    #              type = 'scatter', mode = 'markers',
    #              yaxis = "y", name = "Experimental data",
    #              marker = list(color = "black"),
    #              hoverinfo = "text",
    #              text = ~paste('Mb:', chromosome$bp, "\n cM:",chromosome$cM)) %>%
    #
    #     add_trace( x = chromosome$bp, y = RR_object$regFn,  #polynomial
    #                type = "scatter", mode = "lines",
    #                line = list(color = "red"),
    #                yaxis = "y2", name = "Interpolation",
    #                inherit = F)  %>%
    #     add_trace( x = chromosome$bp, y = RR_object$regDr,  #RR
    #                type = "scatter", mode = "lines",
    #                yaxis = "y3",  name = "Recombination rate",
    #                line = list(shape = "linear", width = 6, connectgaps = FALSE, color = "#2CA02C"),
    #                inherit = F)  %>%
    #     add_trace( x = chromosome$bp, y = R2DataFrame2D$R2Vect_dir1,
    #                type = "scatter", mode = "lines+markers",
    #                yaxis = "y4",  name = "Rsq forward",
    #                marker = list(color = "#D62728" ),
    #                line = list(color= "#D62728" ),
    #                inherit = F) %>%
    #     add_trace( x = chromosome$bp, y = R2DataFrame2D$R2Vect_dir2,
    #                type = "scatter", mode = "lines+markers",
    #                yaxis = "y5",  name = "Rsq backwards" ,
    #                marker = list(color = "#9575D2" ),
    #                line = list(color= "#9575D2" ),
    #                inherit = F) %>%
    #
    #
    #     add_lines( x = heteroChromatinBoundaries$heteroBoundLeft,
    #               type = "scatter", mode = "lines",
    #               yaxis = "y6",  name = "Left_HB",
    #               line = list(color= "#D62728", dash= "dash", shape= "linear"),
    #               inherit = F)  %>%
    #     add_annotations(
    #         xref = "x"
    #         ,y = 80
    #         ,x = heteroChromatinBoundaries$heteroBoundRight
    #         ,text = paste("<b>",heteroChromatinBoundaries$heteroBoundRight,"</b> ")
    #         ,color = "#9575D2"
    #         ,anchor = "y6"
    #         ,showarrow = TRUE
    #         ,ax = 45
    #         ,visible = TRUE
    #     ) %>%
    #
    #     layout(#title = paste(genomeName,"- Chr", chrID),
    #            xaxis = list(
    #                showline = TRUE
    #                ,title = paste( "Physical location (Mb)" )
    #                ,color = "#0072B2"
    #                ,size = 3
    #                ,zeroline = FALSE
    #                ,ticks = "inside"
    #                ,showgrid = FALSE
    #                ,exponentformat = "power"
    #            )
    #            ,yaxis = list(  # the data points axis is invisible since it matches the one of the polynomial
    #                visible = FALSE
    #            )
    #            ,yaxis2 = list(
    #                showline = TRUE
    #                ,side = "left"
    #                ,overlaying = "y"
    #                ,title = "Genetic location (cM)"
    #                ,color = "0072B2"
    #                ,size = 3
    #                ,zeroline = FALSE
    #                ,anchor = "y"
    #                ,showgrid = FALSE
    #                ,ticks = "inside"
    #                ,exponenformat = "power"
    #                ,showlegend = FALSE
    #            )
    #            ,yaxis3 = list(
    #                showline = TRUE
    #                ,side = "right"
    #                ,overlaying = "y"
    #                ,title = "Recombination rate (cM/Mb)"
    #                ,color = "#2CA02C"
    #                ,size = 3
    #                ,zeroline = FALSE
    #                ,anchor = "y"
    #                ,showgrid = FALSE
    #                ,ticks = "inside"
    #                ,exponentformat = "power"
    #            )
    #            ,yaxis4 = list(
    #                showline = FALSE
    #                ,overlaying = "y"
    #                # ,size = 3
    #                ,zeroline = FALSE
    #                ,anchor = "y"
    #                ,showgrid = FALSE
    #                ,autorange = TRUE
    #                ,showticklabels = FALSE
    #            )
    #            ,yaxis5 = list(
    #                showline = FALSE
    #                ,overlaying = "y"
    #                # ,size = 3
    #                ,zeroline = FALSE
    #                ,anchor = "y"
    #                ,showgrid = FALSE
    #                ,autorange = TRUE
    #                ,showticklabels = FALSE
    #            )
    #            ,yaxis6 = list(
    #                showline = FALSE
    #                ,overlaying = "y"
    #                ,size = 3
    #                ,zeroline = FALSE
    #                ,showgrid = FALSE
    #                ,showticklabels = FALSE
    #            )
    #     )
    # export(p6, file = str_replace_all( paste(plots_path, genomeName,"- Chr", chrID, "_p6.png"), fixed(" "), ""))
    # print(p6)
    # #------------------------------------------------------------------
    # p7 = add_trace(p6, x = heteroChromatinBoundaries$heteroBoundRight,
    #               type = "scatter", mode = "lines",
    #               yaxis = "y7",  name = "Right_HB",
    #               line = list(color= "#9575D2", dash= "dash", shape= "linear"),
    #               inherit = F)  %>%
    #     add_annotations(
    #         xref = "x"
    #         ,y = 80
    #         ,x = heteroChromatinBoundaries$heteroBoundLeft
    #         ,text = paste("<b>",heteroChromatinBoundaries$heteroBoundLeft,"</b> ")
    #         ,color = "#D62728"
    #         ,anchor = "y7"
    #         ,showarrow = TRUE
    #         ,ax = -45
    #         ,visible = TRUE
    #     ) %>%
    #
    #     layout(#title = paste(genomeName,"- Chr", chrID),
    #            xaxis = list(
    #                showline = TRUE
    #                ,title = paste( "Physical location (Mb)" )
    #                ,color = "#0072B2"
    #                ,size = 3
    #                ,zeroline = FALSE
    #                ,ticks = "inside"
    #                ,showgrid = FALSE
    #                ,exponentformat = "power"
    #            )
    #            ,yaxis = list(  # the data points axis is invisible since it matches the one of the polynomial
    #                visible = FALSE
    #            )
    #            ,yaxis2 = list(
    #                showline = TRUE
    #                ,side = "left"
    #                ,overlaying = "y"
    #                ,title = "Genetic location (cM)"
    #                ,color = "0072B2"
    #                ,size = 3
    #                ,zeroline = FALSE
    #                ,anchor = "y"
    #                ,showgrid = FALSE
    #                ,ticks = "inside"
    #                ,exponenformat = "power"
    #                ,showlegend = FALSE
    #            )
    #            ,yaxis3 = list(
    #                showline = TRUE
    #                ,side = "right"
    #                ,overlaying = "y"
    #                ,title = "Recombination rate (cM/Mb)"
    #                ,color = "#2CA02C"
    #                ,size = 3
    #                ,zeroline = FALSE
    #                ,anchor = "y"
    #                ,showgrid = FALSE
    #                ,ticks = "inside"
    #                ,exponentformat = "power"
    #            )
    #            ,yaxis4 = list(
    #                showline = FALSE
    #                ,overlaying = "y"
    #                # ,size = 3
    #                ,zeroline = FALSE
    #                ,anchor = "y"
    #                ,showgrid = FALSE
    #                ,autorange = TRUE
    #                ,showticklabels = FALSE
    #            )
    #            ,yaxis5 = list(
    #                showline = FALSE
    #                ,overlaying = "y"
    #                # ,size = 3
    #                ,zeroline = FALSE
    #                ,anchor = "y"
    #                ,showgrid = FALSE
    #                ,autorange = TRUE
    #                ,showticklabels = FALSE
    #            )
    #            ,yaxis6 = list(
    #                showline = FALSE
    #                ,overlaying = "y"
    #                ,size = 3
    #                ,zeroline = FALSE
    #                ,showgrid = FALSE
    #                ,showticklabels = FALSE
    #            )
    #            ,yaxis7 = list(
    #                showline = FALSE
    #                ,overlaying = "y"
    #                ,size = 3
    #                ,zeroline = FALSE
    #                ,showgrid = FALSE
    #                ,showticklabels = FALSE
    #            )
    #            # ,showlegend = FALSE
    #     )
    # export(p7, file = str_replace_all( paste(plots_path, genomeName,"- Chr", chrID, "_p7.png"), fixed(" "), ""))
    # print(p7)
    #------------------------------------------------------------------

    if(chrID != "X"){
        showLeftArrow = TRUE
    }else{
        showLeftArrow = FALSE
        heteroChromatinBoundaries$heteroBoundLeft = heteroChromatinBoundaries$heteroBoundRight
    }

    p = plot_ly( data = chromosome, x = ~mb, y= ~cm,
                   type = 'scatter', mode = 'markers',
                   yaxis = "y", name = "Experimental data",
                   marker = list(color = "black"), #--------------------------------??????????????????????
                   hoverinfo = text,
                   text = ~paste('Mb:', mb, "\n cM:",cm)
                   # ,inherit = F
                ) %>%
        add_trace(x = chromosome$bp, y = RR_object$regFn,  #polynomial
                  type = "scatter", mode = "lines",
                  line = list(color = "blue"),
                  yaxis = "y", name = "Interpolation"
                 # ,inherit = F
                )  %>%
        add_trace( x = chromosome$bp, y = RR_object$regDr,  #RR
                 type = "scatter", mode = "lines",
                 yaxis = "y3",  name = "Recombination rate",
                 line = list(shape = "linear", width = 5, connectgaps = FALSE, color = "forestgreen")  #2CA02C"
                 # ,inherit = F
                 )  %>%
        add_trace(x = chromosome$bp, y = R2DataFrame2D$R2Vect_dir1,
                  type = "scatter", mode = "lines",
                  yaxis = "y4",  name = "Rsq forward",
                  line = list(color= "red" )
                  # ,inherit = F
                  )  %>%
        add_trace(x = chromosome$bp, y = R2DataFrame2D$R2Vect_dir2,
                  type = "scatter", mode = "lines",
                  yaxis = "y4",  name = "Rsq backwards" ,
                  line = list(color= "blueviolet") #9575D2
                  # ,inherit = F
                  )  %>%
        add_trace(x = heteroChromatinBoundaries$heteroBoundLeft,
                  type = "scatter", mode = "lines",
                  yaxis = "y6",  name = "Left_HB",
                  line = list(color= "red", dash= "dash", shape= "linear", width = 3)#D62728
                  # ,inherit = T
                  ,showlegend = showLeftArrow)  %>%
        add_trace(x = heteroChromatinBoundaries$heteroBoundRight,
                  type = "scatter", mode = "lines",
                  yaxis = "y7",  name = "Right_HB",
                  line = list(color= "blueviolet", dash= "dash", shape= "linear", width = 3) #9575D2
                  # ,inherit = T
                  )  %>%


        add_annotations(
            xref = "x"
            ,y = 80
            ,x = heteroChromatinBoundaries$heteroBoundRight
            ,text = paste("<b>",heteroChromatinBoundaries$heteroBoundRight,"</b> ")
            ,font = list(size = 16, color = "blueviolet")
            ,anchor = "y6"
            ,showarrow = TRUE
            ,arrowcolor = "blueviolet"
            ,arrowwidth = 3
            ,ax = 50
            ,visible = TRUE
        ) %>%
        add_annotations(
            xref = "x"
            ,y = 80
            ,x = heteroChromatinBoundaries$heteroBoundLeft
            ,text = paste("<b>",heteroChromatinBoundaries$heteroBoundLeft,"</b> ")
            ,font = list(size = 16, color = "red")
            ,anchor = "y7"
            ,showarrow = TRUE
            ,arrowcolor = "red"
            ,arrowwidth = 3
            ,ax = -50
            ,visible = showLeftArrow
        ) %>%

        layout(#title = paste(genomeName,"- Chr", chrID)
               xaxis = list(
                   showline = TRUE
                   ,title = paste( "Physical location (Mb)" )
                   ,titlefont = list(size= 20)
                   ,color = "black"
                   ,zeroline = FALSE
                   ,showgrid = FALSE
                   ,exponentformat = "power"
                   ,tickwidth = 3
                   ,tickcolor = "black"
                   ,showticklabels = TRUE
                   ,tickfont = list(size = 18)
               )
               ,yaxis = list(  # the data points axis is invisible since it matches the one of the polynomial
                   showline = TRUE
                   ,side = "left"
                   ,title = "Genetic location (cM)"
                   ,titlefont = list(size= 20)
                   ,color = "blue"
                   ,zeroline = FALSE
                   ,showgrid = FALSE
                   ,exponenformat = "power"
                   ,tickwidth = 3
                   ,tickcolor = "blue"
                   ,showticklabels = TRUE
                   ,tickfont = list(size = 18)
               )
               ,yaxis2 = list(
                   visible = FALSE
               )
               ,yaxis3 = list(
                   showline = TRUE
                   ,side = "right"
                   ,overlaying = "y"
                   ,title = "Recombination rate (cM/Mb)"
                   ,titlefont = list(size= 20)
                   ,color = "forestgreen" #2CA02C"
                   ,zeroline = FALSE
                   ,anchor = "y"
                   ,showgrid = FALSE
                   ,exponentformat = "power"
                   ,tickwidth = 3
                   ,showticklabels = TRUE
                   ,tickfont = list(size = 18)
               )
               ,yaxis4 = list(
                   visible = FALSE
                   ,showline = TRUE
                   ,side = "right"
                   ,overlaying = "y"
                   ,title = "Cumulative R2"
                   ,titlefont = list(size= 14)
                   ,zeroline = FALSE
                   ,anchor = "y"
                   ,showgrid = FALSE
                   ,autorange = TRUE
                   ,tickwidth = 3
                   ,showticklabels = TRUE
               )
               ,yaxis5 = list(
                   visible = FALSE
               )
               ,yaxis6 = list(
                   showline = FALSE
                   ,overlaying = "y"
                   ,zeroline = FALSE
                   ,anchor = "y"
                   ,showgrid = FALSE
                   ,showticklabels = FALSE
               )
               ,yaxis7 = list(
                   showline = FALSE
                   ,overlaying = "y"
                   ,zeroline = FALSE
                   ,anchor = "y"
                   ,showgrid = FALSE
                   ,showticklabels = FALSE
               )
               ,showlegend = TRUE
               ,legend =  list(font = list(size = 15, color = "#000") #family = "sans-serif",
                               ,bgcolor = "#E2E2E2"
                               ,bordercolor = "#FFFFFF"
                               ,borderwidth = 2
                               ,x= 1.15
                               ,xanchor = "left"
               )
        )
    # orca(p, file = str_replace_all( paste(plots_path, genomeName,"- Chr", chrID, ".png"), fixed(" "), ""), format = tools::file_ext(file), scale = NULL,
    #      width = 1400, height = 900, mathjax = FALSE, parallel_limit = NULL,
    #      verbose = FALSE, debug = FALSE, safe = FALSE) # latest version of : export

    print(p)
    return(p)
}
