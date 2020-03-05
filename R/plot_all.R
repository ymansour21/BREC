#' Generate the interactive plot for recombinantion rate and chromatin boundaries
#'
#' generate interactive plot for the specified chromosome using plot_ly
#'
#' the generated plot is saved as png image and the interactive version will open up in the browser as html page
#'
#' @seealso \code{link{plolty}} and \code{link{ggplot2}}
#'
#' @param chromosome xxx
#' @param RR_object xxx
#' @param genomeName x
#' @param chrID x
#' @param R2DataFrame2D x
#' @param heteroChromatinBoundaries x
#' @param swSize x
#' @param plots_path xx
#' @param telomeres_boundaries ...
#' @param chrType_object ....
#'
#' @import plotly
#'
#' @return plot_ly object
#' @export

plot_all <- function(chromosome, RR_object, genomeName, chrID, R2DataFrame2D, heteroChromatinBoundaries, swSize, telomeres_boundaries, plots_path, chrType_object) {

  min_rr = chromosome$mb[match(min(RR_object$regDr), RR_object$regDr)]

  cusMargins<- list(
      r = 100
      # pad = 9
  )
  chrType = chrType_object$chr_type

  last_marker_pos = chromosome$mb[nrow(chromosome)] # needed for Highlighting HCB on pthe plot
  first_marker_pos = chromosome$mb[1]

  rect_y1 = max(RR_object$regDr)
  rect_y0 = min(RR_object$regDr)


  if(chrType == 1){   #whole chromosome : works on metacentric chromosomes =================================================================================

    # === Highlighting with Rectangles => shapes x coordinates to match HCB ===
    # ----add---- metacentric --
    x0_meta_centro = heteroChromatinBoundaries$heteroBoundLeft
    x1_meta_centro = heteroChromatinBoundaries$heteroBoundRight

    x0_left_telo = first_marker_pos
    x1_left_telo = telomeres_boundaries$telo_left

    x0_right_telo = telomeres_boundaries$telo_right
    x1_right_telo = last_marker_pos

    p = plot_ly(data = chromosome, x = ~mb, y= ~cm,
                  type = 'scatter', mode = 'markers',
                  yaxis = "y", name = "Genetic marker",
                  hoverinfo = text,
                  text = ~paste('Mb:', mb, "\n cM:",cm)
                  ) %>% # plot dataSet
        add_trace(x = chromosome$mb, y = RR_object$regFn,  #polynomial
                  type = "scatter", mode = "lines",
                  yaxis = "y", name = "Regression model")  %>%
        add_trace(x = chromosome$mb, y = RR_object$regDr,  #RR
                  type = "scatter", mode = "lines",
                  yaxis = "y3",  name = "Recombination rate",
                  line = list(shape = "linear", width = 5, connectgaps = FALSE))  %>%
        # add_trace(x = chromosome$mb, y = R2DataFrame2D$R2Vect_dir1,
        #           type = "scatter", mode = "lines+markers",
        #           yaxis = "y4",  name = "Rsq forward",
        #           line = list(color= "#D62728" )) %>%
        # add_trace(x = chromosome$mb, y = R2DataFrame2D$R2Vect_dir2,
        #           type = "scatter", mode = "lines+markers",
        #           yaxis = "y4",  name = "Rsq backwards" ,
        #           line = list(color= "#9575D2" )) %>%
        add_lines(x = heteroChromatinBoundaries$heteroBoundLeft,
                  type = "scatter", mode = "lines",
                  yaxis = "y6",  name = "Centromeric boundary",
                  line = list(color= "brown", dash= "dash", shape= "linear", width = 3)
                  ,showlegend = TRUE)  %>%
        add_trace(x = heteroChromatinBoundaries$heteroBoundRight,
                  type = "scatter", mode = "lines",
                  yaxis = "y7",  name = "Centromeric boundary",
                  line = list(color= "brown", dash= "dash", shape= "linear", width = 3))  %>%
        add_lines(x = telomeres_boundaries$telo_left,
                  type = "scatter", mode = "lines",
                  yaxis = "y8",  name = "Telomeric boundary",
                  line = list(color= "grey", dash= "dash", shape= "linear", width = 3)
                  ,showlegend = TRUE)  %>%
        add_trace(x = telomeres_boundaries$telo_right,
                  type = "scatter", mode = "lines",
                  yaxis = "y9",  name = "Telomeric boundary",
                  line = list(color= "grey", dash= "dash", shape= "linear", width = 3))  %>%

        add_annotations(
          xref = "x"
          ,y = 80
          ,x = heteroChromatinBoundaries$heteroBoundRight
          ,text = paste("<b>",heteroChromatinBoundaries$heteroBoundRight,"</b> ")
          ,font = list(size = 16,color = "brown") ##9575D2
          ,anchor = "y6"
          ,showarrow = TRUE
          ,arrowcolor = "black" #"#9575D2"
          ,arrowwidth = 3
          ,ax = 50
          ,visible = TRUE
        ) %>%
        add_annotations(
          xref = "x"
          ,y = 80
          ,x = heteroChromatinBoundaries$heteroBoundLeft
          ,text = paste("<b>",heteroChromatinBoundaries$heteroBoundLeft,"</b> ")
          ,font = list(size = 16,color =  "brown") #"#D62728")
          ,anchor = "y7"
          ,showarrow = TRUE
          ,arrowcolor = "black" #"#D62728"
          ,arrowwidth = 3
          ,ax = -50
          ,visible = TRUE
        ) %>%
        add_annotations(
            xref = "x"
            ,y = 80
            ,x = telomeres_boundaries$telo_left
            ,text = paste("<b>",telomeres_boundaries$telo_left,"</b> ")
            ,font = list(size = 16,color = "black")
            ,anchor = "y8"
            ,showarrow = TRUE
            ,arrowcolor = "black"
            ,arrowwidth = 3
            ,ax = 50
            ,visible = TRUE
        ) %>%
        add_annotations(
            xref = "x"
            ,y = 80
            ,x = telomeres_boundaries$telo_right
            ,text = paste("<b>",telomeres_boundaries$telo_right,"</b> ")
            ,font = list(size = 16,color = "black")
            ,anchor = "y9"
            ,showarrow = TRUE
            ,arrowcolor = "black"
            ,arrowwidth = 3
            ,ax = -50
            ,visible = TRUE
        ) %>%

        layout(title = list(
                  text = paste(genomeName," ", "Chromosome", chrID, "-", chrType_object$chr_sub_type)
                  ,font = list(size= 15, color = "red")
                )
               ,xaxis = list(
                 showline = TRUE
                 ,title = paste( "Physical location (Mb)" )
                 ,titlefont = list(size= 20)
                 ,color = "#0072B2"
                 ,zeroline = FALSE
                 ,showgrid = FALSE
                 ,exponentformat = "power"
                 ,tickwidth = 3
                 ,tickcolor = "#0072B2"
                 ,showticklabels = TRUE
                 ,tickfont = list(size = 18)
               )
               ,yaxis = list(  # the data points axis is invisible since it matches the one of the polynomial
                   showline = TRUE
                   ,side = "left"
                   ,title = "Genetic location (cM)"
                   ,titlefont = list(size= 20)
                   ,color = "#0072B2"
                   ,zeroline = FALSE
                   ,showgrid = FALSE
                   ,exponenformat = "power"
                   ,tickwidth = 3
                   ,tickcolor = "#0072B2"
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
                 ,color = "#2CA02C"
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
                 ,showline = FALSE
                 ,side = "right"
                 ,overlaying = "y"
                 ,title = "Cumulative Rsq"
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
               ,yaxis8 = list(
                   showline = FALSE
                   ,overlaying = "y"
                   ,zeroline = FALSE
                   ,anchor = "y"
                   ,showgrid = FALSE
                   ,showticklabels = FALSE
               )
               ,yaxis9 = list(
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

               ,shapes = list( # Highlighting with Rectangles
                 list(type = "rect",  name = "Centromeric region",
                      fillcolor = "brown", line = list(color = "brown"), opacity = 0.3,
                      x0 = x0_meta_centro, x1 = x1_meta_centro, xref = "x",
                      y0 = rect_y0, y1 = rect_y1, yref = "y3"),
                 list(type = "rect",  name = "Telomeric region",
                      fillcolor = "grey", line = list(color = "grey"), opacity = 0.3,
                      x0 = x0_left_telo, x1 = x1_left_telo, xref = "x",
                      y0 = rect_y0, y1 = rect_y1, yref = "y3"),
                 list(type = "rect",  name = "Teloomeric region",
                      fillcolor = "grey", line = list(color = "grey"), opacity = 0.3,
                      x0 = x0_right_telo, x1 = x1_right_telo, xref = "x",
                      y0 = rect_y0, y1 = rect_y1, yref = "y3")
               )

        )

  }else{  #chromosomal arm : works on telocentric chromosomes =====================================================================

    # === Highlighting with Rectangles => shapes x coordinates to match HCB ===
    if(telomeres_boundaries$telo_arm < heteroChromatinBoundaries$heteroBoundArm){
      # -------- telocentric -- left arm
      x0_arm_centro = min(heteroChromatinBoundaries$heteroBoundArm, last_marker_pos)
      x1_arm_centro = max(heteroChromatinBoundaries$heteroBoundArm, last_marker_pos)
      x0_arm_telo = min(telomeres_boundaries$telo_arm, first_marker_pos)
      x1_arm_telo = max(telomeres_boundaries$telo_arm, first_marker_pos)

      arrow_angle_left = 50
      arrow_angle_right = -50
    }else{
      # -------- telocentric -- right arm
      x0_arm_centro = min(heteroChromatinBoundaries$heteroBoundArm, first_marker_pos)
      x1_arm_centro = max(heteroChromatinBoundaries$heteroBoundArm, first_marker_pos)
      x0_arm_telo = min(telomeres_boundaries$telo_arm, last_marker_pos)
      x1_arm_telo = max(telomeres_boundaries$telo_arm, last_marker_pos)

      arrow_angle_left = -50
      arrow_angle_right = 50
    }

      p = plot_ly(data = chromosome, x = ~mb, y= ~cm,
                  type = 'scatter', mode = 'markers',
                  yaxis = "y", name = "Genetic marker",
                  hoverinfo = text,
                  text = ~paste('Mb:', mb, "\n cM:",cm)
      ) %>% # plot dataSet
          add_trace(x = chromosome$mb, y = RR_object$regFn,  #polynomial
                    type = "scatter", mode = "lines",
                    yaxis = "y", name = "Regression model")  %>%
          add_trace(x = chromosome$mb, y = RR_object$regDr,  #RR
                    type = "scatter", mode = "lines",
                    yaxis = "y3",  name = "Recombination rate",
                    line = list(shape = "linear", width = 5, connectgaps = FALSE))  %>%
          # add_trace(x = chromosome$mb, y = R2DataFrame2D$R2Vect_dir1,
          #           type = "scatter", mode = "lines+markers",
          #           yaxis = "y4",  name = "Rsq forward",
          #           line = list(color= "#D62728" )) %>%
          # add_trace(x = chromosome$mb, y = R2DataFrame2D$R2Vect_dir2,
          #           type = "scatter", mode = "lines+markers",
          #           yaxis = "y4",  name = "Rsq backwards" ,
          #           line = list(color= "#9575D2" )) %>%
          add_lines(x = heteroChromatinBoundaries$heteroBoundArm,
                    type = "scatter", mode = "lines",
                    yaxis = "y6",  name = "Centromeric boundary",
                    line = list(color= "brown", dash= "dash", shape= "linear", width = 3)
                    ,showlegend = TRUE)  %>%
          add_lines(x = telomeres_boundaries$telo_arm,
                    type = "scatter", mode = "lines",
                    yaxis = "y8",  name = "Telomeric boundary",
                    line = list(color= "black", dash= "dash", shape= "linear", width = 3)
                    ,showlegend = TRUE)  %>%

          add_annotations(
              xref = "x"
              ,y = 80
              ,x = heteroChromatinBoundaries$heteroBoundArm
              ,text = paste("<b>",heteroChromatinBoundaries$heteroBoundArm,"</b> ")
              ,font = list(size = 16,color = "brown")
              ,anchor = "y6"
              ,showarrow = TRUE
              ,arrowcolor = "black"
              ,arrowwidth = 3
              ,ax = arrow_angle_right
              ,visible = TRUE
          ) %>%
          add_annotations(
              xref = "x"
              ,y = 80
              ,x = telomeres_boundaries$telo_arm
              ,text = paste("<b>",telomeres_boundaries$telo_arm,"</b> ")
              ,font = list(size = 16,color = "black")
              ,anchor = "y8"
              ,showarrow = TRUE
              ,arrowcolor = "black"
              ,arrowwidth = 3
              ,ax = arrow_angle_left
              ,visible = TRUE
          ) %>%

          layout(title = list(
                    text = paste(genomeName," ", "Chromosome", chrID, "-", chrType_object$chr_sub_type)
                    ,font = list(size= 15, color = "red")
                  )
                  ,xaxis = list(
                     showline = TRUE
                     ,title = paste( "Physical location (Mb)" )
                     ,titlefont = list(size= 20)
                     ,color = "#0072B2"
                     ,zeroline = FALSE
                     ,showgrid = FALSE
                     ,exponentformat = "power"
                     ,tickwidth = 3
                     ,tickcolor = "#0072B2"
                     ,showticklabels = TRUE
                     ,tickfont = list(size = 18)
                 )
                 ,yaxis = list(  # the data points axis is invisible since it matches the one of the polynomial
                     showline = TRUE
                     ,side = "left"
                     ,title = "Genetic location (cM)"
                     ,titlefont = list(size= 20)
                     ,color = "#0072B2"
                     ,zeroline = FALSE
                     ,showgrid = FALSE
                     ,exponenformat = "power"
                     ,tickwidth = 3
                     ,tickcolor = "#0072B2"
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
                     ,color = "#2CA02C"
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
                     ,showline = FALSE
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
                 ,yaxis8 = list(
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

                 ,shapes = list( # Highlighting with Rectangles
                       list(type = "rect", name = "Centromeric region",
                            fillcolor = "brown", line = list(color = "brown"), opacity = 0.3,
                            x0 = x0_arm_centro, x1 = x1_arm_centro, xref = "x",
                            y0 = rect_y0, y1 = rect_y1, yref = "y3"),
                       list(type = "rect", name = "Telomeric region",
                            fillcolor = "grey", line = list(color = "grey"), opacity = 0.3,
                            x0 = x0_arm_telo, x1 = x1_arm_telo, xref = "x",
                            y0 = rect_y0, y1 = rect_y1, yref = "y3")
                   )
          )

  }

  return(p)
}
