# # #  inspired from https://www.datanovia.com/en/blog/how-to-plot-a-smooth-line-using-ggplot2/
# #
# library(ggplot2)
#
#
# inputData = read.csv(file = "data/Dmel_R6_formatted_v2.csv", header = T, sep = "\t")
# chrList = get_list_of_chromosomes(inputData)
#
# # transform_inputData_to_chromosomes() adapted : here using specific arm sizes
# newInputData = data.frame()
# for (i in seq(1, length(chrList)-1, by =2 )){ # refine stop condition for the loop as per number of chromosomic arms + lonly arms must be at the end of the list
#     if(i == 1){
#         lenghtOfFirstArm = 23.513712 # size(2L_R6)
#     }else if(i == 3){
#         lenghtOfFirstArm = 28.110227 # size(3L_R6)
#     }
#     firstArm = get_chromosome_from_Arms(inputData, chrList[i])
#     secondArm = get_chromosome_from_Arms(inputData, chrList[i+1])
#     chromosome = transform_2arms_to_chromosome(firstArm, secondArm, lenghtOfFirstArm) # merge both arms into one chr : remember coord normalisation
#     newInputData = rbind.data.frame(newInputData, chromosome)
# }
# chromosome = get_chromosome_from_Arms(inputData, chrList[length(chrList)])
# newInputData = rbind.data.frame(newInputData, chromosome)
# inputData = newInputData
# chrList = get_list_of_chromosomes(inputData)
#
#
# for (chrID in "2") {#chrList
#
#     print(c("=========== chr in process ====> ", chrID, "==================="))
#     refChromosome = get_chromosome_from_inputData(inputData, chrID)
#     print(c("refChromosomeSize",nrow(refChromosome)))
#     # cleaning step
#     refChromosome = clean_chromosome_data(refChromosome, genomeName = "DmelR6", chrID) #=> chr2 : 240 instead of 267
#     refChromosomeSize = nrow(refChromosome)
#     print(c("refChromosome after cleaning --> size = ", refChromosomeSize))
#
#     # ************ RUN BREC TO GET testCB dataframe*******************************************************************
#     # ********************************************************************************************************************************
#      cm = refChromosome$cm
#      mb = refChromosome$mb
#
#
#
#     p1 <- ggplot(refChromosome, aes(mb, cm)) +
#         geom_point()
#
#     # Add regression line
#     p2 = p1 + geom_smooth(method = lm)
#     p2
#     # Polynomial interpolation
#     # Remove the confidence bande: se = FALSE
#     p3= p1 + geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE)
#     p3
#     # loess method: local regression fitting
#     p4 = p1 + geom_smooth(method = "loess")
#     p4
#     # Spline interpolation
#     spline.d <- as.data.frame(spline(refChromosome$mb, refChromosome$cm))
#     p5 = p1 + geom_line(data = spline.d, aes(x = x, y = y))
#     p5
# }
