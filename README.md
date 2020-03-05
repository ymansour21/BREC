## BREC

BREC is an automated, computational and non-genome-specific solution based on the Marey maps method in order to identify eu-heterochromatin boundaries along chromosomes, and provide local recombination rate estimates. This approach allows to determin the location of the centromeric and telomeric regions known to present a reduced recombination rate in most eukaryotic genomes.

## Download and install BREC package from within an R enviroment

```r
# Install devtools and shiny from CRAN
install.packages("devtools", "shiny")
# Load installed libraries
library(devtools, shiny)
           
# Download and install the BREC package from the GitHub repository
install_github("ymansour21/BREC")
# Load Brec and shiny
library(Brec)
library(shiny) 
# Launch Brec graphical interface in your default internet browser
runApp("shinyApp/Brec_dashboard.R", launch.browser = TRUE)
```

Further details and examples will be avalable soon :)
