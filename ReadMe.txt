Website for shinyApp: https://wparker-uchicago.shinyapps.io/ventilator_allocation/

If the user is interested in testing different allocation strategies, follow the link above for the shinyApp.
Instructions:
1. On the left-side, the tab allows you to choose the number of patients and the number of ventilators.
2. Choose a number of ventilators that is less than the number of patients in order to simulate a scarce resource scenario.
3. On the right-side, the tabs allow you to visualize the results of the different allocation strategies.
4. The "Compare Systems" tab allows you to visualize and compare the results of all 8 systems.

Figure legend:
Age in years is the x-axis and SOFA score is the y-axis. 
Each point represents a patient in the simulation. 
The shape of the point is related to the comorbidity burden of the patient. 
The color represents whether or not they recieved a ventilator, and if they recieved a ventilator, whether they survived or died.


If the user is interested in the source code for the shinyapp, it is available at: https://github.com/08wparker/ventilator_allocation/blob/master/ventilator_app.R
Required R packages:
library(shiny)
library(tidyverse)
library(truncnorm)
library(shinythemes)

	
If the user is interested in simulating 10,000 runs of these different allocation strategies, the R code is available at: https://github.com/08wparker/ventilator_allocation/blob/master/analysis.Rmd
Required R packages:
library(tidyverse)
library(truncnorm)
library(cowplot)


Instructions:
1. Download required packages on R
2. Run code on R markdown
3. Below Line 123: Figure: SOFA vs mortality based on large multi-center study in JAMA
4. Line 140-151: The following parameters can be adjusted to reflect institutional variations in SOFA scores:
	A. sofa_int = mean SOFA score for population of patients requiring intubation
	B. sd_sofa = standard deviation of SOFA score in this population

Figures:
1. Below Line 261: Figure: Age vs Comorbidity burden based on University of Chicago data
2. Below Line 268: Figure: SOFA vs Comorbidity burden based on University of Chicago data
3. Below Line 419: Figure: Sample simulation run of Sickest-First allocation strategy
4. Below Line 491: Figure: Sample simulation run of New York state allocation strategy
5. Below Line 546: Figure: Sample simulation run of Maryland state allocation strategy
6. Below Line 604: Figure: Sample simulation run of Pittsburgh allocation strategy
7. Below Line 651: Figure: Sample simulation run of Lottery allocation strategy
8. Below Line 696: Figure: Sample simulation run of Youngest-First allocation strategy
9. Below Line 743: Figure: Sample simulation run of Maximizing Lives Saved allocation strategy
10. Below Line 797: Figure: Sample simulation run of Maximizing Life-years Saved allocation strategy
11. Below Line 858: Figure: Sample simulation runs comparing all eight strategies
12. Below Line 897: Figure: Violin plot showing the results of 10,000 simulations in terms of Lives Saved across the 8 allocation strategies
13. Below Line 912: Figure: Violin plot showing the results of 10,000 simulations in terms of Life-years Saved across the 8 allocation strategies



