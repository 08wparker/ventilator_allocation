Website for shinyApp: https://wparker-uchicago.shinyapps.io/ventilator_allocation/

If the user is interested in testing different allocation strategies, follow the link above for the shinyApp.
Instructions:
1. On the left-side, the tab allows you to choose the number of patients and the number of ventilators.
2. Choose a number of ventilators that is less than the number of patients in order to simulate a scarce resource scenario.
3. Press the simulate button.
4. On the right-side, the tabs allow you to visualize the results of the different ventilator allocation strategies.
5. The "Compare Systems" tab allows you to visualize and compare the results of all 8 systems.

Figure legend:
X-axis: Age in years
Y-axis: SOFA score 
Each dot represents a patient in the simulation. 
The shape of the dot is related to the comorbidity burden of the patient. 
The color of the dot represents whether or not they received a ventilator, and if they received a ventilator, whether they survived or died.

If the user is interested in detailed results of the simulation, it is available at: https://rpubs.com/sbhavani/599492
Tabs on the left-side for navigation:
	A. Simulation using CDC data - simulation of age, comorbidity burden and SOFA scores
	B. Applying triage rules to the samples - example simulation for each allocation score
	C. Commented out simulation plot - comparison of example simulation across all eight allocation systems
	D. Comparing allocation systems - comparison of lives and life-years saved across 10,000 simulation runs
	
If the user is interested in the source code for the shinyapp, it is available at: https://github.com/08wparker/ventilator_allocation/blob/master/ventilator_app.R
Required R packages:
library(shiny)
library(tidyverse)
library(truncnorm)
library(shinythemes)
	
If the user is interested in simulating multiple runs of these different allocation strategies, the R code is available at: https://github.com/08wparker/ventilator_allocation/blob/master/analysis.Rmd
Required R packages:
library(tidyverse)
library(truncnorm)
library(cowplot)

Instruction to alter parameters or run code in R:
1. Download required packages on R
2. Line 133: Alter the number of patients to use in the simulation (default 1000)
3. Line 143-151: The following parameters (A-I) reflect age~comorbidity~SOFA relationships, and should be altered only if robust institutional data reflects the need:
	A. sofa_int - intercept of SOFA score in regression: SOFA ~ age + (major comorbidity) + (severe comorbidity)
	B. sd_sofa - standard deviation of SOFA scores in population
	C. age_slope - increase in SOFA score per unit increase in age in regression: SOFA ~ age + (major comorbidity) + (severe comorbidity)
	D. major_sofa - increase in SOFA score for major comorbidity burden compared to none in regression: SOFA ~ age + (major comorbidity) + (severe comorbidity)
	E. severe_sofa - increase in SOFA score for severe comorbidity burden compared to none in regression: SOFA ~ age + (major comorbidity) + (severe comorbidity)
	F. major_cons - log odds of having major comorbidity compared to none in multinomial regression: Comorbidity burden ~ age
	G. major_slope - increase in log odds of having major comorbidity compared to none with each year increase in age in multinomial regression: Comorbidity burden ~ age
	H. severe_cons - log odds of having severe comorbidity compared to none in multinomial regression: Comorbidity burden ~ age
	I. severe_slope - increase in log odds of having severe comorbidity compared to none with each year increase in age in multinomial regression: Comorbidity burden ~ age
4. Line 283: Alter the number of simulation runs (default 10000)
5. Line 299: Alter the number of ventilators available (degree scarcity is the number of ventilators divided by the number of patients - default 0.5. This default for 1000 patients reflects 500 ventilators)
6. Run all chunks
