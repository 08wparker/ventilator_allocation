#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(truncnorm)
library(shinythemes)


ui <- fluidPage(theme = shinytheme("sandstone"),
    # App title ----
    fluidRow(column(9, h1("Simulation of Ventilator Allocation Strategies During the COVID-19 Pandemic"))),
             #column(3, tags$img( src = "U_of_shield.png", height = 106, width = 83, align = "right"))),
    
    withMathJax(),
    
    h4(tags$a(href = "https://github.com/08wparker/ventilator_allocation", 
             "https://github.com/08wparker/ventilator_allocation")),
    
    hr(),
    
    sidebarLayout(
        sidebarPanel(
            h5("Select the number of patients requiring mechanical ventilation 
                                       and the number of ventilators available at the hospital"),
                    sliderInput(inputId = "patients",
                                      label = "Number of patients",
                                      min = 1,
                                      max = 2000,
                                      value = 1000),
                     sliderInput(inputId = "vents",
                                      label = "Number of ventilators",
                                      min = 1,
                                      max = 2000,
                                      value = 500),
                     actionButton("sim", "Simulate",class = "btn-primary"),
                    hr(),
                    h4("Outcomes by Allocation System"),
                    tableOutput("table"),
                    textOutput("total_life_years"),
            width =  4),
        
    mainPanel(
        h2("Select Allocation System"),
        
        tabsetPanel(
                    tabPanel("Sickest First", 
                             plotOutput('sickest'),
                             helpText("Prioritizing patients with the highest SOFA scores")),
                    tabPanel("Lottery", 
                             plotOutput('lottery'),
                             helpText("Random ventilator assignment")),
                    tabPanel("Youngest First", 
                             plotOutput('youngest'),
                             helpText("Prioritizing the youngest patients")),
                    tabPanel("New York", 
                             plotOutput('ny'),
                             helpText("Prioritizing usng SOFA tiers, with random assignment within tiers (SOFA<7, SOFA 8-11, SOFA>11)")),
                    tabPanel("Maryland",
                             plotOutput('maryland'),
                             helpText("Prioritizing using SOFA tiers with additional points for severe comorbidity; age used as tie-breaker within tiers (SOFA<9, SOFA 9-11, SOFA 12-14, SOFA>14)")),
                    tabPanel("Pittsburgh",
                             plotOutput('multi'),
                             helpText("Prioritizing using SOFA tiers with additional points for major or severe comorbidity; age used as tie-breaker within tiers (SOFA<6, SOFA 6-8, SOFA 9-11, SOFA>11)")),
                    tabPanel("Maximize Lives Saved", 
                             plotOutput('max_icu'),
                             helpText("Prioritizing patients with the lowest SOFA scores")),
                    tabPanel("Maximize Life-Years Saved", 
                             plotOutput('max_lf'),
                             helpText("Rank patients based on $$Score = P(survival)*(100 - age)$$")
                             ),
                    tabPanel("Compare Systems", plotOutput('combined')),
                    type = "tabs",
                    selected = "Compare Systems"
        ), 
        headerPanel(" "),
        headerPanel(" "),
        headerPanel(" "),
        headerPanel(" "),
        headerPanel(" "),
        p("Simulated results under varying ventilator triage rules,",
          strong("Click each tab for details"),
          "The x-axis is the patient's age and the y-axis is the patient's SOFA score at the time of respiratory failure.",
          "Colors correspond to allocation and outcome.",
          "Green: allocated a ventilator and survived, Red: allocated a ventilator and died despite treatment,",
          "Blue: assigned to palliative care (comfort measures) only with no ventilator allocation"),
        width = 8)
    ),
    
    
    h2("Simulated Population"),
    
    hr(),
    
    fluidRow(
            column(4, h4("Chronic disease distribution by Age"), 
                     plotOutput("age_chronic"),
                     helpText("Age distribution from",
                              tags$a(href = "https://www.cdc.gov/mmwr/volumes/69/wr/mm6912e2.htm?s_cid=mm6912e2_w", 
                                     "CDC Report: Severe Outcomes Among Patients with Coronavirus Disease 2019 (COVID-19) — United States, February 12–March 16, 2020"))
              ),
             column(4, h4("SOFA distribution by Age"),
                    tableOutput("sofa_age_table"),
                    helpText("SOFA and chronic disease state distribution dervived clinical state at the time of intubation from a",
                             tags$a(href = "https://pubmed.ncbi.nlm.nih.gov/27649072/", "patients with suspected infection at a large tertiary care hospital system"))),

             column(4, h4("SOFA distribution by chronic disease state"), 
                    plotOutput("sofa_chronic"))
             ),
    
    hr(),
    h2("SOFA calibration"),
    fluidRow(column(6,p(tags$a(href = "https://en.wikipedia.org/wiki/SOFA_score", "The Sequential Organ Failure Assesment (SOFA) score"), 
                        "is a validated bedside predictor of hospital mortality. The calibration of SOFA scores used in this model is from", 
                        tags$a(href = "https://jamanetwork.com/journals/jama/fullarticle/2598267", 
                                            "Prognostic Accuracy of the SOFA Score, SIRS Criteria, and qSOFA Score for In-Hospital Mortality Among Adults With Suspected Infection Admitted to the Intensive Care Unit. Raith et al. JAMA, 2017") 
    )),
             column(6,plotOutput("sofa_calib"))),
    
    h4(strong("Siva Bhavani, MD"), " William Miller MD, Xuan Han MD, Monica Malec MD, Lainie F Ross MD, PhD, Mark Siegler MD,", strong("William F. Parker MD, MS")),
)


numextract <- function(string){ 
    as.numeric(str_extract(string, "\\-*\\d+\\.*\\d*"))
} 

comma <- function(x){
    case_when(
        abs(x) > 10 ~ format(x, digits = 0, big.mark = ",", scientific = F),
        abs(x) > 1 ~ format(x, digits = 2, big.mark = ",", scientific = F),
        TRUE ~ format(x, digits = 2, big.mark = ",", scientific = F)
    )
    
}

## load in needed data
cdc_data <- read_csv("cdc_data.csv") %>%
    separate(`Age Group`, c("Age", "N"), sep = "\\s") %>%
    mutate(N = numextract(N),
           Age = factor(Age, levels = c("0–19", "20–44", "45–54",
                                        "55–64", "65–74", "75–84",
                                        "≥85")))

## select the "worst case" ICU age distribution from the CDC data
ICU_dist <- cdc_data %>%
    select(Age,
           N,
           hosp = low_hosp,
           icu = low_ICU,
           dead = low_death) %>%
    mutate(n_hosp = N*hosp/100,
           n_icu = N*icu/100,
           n_dead = N*dead/100)

total_ICU <- sum(ICU_dist$n_icu)

ICU_dist <- ICU_dist %>%
    mutate(pct_icu_pop = n_icu/total_ICU) %>%
    filter(Age != "0–19") %>%
    mutate(
        max_age = case_when(
            Age == "20–44" ~ 44,
            Age == "45–54" ~ 54,
            Age == "55–64" ~ 64,
            Age == "65–74" ~ 74,
            Age == "75–84" ~ 84,
            TRUE ~ 94
        ),
        min_age = case_when(
            Age == "20–44" ~ 20,
            Age == "45–54" ~ 45,
            Age == "55–64" ~ 55,
            Age == "65–74" ~ 65,
            Age == "75–84" ~ 75,
            TRUE ~ 85)
    ) %>% 
    select(Age, min_age, max_age, pct_icu_pop)


# Read in a file containing the SOFA calibration data
SOFA <- read_csv("SOFA.csv") 

tot_SOFA <- sum(SOFA$N)

SOFA <- SOFA %>%
    mutate(sofa_num = ifelse(SOFA == ">=20", 20, as.numeric(SOFA)),
           SOFA = factor(SOFA, levels = c(seq(0,19), ">=20")),
           pct_SOFA = N/tot_SOFA)


# Main population and SOFA simulation function

simulate_ICU_pop <- function(df, N = tot_patients, 
                             sofa, 
                             sofa_int = 6.915957, 
                             sd_sofa = 2.959331, 
                             age_slope = .0084102,
                             major_sofa = .4131818,
                             severe_sofa = .7917162,
                             major_cons = -2.226575,
                             major_slope = .0232133,
                             severe_cons = -5.306193,
                             severe_slope = .0366836){
  
  probs <- df$pct_icu_pop
  
  age_cats <- rmultinom(1, size = N, pr = probs)
  
  sample <- tibble( age = numeric())
  
  i <- 1
  
  #  age <- vector(mode = "numeric", length = N)
  
  for (n_cats in age_cats) {
    
    subgroup <- df[i,]
    
    # randomly sample ages from the empiric distribution which is a series of uniforms
    min_age <- subgroup$min_age
    max_age <- subgroup$max_age
    age_sample <- runif(n = n_cats, min = min_age, max = max_age)
    
    age_sample
    
    sample <- sample %>% 
      rbind(tibble(age = age_sample)) 
    
    i <- i + 1
    
  }    
  
  age_sample <- sample$age
  
  # sample chronic conditions
  z_major <-  major_slope*age_sample + major_cons
  z_severe <-  severe_slope*age_sample + severe_cons
  
  p_major <- exp(z_major)/(1 + exp(z_major) + exp(z_severe))
  
  p_severe <- exp(z_severe)/(1 + exp(z_major) + exp(z_severe))
  
  p_none <- 1/(1 + exp(z_major) + exp(z_severe))
  
  multi_nom_samp <- function(none, chronic, severe){
    sample(x = c("none", "major", "severe"), 
           size =1, 
           prob = c(none, chronic, severe))
  }
  
  chronic_disease_state <- mapply(multi_nom_samp, 
                                  none = p_none, 
                                  chronic = p_major, 
                                  severe = p_severe)
  
  sample <- sample %>%
    cbind(chronic_disease_state) %>%
    mutate(mu = case_when(
      chronic_disease_state == "none" ~ sofa_int + age_slope*age,
      chronic_disease_state == "major" ~ sofa_int + age_slope*age + major_sofa,
      chronic_disease_state == "severe"  ~ sofa_int + age_slope*age + severe_sofa)
    )
  
  
  sofa_num <- round(rtruncnorm(mean = sample$mu, n =1, a =3, b = 20, sd = sd_sofa))
  
  
  survival_chance <-runif(n = N)
  
  sofa <- SOFA
  sample <- sample %>%
    cbind(sofa_num) %>%
    cbind(survival_chance) %>%
    left_join(sofa %>% select(SOFA, sofa_num, death_pct)) %>% 
    mutate(p_surv = (100-death_pct)/100,
           alive = ifelse(survival_chance < p_surv, 1, 0),
           chronic_disease_state = factor(chronic_disease_state, 
                                          levels = c( "major","none", "severe"))) %>%
    select(age, chronic_disease_state, sofa_num, SOFA, p_surv, alive) %>%
    mutate(age_group = case_when(
      age < 20 ~ "0–19",
      age < 45 ~ "20-44",
      age < 55 ~ "45-54",
      age < 65 ~ "55-64",
      age < 75 ~ "65-74",
      age < 85 ~ "75-84",
      TRUE ~ "≥85"
    ), 
    age_group = factor(age_group, levels = c("0–19", "20-44", "45-54", "55-64", "65-74", "75-84",  "≥85"))
    )
  
  
  return(sample)
}


# Allocation system code
sofa_chronic_age_tie <- function(sim_pop, num_vents){
  
  lottery <- runif(n = sim_pop %>% nrow())
  
  allocate <- sim_pop %>%
    cbind(lottery) %>%
    mutate(score = case_when(sofa_num < 6 ~ 1,
                             sofa_num < 9 ~ 2,
                             sofa_num < 12 ~ 3,
                             TRUE ~ 4) +
             case_when(chronic_disease_state == "major" ~ 2,
                       chronic_disease_state == "severe" ~ 4,
                       TRUE ~ 0),
           age_cat_2 = case_when(
             age < 41 ~ 1,
             age < 61 ~ 2,
             age < 76 ~ 3,
             TRUE ~ 4)
    ) %>%
    arrange(score, age_cat_2, lottery) %>%
    mutate(get_vent = factor(case_when(
      row_number() <= num_vents & alive ==1 ~  "ventilator (survival)", 
      row_number() <= num_vents ~ "ventilator (death)",
      TRUE ~ "palliative care"), levels = c("ventilator (death)", "ventilator (survival)", "palliative care"))
    )
}







sickest_first <- function(sim_pop, num_vents){
    
    allocate <- sim_pop %>%
        arrange(p_surv) %>% 
        mutate(get_vent = factor(case_when(
            row_number() <= num_vents & alive ==1 ~  "ventilator (survival)", 
            row_number() <= num_vents ~ "ventilator (death)",
            TRUE ~ "palliative care"), levels = c("ventilator (death)", "ventilator (survival)", "palliative care"))
        )
    return(allocate)
}


lottery_allocate <- function(sim_pop, num_vents){
    
    lottery <- runif(n = sim_pop %>% nrow())
    
    allocate <- sim_pop %>%
        cbind(lottery) %>%
        arrange(lottery) %>%
        mutate(get_vent = factor(case_when(
            row_number() <= num_vents & alive ==1 ~  "ventilator (survival)", 
            row_number() <= num_vents ~ "ventilator (death)",
            TRUE ~ "palliative care"), levels = c("ventilator (death)", "ventilator (survival)", "palliative care"))
        )
    
    return(allocate)
}

youngest_allocate <- function(sim_pop, num_vents){
    
    allocate <- sim_pop %>%
        arrange(age) %>% 
        mutate(get_vent = factor(case_when(
            row_number() <= num_vents & alive ==1 ~  "ventilator (survival)", 
            row_number() <= num_vents ~ "ventilator (death)",
            TRUE ~ "palliative care"), levels = c("ventilator (death)", "ventilator (survival)", "palliative care"))
        )
    
    return(allocate)
}

ny_allocate <- function(sim_pop, num_vents){
    
    lottery <- runif(n = sim_pop %>% nrow())
    
    
    allocate <- sim_pop %>%
        cbind(lottery) %>%
        mutate(triage_cat = case_when(
            sofa_num < 7 ~ "Highest",
            sofa_num < 12 ~ "Intermediate",
            TRUE ~ "No ventilator"
        ), priority_score = case_when(
            triage_cat == "Highest" ~ lottery,
            triage_cat == "Intermediate" ~ 1 + lottery,
            TRUE ~ 2 + lottery)
        ) %>%
        arrange(priority_score) %>%
        mutate(get_vent = factor(case_when(
            row_number() <= num_vents & alive ==1 ~  "ventilator (survival)", 
            row_number() <= num_vents ~ "ventilator (death)",
            TRUE ~ "palliative care"), levels = c("ventilator (death)", "ventilator (survival)", "palliative care"))
        )
    
    return(allocate)
    
    
}

maryland_allocate <- function(sim_pop, num_vents = vents){
  lottery <- runif(n = sim_pop %>% nrow())
  
  allocate <- sim_pop %>%
    cbind(lottery) %>%
    mutate(score = case_when(sofa_num < 9 ~ 1,
                             sofa_num < 12 ~ 2,
                             sofa_num < 15 ~ 3,
                             TRUE ~ 4) +
             case_when(
               chronic_disease_state == "severe" ~ 3,
               TRUE ~ 0),
           age_cat_2 = case_when(
             age < 50 ~ 1,
             age < 70 ~ 2,
             age < 85 ~ 3,
             TRUE ~ 4)
    ) %>%
    arrange(score, age_cat_2, lottery) %>%
    mutate(get_vent = factor(case_when(
      row_number() <= num_vents & alive ==1 ~  "ventilator (survival)", 
      row_number() <= num_vents ~ "ventilator (death)",
      TRUE ~ "palliative care"), levels = c("ventilator (death)", "ventilator (survival)", "palliative care"))
    )
  
  return(allocate)
}


max_icu_surv <- function(sim_pop, num_vents){
    
    allocate <- sim_pop %>%
        arrange(-p_surv) %>% 
        mutate(get_vent = factor(case_when(
            row_number() <= num_vents & alive ==1 ~  "ventilator (survival)", 
            row_number() <= num_vents ~ "ventilator (death)",
            TRUE ~ "palliative care"), levels = c("ventilator (death)", "ventilator (survival)", "palliative care"))
        )
    return(allocate)
}


max_life_years <- function(sim_pop, num_vents = vents, max_life_span = 100){
  
  allocate <- sim_pop %>%
    mutate( life_left = case_when(
      chronic_disease_state == "none" ~ (max_life_span- age),
      chronic_disease_state == "major" ~ 0.75*(max_life_span-age),
      chronic_disease_state == "severe" ~ 1
    ),
    priority_score = p_surv*life_left) %>%
    arrange(-priority_score) %>% 
    mutate(get_vent = factor(case_when(
      row_number() <= num_vents & alive ==1 ~  "ventilator (survival)", 
      row_number() <= num_vents ~ "ventilator (death)",
      TRUE ~ "palliative care"), levels = c("ventilator (death)", "ventilator (survival)", "palliative care"))
    )
  
  return(allocate)
}

# Quantification of system results
lives_saved <- function(allocation){
    allocation %>% filter(get_vent == "ventilator (survival)") %>% nrow()
}


life_years_saved <- function(allocation, max_life_span = 100){
    df <- allocation %>% 
        filter(get_vent == "ventilator (survival)") %>%
        mutate(life_left = case_when(
               chronic_disease_state == "none" ~ (100- age),
               chronic_disease_state == "major" ~ 0.5*(100-age),
               TRUE ~ 1
               )
        )
    
    return(sum(df$life_left))
}


# wrapper function for graphs
text_size <- 14
plot_allocation <- function(allocation, text_size = 14){
  allocation %>%
    ggplot(aes(x = age, y = SOFA, color = get_vent, shape = chronic_disease_state)) +
    scale_y_discrete(drop = FALSE) +
    geom_point(size = 3) + labs(color = "Allocation", x = "Age", shape = "Chronic comorbidities") +
    theme(
      legend.text=element_text(size=text_size),
      axis.title = element_text(size = text_size),
      legend.title = element_text(size = text_size))
}


# Define server logic
server <- function(input, output) {
    
    observeEvent(input$sim, {
        
        sim_pop <- reactive({
            simulate_ICU_pop(ICU_dist, 
                             N = input$patients, 
                             sofa = SOFA)
        })
    
        
        sickest <- reactive({
            sickest_first(sim_pop(), input$vents)
        })
        lottery <- reactive({
            lottery_allocate(sim_pop(), input$vents)
        })
    
        youngest <- reactive({
            youngest_allocate(sim_pop(), input$vents)
        })
        
        ny_allocation <- reactive({
            ny_allocate(sim_pop(), input$vents)
        })
        
        maryland_allocation <- reactive({
          maryland_allocate(sim_pop(), input$vents)
        })
    
        max_icu <- reactive({
            max_icu_surv(sim_pop(), input$vents)
        })
    
        max_lf <- reactive({
            max_life_years(sim_pop(), input$vents)
        })
        
        multi <- reactive({
          sofa_chronic_age_tie(sim_pop(), input$vents)
        })
    
        output$age_dist <- renderPlot({
            sim_pop() %>%
                ggplot(aes(x = age_group)) + 
                geom_histogram(stat = "count", fill = "cyan4", color = "black") +
                labs(x = "Age") 
        })
        
        output$sofa_dist <- renderPlot({
            sim_pop() %>%
                ggplot(aes(x = SOFA)) +
                scale_x_discrete(drop = FALSE) +
                geom_histogram(stat = "count", fill = "darkgoldenrod1",color = "black")
        })
    
        
        output$multi <- renderPlot({
          plot_allocation(multi())
        }, height=400, width=800)
        
        output$sickest <- renderPlot({
            plot_allocation(sickest())
            
        }, height=400, width=800)
        
        output$lottery <- renderPlot({
                plot_allocation(lottery())
            }, height=400, width=800)
    
        output$youngest <- renderPlot({
            plot_allocation(youngest())
        })
      

        output$ny <- renderPlot({
          plot_allocation(ny_allocation())
        }, height=400, width=800)
        
        output$maryland <- renderPlot({
          plot_allocation(maryland_allocation())
        }, height=400, width=800)
        
        
        output$max_icu <- renderPlot({
            plot_allocation(max_icu())
        }, height=400, width=800)
    
        output$max_lf <- renderPlot({
            plot_allocation(max_lf())
        }, height=400, width=800)
        
        
        output$combined <- renderPlot({
            lottery() %>% 
                mutate(system = "Lottery") %>%
                select(age, SOFA, get_vent, system) %>%
                rbind(sickest() %>% 
                          mutate(system = "Sickest first") %>%
                          select(age, SOFA, get_vent,  system)) %>%
                rbind(youngest() %>% 
                          mutate(system = "Youngest first") %>%
                          select(age, SOFA, get_vent, system)) %>%            
                rbind(ny_allocation() %>% 
                          mutate(system = "New York") %>%
                          select(age, SOFA, get_vent, system)) %>%
                rbind(max_icu() %>% 
                          mutate(system = "Maximize lives")  %>%
                          select(age, SOFA, get_vent, system)) %>%
                rbind(max_lf() %>% 
                          mutate(system = "Maximize life-years") %>%
                          select(age, SOFA, get_vent, system)) %>%
            rbind(max_lf() %>% 
                    mutate(system = "Maryland") %>%
                    select(age, SOFA, get_vent, system)) %>%
            rbind(max_lf() %>% 
                    mutate(system = "Pittsburgh") %>%
                    select(age, SOFA, get_vent, system)) %>%
                mutate(system = factor(system, levels = c("Sickest first",
                                                          "New York", 
                                                          "Maryland",
                                                          "Pittsburgh",
                                                          "Lottery",
                                                          "Youngest first", 
                                                          "Maximize lives", 
                                                          "Maximize life-years"))) %>%
                ggplot(aes(x = age, y = SOFA, color = get_vent)) +
                geom_point(size = 2) + facet_wrap(~system, nrow = 2) + 
                scale_y_discrete(breaks = c("0", "5", "10", "15", ">=20"))+
                labs(color = " ", x = "Age") + 
                theme(legend.position = "right",
                      legend.text=element_text(size=text_size),
                            axis.text = element_text(size = text_size),
                            axis.title = element_text(size = text_size),
                            legend.title = element_text(size = text_size),
                            strip.text = element_text(size=text_size))
        }, height = 500, width = 1000)
        
        

        
        output$total_life_years <- reactive({
            paste0("The total possible life-years of the population = ", comma(100*input$patients - sum(sim_pop()$age)))
        })
        
        output$table <- renderTable({
            
                total_life_years <- 100*input$patients - sum(sim_pop()$age)
                
                systems <- c("Sickest First",
                             "New York", 
                             "Maryland",
                             "Pittsburgh",
                             "Lottery",
                             "Youngest first", 
                             "Maximize Lives Saved", 
                             "Maximize Life Years")
        
                lives <- c(lives_saved(sickest()),
                           lives_saved(lottery()),
                           lives_saved(youngest()),
                           lives_saved(ny_allocation()),
                           lives_saved(maryland_allocation()),
                           lives_saved(multi()),
                           lives_saved(max_icu()),
                           lives_saved(max_lf()))
                
                life_years <- c(life_years_saved(sickest()),
                                life_years_saved(lottery()),
                                life_years_saved(youngest()),
                                life_years_saved(ny_allocation()),
                                life_years_saved(maryland_allocation()),
                                life_years_saved(multi()),
                                life_years_saved(max_icu()),
                                life_years_saved(max_lf()))
                
                tibble(system = systems,
                       lives = as.numeric(lives), 
                       life_years = life_years) %>%
                    mutate(
                        "Survivors" = paste0(lives," (", round(100*lives/input$patients), "%)"), 
                        "Life-years saved" = paste0(comma(life_years),
                                                    " (", round(100*life_years/total_life_years), "%)")) %>%
                    select(system, `Survivors`, `Life-years saved`)
        })
        
        output$sofa_age_table <- renderTable({
            sim_pop() %>%
                group_by(age_group) %>%
                summarise(mean_sofa = round(mean(sofa_num),2),
                          survival = paste0(round(100*mean(p_surv)), "%")) %>%
                select("Age" = age_group, "Mean SOFA" = mean_sofa, "Survival (with ventilator)" = survival)
        })
        
        
        output$sofa_chronic <- renderPlot({
          sim_pop() %>%
            group_by(SOFA) %>%
            count(chronic_disease_state) %>%
            mutate(pct_disease = 100*n/input$patients) %>%
            ggplot(aes(x = SOFA, 
                       y= pct_disease, 
                       fill = factor(chronic_disease_state,
                                     levels = c("major", "none", "severe")))) +
            geom_bar(stat = "Identity") + 
            labs(fill = "Chronic Disease State", x = "SOFA", y = "Percent (%)")+ 
            theme(legend.position = "bottom")
        })
        
        output$sofa_calib <- renderPlot({
            SOFA %>%
                ggplot(aes(x =SOFA, y = death_pct)) +
                geom_bar(stat = "Identity") + labs(y = "Mortality (%)")
        })
        
        output$age_chronic <- renderPlot({
                sim_pop() %>%
                  group_by(age_group) %>%
                  count(chronic_disease_state) %>%
                  mutate(n_group = sum(n),
                         pct_disease = 100*(n/input$patients)) %>%
                  ggplot(aes(x = age_group, y = pct_disease, fill = factor(chronic_disease_state,
                                                                           levels = c("major", "none", "severe")))) +
                  geom_bar(stat = "Identity") + 
                  labs(fill = "Chronic Disease State", x = "Age", y = "Percent (%)")+ 
                  theme(legend.position = "bottom")
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
