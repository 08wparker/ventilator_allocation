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
    fluidRow(column(6, h1("Simulating ventilator allocation")), 
             column(6, tags$img( src = "U_of_shield.png", height = 106, width = 83, align = "right"))),
    
    h4(strong("Siva Bhavani, MD"), " William Miller MD, Xuan Han MD, Monica Malec MD, Lainie F Ross MD, PhD, Mark Siegler MD,", strong("William F. Parker MD, MS")),
    p("source code available at", 
      tags$a(href = "https://github.com/08wparker/ventilator_allocation", 
             "https://github.com/08wparker/ventilator_allocation")),
    
    hr(),
    
    sidebarLayout(
        sidebarPanel(
            h5("Select the number of patients requiring mechanical ventilation 
                                       and the number of ventilators available at the hospital"),
                    sliderInput(inputId = "patients",
                                      label = "Number of patients",
                                      min = 1,
                                      max = 1000,
                                      value = 500),
                     sliderInput(inputId = "vents",
                                      label = "Number of ventilators",
                                      min = 1,
                                      max = 1000,
                                      value = 250),
                     actionButton("sim", "Simulate",class = "btn-primary"),
                    hr(),
                    h4("Outcomes by Allocation System"),
                     tableOutput("table"),
                    textOutput("total_life_years"),
            width =  4),
        
    mainPanel(
        h2("Select Allocation System"),
        tabsetPanel(
                    tabPanel("Sickest First", plotOutput('sickest')),
                    tabPanel("Lottery", plotOutput('lottery')),
                    tabPanel("New York (SOFA tiers)", plotOutput('ny')),
                    tabPanel("Youngest First", plotOutput('youngest')),
                    tabPanel("Maximize Lives Saved", plotOutput('max_icu')),
                    tabPanel("Maximize Life-Years Gained", plotOutput('max_lf')),
                    tabPanel("Compare Systems", plotOutput('combined')),
                    type = "tabs",
                    selected = "Compare Systems"
        ), width = 8)
    ),
    
    

    fluidRow(column(4, h2("Simulated Population")),
             column(4,sliderInput(inputId = "mean_sofa",
                                  label = "Mean Sofa Score",
                                  min = 5,
                                  max = 15,
                                  value = 7),
                    p("SOFA distribution drawn from a truncated normal (range 3-20) with the specified mean score for a 65-year old patient")),
                column(4,
                    sliderInput(inputId = "age_slope",
                                label = "Age-SOFA relationship",
                                min = 0,
                                max = 0.2,
                                value = 0.1),
                    p("Increase in mean SOFA score for every year of life"))
    ),
    
    hr(),
    
    fluidRow(column(4, h4("Simulated Age Distribution"), 
                    plotOutput("age_dist"),
                    p("Age distribution from",
                           tags$a(href = "https://www.cdc.gov/mmwr/volumes/69/wr/mm6912e2.htm?s_cid=mm6912e2_w", "CDC Report: Severe Outcomes Among Patients with Coronavirus Disease 2019 (COVID-19) — United States, February 12–March 16, 2020"))
                    ),
             column(4, h4("Simulated SOFA Distribution"), plotOutput("sofa_dist")),
             column(4, h4("SOFA distribution by Age"),tableOutput("sofa_age_table"))),
    
    hr(),
    h2("SOFA calibration"),
    fluidRow(column(6,p(tags$a(href = "https://en.wikipedia.org/wiki/SOFA_score", "The Sequential Organ Failure Assesment (SOFA) score"), 
                        "is a validated bedside predictor of hospital mortality. The calibration of SOFA scores used in this model is from", 
                        tags$a(href = "https://jamanetwork.com/journals/jama/fullarticle/2598267", 
                                            "Prognostic Accuracy of the SOFA Score, SIRS Criteria, and qSOFA Score for In-Hospital Mortality Among Adults With Suspected Infection Admitted to the Intensive Care Unit. Raith et al. JAMA, 2017") 
    )),
             column(6,plotOutput("sofa_calib")))
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
worst_case <- cdc_data %>%
    select(Age,
           N,
           hosp = high_hosp,
           icu = high_ICU,
           dead = high_death) %>%
    mutate(n_hosp = N*hosp/100,
           n_icu = N*icu/100,
           n_dead = N*dead/100)

total_ICU <- sum(worst_case$n_icu)

worst_case <- worst_case %>%
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
simulate_ICU_pop <- function(df, N = tot_patients, sofa, mean_sofa, sd_sofa = 3.5, age_slope){
    
    probs <- df$pct_icu_pop
    
    age_cats <- rmultinom(1, size = N, pr = probs)
    
    sample <- tibble(age_group = character(),
                     age = numeric(),
                     alive = numeric(),
                     dead = numeric(),
                     p_surv = numeric())
    
    i <- 1
    
    
    for (n_cats in age_cats) {
        
        subgroup <- df[i,]
        
        subgroup
        
        # randomly sample ages from uniform distribution
        min_age <- subgroup$min_age
        max_age <- subgroup$max_age
        age_sample <- runif(n = n_cats, min = min_age, max = max_age)
        
        
        # sample SOFA score- from a truncated normal distribution
        group_mean_sofa <- mean_sofa + age_slope*(min_age - 65)
        
        sofa_scores <- tibble(sofa_num = round(rtruncnorm(n = n_cats, 
                                                          a = 3,
                                                          b = 20,
                                                          mean = group_mean_sofa, 
                                                          sd = sd_sofa))) %>%
            mutate(sofa_num = case_when(
                sofa_num>= 20 ~ 20,
                sofa_num < 3 ~ 3 ,
                TRUE ~ sofa_num)) %>%
            left_join(sofa %>% select(SOFA, sofa_num, death_pct)) %>%
            mutate(p_surv = (100-death_pct)/100)
        
        
        # sample actual survival outcomes
        chance <- runif(n = n_cats)
        
        sub_sample <- tibble(age_group = subgroup$Age,
                             age = age_sample) %>%
            cbind(sofa_scores) %>%
            cbind(chance) %>%
            mutate(age = round(age),
                   alive = ifelse(chance < p_surv, 1, 0))
        
        sample <- sample %>% rbind(sub_sample)
        
        i <- i + 1
    }
    
    return(sample %>% select(age_group, age, SOFA, sofa_num, p_surv, alive))
}


# Allocation system code
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


max_life_years <- function(sim_pop, num_vents){
    allocate <- sim_pop %>%
        mutate( priority_score = p_surv*(100-age)) %>%
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
        mutate(life_left = (100- age))
    
    return(sum(df$life_left))
}


# parameters for graphs
text_size <- 14


# Define server logic
server <- function(input, output) {
    
    observeEvent(input$sim, {
        
        sim_pop <- reactive({
            simulate_ICU_pop(worst_case, 
                             N = input$patients, 
                             sofa = SOFA, 
                             mean_sofa = input$mean_sofa,
                             age_slope = input$age_slope)
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
    
        max_icu <- reactive({
            max_icu_surv(sim_pop(), input$vents)
        })
    
        max_lf <- reactive({
            max_life_years(sim_pop(), input$vents)
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
    
        
        output$sickest <- renderPlot({
            sickest() %>%
                ggplot(aes(x = age, y = SOFA, color = get_vent)) +
                scale_y_discrete(drop = FALSE) +
                geom_point(size = 3) + labs(color = "", x = "Age") +
                theme(
                    legend.text=element_text(size=text_size),
                    axis.title = element_text(size = text_size),
                    legend.title = element_text(size = text_size)) 
            
        })
        
        output$lottery <- renderPlot({
                lottery() %>%
                    ggplot(aes(x = age, y = SOFA, color = get_vent)) +
                    scale_y_discrete(drop = FALSE) +
                    geom_point(size = 3) + labs(color = "", x = "Age") +
                    theme(
                          legend.text=element_text(size=text_size),
                          axis.title = element_text(size = text_size),
                          legend.title = element_text(size = text_size)) 
            })
    
        output$youngest <- renderPlot({
            youngest() %>%
                ggplot(aes(x = age, y = SOFA, color = get_vent)) +
                scale_y_discrete(drop = FALSE) +
                geom_point(size = 3) + labs(color = "", x = "Age") +
                theme(
                      legend.text=element_text(size=text_size),
                      axis.title = element_text(size = text_size),
                      legend.title = element_text(size = text_size)) 
        })
        
        
        output$ny <- renderPlot({
            
            intermediate_high <-(which(levels(ny_allocation()$SOFA) == "6")) + 0.5
            
            intermediate_none <-(which(levels(ny_allocation()$SOFA) == "11")) + 0.5
            
            
            ny_allocation() %>%
                ggplot(aes(x = age, y = SOFA, color = get_vent, shape = triage_cat)) +
                geom_point(size = 3) + labs(color = "", shape = "NY triage category", x = "Age") +
                scale_y_discrete(drop = FALSE) +
                geom_hline(aes(yintercept = intermediate_none), linetype = "solid") + 
                geom_hline(aes(yintercept = intermediate_high), linetype = "dashed") +
                theme(legend.text=element_text(size=text_size),
                      axis.title = element_text(size = text_size),
                      legend.title = element_text(size = text_size))
        })
    
        output$max_icu <- renderPlot({
            max_icu() %>%
                ggplot(aes(x = age, y = SOFA, color = get_vent)) +
                scale_y_discrete(drop = FALSE) +
                geom_point(size = 3) + labs(color = "", x = "Age") +
                theme(
                      legend.text=element_text(size=text_size),
                      axis.title = element_text(size = text_size),
                      legend.title = element_text(size = text_size))
        })
    
        output$max_lf <- renderPlot({
            max_lf() %>%
                ggplot(aes(x = age, y = SOFA, color = get_vent)) +
                scale_y_discrete(drop = FALSE) +
                geom_point(size = 3) + labs(color = "", x = "Age") +
                theme(
                      legend.text=element_text(size=text_size),
                      axis.title = element_text(size = text_size),
                      legend.title = element_text(size = text_size))  
        })
        
        
        output$combined <- renderPlot({
            lottery() %>% 
                mutate(system = "Lottery") %>%
                select(age, SOFA, get_vent, system) %>%
                rbind(sickest() %>% 
                          mutate(system = "Sickest First") %>%
                          select(age, SOFA, get_vent,  system)) %>%
                rbind(ny_allocation() %>% 
                          mutate(system = "New York (SOFA tiers)") %>%
                          select(age, SOFA, get_vent, system)) %>%
                rbind(youngest() %>% 
                          mutate(system = "Youngest First") %>%
                          select(age, SOFA, get_vent, system)) %>%
                rbind(max_icu() %>% 
                          mutate(system = "Maximize Lives Saved")  %>%
                          select(age, SOFA, get_vent, system)) %>%
                rbind(max_lf() %>% 
                          mutate(system = "Maximize life-years saved") %>%
                          select(age, SOFA, get_vent, system)) %>%
                mutate(system = factor(system, levels = c("Sickest First",
                                                          "New York (SOFA tiers)", 
                                                          "Lottery", 
                                                          "Maximize Lives Saved", 
                                                          "Youngest First", 
                                                          "Maximize life-years saved"))) %>%
                ggplot(aes(x = age, y = SOFA, color = get_vent)) +
                geom_point(size = 2) + facet_wrap(~system) + 
                scale_y_discrete(breaks = c("0", "5", "10", "15", ">=20"))+
                labs(color = " ", x = "Age") + 
                theme(legend.position = "bottom",
                      legend.text=element_text(size=text_size),
                            axis.text = element_text(size = text_size),
                            axis.title = element_text(size = text_size),
                            legend.title = element_text(size = text_size),
                            strip.text = element_text(size=text_size))
        })
        
        
        
        
        output$total_life_years <- reactive({
            paste0("The total possible life-years were ", comma(100*input$patients - sum(sim_pop()$age)))
        })
        
        output$table <- renderTable({
            
                total_life_years <- 100*input$patients - sum(sim_pop()$age)
                
                systems <- c("Sickest First",
                             "Lottery", 
                             "Youngest first", 
                             "New York (SOFA tiers)", 
                             "Maximize Lives Saved", 
                             "Maximize Life Years")
                lives <- c(lives_saved(sickest()),
                           lives_saved(lottery()),
                           lives_saved(youngest()),
                           lives_saved(ny_allocation()),
                           lives_saved(max_icu()),
                           lives_saved(max_lf()))
                
                life_years <- c(life_years_saved(sickest()),
                                life_years_saved(lottery()),
                                life_years_saved(youngest()),
                                life_years_saved(ny_allocation()),
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
        
        output$sofa_calib <- renderPlot({
            SOFA %>%
                ggplot(aes(x =SOFA, y = death_pct)) +
                geom_bar(stat = "Identity") + labs(y = "Mortality (%)")
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
