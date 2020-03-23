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

ui <- fluidPage(
    # App title ----
    titlePanel("Ventilator allocation"),
    
    fluidRow(
        column(6,
               sliderInput(inputId = "patients",
                           label = "Number of patients",
                           min = 1,
                           max = 1000,
                           value = 1000)),
        column(6,sliderInput(inputId = "vents",
                             label = "Number of ventilators",
                             min = 0,
                             max = 1000,
                             value = 500))),

    fluidRow(column(6, plotOutput("age_dist")),
             column(6, plotOutput("sofa_dist"))),
    
    
    fluidRow(column(6, plotOutput('lottery')),
             column(6, plotOutput('youngest'))),
    
    fluidRow(column(6, plotOutput('max_icu')),
             column(6, plotOutput('max_lf'))),
    
    fluidRow(tableOutput("table"))
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


SOFA <- read_csv("SOFA.csv") 

tot_SOFA <- sum(SOFA$N)

SOFA <- SOFA %>%
    mutate(sofa_num = ifelse(SOFA == ">=20", 20, as.numeric(SOFA)),
           SOFA = factor(SOFA, levels = c(seq(0,19), ">=20")),
           pct_SOFA = N/tot_SOFA)

sd_SOFA <- SOFA %>%
    uncount(N)

sd_SOFA <- sqrt(var(sd_SOFA$sofa_num))


simulate_ICU_pop <- function(df, N = tot_patients, sofa, mean_sofa = 8, sd_sofa = 3.5){
    
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
        
        
        # sample SOFA score- assuming no correlation between age and SOFA
        
        #sofa_counts <- rmultinom(n =1, size = n_cats, pr = sofa$pct_SOFA)
        # # sofa_scores <- tibble(SOFA = sofa$SOFA,
        # #        counts = sofa_counts ) %>%
        # #   uncount(counts) %>%
        #  left_join(sofa %>% select(SOFA, death_pct)) %>%
        #   mutate(p_surv = (100-death_pct)/100)
        
        group_mean_sofa <- mean_sofa + (min_age - 65)/30
        
        sofa_scores <- tibble(sofa_num = round(rnorm(n = n_cats, mean = group_mean_sofa, sd = sd_sofa))) %>%
            mutate(sofa_num = case_when(
                sofa_num>= 20 ~ 20,
                sofa_num < 0 ~ 0 ,
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

lottery_allocate <- function(sim_pop, num_vents){
    
    lottery <- runif(n = sim_pop %>% nrow())
    
    allocate <- sim_pop %>%
        cbind(lottery) %>%
        arrange(lottery) %>%
        mutate(get_vent = factor(case_when(
            row_number() <= num_vents & alive ==1 ~  "ventilator (survival)", 
            row_number() <= num_vents ~ "ventilator (death in ICU)",
            TRUE ~ "palliative care"), levels = c("ventilator (death in ICU)", "ventilator (survival)", "palliative care"))
        )
    
    return(allocate)
}

youngest_allocate <- function(sim_pop, num_vents){
    
    allocate <- sim_pop %>%
        arrange(age) %>% 
        mutate(get_vent = factor(case_when(
            row_number() <= num_vents & alive ==1 ~  "ventilator (survival)", 
            row_number() <= num_vents ~ "ventilator (death in ICU)",
            TRUE ~ "palliative care"), levels = c("ventilator (death in ICU)", "ventilator (survival)", "palliative care"))
        )
    
    return(allocate)
}



max_icu_surv <- function(sim_pop, num_vents){
    
    allocate <- sim_pop %>%
        arrange(-p_surv) %>% 
        mutate(get_vent = factor(case_when(
            row_number() <= num_vents & alive ==1 ~  "ventilator (survival)", 
            row_number() <= num_vents ~ "ventilator (death in ICU)",
            TRUE ~ "palliative care"), levels = c("ventilator (death in ICU)", "ventilator (survival)", "palliative care"))
        )
    return(allocate)
}


max_life_years <- function(sim_pop, num_vents){
    allocate <- sim_pop %>%
        mutate( priority_score = p_surv*(100-age)) %>%
        arrange(-priority_score) %>% 
        mutate(get_vent = factor(case_when(
            row_number() <= num_vents & alive ==1 ~  "ventilator (survival)", 
            row_number() <= num_vents ~ "ventilator (death in ICU)",
            TRUE ~ "palliative care"), levels = c("ventilator (death in ICU)", "ventilator (survival)", "palliative care"))
        )
    
    return(allocate)
}


lives_saved <- function(allocation){
    allocation %>% filter(get_vent == "ventilator (survival)") %>% nrow()
}


life_years_saved <- function(allocation, max_life_span = 100){
    df <- allocation %>% 
        filter(get_vent == "ventilator (survival)") %>%
        mutate(life_left = (100- age))
    
    return(sum(df$life_left))
}


g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}

# Define server logic
server <- function(input, output) {
        
    sim_pop <- reactive({
        simulate_ICU_pop(worst_case, N = input$patients, sofa = SOFA)
    })

    lottery <- reactive({
        lottery_allocate(sim_pop(), input$vents)
    })

    youngest <- reactive({
        youngest_allocate(sim_pop(), input$vents)
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
            geom_histogram(stat = "count") +
            ggtitle("Simulated Age Distribution")
    })
    
    output$sofa_dist <- renderPlot({
        sim_pop() %>%
            ggplot(aes(x = SOFA)) +
            geom_histogram(stat = "count") + 
            ggtitle("Simulated SOFA score Distribution")
    })

    output$lottery <- renderPlot({
            lottery() %>%
                ggplot(aes(x = age, y = SOFA, color = get_vent)) +
                geom_point() + labs(color = "") +
                theme(legend.position = "bottom") +
                ggtitle("Random Ventilator Allocation")
        })

    output$youngest <- renderPlot({
        youngest() %>%
            ggplot(aes(x = age, y = SOFA, color = get_vent)) +
            geom_point() + labs(color = "") +
            theme(legend.position = "bottom") +
            ggtitle("Youngest First allocation")
    })

    output$max_icu <- renderPlot({
        max_icu() %>%
            ggplot(aes(x = age, y = SOFA, color = get_vent)) +
            geom_point() + labs(color = "") +
            theme(legend.position = "bottom") +
            ggtitle("Maximize ICU Survival")
    })

    output$max_lf <- renderPlot({
        max_lf() %>%
            ggplot(aes(x = age, y = SOFA, color = get_vent)) +
            geom_point() + labs(color = "") +
            theme(legend.position = "bottom") +
            ggtitle("Maximize Life Years Gained")
    })
    
    output$table <- renderTable({
        
            total_life_years <- 100*input$patients - sum(sim_pop()$age)
            
            systems <- c("Lottery", "Youngest First allocation", "Maximize ICU Survival", "Maximize Life Years Gained")
            lives <- c(lives_saved(lottery()),
                       lives_saved(youngest()),
                       lives_saved(max_icu()),
                       lives_saved(max_lf()))
            
            life_years <- c(life_years_saved(lottery()),
                            life_years_saved(youngest()),
                            life_years_saved(max_icu()),
                            life_years_saved(max_lf()))
            
            tibble(system = systems, 
                   "ICU survivors (N)" = lives,
                   "Life-years saved" = life_years) %>%
                mutate("ICU survival %" = paste0(round(100*`ICU survivors (N)`/input$patients), "%"),
                       "Life years %" = paste0(round(100*`Life-years saved`/total_life_years), "%"),
                       `Life-years saved` = comma(`Life-years saved`))  %>%
                select(system, `ICU survivors (N)`, `ICU survival %`, `Life-years saved`, `Life years %`)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
