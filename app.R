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
library(shinydashboard)
library(sf)
library(scales)
library(gridExtra)
library(ggpubr)
library(shinyscreenshot)

locations <- read_csv("./data/locations.csv")

pcode1_shape <- 
  sf::st_read("./data/ken_adm_iebc_20191031_shp/ken_admbnda_adm1_iebc_20191031.shp", 
              quiet = TRUE) %>% 
  rename_with(str_to_lower)

counties <- read_csv("./data/counties_complete.csv") 

asal_county_list <- counties %>% 
  distinct(county) %>% 
  filter(county != "National") %>% 
  arrange(county) %>% 
  pull()

pie_chart <- function(tbl){
  
  tbl %>% 
    mutate(prop = value / sum(value)) %>%
    mutate(label = str_to_title(sex_modifier)) %>% 
    ggplot(aes(x ="", y = prop, fill = sex_modifier)) + 
    geom_col(width = 1, colour = "white") + 
    coord_polar("y", start = 0) + 
    theme_void() + 
    geom_label(aes(y = prop, 
                   label = paste0(label, "\n", 
                                  comma(value), 
                                  "\n", 
                                  percent(prop, accuracy = .1))), 
               colour = "white", 
               position = position_stack(vjust = .5), 
               size = 5) + 
    scale_fill_manual(values = c( "#4d6bbd", "#ed6b24")) +
    theme(legend.position = "none", 
          plot.title = element_text(size = 10), 
          plot.subtitle = element_text(size = 30,
                                       colour = "#5370BF")) 
  
}

make_dodged_plot_4 <- function(tbl) {
  
  tbl %>% 
    mutate(age_modifier = str_to_title(age_modifier),
           age_modifier = str_replace_all(age_modifier, "Preprimary_ece", "Pre-primary\n/ECE"),
           county = fct_relevel(county, c("National", input$v_county_4))) %>%
    mutate(age_modifier = fct_relevel(age_modifier, c("Secondary",
                                                      "Primary",
                                                      "Pre-primary\n/ECE"))) %>% 
    arrange(age_modifier, county) %>% 
    ggplot(aes(x = value, y = age_modifier, fill = fct_rev(county))) +
    geom_col(position = position_dodge()) + 
    geom_text(aes(label = value), position = position_dodge(width = .9), 
              hjust = 1, colour = "white", size = 5) + 
    scale_fill_manual(values = c("#2b54ad", "#5d90d3"), 
                      breaks = c(input$v_county_4, "National")) + 
    theme(legend.position = "top",  
          panel.background = element_rect("#f4f4f4"), 
          legend.text = element_text(size = 8), 
          plot.title = element_text(size = 15), 
          plot.subtitle = element_text(size = 15), 
          axis.text.y = element_text(size = 10)) + 
    guides(fill = guide_legend(override.aes = list(size = 0.3)))
}

# ui


ui <- dashboardPage(
  dashboardHeader(title = tags$a(href="https://kenya-eie-wg.github.io/", 
                                 tags$img(src = "https://raw.githubusercontent.com/kenya-eie-wg/kenya_education_county_profiles/main/eie_wg_logo_small.png"))), 
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Key Figures", 
               tabName = "key_figures", 
               icon = icon("dashboard")), 
      
      menuItem("Education Indicators", 
               tabName = "education_indicators", 
               icon = icon("calendar")), 
      
      menuItem("Child Protection Indicators", 
               tabName = "child_protection_indicators", 
               icon = icon("hands-holding-child")
               ), 
      
      menuItem("School-based Ratios", 
               tabName = "school_based_ratios", 
               icon = icon("school")), 
      
      menuItem("ASAL Counties Map", 
               tabName = "map_asal_counties", 
               icon = icon("earth-africa"))
    )
  ), 
  
  dashboardBody(
    
    tags$head(tags$style(HTML(
      '.myClass { 
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
    '))),
    tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Kenya Education County Profiles </span>\');
      })
     ')), 
    
    tabItems(
      tabItem(tabName = "key_figures",
              
              fluidRow(box(uiOutput("county_selector_1"),
                           width = 5), 
                       box(textOutput("sources_1"), 
                           width = 5),
                       box(actionButton(inputId = "screenshot_1", 
                                        label = "Screenshot Page"), 
                           width = 2)),
              
              fluidRow( 
                       box(plotOutput("county_population_1"), 
                              width = 6),
                       box(plotOutput("school_aged_children_1"), 
                              width = 6) 
                       
                       ), 
              
              fluidRow(box(plotOutput("oos_preprimary_1"), 
                           width = 4),
                       box(plotOutput("oos_primary_1"), 
                           width = 4),
                       box(plotOutput("oos_secondary_1"), 
                           width = 4)
                       )
              ), 
      
      tabItem(tabName = "education_indicators", 
              
              fluidRow(box(uiOutput("county_selector_2"), width = 5), 
                       box(textOutput("sources_2"), width = 5), 
                       box(actionButton(inputId = "screenshot_2", 
                                        label = "Screenshot Page"), width = 2)), 
              
              fluidRow(box(plotOutput("net_enrolment_rate_2"), width = 6), 
                       box(plotOutput("gender_parity_index_2"), width = 6)), 
              
              fluidRow(box(plotOutput("dropouts_absenteeism_2"), width = 6), 
                       box(plotOutput("transition_rate_2"), width = 6))
              ), 
      
      tabItem(tabName = "child_protection_indicators", 
              
              fluidRow(box(uiOutput("county_selector_3"), width = 5), 
                       box(textOutput("sources_3"), width = 5), 
                       box(actionButton(inputId = "screenshot_3", 
                                        label = "Screenshot Page"), width = 2)), 
              
              fluidRow(box(plotOutput("child_protection_indicators_3"), 
                           width = 10))
              ), 
      
      tabItem(tabName = "school_based_ratios", 
              
              fluidRow(box(uiOutput("county_selector_4"), width = 5), 
                       box(textOutput("sources_4"), width = 5),
                       box(actionButton(inputId = "screenshot_4", 
                                        label = "Screenshot Page"), width = 2)), 
              
              fluidRow(box(plotOutput("education_institutions_4"), width = 6), 
                       box(plotOutput("learner_teacher_ratio_4"), width = 6)), 
              
              fluidRow(box(plotOutput("learner_classroom_ratio_4"), width = 6), 
                       box(plotOutput("learner_toilet_ratio_4"), width = 6))
      ),
      
      tabItem(tabName = "map_asal_counties", 
              
              fluidRow(box(uiOutput("county_selector_5"), width = 5),
                       box(actionButton(inputId = "screenshot_5", 
                                        label = "Screenshot Page"), width = 2)),
              
              fluidRow(box(plotOutput("map_asal_counties_5"),
                           width = 12, height = 425)))
    )
  )
)


# Server 
server <- function(input, output) {
  
    output$county_selector_1 <- renderUI({
      
      
      selectInput(inputId = "v_county_1", 
                  label = "Select County", 
                  choices = asal_county_list)
      
    })
    
    output$county_selector_2 <- renderUI({
      
      
      selectInput(inputId = "v_county_2", 
                  label = "Select County", 
                  choices = asal_county_list)
      
    })
    
    output$county_selector_3 <- renderUI({
      
      
      selectInput(inputId = "v_county_3", 
                  label = "Select County", 
                  choices = asal_county_list)
      
    })
    
    output$county_selector_4 <- renderUI({
      
      
      selectInput(inputId = "v_county_4", 
                  label = "Select County", 
                  choices = asal_county_list)
      
    })
    
    output$county_selector_5 <- renderUI({
      
      
      selectInput(inputId = "v_county_5", 
                  label = "Select County", 
                  choices = asal_county_list)
      
    })

    
  output$county_population_1 <- renderPlot({
      
      req(input$v_county_1)
      
      counties %>% 
        filter(county == input$v_county_1) %>% 
        filter(indicator == "county_population" & sex_modifier != "total") %>% 
        pie_chart() +
        labs(title = expression(paste("County populatio", n^1)),
             subtitle = paste0(counties %>%
                                 filter(county == input$v_county_1) %>% 
                                 filter(indicator == "county_population" &
                                          sex_modifier == "total" &
                                          age_modifier == "total") %>%
                                 pull(value) %>%
                                 format(big.mark = ",")))
      
      
    })
    
  output$school_aged_children_1 <- renderPlot({
      
      req(input$v_county_1)
      
     counties %>% 
        filter(county == input$v_county_1) %>% 
        filter(str_detect(indicator, "school_age_children") & 
                 sex_modifier != "total") %>% 
        pie_chart() +
        labs(title = expression(paste("School-aged children (4-17 years", ")"^1)),
             subtitle = paste0(counties %>%
                                 filter(county == input$v_county_1) %>%  
                                 filter(indicator == "school_age_children" &
                                          sex_modifier == "total" &
                                          age_modifier == "total") %>%
                                 pull(value) %>%
                                 format(big.mark = ",")))
      
      
    })
 

  output$oos_preprimary_1 <- renderPlot({
    
    req(input$v_county_1)
    
    counties %>% 
      filter(county == input$v_county_1) %>% 
      filter(str_detect(indicator, "out_of_school_children") & 
               age_modifier == "preprimary_ece" & 
               sex_modifier != "total") %>% 
      pie_chart() + 
      labs(title = expression(paste("Out-of-school children, Preprimary/ECE (4-5 years", ")"^2)), 
           subtitle = paste0(counties %>%
                               filter(county == input$v_county_1) %>% 
                               filter(str_detect(indicator, "out_of_school_children") &
                                        sex_modifier == "total" &
                                        age_modifier == "preprimary_ece") %>%
                               pull(value) %>%
                               format(big.mark = ","))) + 
      theme(plot.subtitle = element_text(hjust = 0))
    
      })
  
  output$oos_primary_1 <- renderPlot({
    
    req(input$v_county_1)
    
    counties %>% 
      filter(county == input$v_county_1) %>% 
      filter(str_detect(indicator, "out_of_school_children") & 
               age_modifier == "primary" & 
               sex_modifier != "total") %>% 
      pie_chart() + 
      labs(title = expression(paste("Out-of-school children, Primary (6-13 years", ")"^2)),
           subtitle = paste0(counties %>%
                               filter(county == input$v_county_1) %>%  
                               filter(str_detect(indicator, "out_of_school_children") &
                                        sex_modifier == "total" &
                                        age_modifier == "primary") %>%
                               pull(value) %>%
                               format(big.mark = ","))) + 
      theme(plot.subtitle = element_text(hjust = 0))
    
  })
  
  output$oos_secondary_1 <- renderPlot({
    
    req(input$v_county_1)
    
    counties %>% 
      filter(county == input$v_county_1) %>% 
      filter(str_detect(indicator, "out_of_school_children") & 
               age_modifier == "secondary" & 
               sex_modifier != "total") %>% 
      pie_chart() + 
      labs(title = expression(paste("Out-of-school children, Secondary (14-17 years", ")"^2)),
           subtitle = paste0(counties %>%
                               filter(county == input$v_county_1) %>% 
                               filter(str_detect(indicator, "out_of_school_children") &
                                        sex_modifier == "total" &
                                        age_modifier == "secondary") %>%
                               pull(value) %>%
                               format(big.mark = ","))) + 
      theme(plot.subtitle = element_text(hjust = 0))
    
  })
  
  output$net_enrolment_rate_2 <- renderPlot({
    
    req(input$v_county_2)
    
    counties %>% 
      filter(county %in% c("National", input$v_county_2)) %>% 
      filter(str_detect(indicator, "Net Enrolment") & sex_modifier == "total") %>% 
      mutate(age_modifier = str_to_title(age_modifier),
             age_modifier = str_replace_all(age_modifier, "Preprimary_ece", "Pre-primary\n/ECE"),
             county = fct_relevel(county, c("National", input$v_county_2))) %>%
      mutate(age_modifier = fct_relevel(age_modifier, c("Secondary",
                                                        "Primary",
                                                        "Pre-primary\n/ECE"))) %>% 
      arrange(age_modifier, county) %>% 
      ggplot(aes(x = value, y = age_modifier, fill = fct_rev(county))) +
      geom_col(position = position_dodge()) + 
      geom_text(aes(label = value), position = position_dodge(width = .9), 
                hjust = 1, colour = "white", size = 5) + 
      scale_fill_manual(values = c("#2b54ad", "#5d90d3"), 
                        breaks = c(input$v_county_2, "National")) + 
      theme(legend.position = "top",  
            panel.background = element_rect("#f4f4f4"), 
            legend.text = element_text(size = 8), 
            plot.title = element_text(size = 15), 
            plot.subtitle = element_text(size = 15), 
            axis.text.y = element_text(size = 10)) + 
      guides(fill = guide_legend(override.aes = list(size = 0.3))) +
      labs(subtitle = expression(paste("Net Enrolment Rate 202", "0"^5)), 
           y = "", x = "Net enrolment rate", fill = "") 
  })
  
  output$gender_parity_index_2 <- renderPlot({
    
    req(input$v_county_2) 
    
    counties %>% 
      filter(county %in% c("National", input$v_county_2)) %>% 
      filter(str_detect(indicator, "Gender Parity")) %>% 
      mutate(age_modifier = str_to_title(age_modifier),
             age_modifier = str_replace_all(age_modifier, "Preprimary_ece", "Pre-primary\n/ECE"),
             county = fct_relevel(county, c("National", input$v_county_2))) %>%
      mutate(age_modifier = fct_relevel(age_modifier, c("Secondary",
                                                        "Primary",
                                                        "Pre-primary\n/ECE"))) %>% 
      arrange(age_modifier, county) %>% 
      ggplot(aes(x = value, y = age_modifier, fill = fct_rev(county))) +
      geom_col(position = position_dodge()) + 
      geom_text(aes(label = value), position = position_dodge(width = .9), 
                hjust = 1, colour = "white", size = 5) + 
      scale_fill_manual(values = c("#2b54ad", "#5d90d3"), 
                        breaks = c(input$v_county_2, "National")) + 
      theme(legend.position = "top",  
            panel.background = element_rect("#f4f4f4"), 
            legend.text = element_text(size = 8), 
            plot.title = element_text(size = 15), 
            plot.subtitle = element_text(size = 15), 
            axis.text.y = element_text(size = 10)) + 
      guides(fill = guide_legend(override.aes = list(size = 0.3))) +
      labs(subtitle = "Gender Parity Index", 
           y = "", x = "Gender parity index", fill = "")
    
  })
  
  output$dropouts_absenteeism_2 <- renderPlot({
    
    req(input$v_county_2)
    
    counties %>% 
      filter(county %in% c(input$v_county_2)) %>%
      filter(indicator == "Students' dropout/Absenteeism during 2022" & sex_modifier != "total") %>% 
      mutate(age_modifier = str_to_title(age_modifier),
             sex_modifier = str_to_title(sex_modifier), 
             age_modifier = str_replace_all(age_modifier, "Preprimary_ece", "Pre-primary\n/ECE")) %>%
      mutate(age_modifier = fct_relevel(age_modifier, c("Secondary",
                                                        "Primary",
                                                        "Pre-primary\n/ECE"))) %>% 
      ggplot(aes(x = value, y = age_modifier, fill = sex_modifier)) +
      geom_col(position = position_dodge()) + 
      geom_text(aes(label = ifelse(value != 0, value, "")),
                position = position_dodge(width = .9), 
                hjust = 1, colour = "white", size = 3) + 
      geom_text(aes(label = ifelse(value == 0, value, "")),
                position = position_dodge(width = .9),
                hjust = -.5, 
                colour = "grey50", size = 3) + 
      scale_fill_manual(values = c("#002021", "#19686a")) + 
      scale_x_continuous(labels = comma) + 
      theme(legend.position = "top", 
            panel.background = element_rect("#f4f4f4"), 
            legend.text = element_text(size = 8), 
            plot.title = element_text(size = 15),  
            plot.subtitle = element_text(size = 15), 
            axis.text.y = element_text(size = 10)) + 
      guides(fill = guide_legend(override.aes = list(size = 0.3))) +
      labs(subtitle = expression(paste("Dropouts/Absenteeism in 202", "2"^5)),
           x = "Number of children", y = "", 
           fill =  "")
  })
  
  output$transition_rate_2 <- renderPlot({
    
    req(input$v_county_2) 
    
    counties %>% 
      filter(county %in% c(input$v_county_2, "National")) %>% 
      filter(str_detect(indicator, "Primary-to-Secondary")) %>% 
      mutate(sex_modifier = str_to_title(sex_modifier)) %>% 
      mutate(sex_modifier = fct_relevel(sex_modifier, "Total", "Female", "Male")) %>% 
      arrange(sex_modifier) %>% 
      ggplot(aes(x = value, y = fct_rev(sex_modifier), fill = fct_rev(county))) + 
      geom_col(position = position_dodge()) + 
      geom_text(aes(label = value), position = position_dodge(width = .9), 
                hjust = 1, colour = "white", size = 5) +
      scale_fill_manual(values = c("#2b54ad", "#5d90d3"), 
                        breaks = c(input$v_county_2, "National")) + 
      theme(legend.position = "top",  
            panel.background = element_rect("#f4f4f4"), 
            legend.text = element_text(size = 8), 
            plot.title = element_text(size = 15), 
            plot.subtitle = element_text(size = 13), 
            axis.text.y = element_text(size = 10)) + 
      guides(fill = guide_legend(override.aes = list(size = 0.3))) + 
      labs(x =  "Primary-to-secondary transition rate", 
           y = "", 
           subtitle = "Primary-to-Secondary Transition Rate", 
           fill = "")
  })
  
  output$child_protection_indicators_3 <- renderPlot({
    
    req(input$v_county_3)
    
    counties %>% 
      filter(county %in% c(input$v_county_3, "National")) %>% 
      filter(str_detect(indicator, "GBV|FGM|Pregnancy")) %>% 
      arrange(indicator) %>%
      mutate(indicator = str_wrap(indicator, width = 50)) %>% 
      mutate(indicator = 
               fct_relevel(
                 indicator,
                 c("Percentage of Teenage Pregnancy for girls/women\naged 15-19 yrs who have ever been pregnant",
                   "GBV: Percentage of girls/women who have\nexperienced physical violence since age 15",
                   "GBV: Percentage of girls/women aged 15-49 yrs who\nhave ever experienced sexual violence", 
                   "FGM: Percentage of women aged 15-49 yrs who were\never circumcised across Kenya"))) %>% 
      ggplot(aes(x = value, 
                 y = fct_rev(indicator))) + 
      geom_col(fill = "#2b54ad", width = .5) + 
      scale_x_continuous(labels = percent) +
      geom_text(aes(label = percent(value, accuracy = .1)), 
                hjust = 1, 
                colour = "white") + 
      theme(panel.background = element_rect("#f4f4f4"), 
            axis.text.y = element_text(size = 11), 
            plot.title = element_text(size = 15)) +
      labs(title = bquote("Child Protection Indicators"), 
           y = "", x = "")
  })
  
  output$education_institutions_4 <- renderPlot({
    
    req(input$v_county_4)
    
    counties %>% 
      mutate(indicator = str_replace_all(indicator, "education Institutions", "Education Institutions")) %>% 
      filter(county %in% c(input$v_county_4)) %>% 
      filter(str_detect(indicator, "Number of Education Institutions")) %>% 
      mutate(age_modifier = str_to_title(age_modifier),
             age_modifier = str_replace_all(age_modifier, "Preprimary_ece", "Pre-primary\n/ECE")) %>%
      mutate(age_modifier = fct_relevel(age_modifier, c("Secondary",
                                                        "Primary",
                                                        "Pre-primary\n/ECE"))) %>% 
      ggplot(aes(x = value, y = age_modifier, fill = county)) + 
      geom_col(width = .5) +
      geom_text(aes(label = value), 
                hjust = 1, 
                colour = "white") + 
      scale_fill_manual(values = c("#2b54ad")) + 
      theme(panel.background = element_rect("#f4f4f4"), 
            plot.margin = margin(0, 0, 0, 10, "pt"), 
            legend.text = element_text(size = 8), 
            plot.subtitle = element_text(size = 15), 
            legend.position = "top",
            axis.text.y = element_text(size = 11)) +
      labs(subtitle = "Education Institutions",
           y = "", x = "Number of institutions", fill = "")
  })
  
  output$learner_teacher_ratio_4 <- renderPlot({
    
    req(input$v_county_4)
    
    counties %>%
      filter(county %in% c(input$v_county_4, "National")) %>% 
      filter(str_detect(indicator, "Learner to teacher")) %>%
      group_by(age_modifier, county) %>% 
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>% 
      mutate(age_modifier = str_to_title(age_modifier),
             age_modifier = str_replace_all(age_modifier, "Preprimary_ece", "Pre-primary\n/ECE"),
             county = fct_relevel(county, c("National", input$v_county_4))) %>%
      mutate(age_modifier = fct_relevel(age_modifier, c("Secondary",
                                                        "Primary",
                                                        "Pre-primary\n/ECE"))) %>% 
      arrange(age_modifier, county) %>% 
      ggplot(aes(x = value, y = age_modifier, fill = fct_rev(county))) +
      geom_col(position = position_dodge()) + 
      geom_text(aes(label = value), position = position_dodge(width = .9), 
                hjust = 1, colour = "white", size = 5) + 
      scale_fill_manual(values = c("#2b54ad", "#5d90d3"), 
                        breaks = c(input$v_county_4, "National")) + 
      theme(legend.position = "top",  
            panel.background = element_rect("#f4f4f4"), 
            legend.text = element_text(size = 8), 
            plot.title = element_text(size = 15), 
            plot.subtitle = element_text(size = 15), 
            axis.text.y = element_text(size = 10)) + 
      guides(fill = guide_legend(override.aes = list(size = 0.3))) +  
      labs(x = "Learner-to-Teacher Ratio", y = "", 
           subtitle = "Learner-to-Teacher Ratio", 
           fill = "") +
      theme(plot.margin = margin(0, 0, 0, 10, "pt"))
  })
  
  output$learner_classroom_ratio_4 <- renderPlot({
    
    req(input$v_county_4)
    
    counties %>%
      filter(county %in% c(input$v_county_4, "National")) %>% 
      filter(str_detect(indicator, "Classroom")) %>% 
      mutate(age_modifier = str_to_title(age_modifier),
             age_modifier = str_replace_all(age_modifier, "Preprimary_ece", "Pre-primary\n/ECE"),
             county = fct_relevel(county, c("National", input$v_county_4))) %>%
      mutate(age_modifier = fct_relevel(age_modifier, c("Secondary",
                                                        "Primary",
                                                        "Pre-primary\n/ECE"))) %>% 
      arrange(age_modifier, county) %>% 
      ggplot(aes(x = value, y = age_modifier, fill = fct_rev(county))) +
      geom_col(position = position_dodge()) + 
      geom_text(aes(label = value), position = position_dodge(width = .9), 
                hjust = 1, colour = "white", size = 5) + 
      scale_fill_manual(values = c("#2b54ad", "#5d90d3"), 
                        breaks = c(input$v_county_4, "National")) + 
      theme(legend.position = "top",  
            panel.background = element_rect("#f4f4f4"), 
            legend.text = element_text(size = 8), 
            plot.title = element_text(size = 15), 
            plot.subtitle = element_text(size = 15), 
            axis.text.y = element_text(size = 10)) + 
      guides(fill = guide_legend(override.aes = list(size = 0.3))) +  
      labs(x = "Learner-to-Classroom Ratio", y = "", 
           subtitle = "Learner-to-Classroom Ratio", 
           fill = "")
  })
  
  
  output$learner_toilet_ratio_4 <- renderPlot({
    
    req(input$v_county_4)
    
    counties %>% 
      mutate(indicator = ifelse(county == "National" & 
                                  indicator == "Learner to Toilet Ratio in Public Schools",
                                "learner_toilet_ratio_recalculated", indicator)) %>% 
      filter(county %in% c(input$v_county_4, "National")) %>% 
      filter(str_detect(indicator, "learner_toilet_ratio_recalculated")) %>%
      group_by(age_modifier, county) %>% 
      summarise(value = sum(value, na.rm = TRUE),
                .groups = "drop") %>% 
      mutate(value = round(value)) %>% 
      mutate(age_modifier = str_to_title(age_modifier),
             age_modifier = str_replace_all(age_modifier, "Preprimary_ece", "Pre-primary\n/ECE"),
             county = fct_relevel(county, c("National", input$v_county_4))) %>%
      mutate(age_modifier = fct_relevel(age_modifier, c("Secondary",
                                                        "Primary",
                                                        "Pre-primary\n/ECE"))) %>% 
      arrange(age_modifier, county) %>% 
      ggplot(aes(x = value, y = age_modifier, fill = fct_rev(county))) +
      geom_col(position = position_dodge()) + 
      geom_text(aes(label = value), position = position_dodge(width = .9), 
                hjust = 1, colour = "white", size = 5) + 
      scale_fill_manual(values = c("#2b54ad", "#5d90d3"), 
                        breaks = c(input$v_county_4, "National")) + 
      theme(legend.position = "top",  
            panel.background = element_rect("#f4f4f4"), 
            legend.text = element_text(size = 8), 
            plot.title = element_text(size = 15), 
            plot.subtitle = element_text(size = 15), 
            axis.text.y = element_text(size = 10)) + 
      guides(fill = guide_legend(override.aes = list(size = 0.3))) + 
      labs(x = "Learner-to-Toilet Ratio", y = "", 
           subtitle = expression(paste("Learner-to-Toilet Rati", "o"^3)),
           fill = "")
  })
    
  output$map_asal_counties_5 <- renderPlot({
    
    req(input$v_county_5)
    
    pcode1_shape %>% 
      st_as_sf() %>% 
      mutate(is_county = ifelse(adm1_en == input$v_county_5, 
                                input$v_county_5, 
                                "Other counties"), 
             is_county = fct_relevel(is_county, 
                                     c(input$v_county_5, 
                                       "Other counties"))) %>% 
      ggplot() + 
      geom_sf(size = .1, colour = "grey70", 
              aes(fill = is_county)) + 
      geom_sf_text(aes(label = adm1_en), 
                   size = 3, 
                   check_overlap = TRUE, 
                   colour = "grey20") + 
      theme_void() + 
      scale_fill_manual(values = c("#ed6b24", "grey90")) + 
      theme(legend.position = "none",
            plot.background = element_rect(color = "grey85", linewidth = 1), 
            plot.title = element_text(hjust = .5)) + 
      labs(title = paste0("Location of ", input$v_county_5, " County"))
  })
  
  output$sources_1 <- renderText({
    
    print("SOURCES: [1] 2019 Kenya Population and Housing Census and 2023 Population Projections, Analytical Report, (KNBS). [2] 2021 Out-of-School Children Initiative study, (K MoE).")
  })
  
  output$sources_2 <- renderText({
    
    print("SOURCES: [3] 2020 Basic Education Statistical Book, (K MoE).  [5] Enrolment figures from 2022 Short and long Rain Assessments (NDMA). [6] In this profile, Primary level includes Grades 1-8.")
  })
  
  output$sources_3 <- renderText({
    
    print("SOURCES: [4] 2022 Kenya Demographic and Health Survey, Key Indicators Report, (KNBS).")
  })
  
  output$sources_4 <- renderText({
    
    print("SOURCES: [3] 2020 Basic Education Statistical Book, (K MoE). [6] In this profile, Primary level includes Grades 1-8.    ")
  })

  
  observeEvent(input$screenshot_1, {
    shinyjs::addCssClass(selector = "body", class = "sidebar-collapse")
    screenshot(
      # selector = "body > div > div > section"
      filename = paste0(input$v_county_1, "_key_figures_plots")
    )
  })
  
  observeEvent(input$screenshot_2, {
    shinyjs::addCssClass(selector = "body", class = "sidebar-collapse")
    screenshot(
      # selector = "body > div > div > section"
      filename = paste0(input$v_county_2, "_education_indicators_plots")
    )
  })
  
  observeEvent(input$screenshot_3, {
    shinyjs::addCssClass(selector = "body", class = "sidebar-collapse")
    screenshot(
      # selector = "body > div > div > section"
      filename = paste0(input$v_county_3, "_child_protection_indicators_plot")
    )
  })
  
  observeEvent(input$screenshot_4, {
    shinyjs::addCssClass(selector = "body", class = "sidebar-collapse")
    screenshot(
      # selector = "body > div > div > section"
      filename = paste0(input$v_county_4, "_school_based_ratios_plots")
    )
  })
  
  observeEvent(input$screenshot_5, {
    shinyjs::addCssClass(selector = "body", class = "sidebar-collapse")
    screenshot(
      # selector = "body > div > div > section"
      filename = paste0(input$v_county_5, "_location_map")
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

