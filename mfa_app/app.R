
# Global  -----------------------------------------------------------------

# Load Libraries ----------------------------------------------------------
library(shiny)
library(bslib)
library(here)
library(tidyr)
library(purrr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(shinyWidgets) # extend shiny widget options
library(shinycssloaders)
library(rsconnect) # delete? 
library(httr)
library(xml2)
library(readxl)
library(readr)
library(tidyr)
library(janitor)
library(stringr)
library(useeior) # EPA EEIO model 
library(forcats)

# Get RDS Files  ----------------------------------------------------------

base_url   <- "https://dmap-data-commons-ord.s3.amazonaws.com/"
list_url   <- paste0(base_url, "?list-type=2&prefix=USEEIO-State/")

# Regex matches any two-letter state acronym + any two-digit year, e.g.
# "USEEIO-State/CTEEIOv1.0-s-12.rds"
file_pattern <- "([A-Z]{2})EEIOv1\\.0-s-(\\d{2})\\.rds$"

get_bucket_keys <- function() {
  all_keys <- character()
  token <- NULL
  
  repeat {
    url <- if (is.null(token)) {
      list_url
    } else {
      paste0(list_url, "&continuation-token=", URLencode(token, reserved = TRUE))
    }
    
    resp <- GET(url)
    doc  <- read_xml(content(resp, as = "text", encoding = "UTF-8"))
    
    keys <- xml_text(xml_find_all(doc, "//*[local-name()='Key']"))
    all_keys <- c(all_keys, keys)
    
    is_truncated <- xml_text(xml_find_all(doc, "//*[local-name()='IsTruncated']"))
    
    if (length(is_truncated) == 0 || is_truncated != "true") break
    
    token <- xml_text(xml_find_all(doc, "//*[local-name()='NextContinuationToken']"))
    if (length(token) == 0 || token == "") break
  }
  
  all_keys
}

all_keys  <- get_bucket_keys()
rds_files <- all_keys[grepl(file_pattern, all_keys)]

# Pull out every distinct state acronym present in the bucket, to populate
# the dropdown dynamically (no hardcoded state list to maintain).
available_states <- sort(unique(sub(paste0(".*", file_pattern), "\\1", rds_files)))


# Load data --------------------------------------------------------

lifetimes <- read.csv(here::here("data","static","lifetimes_clean.csv"))
ca_rr_pack <- read.csv(here::here("data", "static", "ca_rr_pack.csv")) |>
  rename(bau_rr_sect = bau_rr)
ca_rr <- read.csv(here::here("data", "static", "ca_rr.csv"))
ca_incineration <- read.csv(here::here("data", "static", "incineration_clean.csv")) # add national avg
emission_factors <- read.csv(here('data', 'static', 'emission_factors_clean.csv'))
bea_to_plastic <- read_csv(here("data", "raw", "plastic_sector_classification.csv"))
scaled_na_consumption <- read_csv(here::here("data", "raw", "scaled_na_consumption .csv")) 



# Source functions  -------------------------------------------------------

list.files(here::here("functions"), full.names = TRUE) |>
  purrr::walk(source)


# Plot Global -------------------------------------------------------------

sector_labels <- c(
  pack = "Packaging",
  buil = "Building/Construction",
  tran = "Transportation",
  heal = "Healthcare",
  comm = "Commercial/Institutional",
  elec = "Electrical/Electronic",
  hous = "Household/Leisure/Sports",
  mach = "Machinery",
  text = "Textiles",
  othe = "Other",
  agri = "Agriculture")


# Pre-load CA as default to create outputs faster ---------------------------------------

ca_consum_bau_default <- readRDS(here::here("data", "static", "ca_consum_bau_default.rds"))


# Make list of state choices ----------------------------------------------

state_choices <- c(
  "Alabama" = "AL", "Alaska" = "AK", "Arizona" = "AZ", "Arkansas" = "AR",
  "California" = "CA", "Colorado" = "CO", "Connecticut" = "CT", "Delaware" = "DE",
  "Florida" = "FL", "Georgia" = "GA", "Hawaii" = "HI", "Idaho" = "ID",
  "Illinois" = "IL", "Indiana" = "IN", "Iowa" = "IA", "Kansas" = "KS",
  "Kentucky" = "KY", "Louisiana" = "LA", "Maine" = "ME", "Maryland" = "MD",
  "Massachusetts" = "MA", "Michigan" = "MI", "Minnesota" = "MN", "Mississippi" = "MS",
  "Missouri" = "MO", "Montana" = "MT", "Nebraska" = "NE", "Nevada" = "NV",
  "New Hampshire" = "NH", "New Jersey" = "NJ", "New Mexico" = "NM", "New York" = "NY",
  "North Carolina" = "NC", "North Dakota" = "ND", "Ohio" = "OH", "Oklahoma" = "OK",
  "Oregon" = "OR", "Pennsylvania" = "PA", "Rhode Island" = "RI", "South Carolina" = "SC",
  "South Dakota" = "SD", "Tennessee" = "TN", "Texas" = "TX", "Utah" = "UT",
  "Vermont" = "VT", "Virginia" = "VA", "Washington" = "WA", "West Virginia" = "WV",
  "Wisconsin" = "WI", "Wyoming" = "WY"
)


# Make list of sector choices  --------------------------------------------

sector_choices <- c("Packaging" = "pack") # add more sectors here later. Follow 4 letter naming convention to match code. 

# UI ----------------------------------------------------------------------

ui <- page_navbar(
  
  ## Font Selection ----------------------------------------------------------
  
  tags$link(
    rel = "stylesheet",
    href = "https://fonts.googleapis.com/css2?family=Baskervville:wght@400;500;600;700&family=Epilogue:wght@400;500;600;700&display=swap"
  ),
  
  tags$style(
    HTML(
      "
    /* Headers and titles */
    h1, h2, h3, h4, h5, h6 {
      font-family: 'Baskervville', sans-serif;
    }
    h2 {
      color: #303C9F;
    }
    /* Body text */
    body {
      font-family: 'Epilogue', serif;
    }
    /* Shiny inputs, buttons, etc. */
    button,
    input,
    select,
    textarea,
    .form-control,
    .btn,
    .selectize-input,
    .selectize-dropdown {
      font-family: 'Epilogue', serif;
    }

    /* Top navbar row + title background */
    .navbar {
      background-color: #A1BBD3 !important;
    }

    .navbar-brand,
    .navbar .nav-link {
      color: #303C9F !important;
    }
    

    /* Sub-tabs (tabsetPanel) text color only — keeps original Bootstrap styling otherwise */
    .nav-tabs .nav-link {
      color: black;
    }

    .nav-tabs .nav-link.active {
      color: black ;
    }
    "
    )
  ),
  
  ## Color Selection ---------------------------------------------------------
  #header = tags$head(includeCSS("www/custom_styles.css")), # ADJUST COLORS IN WWW CSS FILE
  #967DA1 lavender 
  #A1BBD3 light blue
  #303C9F dark blue 
  #687E03 Green 
  
  
  ## add photo background  ---------------------------------------------------
  
  tags$style(HTML("
  .pollution-card {
    position: relative;
    background-image:
      linear-gradient(
        rgba(0, 0, 0, 0.45),
        rgba(0, 0, 0, 0.45)
      ),
    url('shutterstock_645210340.jpg');
    
    background-size: cover;
    background-position: center;
    background-repeat: no-repeat;
    
    height: 900px;
    padding: 25px;
    border-radius: 12px;
    
    color: white;
    
    display: flex;
    align-items: flex-start;
  }
  .pollution-card h2 {
    color: white;
    margin: 0;
  }
  .pollution-card .citation {
    position: absolute;
    bottom: 8px;
    right: 12px;
    font-size: 10px;
    color: rgba(255, 255, 255, 0.7);
    line-height: 1;
  }
")),
  
  ## Button Selection --------------------------------------------------------
  tags$head(
    tags$style(HTML("
    .btn-custom {
      background-color: #A1B8D3;
      color: white;
      border-color: #A1B8D3;
    }
    .btn-custom:hover {
      background-color: #303C9F;
      color: white;
    }
  "))
  ),
  
  title = "Plastic Policy Impact Model",
  id = "main_tabs",
  theme = bs_theme(version = 5),
  fillable = FALSE,
  
  ## Side Panel: State Inputs ------------------------------------------------
  
  sidebar = sidebar(
    width = 400,
    selectInput(
      inputId = "state",
      label = "State:",
      choices = state_choices, 
      selected = "CA"
    ), # END state input
    
    selectInput(
      inputId = "sector",
      label = "Sector:",
      choices = sector_choices,
    ), # END sector input
    br(), 
    br(),
    div(
      class = "pollution-card",
      
      h2("Plastic pollution has reached a crisis point –",
         tags$strong("policy is essential to turn the tide."),
         span(class = "citation", "© Lycia/Shutterstock")
      )
      
      
      
      # Additional shared inputs placeholder (e.g. choose recycling rate CA or National Average, incineration)
      # They will also persist across tabs.
    )
  ),
  
  
  ## Welcome ----------------------------------------------------------------
  
  nav_panel(
    "Welcome",
    fillable = FALSE,
    h2("Why it matters"),
    h5(
      "Advocacy groups and law makers face critical data-gaps when seeking policy solutions to curb plastic pollution. The Plastic Policy Impact Model seeks to enable regulators to make informed, science-backed decisions. Through interactive policy levers, targets and timelines, users can visualize how policy can change the trajectory of plastic production and the cost of inaction. The plastic crisis emerged within a single generation, with large-scale production and use only dating back to ~1950.",
      tags$sup("1"),
      " It can be reversed just as quickly if we take bold action now."
    ),
    br(),
    h2("How to use it"),
    h5(
      strong("Welcome"), " —  select your state in the side panel to get started.",
      br(), br(),
      strong("The Problem"), " — understand the urgency of the plastic crisis, including your states business as usual projections.",
      br(), br(),
      strong("Explore Solutions"), " — model individual and combined policy solutions to curb plastic production.",
      br(), br(),
      strong("CA SB54"), " —  learn about California’s landmark Plastic Pollution Prevention and Packaging Producer Responsibility Act and model potential impacts of delayed policy implementation.",
      br(), br(),
      strong("Compare Solutions"), " — see your solutions side-by-side by selecting two of your previously modeled solutions or CA SB54.",
      br(), br(),
      strong("About"), " — more about the model and source code."
    ),
    br(), 
    
    h2("What it does"),
    h5("This is the first state-level, time-dependent material flow analysis (MFA) of plastics. It quantifies plastic consumption, waste generation, and end-of-life management across all major use sectors from 1950-2050 and evaluates the projected impacts of key policy interventions, including California’s landmark Plastic Pollution Prevention and Packaging Producer Responsibility Act, Senate Bill 54 (SB 54)."),
    br(), 
    h5("The model assesses three policy strategies, both separately and combined:", tags$strong("source reduction, recycling rate, and recycled content mandates.")),
    br(), br(),
    a(href = "https://www.scienceforconservation.org/assets/downloads/CA_Plastic_Use_TNC_2025.pdf",
      target = "_blank", class = "btn btn-custom ", "For detailed methodology, read the full report here.")),
  
  ## The Problem  ------------------------------------------------------------
  
  nav_panel(
    "The Problem",
    h2(uiOutput("state_sum_intro")), 
    br(),
    h5(
      "Plastic production, use, and disposal pose numerous risks to human health, and are associated with increased rates of cardiovascular, pulmonary, renal diseases, and cancers.",
      tags$sup("2,3"),
      "Microplastics have been detected in the air we breathe, the food we eat, and the water we drink. Alarmingly, these particles have been found in nearly every part of the human body tested, including blood, lungs, liver, kidneys, placenta, and even breast milk.",
      tags$sup("4,5,6,7,8"),
      br(), br(),
      "The adverse effects of plastics and plastic pollution disproportionately affect socioeconomically disadvantaged and marginalized communities.",
      tags$sup("2,11"),
      "Despite growing public concern,", tags$i("plastic production continues to skyrocket.")),
    br(), 
    a(href = "https://www.nature.org/en-us/about-us/where-we-work/united-states/california/stories-in-california/stop-plastic-waste/",
      target = "_blank", class = "btn btn-custom btn-sm", "Learn more about The Nature Conservancy’s strategies to reduce plastic here."),
    br(), br(),
    layout_columns(
      div(
        style = "border-radius: 12px; padding: 15px; border:4px solid black; height: 100%; display: flex; flex-direction: column; justify-content: space-between;",
        class = "text-center",
        h4(
          icon("bottle-water", class = "fa-2xl", style = "color: black"), " Total Plastic Consumption", br(),
          withSpinner(uiOutput("sum_bau", inline = TRUE), type = 1)), 
          h6("million metric tons (Mt) of plastic"),
          h6("from 2025 to 2050."), 
          a(href = "https://www.themeasureofthings.com/results.php?comp=weight&unit=mt&amt=1",
            target = "_blank", class = "btn btn-custom btn-sm", "Contextualize your output")
        ),
      div(
        style = "border-radius: 12px; padding: 20px; border: 4px solid black; height: 100%; display: flex; flex-direction: column; justify-content: space-between;",
        class = "text-center",
        h4(
          icon("industry", class = "fa-2xl",  style = "color: black"), " Greenhouse Gas Emissions", br(),
          withSpinner(uiOutput("ghg_bau", inline = TRUE), type = 1)), 
         h6("million metric tons (Mt) of CO2 equivalent"), 
         h6("from 2025 to 2050"), 
          a(href = "https://www.epa.gov/energy/greenhouse-gas-equivalencies-calculator",
            target = "_blank", class = "btn btn-custom btn-sm", "Contextualize your output")
        )
    ),
    h6(tags$strong("Figure 1."), "Cumulative projected impacts from 2025 to 2050 of the business as usual scenario for plastic consumption and greenhouse gas emissions associated with the plastic life cycle from production to disposal. Results reflect the state selected in the sidebar and all plastic sectors."),
    br(),
    h2(
      uiOutput("state_full", inline = TRUE),("Plastic Consumption By Sector 1950-2050")
    ), br(), withSpinner(plotOutput("bau_overview_plot", height = "500px"), type = 1),
    h6(tags$strong("Figure 2."), "Projected annual plastic consumption by sector under the business as usual scenario in million metric tons (Mt) from 1950-2050.")
    
  ), # END NavPanel 
  
  ## ---------------- Explore Solutions (with sub-tabs) ----------------
  nav_panel(
    "Explore Solutions",
    br(),
    h6("This model includes three types of policy interventions–source reduction (SR), recycling rates (RR), and post-consumer recycled content mandates (PCR)–aimed at reducing plastic waste generation through decreasing overall plastic production or increasing reuse and recycling of plastics. Click the tabs below to learn about each type of policy intervention or the Combined Policy tab to explore cumulative impacts. For all policies, projected impacts are presented as the change in primary plastic production in millions of metric tons (Mt)--which is assumed to be equivalent to changes in consumption–and change in associated greenhouse gas emissions in millions of metric tons of carbon dioxide equivalents (Mt CO2e) from the implementation year through 2050 relative to business as usual (BAU)."),
    br(),
    tabsetPanel(
      id = "individual_policy_tabs",
      
      ### Source Reduction--------------------------------------------------------
      
      tabPanel(
        "Source Reduction",
        br(),
        fluidRow(
          column(
            width = 3,
            numericInput(
              "target_sr",
              "Rate (%):",
              value = 0,
              min = 0,
              max = 100
            ),
            selectInput(
              "baseline_year_sr",
              "Baseline Year:",
              choices = 1950:2025,
              selected = 2023
            ),
            selectInput(
              "target_year_sr",
              "Target Year:",
              choices = 2026:2050,
              selected = 2030
            ),
            selectInput(
              "implement_year_sr",
              "Implementation Year:",
              choices = 2024:2050,
              selected = 2026
            ),
            
            br(), 
            actionButton("run_sr", "Model Policy", class = "btn-custom") # END Run Button
          ), 
          column(
            width = 9,
            #### SR Change from BAU ---------------------------------------------------------
            h2( "What is a source reduction intervention?"),
            h6("The source reduction intervention involves cutting back on the consumption of plastic. The upstream effect is assumed to be an equal reduction in virgin plastic production, one of the first steps in the plastic lifecycle, and the downstream effect is an equal reduction in waste generation. 
"),   
br(), h6("The source reduction policy is modeled as an absolute reduction in plastic consumption relative to a baseline and includes the following customizable parameters:"),
br(),
h6(tags$ul(
  tags$li(tags$strong("Rate (%):"), " the desired percent reduction in consumption from the chosen plastic sector"),
  tags$li(tags$strong("Baseline Year:"), " the year used to determine baseline plastic consumption in the chosen sector; the percent reduction must be achieved relative to plastic consumption in this year"),
  tags$li(tags$strong("Target Year:"), " the year in which the reduction target should be met"),
  tags$li(tags$strong("Implementation Year:"), " the year where implementation of the regulation begins. Changes from the policy are detected the following year")
)),
br(),
h6("The reduction is modeled as a linear decrease in the volume of plastic consumed in the chosen sector from the implementation year until the target year, which is when the full source reduction target has been reached. Plastic consumption in the chosen sector remains fixed at this value through 2050."),
            br(),
            h2("Projected Source Reduction Impacts:", uiOutput("sector_title_sr", inline = TRUE)),
            layout_columns(
              div(
                style = "border-radius: 12px; padding: 15px; border:4px solid #687E03; height: 100%; display: flex; flex-direction: column; justify-content: space-between;",
                class = "text-center",
                h4(
                  icon(
                    name = "bottle-water",
                    class = "fa-2xl",
                    style = "color: #687E03"
                  ),
                  "Change in Primary Plastic Production",
                  withSpinner(uiOutput("sr_avoid_prod", inline = TRUE), type = 1)
                ),
                h6(" million metric tons (Mt) of plastic"),
                h6("from implementation year to 2050"),
                a(
                  href = "https://www.themeasureofthings.com/results.php?comp=weight&unit=mt&amt=1",
                  target = "_blank",
                  class = "btn btn-custom btn-sm",
                  "Contextualize your output"
                )
              ),
              div(
                style = "border-radius: 12px; padding: 15px; border: 4px solid #687E03; height: 100%; display: flex; flex-direction: column; justify-content: space-between;",
                class = "text-center",
                h4(
                  icon("industry", class = "fa-2xl", style = "color: #687E03"),
                  "Change in GHG Emissions",
                  br(),
                  withSpinner(uiOutput("sr_ghg_diff", inline = TRUE), type = 1)
                ),
                h6("million metric tons (Mt) of CO2 equivalent"),
                h6("from implementation year to 2050"),
                a(
                  href = "https://www.epa.gov/energy/greenhouse-gas-equivalencies-calculator",
                  target = "_blank",
                  class = "btn btn-custom btn-sm",
                  "Contextualize your output"
                )
              )
            ),
            h6(tags$strong("Figure 3."), "Cumulative projected impacts from implementation year to 2050 of the selected source reduction policy on the packaging sector compared to the business as usual scenario for plastic production and greenhouse gas emissions associated with the plastic life cycle from production to disposal. The results reflect the state selected in the sidebar."),
            
            #### Plots SR----------------------------------------------------------
            
            h2("Projected Change In Annual Plastic Production: SR vs. BAU"),
            withSpinner(plotOutput("sr_consum_line_chart"), type = 1),
            h6(tags$strong("Figure 4."), "Projected annual plastic production from 1950-2050 under  business as usual, black solid line, and with a source reduction policy intervention on the packaging sector, dashed green line. Annual production is the total across all sectors in million metric tons (Mt). The difference in production between the two scenarios is shaded in green."),
            br(),
            
            #### SR Summary ----------------------
            h2("Source Reduction Intervention Summary"),
            layout_columns( 
              style = "border-radius: 12px; padding: 20px; border:4px solid black; height: 250px; margin: 0 auto;",
              div(class = "text-center",
                  h4("Produced"),
                  icon("bottle-water", class = "fa-2xl", style = "color: black"),
                  withSpinner(uiOutput("sr_total_consumption", inline = TRUE), type = 1),
                  h6 (" million metric tons (Mt) of plastic."),
              ),
              div(class = "text-center",
                  h4("Landfilled"),
                  icon("trash-can", class = "fa-2xl", style = "color: black"),
                  withSpinner(uiOutput("sr_total_landfill", inline = TRUE), type = 1),
                  h6 (" million metric tons (Mt) of plastic.")) ,
              
              div(class = "text-center",
                  h4("Recycled"),
                  icon("recycle", class = "fa-2xl", style = "color: black"),
                  withSpinner(uiOutput("sr_total_recycle", inline = TRUE), type = 1),
                  h6 (" million metric tons (Mt) of plastic.")), 
              
              div(class = "text-center",
                  h4("Incinerated"),
                  icon("dumpster-fire", class = "fa-2xl", style = "color: black"),
                  withSpinner(uiOutput("sr_total_incin", inline = TRUE), type = 1),
                  h6 (" million metric tons (Mt) of plastic.")),
              div(class = "text-center",
                  h4("Emitted"),
                  icon("industry", class = "fa-2xl", style = "color: black"),
                  withSpinner(uiOutput("sr_total_ghg", inline = TRUE), type = 1),
                  h6 (" million metric tons (Mt) of CO2e."))
            ),
            h6(tags$strong("Figure 5."), "Projected totals for all sectors from implementation year to 2050."),
            
          ) # END column
        ) # END fluidrow
      ), # END Tabpanel
      
      
      ## Recycling Rate ----------------------------------------------------------
      
      tabPanel(
        "Recycling Rate",
        br(),
        fluidRow(
          column(
            width = 3,
            numericInput("target_rr", "Rate (%):", value = 0, min = 0, max = 100),
            selectInput("target_year_rr", "Target Year:", choices = 2026:2050, selected = 2030),
            selectInput("implement_year_rr", "Implementation Year:", choices = 2024:2050, selected = 2026),
            br(), 
            actionButton("run_rr", "Model Policy", class = "btn-custom") # END Run Button
          ),
          column(
            
            width = 9,
            ### Model info ----------------------------
            h2("What is a recycling rate intervention?"),
            h6("A recycling rate intervention involves a minimum target for the weight of material that is recycled. Recycling rate policies do not impact the total amount of plastic consumed. However, increasing recycling results in higher availability of secondary plastics which can be used to make new products and in turn decreases primary plastic production, use, and disposal.", 
               br(),
               "The recycling rate intervention is modeled as a linear increase in the recycling rate in the chosen sector between the implementation year and the target year, after which the target recycling rate has been reached. It includes the following customizable parameters:"), br(),
            h6(
              tags$ul(
                tags$li(tags$strong("Rate (%):"), " the target recycling rate, as a percentage"),
                tags$li(tags$strong("Target Year:"), " the year in which the recycling rate target should be met"),
                tags$li(tags$strong("Implementation Year:"), " the year where implementation of the regulation begins. Changes from the policy are detected the following year")
              )
            ),
            br(),
            h6("*Assumptions: The model uses a displacement rate of 80%, meaning that every ton of secondary plastic produced from recycling displaces 0.8 tons of primary plastic. All recycling modeled is mechanical recycling only."),
            
            ### RR change from BAU  -------------------------------------------------------------
            h2("Projected Recycling Rate Impacts:", uiOutput("sector_title_rr", inline = TRUE)),
            layout_columns(
              div(
                style = "border-radius: 12px; padding: 15px; border:4px solid #687E03; height: 100%; display: flex; flex-direction: column; justify-content: space-between;",
                class = "text-center",
                h4(
                  icon(
                    name = "recycle",
                    class = "fa-2xl",
                    style = "color: #687E03"
                  ),
                  "Change in Primary Plastic Production",
                  withSpinner(uiOutput("rr_avoid_prod", inline = TRUE), type = 1)
                ),
                h6(" million metric tons (Mt) of plastic"),
                h6("from implementation year to 2050"),
                a(
                  href = "https://www.themeasureofthings.com/results.php?comp=weight&unit=mt&amt=1",
                  target = "_blank",
                  class = "btn btn-custom btn-sm",
                  "Contextualize your output"
                )
              ),
              div(
                style = "border-radius: 12px; padding: 15px; border: 4px solid #687E03; height: 100%; display: flex; flex-direction: column; justify-content: space-between;",
                class = "text-center",
                h4(
                  icon("industry", class = "fa-2xl", style = "color: #687E03"),
                  "Change in GHG Emissions",
                  br(),
                  withSpinner(uiOutput("rr_ghg_diff", inline = TRUE), type = 1)
                ),
                h6("million metric tons (Mt) of CO2 equivalent"),
                h6("from implementation year to 2050"),
                a(
                  href = "https://www.epa.gov/energy/greenhouse-gas-equivalencies-calculator",
                  target = "_blank",
                  class = "btn btn-custom btn-sm",
                  "Contextualize your output"
                )
              )
            ),
            h6(tags$strong("Figure 6."), "Cumulative projected impacts from implementation year to 2050 of the selected recycling rate policy on the packaging sector compared to the business as usual scenario for plastic production and greenhouse gas emissions associated with the plastic life cycle from production to disposal. The results reflect the state selected in the sidebar."),
            
            br(),
            h2("Projected Plastic Waste Management: RR vs. BAU"),
            withSpinner(plotOutput("rr_eol_plot"), type = 1),
            h6(tags$strong("Figure 7."), "Projected plastic waste management under business as usual (BAU), black, and with a recycling rate intervention on the packaging sector, green. Plastic waste is aggregated across all sectors from the implementation year to 2050 and measured in million metric tons (Mt). Incineration may not be visible because it is not an active form of waste management in all states."),
            br(),
            
            ### RR Summary ----------------------
            h2("Recycling Rate Intervention Summary"),
            layout_columns( 
              style = "border-radius: 12px; padding: 20px; border:4px solid black; height: 250px; margin: 0 auto;",
              div(class = "text-center",
                  h4("Produced"),
                  icon("bottle-water", class = "fa-2xl", style = "color: black"),
                  withSpinner(uiOutput("rr_total_consumption", inline = TRUE), type = 1),
                  h6 (" million metric tons (Mt) of plastic."),
              ),
              div(class = "text-center",
                  h4("Landfilled"),
                  icon("trash-can", class = "fa-2xl", style = "color: black"),
                  withSpinner(uiOutput("rr_total_landfill", inline = TRUE), type = 1),
                  h6 (" million metric tons (Mt) of plastic.")) ,
              div(class = "text-center",
                  h4("Recycled"),
                  icon("recycle", class = "fa-2xl", style = "color: black"),
                  withSpinner(uiOutput("rr_total_recycle", inline = TRUE), type = 1),
                  h6 (" million metric tons (Mt) of plastic.")), 
              
              div(class = "text-center",
                  h4("Incinerated"),
                  icon("dumpster-fire", class = "fa-2xl", style = "color: black"),
                  withSpinner(uiOutput("rr_total_incin", inline = TRUE), type = 1),
                  h6 (" million metric tons (Mt) of plastic.")),
              div(class = "text-center",
                  h4("Emitted"),
                  icon("industry", class = "fa-2xl", style = "color: black"),
                  withSpinner(uiOutput("rr_total_ghg", inline = TRUE), type = 1),
                  h6 (" million metric tons (Mt) of CO2e."))
            ), # END layout_columns 
            h6(tags$strong("Figure 8."), "Projected totals for all sectors from implementation year to 2050.")
          ) # End Column 
        ) # END Tabpanel
      ),
      
      ## Recycled Content --------------------------------------------------------
      
      
      tabPanel(
        "Recycled Content",
        br(),
        fluidRow(
          column(
            width = 3,
            numericInput(
              "target_rc",
              "Rate (%):",
              value = 0,
              min = 0,
              max = 100
            ),
            selectInput(
              "target_year_rc",
              "Target Year:",
              choices = 2026:2050,
              selected = 2030
            ),
            selectInput(
              "implement_year_rc",
              "Implementation Year:",
              choices = 2024:2050,
              selected = 2026
            ),
            br(),
            actionButton("run_rc", "Model Policy", class = "btn-custom")
          ),
          column(
            width = 9,
            h2("What is a recycled content intervention?"),
            
            h6(
              "The recycled content intervention involves requiring new products to be manufactured with a certain proportion of recycled plastics, which displaces the demand on virgin material. Recycled content policies generate demand for secondary plastic, which can counteract the economic barriers of using recycled materials. Recycling rate policies are most effective when implemented in tandem with this policy lever.", br(), "The recycled content mandate is modeled as a linear increase in post-consumer recycled content in the chosen sector between the implementation year and target year, after which the target recycled content rate is reached. It includes the following customizable parameters: 
"),
            br(),
            h6(
              tags$ul(
                tags$li(tags$strong("Rate (%):"), " the target percentage of recycled content"),
                tags$li(tags$strong("Target Year:"), " the year in which the recycled content target should be met"),
                tags$li(tags$strong("Implementation Year:"), " the year where implementation of the regulation begins. Changes from the policy are detected the following year")
              )
            ),
            h6("*Assumptions: The model uses a displacement rate of 80%, meaning that every ton of secondary plastic produced from recycling displaces 0.8 tons of primary plastic. All recycling modeled is mechanical recycling only."),
            
             ##### RC change from BAU  -------------------------------------------------------------
            h2("Projected Recycled Content Impacts:", uiOutput("sector_title_rc", inline = TRUE)),
            layout_columns(
              div(
                style = "border-radius: 12px; padding: 15px; border:4px solid #687E03; height: 100%; display: flex; flex-direction: column; justify-content: space-between;",
                class = "text-center",
                h4(
                  icon(
                    name = "recycle",
                    class = "fa-2xl",
                    style = "color: #687E03"
                  ),
                  "Change in Primary Plastic Production",
                  withSpinner(uiOutput("rc_avoid_prod", inline = TRUE), type = 1)
                ),
                h6(" million metric tons (Mt) of plastic"),
                h6("from implementation year to 2050"),
                a(
                  href = "https://www.themeasureofthings.com/results.php?comp=weight&unit=mt&amt=1",
                  target = "_blank",
                  class = "btn btn-custom btn-sm",
                  "Contextualize your output"
                )
              ),
              div(
                style = "border-radius: 12px; padding: 15px; border: 4px solid #687E03; height: 100%; display: flex; flex-direction: column; justify-content: space-between;",
                class = "text-center",
                h4(
                  icon("industry", class = "fa-2xl", style = "color: #687E03"),
                  "Change in GHG Emissions",
                  br(),
                  withSpinner(uiOutput("rc_ghg_diff", inline = TRUE), type = 1)
                ),
                h6("million metric tons (Mt) of CO2 equivalent"),
                h6("from implementation year to 2050"),
                a(
                  href = "https://www.epa.gov/energy/greenhouse-gas-equivalencies-calculator",
                  target = "_blank",
                  class = "btn btn-custom btn-sm",
                  "Contextualize your output"
                )
              )
            ),
            h6(tags$strong("Figure 9."), "Cumulative projected impacts from implementation year to 2050 of the selected recycled content policy on the packaging sector compared to the business as usual scenario for plastic production and greenhouse gas emissions associated with the plastic life cycle from production to disposal. The results reflect the state selected in the sidebar."),

            br(),
            
            h2( "Projected Primary Plastic Produced: RC vs. BAU"),
            plotOutput("rc_lollipop_plot"),
            h6(tags$strong("Figure 10."), "Projected primary plastic production under business as usual (BAU), black line, and with a post-consumer recycled content mandate on the packaging sector, green line. Virgin plastic production is aggregated across all sectors from the implementation year to 2050 and measured in million metric tons (Mt)."),
            br(),
            
            ##### RC Summary ----------------------
            h2("Recycled Content Intervention Summary"),
            layout_columns( 
              style = "border-radius: 12px; padding: 20px; border:4px solid black; height: 250px; margin: 0 auto;",
              div(class = "text-center",
                  h4("Produced"),
                  icon("bottle-water", class = "fa-2xl", style = "color: black"),
                  withSpinner(uiOutput("rc_total_consumption", inline = TRUE), type = 1),
                  h6 (" million metric tons (Mt) of plastic."),
              ),
              div(class = "text-center",
                  h4("Landfilled"),
                  icon("trash-can", class = "fa-2xl", style = "color: black"),
                  withSpinner(uiOutput("rc_total_landfill", inline = TRUE), type = 1),
                  h6 (" million metric tons (Mt) of plastic.")) ,
              
              div(class = "text-center",
                  h4("Recycled"),
                  icon("recycle", class = "fa-2xl", style = "color: black"),
                  withSpinner(uiOutput("rc_total_recycle", inline = TRUE), type = 1),
                  h6 (" million metric tons (Mt) of plastic.")), 
              
              div(class = "text-center",
                  h4("Incinerated"),
                  icon("dumpster-fire", class = "fa-2xl", style = "color: black"),
                  withSpinner(uiOutput("rc_total_incin", inline = TRUE), type = 1),
                  h6 (" million metric tons (Mt) of plastic.")),
              div(class = "text-center",
                  h4("Emitted"),
                  icon("industry", class = "fa-2xl", style = "color: black"),
                  withSpinner(uiOutput("rc_total_ghg", inline = TRUE), type = 1),
                  h6 (" million metric tons (Mt) of CO2e."))
            ),
            
            h6(tags$strong("Figure 11."), "Projected totals for all sectors from implementation year to 2050."),
            
          )
        )
      ),
      
      
      ## Combined policy ---------------------------------------------------------
      
      #moved to within explore solutions
      
      tabPanel(
        "Combined Policy",
        br(),
        h2("What is a combined policy intervention?"),
        h6("The individual policies of source reduction, recycling rate, and recycled content rates are most effective in reducing environmental impacts when implemented in combination with each other. The total effects of the individual policies are different than the sum of each part due to interactions between interventions. Adjust the inputs below for two or more policy interventions to explore the impacts of a combined policy."),
        br(),
        accordion(
          id = "combined_policy_accordion",
          open = TRUE,  # or c("Source Reduction", "Recycling Rate") to open specific ones by default
          
          accordion_panel(
            "Source Reduction",
            fluidRow(
              column(2, numericInput("target_sr_comp", "Rate (%):", value = 0, min = 0, max = 100)),
              column(2, selectInput("baseline_year_sr_comp", "Baseline Year:", choices = 1950:2025, selected = 2023)),
              column(2, selectInput("target_year_sr_comp", "Target Year:", choices = 2026:2050, selected = 2030)),
              column(2, selectInput("implement_year_sr_comp", "Implementation Year:", choices = 2024:2050, selected = 2026))
            )
          ),
          
          accordion_panel(
            "Recycling Rate",
            fluidRow(
              column(3, numericInput("target_rr_comp", "Rate (%):", value = 0, min = 0, max = 100)),
              column(3, selectInput("target_year_rr_comp", "Target Year:", choices = 2026:2050, selected = 2030)),
              column(3, selectInput("implement_year_rr_comp", "Implementation Year:", choices = 2024:2050, selected = 2026)),
            )
          ),
          
          accordion_panel(
            "Recycled Content",
            fluidRow(
              column(3, numericInput("target_rc_comp", "Rate (%):", value = 0, min = 0, max = 100)),
              column(3, selectInput("target_year_rc_comp", "Target Year:", choices = 2026:2050, selected = 2030)),
              column(3, selectInput("implement_year_rc_comp", "Implementation Year:", choices = 2024:2050, selected = 2026)),
            )
          ),
          br(), 
          actionButton("run_comp", "Model Policy", class = "btn-custom"), ), #END accordion 
        
        br(),
        
        h2("Projected Combined Impacts:", uiOutput("sector_title_comp", inline = TRUE)),
        layout_columns(
          div(
            style = "border-radius: 12px; padding: 15px; border:4px solid #687E03; height: 100%; display: flex; flex-direction: column; justify-content: space-between;",
            class = "text-center",
            h4(
              icon(
                name = "bottle-water",
                class = "fa-2xl",
                style = "color: #687E03"
              ),
              "Change in Primary Plastic Production",
              withSpinner(uiOutput("comp_avoid_prod", inline = TRUE), type = 1)
            ),
            h6(" million metric tons (Mt) of plastic"),
            h6("from implementation year to 2050"),
            a(
              href = "https://www.themeasureofthings.com/results.php?comp=weight&unit=mt&amt=1",
              target = "_blank",
              class = "btn btn-custom btn-sm",
              "Contextualize your output"
            )
          ),
          div(
            style = "border-radius: 12px; padding: 15px; border: 4px solid #687E03; height: 100%; display: flex; flex-direction: column; justify-content: space-between;",
            class = "text-center",
            h4(
              icon("industry", class = "fa-2xl", style = "color: #687E03"),
              "Change in GHG Emissions",
              br(),
              withSpinner(uiOutput("comp_ghg_diff", inline = TRUE), type = 1)
            ),
            h6("million metric tons (Mt) of CO2 equivalent"),
            h6("from implementation year to 2050"),
            a(
              href = "https://www.epa.gov/energy/greenhouse-gas-equivalencies-calculator",
              target = "_blank",
              class = "btn btn-custom btn-sm",
              "Contextualize your output"
            )
          )
        ), # END OUTPUTS
        

      ### Combined plots ----------------------------------------------------------

        
        
        h6(tags$strong("Figure 12."), "Cumulative projected impacts from implementation year to 2050 of the selected combined policy on the packaging sector compared to the business as usual scenario for plastic production and greenhouse gas emissions associated with the plastic life cycle from production to disposal. The results reflect the state selected in the sidebar."),
        hr(),
        h2("Projected Plastic Waste Management: Combined vs. BAU"),
        withSpinner(plotOutput("comp_eol_plot")),
        h6(tags$strong("Figure 13."),  "Placeholder"),
        br(),
        h2("Projected Change in Annual Plastic Production: Combined vs. BAU"),
        withSpinner(plotOutput("comp_consum_line_chart")),
        h6(tags$strong("Figure 14"), "Projected annual plastic production from 1950-2050 under business as usual scenario, black solid line, and with the combined policy intervention on the packaging sector, dashed green line. Annual production is the total across all sectors in million metric tons (Mt). The difference in production between the two scenarios is shaded in green."),
        br(),
        ### Combined Summary ----------------------
        
        h2("Recycled Content Intervention Summary"),
        layout_columns( 
          style = "border-radius: 12px; padding: 20px; border:4px solid black; height: 250px; margin: 0 auto;",
          div(class = "text-center",
              h4("Produced"),
              icon("bottle-water", class = "fa-2xl", style = "color: black"),
              withSpinner(uiOutput("comp_total_consumption", inline = TRUE), type = 1),
              h6 (" million metric tons (Mt) of plastic."),
          ),
          div(class = "text-center",
              h4("Landfilled"),
              icon("trash-can", class = "fa-2xl", style = "color: black"),
              withSpinner(uiOutput("comp_total_landfill", inline = TRUE), type = 1),
              h6 (" million metric tons (Mt) of plastic.")) ,
          
          div(class = "text-center",
              h4("Recycled"),
              icon("recycle", class = "fa-2xl", style = "color: black"),
              withSpinner(uiOutput("comp_total_recycle", inline = TRUE), type = 1),
              h6 (" million metric tons (Mt) of plastic.")), 
          div(class = "text-center",
              h4("Incinerated"),
              icon("dumpster-fire", class = "fa-2xl", style = "color: black"),
              withSpinner(uiOutput("comp_total_incin", inline = TRUE), type = 1),
              h6 (" million metric tons (Mt) of plastic.")),
          div(class = "text-center",
              h4("Emitted"),
              icon("industry", class = "fa-2xl", style = "color: black"),
              withSpinner(uiOutput("comp_total_ghg", inline = TRUE), type = 1),
              h6 (" million metric tons (Mt) of CO2e."))
        ),
        h6(tags$strong("Figure 15."), "Projected totals for all sectors from implementation year to 2050."),
        
      )  # closes Combined Policy tabPanel()
    )    # closes tabsetPanel()
  ), # END nav_panel
  
  
  
  # SB 54 Policy ------------------------------------------------------------
  
  
  nav_panel(
    "CA SB54",
    br(),
    fluidRow(
      column(
        width = 3,
        selectInput(
          "implement_year_54",
          "Implementation Year:",
          choices = 2024:2050,
          selected = 2024
        ),
        selectInput("target_year_54", "Target Year:", choices = 2025:2050, selected = 2032),
        br(), 
        actionButton("run_sb54", "Model Policy", class = "btn-custom") 
      ),
      column(
        width = 9,
        
        h2("What is California Sentate Bill 54?"),
        h6("California’s Plastic Pollution Prevention and Packaging Producer Responsibility Act, Senate Bill 54 (SB 54):",
           tags$ul(
             tags$li("Mandates a 25% source reduction relative to the 2023 baseline year and a 65% recycling rate, for packaging by 2032."),
             tags$li("Protects and restores lands, waters and communities most impacted by plastic pollution by requiring producers to pay $5 billion into an environmental mitigation fund."),
             tags$li("Holds producers financially responsible for improving California’s recycling and composting infrastructure.")),
           "SB 54, which passed in 2022,  is anticipated to begin implementation in 2026. The projected impacts of SB 54 are shown below. However legal pressures continue to threaten the program and may push back its implementation, and possibly target year."), br(),
          h6("Use this tool to see how delays to this landmark legislation may impact the effectiveness of SB4 at reducing plastic production and waste."
        ),
        ## Static SB 54 impacts ----------------------------------------------------
        
        h2(("Projected Impacts from CA SB54")),
        
        layout_columns(
          div(
            style = "border-radius: 12px; padding: 15px; border:4px solid #687E03; height: 100%; display: flex; flex-direction: column; justify-content: space-between;",
            class = "text-center",
            h4(
              icon(
                name = "bottle-water",
                class = "fa-2xl",
                style = "color: #687E03"
              ),
              "Change in Primary Plastic Production",
              withSpinner(uiOutput("sb54_avoid_prod", inline = TRUE), type = 1)
            ),
            h6(" million metric tons (Mt) of plastic"),
            h6("from implementation year to 2050"),
            a(
              href = "https://www.themeasureofthings.com/results.php?comp=weight&unit=mt&amt=1",
              target = "_blank",
              class = "btn btn-custom btn-sm",
              "Contextualize your output"
            )
          ),
          div(
            style = "border-radius: 12px; padding: 15px; border: 4px solid #687E03; height: 100%; display: flex; flex-direction: column; justify-content: space-between;",
            class = "text-center",
            h4(
              icon("industry", class = "fa-2xl", style = "color: #687E03"),
              "Change in GHG Emissions",
              br(),
              withSpinner(uiOutput("sb54_ghg_diff", inline = TRUE), type = 1)
            ),
            h6("million metric tons (Mt) of CO2 equivalent"),
            h6("from implementation year to 2050"),
            a(
              href = "https://www.epa.gov/energy/greenhouse-gas-equivalencies-calculator",
              target = "_blank",
              class = "btn btn-custom btn-sm",
              "Contextualize your output"
            )
          ),  # END STATIC INPUTS
        ), 
        

        ## Plots -------------------------------------------------------------------

        
        h6(tags$strong("Figure 16."), "Cumulative projected impacts from 2025, intended implementation year, to 2050 of CA SB54 on the packaging sector compared to the business as usual scenario for plastic production and greenhouse gas emissions associated with the plastic life cycle from production to disposal. Results reflect the state selected in the sidebar to allow other states to model this landmark policy."),
        br(),
        h2("Projected Plastic Waste Management: CA SB 54 vs. BAU"),
        withSpinner(plotOutput("sb54_eol_plot")),
        h6(tags$strong("Figure 19"), "Projected plastic waste management under business as usual (BAU), black lines, and with a combined policy intervention on the packaging sector, green lines. Plastic waste is aggregated across all sectors from the implementation year to 2050 and measured in million metric tons (Mt). Incineration may not be visible because it is not an active form of waste management in all states. "),
        
        h2("The Cost of Delay"),
        h6("Change the implementation and target years to see how delays may impact the effectiveness of SB 54."),
        layout_columns(
          div(
            style = "border-radius: 12px; padding: 15px; border:4px solid #967DA1; height: 100%; display: flex; flex-direction: column; justify-content: space-between;",
            class = "text-center",
            h4(
              icon(
                name = "bottle-water",
                class = "fa-2xl",
                style = "color: black"
              ),
              "Change in Primary Plastic Production",
              withSpinner(uiOutput("sb54_diff_avoid_prod", inline = TRUE), type = 1)
            ),
            h6(" million metric tons (Mt) of plastic"),
            h6("from implementation year to 2050"),
            a(
              href = "https://www.themeasureofthings.com/results.php?comp=weight&unit=mt&amt=1",
              target = "_blank",
              class = "btn btn-custom btn-sm",
              "Contextualize your output"
            )
          ),
          div(
            style = "border-radius: 12px; padding: 15px; border: 4px solid #967DA1; height: 100%; display: flex; flex-direction: column; justify-content: space-between;",
            class = "text-center",
            h4(
              icon("industry", class = "fa-2xl", style = "color: black"),
              "Change in GHG Emissions",
              br(),
              withSpinner(uiOutput("sb54_diff_ghg", inline = TRUE), type = 1)
            ),
            h6("million metric tons (Mt) of CO2 equivalent"),
            h6("from implementation year to 2050"),
            a(
              href = "https://www.epa.gov/energy/greenhouse-gas-equivalencies-calculator",
              target = "_blank",
              class = "btn btn-custom btn-sm",
              "Contextualize your output"
            )
          )
        ), # END OUTPUTS
        h6(tags$strong("Figure 17."), "Change in cumulative impacts from implementation year to 2050 of the selected delays to CA SB54 compared to CA SB54 without delays for plastic production and greenhouse gas emissions associated with the plastic life cycle from production to disposal. The results reflect the state selected in the sidebar and the packaging sector."),
        h2("Projected Change in Annual Plastic Production:", br(), "SB 54 with and without Delays vs. BAU"),
        withSpinner(plotOutput("sb54_consum_line_chart")),
        h6(tags$strong("Figure 18."), "Projected annual plastic production from 1950-2050 under business as usual, (black solid line), with SB 54 planned implementation (purple dashed line) and with SB 54 delayed implementation (green dashed line). Annual production is the total across all sectors in million metric tons (Mt). The difference in production between business as usual and the delayed SB 54 scenario is shaded in green. The purple shaded area represents the lost plastic production savings due to pushed back implementation and/or target year(s) of SB 54."),
        br(),
      ) # END outputs
    ) # END fluid row
  ), 
  
  # Compare Solutions -------------------------------------------------------
  nav_panel(
    "Compare Solutions",
    br(),
    fluidRow( #start fluid row
      column(
        width = 3,
        selectInput( #start policy A selection
          "policy_a",
          "Policy A:",
          choices = c(
            "Source Reduction" = "sr",
            "Recycling Rate" = "rr",
            "Recycled Content" = "rc",
            "CA SB54" = "sb54",
            "Combined Policy" = "comp"
          ),
          selected = "sr"
        ), # end policy A selection
        selectInput( #start policy B selection
          "policy_b",
          "Policy B:",
          choices = c(
            "Source Reduction" = "sr",
            "Recycling Rate" = "rr",
            "Recycled Content" = "rc",
            "CA SB54" = "sb54",
            "Combined Policy" = "comp"
          ),
          selected = "rr"
        ), #end policy B selection
        br(),
        actionButton("run_compare", "Compare Policies", class = "btn-custom")
      ), #end column
 
    column( #start main column
      width = 9,
    br(), 
    
    
    # Title
    
    h2("Compare Projected Impacts Between Policies"),
    h6("Policy interventions act on different parts of the plastic lifecycle and some policies may be more effective than others at changing primary plastic production, plastic waste disposal, and greenhouse gas emissions. Use this tool to explore these differential outcomes. First, run two or more policies in the “Explore Solutions” tabs, then select them on the menu to the left."),
    br(),
    h6("*To find out more detailed information on your selected policies, check out the “Explore Solutions” tab."),
    
    h2(uiOutput("fig_19_title", inline = TRUE)), #reactive title: see server
    
    #bar plot avoid prod
    h6("A"),
    withSpinner(plotOutput("avoid_prod_comparison_bar")),
    
    #bar plot avoid ghg
    
    h6("B"),
    withSpinner(plotOutput("avoid_ghg_comparison_bar")),
    
    h6(uiOutput("fig_19_caption", inline = TRUE)), #reactive caption: see server
    
    #impacts with icons 
    
    h2(uiOutput("fig_20_title", inline = TRUE)), #reactive title: see server
  
    layout_columns(
      div(
        style = "border-radius: 12px; padding: 15px; border:4px solid #687E03; height: 100%; display: flex; flex-direction: column; justify-content: space-between;",
        class = "text-center",
        h4(
          icon(
            name = "bottle-water",
            class = "fa-2xl",
            style = "color: #687E03"
          ),
          "Change in Primary Plastic Production",
          withSpinner(uiOutput("comparison_avoid_prod", inline = TRUE), type = 1)
        ),
        h6(" million metric tons (Mt) of plastic"),
        h6("from implementation year to 2050"),
        a(
          href = "https://www.themeasureofthings.com/results.php?comp=weight&unit=mt&amt=1",
          target = "_blank",
          class = "btn btn-custom btn-sm",
          "Contextualize your output"
        )
      ),
      div(
        style = "border-radius: 12px; padding: 15px; border: 4px solid #687E03; height: 100%; display: flex; flex-direction: column; justify-content: space-between;",
        class = "text-center",
        h4(
          icon("industry", class = "fa-2xl", style = "color: #687E03"),
          "Change in GHG Emissions",
          br(),
          withSpinner(uiOutput("comparison_ghg_diff", inline = TRUE), type = 1)
        ),
        h6("million metric tons (Mt) of CO2 equivalent"),
        h6("from implementation year to 2050"),
        a(
          href = "https://www.epa.gov/energy/greenhouse-gas-equivalencies-calculator",
          target = "_blank",
          class = "btn btn-custom btn-sm",
          "Contextualize your output"
        )
      )
      ),
    h6(uiOutput("fig_20_caption", inline = TRUE)), #reactive caption: see server
    br(),
    h2(uiOutput("fig_21_title", inline = TRUE)), #reactive title: see server
    withSpinner(plotOutput("comparison_lollipop_plot")),
    h6(uiOutput("fig_21_caption", inline = TRUE)), #reactive caption: server
    
    
  ) #end main column
  ) #end fluid row
  ), #end nav panel
  
  

  
  # About  ------------------------------------------------------------------
  nav_panel(
    "About",
    h2("Plastic Policy Imact Model"),
    h6("An app for comparing plastic policy impacts based on the ", a(href = " https://www.scienceforconservation.org/assets/downloads/CA_Plastic_Use_TNC_2025.pdf", target = "_blank", "Geyer et al. (2025)."), "model without running code."),
    h4( "Report citation"),
    h6("Roland Geyer, Sara Orofino, Eleanor Thomas, and Darcy Bradley (2025) Policy is Essential to Curb Plastic Pollution: The example of California’s Senate Bill 54. The Nature Conservancy, San Francisco, California, USA."),
    br(),
    h4( "What this app compares"),
    h6("Plastic Policy Impact Model is the first state-level, time-dependent material flow analysis (MFA) of plastics.  It draws upon the EPA’s state-level environmentally extended input out models to estimate plastic consumption, waste generation, and waste management across major plastic use sectors from 2012-2020. A hindcast and forecast projection are used to construct a complete time series of sector-level plastic consumption, waste generation, and waste management from 1950-2050 which serves as the business as usual (BAU) scenario. This app enables users to simulate possible policy outcomes from sector-level interventions relative to BAU. It is a policy planning aid to conceptualize potential impacts."),
    br(),
    h4("Assumptions"),
    h6(
      tags$ul(
        tags$li("An 80% displacement rate between secondary and primary plastic production (i.e., every 1 ton of recycled plastic displaces 0.8 tons of virgin resin)."),
        tags$li("A 70% recycling yield loss rate (material collected, separated, and baled for recycling is equivalent to the recycling rate post yield loss)."),
        tags$li("Greenhouse gas emissions are based on emissions from mechanical recycling. Chemical forms of recycling can produce toxic substances, and are usually more greenhouse gas intensive, meaning that our model may underestimate the greenhouse gas emissions from recycling rate interventions if alternative forms are utilized."),
        tags$li("Recycled content is sourced 50-50 from in-state and out-of-state sources under the post-consumer recycled content mandate."),
        tags$li("Projected impacts are presented as the change in primary plastic production in millions of metric tons (Mt)—which is assumed to be equivalent to changes in consumption.")
      )
    ),
    br(),
    h4("Code"),
    h6( "This app: https://github.com/saraorofino/ca-mfa"),
    h4( "Collaborators"),
    h6("This app was made by the", a(href = " https://bren.ucsb.edu/undergrad-mentoring/bren-environmental-leadership-bel-program", target = "_blank", "UCSB Bren Environmental Leadership Fellowship (BEL)"),
    "recipients",
    a(href = "https://www.linkedin.com/in/emmarasmussen/", target = "_blank", "Emma Rasmussen"), "and",
    a(href = "https://www.linkedin.com/in/matthewroco-calvo/", target = "_blank", "Matthew Roco-Calvo.")),
    h4("Contact"),
    h6( "Sara Orofino  — sara.orofino@tnc.org · Ocean Scientist, The Nature Conservancy"),
    h4( "Sources"),
    h6(
      tags$ol(
        style = "margin-left: 20px;",
        tags$li("Geyer, Roland, Jenna R. Jambeck, and Kara Lavender Law. “Production, Use, and Fate of All Plastics Ever Made.” Science Advances 3, no. 7 (2017): e1700782. https://doi.org/10.1126/sciadv.1700782."),
        tags$li("Landrigan, P.J., et al, 2023. The Minderoo-Monaco Commission on Plastics and Human Health. Annals of Global Health 89, 23. https://doi.org/10.5334/aogh.4056"),
        tags$li("Verma, R., Vinoda, K.S., Papireddy, M., Gowda, A.N.S., 2016. Toxic Pollutants from Plastic Waste - A Review. Procedia Environmental Sciences, Waste Management for Resource Utilisation 35, 701–708. https://doi.org/10.1016/j.proenv.2016.07.069"),
        tags$li("Jenner, Lauren C., et al. “Detection of microplastics in human lung tissue using μFTIR spectroscopy.” Science of the Total Environment 831 (2022): 154907."),
        tags$li("Amato-Lourenço, Luís Fernando, et al. “Presence of airborne microplastics in human lung tissue.” Journal of hazardous materials 416 (2021): 126124."),
        tags$li("Ragusa, Antonio, et al. “Plasticenta: First evidence of microplastics in human placenta.” Environment international 146 (2021): 106274."),
        tags$li("Garcia, Marcus A., et al. “Quantitation and identification of microplastics accumulation in human placental specimens using pyrolysis gas chromatography mass spectrometry.” Toxicological Sciences 199.1 (2024): 81-88."),
        tags$li("Hu, Chelin Jamie, et al. “Microplastic presence in dog and human testis and its potential association with sperm count and weights of testis and epididymis.” Toxicological Sciences 200.2 (2024): 235-240."),
        tags$li("Zhu, Long, et al. “Tissue accumulation of microplastics and potential health risks in human.” Science of the Total Environment 915 (2024): 170004."),
        tags$li("Ragusa, Antonio, et al. “Raman microspectroscopy detection and characterisation of microplastics in human breastmilk.” Polymers 14.13 (2022): 2700."),
        tags$li("Stoett, P., 2022. Plastic pollution: A global challenge in need of multi-level justice-centered solutions. One Earth 5, 593–596. https://doi.org/10.1016/j.oneear.2022.05.017")
      )
    )
    
  ) #END nav_panel
) # END UI 


# Server ------------------------------------------------------------------



server <- function(input, output, session) {
  
  get_arrow_icon <- function(val) {
    if(val == 0) {
      NULL
    } else if (val < 0) {
      icon("arrow-up", class = "fa-2xl", style = "color: #e74c3c;")
    } else {
      icon("arrow-down", class = "fa-2xl", style = "color: #687E03;")
    }
  }
  
  # Pop Up Instructions -----------------------------------------------------
  showModal(
    modalDialog(
      title = HTML("Welcome to the Plastic Policy <br> Impact Model"),
      p(
        tags$strong("Step 1."),
        "Choose your state & sector.",
        br(),
        tags$strong ("Step 2."),
        "Model potential policy impacts in the Explore Solutions or CA SB54 tab.",
        br(),
        tags$strong ("Step 3."),
        "Visualize policies side-by-side in the Compare Solutions tab."
      ),
      easyClose = TRUE,
      footer = modalButton("Continue") |>
        tagAppendAttributes(class = "btn-custom")
    )
  )
  
  
  
  # Create BAU from State Input ---------------------------------------------
  state_abbr <- reactive({
    input$state
  })
  
  consum_bau <- reactive({
    if (state_abbr() == "CA") {
      ca_consum_bau_default
    } else {
      calc_consum_bau(
        bea_to_plastic = bea_to_plastic,
        state_abbr = state_abbr(),
        consumption_element = "Consumption_Complete",
        scaled_na_consumption = scaled_na_consumption,
        n_iterations = 4
      )
    }
  })
  # No pre-loading CA old code:
  #consum_bau <- reactive({
  #  results <- calc_consum_bau(
  #    bea_to_plastic = bea_to_plastic,
  #    state_abbr = state_abbr(),
  #   consumption_element = "Consumption_Complete",
  #    scaled_na_consumption = scaled_na_consumption,
  #   n_iterations = 4
  #  )
  # results
  # })
  
  
  # Incineration Static Data Input (future reactive) ------------------------
  
  incineration <- reactive({
    if (state_abbr() == "CA") {
      ca_incineration
    } else {
      warning(
        paste0(
          "No state-specific incineration data for '",
          state_abbr(),
          "'. Using national average."
        )
      )
      avg_incineration
    }
  })
  
  # Run Bau Model Results ---------------------------------------------------
  
  
  bau_results <- reactive({
    results <- run_bau(
      consum_bau(),
      incineration = incineration(),
      emission_factors = emission_factors,
      lifetimes = lifetimes,
      bau_rr = ca_rr,
      target_sector = input$sector
    )
    
    results
  })
  
  ### could make bau_rr_sect reactive in the future for other sector and state recycling rates
  
  
  # The Problem-----------------------------------------------------------------
  
  state_full <- reactive({
    req(input$state)
    names(state_choices)[state_choices == input$state]
  })
  
  output$state_full <- renderUI({
    state_full()
  })
  
  
  output$state_sum_intro <- renderUI({
    tagList("The Cost of Inaction for",state_full())
  })
  
  #EDIT
  sector_full <- reactive({
    req(input$sector)
    names(sector_choices)[sector_choices == input$sector]
  })
  

  output$sector_full <- renderUI({
    tagList(sector_full())
  })
  
  output$sector_title_sr <- renderUI({
    tagList(sector_full(),"Sector")
  })
  
  output$sector_title_rr <- renderUI({
    tagList(sector_full(),"Sector")
  })
  
  output$sector_title_rc <- renderUI({
    tagList(sector_full(),"Sector")
  })
  
  
  output$sector_title_comp <- renderUI({
    tagList(sector_full(),"Sector")
  })
  
  output$sector_title_sb54 <- renderUI({
    tagList(sector_full(),"Sector")
  })
  
  output$sector_title_diff <- renderUI({
    tagList(sector_full(),"Sector")
  })
  
  
  output$sum_bau <- renderUI({
    tags$span(
      style =" font-size: 40px;
      font-weight: bold;font-family: 'Epilogue', serif;",
      format(round(
        consum_bau() |>
          filter(year >= 2025) |>
          pull(mt_plastic_bau) |>
          sum(na.rm = TRUE))
      )) 
  })
  
  
  output$ghg_bau <- renderUI({
    tags$span(
      style = "font-size: 40px; font-weight: bold;font-family: 'Epilogue', serif;",
      format(round(
        bau_results()$ghg_bau$ghg_prod |>
          filter(year >= 2025) |>
          pull(mt_co2e_prod) |>
          sum(na.rm = TRUE)
      )))
  })
  
  
  output$bau_overview_plot <- renderPlot({
    df <- consum_bau() |>
      filter(sector != "all_sec")
    
    
    consum_bau_time_plot <- ggplot(df, aes(x = year, y = mt_plastic_bau, fill = sector)) +
      geom_area(data = filter(df)) +
      labs(x = "Year", y = "Annual Plastic Consumption (Mt)", fill = "Sector") +
      theme_classic(base_family = "Times New Roman", base_size = 20) +
      theme(
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20)
      ) +
      scale_fill_manual(values = c(
        pack = "#1B5E3C",
        buil = "#9ACD32",
        tran = "#7FC7C4",
        heal = "#A8D8B9",
        comm = "#5B9BD5",
        elec = "#2E75B6",
        hous = "#1F4E8C",
        mach = "#0D2C5C",
        text = "#C87137",
        othe = "#C4B8A8",
        agri = "#F5C071"
      ),
      labels = sector_labels)
    consum_bau_time_plot
  })
  
  # ---------------- Individual Policy: Source Reduction ----------------
  
  
  sr_results <- eventReactive(input$run_sr, {
    params <- tibble(
      policy_rate    = input$target_sr / 100,
      # converting from percent
      implement_year = as.numeric(input$implement_year_sr),
      target_year    = as.numeric(input$target_year_sr),
      baseline_year  = as.numeric(input$baseline_year_sr),
      target_sector  = input$sector
    )
    run_policy_sr(
      params,
      bau_results = bau_results(),
      incineration = incineration(),
      consum_bau = consum_bau(),
      bau_rr_sect = ca_rr, 
      lifetimes = lifetimes,
      emission_factors =emission_factors
    )
  })
  
  ## SR Summary value outputs --------------------------------------------------------
  
  output$sr_total_consumption <- renderUI({
    val <- sr_results()$total_consumption_sr
    tagList(
      tags$span(
        style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
        format(round(val))
      ))
  })
  
  
  output$sr_total_landfill <- renderUI({
    val <- sum(sr_results()$eol_sr_data$mt_plastic_landfill, na.rm = TRUE)
    tagList(
      tags$span(
        style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
        format(round(val))
      ))
  })
  
  
  output$sr_total_recycle <- renderUI({
    val <- sum(sr_results()$eol_sr_data$mt_secondary_plastic_output, na.rm = TRUE)
    tagList(
      tags$span(
        style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
        format(round(val))
      ))
  })
  
  output$sr_total_incin <- renderUI({
    val <- sum(sr_results()$eol_sr_data$mt_incin, na.rm = TRUE)
    tagList(
      tags$span(
        style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
        format(round(val))
      ))
  })
  
  
  output$sr_total_ghg <- renderUI({
    val <- sum(sr_results()$total_ghg_sr, na.rm = TRUE)
    tagList(
      tags$span(
        style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
        format(round(val))
      ))
  })
  
  ## SR value outputs against BAU -----------------------
  output$sr_avoid_prod <- renderUI({
    val <- sum(sr_results()$total_avoid_prod_sr, na.rm = TRUE)
    req(val)
    tagList(
      div(
        get_arrow_icon(val),
        tags$span(
          style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
          format(abs(round(val)))
        )) )
  })
  
  output$sr_ghg_diff <- renderUI({
    val <- sr_results()$total_ghg_diff_sr
    tagList(
      div(
        get_arrow_icon(val),
        tags$span(
          style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
          format(abs(round(val)))
        )) 
    )
  })
  
  
  ## SR consum line chart ----------------------------------------------------
  
  output$sr_consum_line_chart <- renderPlot({
    build_consum_line_chart(consum_bau = consum_bau(),
                            scenario_data = sr_results()$consum_sr_data,
                            implement_year = as.numeric(input$implement_year_sr),
                            scenario_label = "Source Reduction")
    
  })
  
  
  
  
  # ---------------- Individual Policy: Recycling Rate ----------------
  
  
  
  
  rr_results <- eventReactive(input$run_rr, {
    params_rr <- tibble(
      target_rr         = input$target_rr / 100,
      #converting from percent
      implement_year_rr = as.numeric(input$implement_year_rr),
      target_year_rr   = as.numeric(input$target_year_rr),
      #baseline_year_rr  = as.numeric(input$baseline_year), # only for sr
      target_sector_rr  = input$sector
    )
    run_policy_rr(
      params_rr,
      bau_results = bau_results(),
      incineration = incineration(),
      consum_bau = consum_bau(),
      bau_rr_sect = ca_rr,
      lifetimes = lifetimes,
      emission_factors = emission_factors
    )
  })
  ## RR Summary value outputs --------------------------------------------------------
  
  output$rr_total_consumption <- renderUI({
    val <- sum(rr_results()$total_consumption_rr) # ERROR IN OUTPUT
    tagList(
      tags$span(
        style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
        format(round(val))
      ))
  })
  
  
  output$rr_total_landfill <- renderUI({
    val <- sum(rr_results()$eol_rr_data$mt_plastic_landfill, na.rm = TRUE)
    tagList(
      tags$span(
        style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
        format(round(val))
      ))
  })
  
  
  output$rr_total_recycle <- renderUI({
    val <- sum(rr_results()$eol_rr_data$mt_secondary_plastic_output, na.rm = TRUE)
    tagList(
      tags$span(
        style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
        format(round(val))
      ))
  })
  
  output$rr_total_incin <- renderUI({
    val <- sum(rr_results()$eol_rr_data$mt_incin, na.rm = TRUE)
    tagList(
      tags$span(
        style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
        format(round(val))
      ))
  })
  
  
  output$rr_total_ghg <- renderUI({
    val <- sum(rr_results()$total_ghg_rr, na.rm = TRUE)
    tagList(
      tags$span(
        style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
        format(round(val))
      ))
  })
  
  # RR Outputs BAU--------------------------------------------------------------
  
  
  output$rr_avoid_prod <- renderUI({
    val <- sum(rr_results()$total_avoid_prod_rr, na.rm = TRUE)
    req(val)
    tagList(
      div(
        get_arrow_icon(val),
        tags$span(
          style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
          format(round(abs(val)))
        )) 
    )
  }) 
  
  output$rr_ghg_diff <- renderUI({
    val <- sum(rr_results()$total_ghg_diff_rr, na.rm =TRUE)
    tagList(
      div(
        get_arrow_icon(val),
        tags$span(
          style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
          format(round(abs((val)))
          )
        ))) 
  })
  
  
  
  ## RR EOL plot -------------------------------------------------------------
  
  rr_eol_compare_data <- reactive({
    #builds the comparison data separately, will be able to reference this if we wanted to use outputs for icons, tables, etc
    rr_res <- rr_results()
    build_eol_comparison_data(
      bau_results()$eol_bau,
      rr_res$eol_rr_data,
      "Recycling Rate",
      implement_year = input$implement_year_rr
    )
  })
  
  output$rr_eol_plot <- renderPlot({
    build_eol_comparison_plot(
      rr_eol_compare_data(),
      "Recycling Rate",
      "#687E03"
    )
  })
  
  
  
  
  
  
  # ---------------- Individual Policy: Recycled Content ----------------
  
  
  
  rc_results <- eventReactive(input$run_rc, {
    params_rc <- tibble(
      target_rc         = input$target_rc / 100,
      # converting from percent
      implement_year_rc = as.numeric(input$implement_year_rc),
      target_year_rc    = as.numeric(input$target_year_rc),
      target_sector_rc  = input$sector
    )
    
    run_policy_rc(params = params_rc, 
                  bau_results = bau_results(), 
                  incineration = incineration(), 
                  consum_bau = consum_bau(),
                  bau_rr_sect = ca_rr,
                  lifetimes = lifetimes,
                  emission_factors = emission_factors
                  )
  })
  ## RC Summary Outputs -----------------------
  output$rc_total_consumption <- renderUI({
    val <- sum(rc_results()$total_consumption_rc) # ERROR IN OUTPUT
    tagList(
      tags$span(
        style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
        format(round((val)))
      ))
  })
  
  
  output$rc_total_landfill <- renderUI({
    val <- sum(rc_results()$eol_rc_data$mt_plastic_landfill, na.rm = TRUE)
    tagList(
      tags$span(
        style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
        format(round(val))
      ))
  })
  
  
  output$rc_total_recycle <- renderUI({
    val <- sum(rc_results()$eol_rc_data$mt_secondary_plastic_output, na.rm = TRUE)
    tagList(
      tags$span(
        style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
        format(round((val)))
      ))
  })
  
  output$rc_total_incin <- renderUI({
    val <- sum(rc_results()$eol_rc_data$mt_incin, na.rm = TRUE)
    tagList(
      tags$span(
        style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
        format(round(val))
      ))
  })
  
  
  output$rc_total_ghg <- renderUI({
    val <- sum(rc_results()$total_ghg_rc, na.rm = TRUE)
    tagList(
      tags$span(
        style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
        format(round(val))
      ))
  })
  
  ##### RC Outputs BAU --------------------------------------------------------------
  
  output$rc_total_consumption <- renderUI({
    val <- rc_results()$total_consumption_rc
    tagList(
      tags$span(style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;", format(round(val)))
    )
  })
  
  output$rc_avoid_prod <- renderUI({
    val <- sum(rc_results()$total_avoid_prod_rc, na.rm = TRUE)
    req(val)
    tagList(
      div(
        get_arrow_icon(val),
        tags$span(
          style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
          format(abs(round(val))))
      )
    )
  })
  
  output$rc_ghg_diff <- renderUI({
    val <- sum(rc_results()$total_avoid_ghg_rc, na.rm =TRUE)
    req(val)
    tagList(
      div(
        get_arrow_icon(val),
        tags$span(
          style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
          format(abs(round(val))))
      )
    )
  })
  
  
  # RC Lollipop chart -------------------------------------------------------
  
  output$rc_lollipop_plot <- renderPlot({
    build_rc_comparison_plot(
      consum_bau = consum_bau(),
      avoid_prod_rc = rc_results()$total_avoid_prod_rc,
      scenario_color = "#687E03",
      implement_year = as.numeric(input$implement_year_rc)
    )
  }) 
  
  # ---------------- SB54 ----------------
  output$sb54_summary_table <- renderTable({
    placeholder_table("SB54")
  })
  output$sb54_plot <- renderPlot({
    placeholder_plot("SB54")
  })
  
  
  
  ## sb54 reactive (delays) -----------------------------------------------------------
  
  
  
  sb54_results <- eventReactive(input$run_sb54, {
    params_sb54 <- tibble(
      implement_year_54 = as.numeric(input$implement_year_54),
      target_year       = as.numeric(input$target_year_54)
    )
    
    run_policy_sb54( params = params_sb54, 
                     bau_results = bau_results(),
                     incineration = incineration(), 
                     consum_bau = consum_bau(),
                     bau_rr_sect = ca_rr, 
                     lifetimes = lifetimes,
                     emission_factors = emission_factors)
  })
  
  
  
  
  
  ## sb54 default (no delay) -------------------------------------------------
  #even though non reactive, must use reactive form to work in plotting
  
  sb54_default_results <- reactive({
    params_sb54_default <- tibble(
      implement_year_54 = as.numeric(2024),
      target_year = as.numeric(2032)
    )
    
    run_policy_sb54(params = params_sb54_default,
                    bau_results = bau_results(), 
                    incineration = incineration(),
                    consum_bau = consum_bau(),
                    bau_rr_sect = ca_rr, 
                    lifetimes = lifetimes,
                    emission_factors = emission_factors)
  })
  ## SB value outputs --------------------------------------------------------
  
  output$sb54_total_consumption <- renderUI({
    val <- sb54_default_results()$total_consumption_sb54 # Check column names, is sb54_default_results reactive? 
    req(val)
    tagList(
      div(
        get_arrow_icon(val),
        tags$span(
          style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
          format(round(val) )
        )) 
    )
  })
  
  output$sb54_avoid_prod <- renderUI({
    val <- sum(sb54_default_results()$total_avoid_prod_sb54, na.rm = TRUE) # Check column names, is sb54_default_results reactive?
    req(val)
    tagList(
      div(
        get_arrow_icon(val),
        tags$span(
          style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
          format(round(val))
        )) 
    )
  })
  
  output$sb54_ghg_diff <- renderUI({
    val <- sb54_default_results()$total_ghg_diff_sb54 # Check column names, is sb54_default_results reactive?
    req(val)
    tagList(
      div(
        get_arrow_icon(val),
        tags$span(
          style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
          format(round(val))
        ))
    )
  })
  
  # Delayed Implementation Outputs  --------------------------------------
  
  output$sb54_delay_total_consumption <- renderUI({
    val <- sb54_results()$total_consumption_sb54 # Check column names, is sb54_default_results reactive? 
    req(val)
    tagList(
      div(
        get_arrow_icon(val),
        tags$span(
          style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
          format(round(val) )
        ))
    )
  })
  
  output$sb54_delay_avoid_prod <- renderUI({
    val <- sum(sb54_results()$total_avoid_prod_sb54, na.rm = TRUE) 
    req(val)
    tagList(
      div(
        get_arrow_icon(val),
        tags$span(
          style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
          format(round(val))
        )) 
    )
  })
  
  output$sb54_delay_ghg_diff <- renderUI({
    val <- sb54_results()$total_ghg_diff_sb54 
    req(val)
    tagList(
      div(
        get_arrow_icon(val),
        tags$span(
          style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
          format(round(val)))
      )
    )
  })
  

# Delay difference outputs ------------------------------------------------

  output$sb54_diff_avoid_prod <- renderUI({
    val_default <- sum(sb54_default_results()$total_avoid_prod_sb54, na.rm = TRUE)
    val_delay <- sum(sb54_results()$total_avoid_prod_sb54, na.rm = TRUE)
    val <- val_delay - val_default
    req(val)
    tagList(
      div(
        get_arrow_icon(val),
        tags$span(
          style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
          format(round(val))
        ))
    )
  })
  
  output$sb54_diff_ghg <- renderUI({
    val_default <- sum(sb54_default_results()$total_ghg_diff_sb54 , na.rm = TRUE)
    val_delay <- sum(sb54_results()$total_ghg_diff_sb54 , na.rm = TRUE)
    val <- val_delay - val_default
    req(val)
    tagList(
      div(
        get_arrow_icon(val),
        tags$span(
          style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
          format(round(val))
        ))
    )
  })
  
  ## SB54 EOL plot -----------------------------------------------------------
  
  
  
  sb54_eol_compare_data <- reactive({
    #builds the comparison data separately, will be able to reference this if we wanted to use outputs for icons, tables, etc
    sb54_res <- sb54_results()
    build_eol_comparison_data(bau_results()$eol_bau,
                              sb54_res$eol_sb54_data,
                              "SB54",
                              input$implement_year_54)
  })
  
  output$sb54_eol_plot <- renderPlot({
    build_eol_comparison_plot(
      sb54_eol_compare_data(),
      "SB54",
      "#687E03"
    )
  })
  
  
  ## SB 54 consum line chart -------------------------------------------------
  
  output$sb54_consum_line_chart <- renderPlot({
    #uses current build_consum_line_chart function to build the comparison between BAU and delayed/reactive sb54
    sb54_consum_line_chart <- build_consum_line_chart(consum_bau = consum_bau(),
                                                      scenario_data = sb54_results()$consum_sb54_data,
                                                      implement_year = as.numeric(input$implement_year_54),
                                                      scenario_label = "SB54 with Delays")
    
    #adding a third line with 'default' /non delayed sb54 values
    sb54_consum_line_chart <- sb54_consum_line_chart +
      geom_line(
        data = sb54_default_results()$consum_sb54_data |> 
          filter(sector == "all_sec") |> 
          mutate(year = as.numeric(year)),
        aes(x = year, y = mt_plastic_sr, color = "SB54 without Delay"),
        linetype = "dashed"
      ) +
      scale_color_manual(values = c(
        "Business as Usual" = "black",
        "SB54 with Delays"  = "#687E03",
        "SB54 without Delay"   = "#967DA1"
      ))
    
    #joining reactive and default sb54 scenarios to build a ribbon between them
    sb54_compare_data <- sb54_results()$consum_sb54_data |> 
      filter(sector == "all_sec") |> 
      mutate(year = as.numeric(year)) |> 
      select(year, mt_plastic_sr_reactive = mt_plastic_sr) |> 
      left_join(
        sb54_default_results()$consum_sb54_data |> 
          filter(sector == "all_sec") |> 
          mutate(year = as.numeric(year)) |> 
          select(year, mt_plastic_sr_default = mt_plastic_sr),
        by = "year"
      )
    
    #adding a ribbon between the reactive and default sb54 lines
    sb54_consum_line_chart +
      geom_ribbon(
        data = sb54_compare_data,
        aes(x = year, ymin = pmin(mt_plastic_sr_reactive, mt_plastic_sr_default),
            ymax = pmax(mt_plastic_sr_reactive, mt_plastic_sr_default)),
        fill = "#967DA1",
        alpha = 0.2,
        inherit.aes = FALSE
      )
    
  })
  
  
  
  
  # ---------------- Combined Policy ----------------
  
  
  comp_results <- eventReactive(input$run_comp, {
    params_comp <- tibble(
      # sr — renamed to match what run_policy_comp() expects
      policy_rate_sr    = input$target_sr_comp / 100,
      baseline_year_sr  = as.numeric(input$baseline_year_sr_comp),
      target_year_sr    = as.numeric(input$target_year_sr_comp),
      implement_year_sr = as.numeric(input$implement_year_sr_comp),
      target_sector_sr  = input$sector,
      
      # rr
      policy_rate_rr    = input$target_rr_comp / 100,
      target_year_rr    = as.numeric(input$target_year_rr_comp),
      implement_year_rr = as.numeric(input$implement_year_rr_comp),
      target_sector_rr  = input$sector,
      
      # rc
      policy_rate_rc    = input$target_rc_comp / 100,
      target_year_rc    = as.numeric(input$target_year_rc_comp),
      implement_year_rc = as.numeric(input$implement_year_rc_comp),
      target_sector_rc  = input$sector,
      baseline_rc       = 0,
      # not exposed in UI yet — hardcoded default
      is_scrap_consump  = 0.5    # not exposed in UI yet — hardcoded default
    )
    
    run_policy_comp(params_comp = params_comp,
                    bau_results = bau_results(), 
                    incineration = incineration(),
                    consum_bau = consum_bau(),
                    lifetimes = lifetimes, 
                    emission_factors = emission_factors,
                    bau_rr_sect = ca_rr
                    )
  })
  
  #output$combined_policy_summary_table <- renderTable({
  #  comp_res <- comp_results()
  
  # tibble(
  #  Impact = c(
  #     "Total Consumption (MT)",
  #    "Avoided Primary Production (MT)",
  #    "Avoided GHG (MT CO2e)"
  #  ),
  #  value = c(
  #    comp_res$total_consumption_comp,
  #    comp_res$total_avoid_prod_comp,
  #    comp_res$total_ghg_diff_comp
  # )
  # )
  # })
  
  ## Compbined Summary Outputs -----------------------
  output$comp_total_consumption <- renderUI({
    val <- sum(comp_results()$total_consumption_comp) 
    tagList(
      tags$span(
        style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
        format(round(val))
      ))
  })
  
  
  output$comp_total_landfill <- renderUI({
    val <- sum(comp_results()$eol_comp_data$mt_plastic_landfill, na.rm = TRUE)
    tagList(
      tags$span(
        style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
        format(round(val))
      ))
  })
  
  
  output$comp_total_recycle <- renderUI({
    val <- sum(comp_results()$eol_comp_data$mt_secondary_plastic_output, na.rm = TRUE)
    tagList(
      tags$span(
        style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
        format(round(val))
      ))
  })
  
  output$comp_total_incin <- renderUI({
    val <- sum(comp_results()$eol_comp_data$mt_incin, na.rm = TRUE)
    tagList(
      tags$span(
        style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
        format(round(val))
      ))
  })
  
  
  output$comp_total_ghg <- renderUI({
    val <- sum(comp_results()$total_ghg_comp, na.rm = TRUE)
    tagList(
      tags$span(
        style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
        format(round(val))
      ))
  })
  
  ##### Combined Outputs BAU --------------------------------------------------------------
  
  
  output$comp_total_consumption <- renderUI({
    val <- comp_results()$total_consumption_comp
    tagList(
      div(
        tags$span(
          style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
          format(round(val) )
        ))
    )
  })
  
  output$comp_avoid_prod <- renderUI({
    val <- sum(comp_results()$total_avoid_prod_comp, na.rm = TRUE)
    req(val)
    tagList(
      div(
        get_arrow_icon(val),
        tags$span(
          style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
          format(round(val))
        ))
    )
  })
  
  output$comp_ghg_diff <- renderUI({
    val <- comp_results()$total_ghg_diff_comp
    req(val)
    tagList(
      div(
        get_arrow_icon(val),
        tags$span(
          style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
          format(round(val))
        ))
    )
  })
  
  
  ## Combined EOL plot -------------------------------------------------------
  
  comp_eol_compare_data <- reactive({
    comp_res <- comp_results()
    
    build_eol_comparison_data(
      bau_results()$eol_bau,
      comp_res$eol_comp_data,
      "Combined Policy",
      input$implement_year_sr_comp
    )
  })
  
  output$comp_eol_plot <- renderPlot({
    build_eol_comparison_plot(
      comp_eol_compare_data(),
      "Combined Policy",
      "#687E03"
    )
  })
  
  
  ## combined consum line chart ----------------------------------------------
  
  
  
  output$comp_consum_line_chart <- renderPlot({
    build_consum_line_chart(consum_bau = consum_bau(),
                            scenario_data = comp_results()$consum_comp_data,
                            implement_year = as.numeric(input$implement_year_sr_comp),
                            scenario_label = "Combined Policy")
    
  })
  
  
  
  # ---------------- Comparison ----------------
  output$comparison_summary_table <- renderTable({
    placeholder_table("Comparison")
  })
  output$comparison_plot <- renderPlot({
    placeholder_plot("Comparison")
  })
  
  ## Reactive titles/text
  
  # figure 19 title
  
  output$fig_19_title <- renderUI({
    res <- comparison_results()  
    tagList(
      "Projected Difference in Impact:", br(),
      paste0(policy_labels[[input$policy_a]], " - ", policy_labels[[input$policy_b]])
    )
    
  })
  
  # figure 19 caption
  
  output$fig_19_caption <- renderUI({
    res <- comparison_results()
    
    h6(tags$strong("Figure 19."), "Cumulative impacts relative to business as usual (BAU) of", 
       paste0(policy_labels[[input$policy_a]], " green bars, and ", policy_labels[[input$policy_b]]), 
       
       ",purple bars bars from implement year(s) to 2050 on (A) virgin plastic production in million metric tons (Mt), and (B) greenhouse gas emissions in million metric tons of CO2 equivalent (Mt CO2e).")
    
  })
  
  #figure 20 (key numerical ouputs)
  output$fig_20_title <- renderUI({
    res <- comparison_results()  
    tagList(
      "Projected Difference in Impact:", br(),
      paste0(policy_labels[[input$policy_a]], " - ", policy_labels[[input$policy_b]])
    )
   
  })
  
  #figure 20 caption
  
  output$fig_20_caption <- renderUI({
    res <- comparison_results()
    
    h6(tags$strong("Figure 20."), "Cumulative difference in projected impacts from respective implementation years to 2050 between", 
       paste0(policy_labels[[input$policy_a]], " and ", policy_labels[[input$policy_b]]), 
    
    ".Projected impacts are based on the select policies on the packaging sector compared to the business as usual scenario for plastic production and greenhouse gas emissions associated with the plastic life cycle from production to disposal. The results reflect the state selected in the sidebar.")
    
  })
  
  # figure 21 title (EOL lollipop)
  
  output$fig_21_title <- renderUI({
    res <- comparison_results()  
    tagList(
      "Projected Plastic Waste Management:", br(),
      paste0(policy_labels[[input$policy_a]], ", ", policy_labels[[input$policy_b]]),
      "and BAU"
    )
  })
  
  # figure 21 captions
  
  output$fig_21_caption <- renderUI({
    res <- comparison_results()
    
    h6(tags$strong("Figure 21."), "Projected plastic waste management under business as usual (BAU), black lines, with a ", 
       paste0(policy_labels[[input$policy_a]]),
       " intervention on the packaging sector, green lines, and with a",
         paste0(policy_labels[[input$policy_b]]), 
       "intervention on the packaging sector, purple lines. Plastic waste is aggregated across all sectors from the implementation year to 2050 and measured in million metric tons (Mt). Incineration may not be visible because it is not an active form of waste management in all states."
       )
    
  })
  
  ## pulling each policy (compare) -----------------------------------------------------  
  ## 
  ## sector labels for functions to reference
  
  policy_labels <- c(
    sr   = "Source Reduction",
    rr   = "Recycling Rate",
    rc   = "Recycled Content",
    sb54 = "CA SB54",
    comp = "Combined Policy"
  )
  
  
  comparison_results <- eventReactive(input$run_compare, { # when 'run_compare' , create comapre_results DF list
    
    get_policy_result <- function(policy_code) { #creating a function to pull results from each tab 
      tryCatch( #using tryCatch to create appropriate errors if results are null
        switch(policy_code,
               sr   = sr_results(),
               rr   = rr_results(),
               rc   = rc_results(),
               sb54 = sb54_results(),
               comp = comp_results()
        ),
        error = function(e) NULL
      )
    } #end get_policy_result function
    
    policy_labels <- c(
      sr   = "Source Reduction",
      rr   = "Recycling Rate",
      rc   = "Recycled Content",
      sb54 = "CA SB54",
      comp = "Combined Policy"
    ) #creating policy labels to reference in errors (so that it doesn't return acronym)  
  
    
    policy_a_data <- get_policy_result(input$policy_a)
    policy_b_data <- get_policy_result(input$policy_b)
    
    validate(
      need(
        !is.null(policy_a_data),
        paste0("Please run the '", policy_labels[[input$policy_a]], "' intervention on its tab first.")
      ),
      need(
        !is.null(policy_b_data),
        paste0("Please run the '", policy_labels[[input$policy_b]], "' intervention on its tab first.")
      )
    )
    
    return(list(
      policy_a_data = policy_a_data,
      policy_b_data = policy_b_data
      
    )) #returns respective list of dataframes for policy a and b
    
  }) # end eventReactive for compare_results
  
  
  ## function to pull avoided production between two policies
  
  get_avoid_prod <- function(policy_code, policy_data) {
    switch(policy_code,
           sr   = policy_data$total_avoid_prod_sr,
           rr   = policy_data$total_avoid_prod_rr,
           rc   = policy_data$total_avoid_prod_rc,
           sb54 = policy_data$total_avoid_prod_sb54,
           comp = policy_data$total_avoid_prod_comp
    )
  } #end get_avoid_prod
  
  ## calculating the difference in avoid prod and putting it as an output
  
  output$comparison_avoid_prod <- renderUI({
    res <- comparison_results() #uses "comparison_results" reactive
    
    avoid_prod_a <- get_avoid_prod(input$policy_a, res$policy_a_data)
    avoid_prod_b <- get_avoid_prod(input$policy_b, res$policy_b_data)
    
    val <- avoid_prod_a - avoid_prod_b #calculate the difference between avoided production after pulling results
    
    tagList(
      div(
        get_arrow_icon(val),
        tags$span(
          style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
          format(abs(round(val)))
        )
      )
    ) #end tag list
    
  })
  
  ## function to pull avoided GHG between two policies
  
  get_avoid_ghg <- function(policy_code, policy_data) {
    switch(policy_code,
           sr   = policy_data$total_ghg_diff_sr,
           rr   = policy_data$total_ghg_diff_rr,
           rc   = policy_data$total_avoid_ghg_rc, #uses different calculation, ghg saved from displacement of virgin plastics
           sb54 = policy_data$total_ghg_diff_sb54,
           comp = policy_data$total_ghg_diff_comp
    )
  } #end get_avoid_ghg
  
  
  ## function to pull 
  
  get_eol_data <- function(policy_code, policy_data) {
    switch(policy_code,
           sr   = policy_data$eol_sr_data,
           rr   = policy_data$eol_rr_data,
           rc   = policy_data$eol_rc_data,
           sb54 = policy_data$eol_sb54_data,
           comp = policy_data$eol_comp_data
    )
  }
  
  ## total consumption
  
  get_total_consumption <- function(policy_code, policy_data) {
    switch(policy_code,
           sr   = policy_data$total_consumption_sr,
           rr   = policy_data$total_consumption_rr,
           rc   = policy_data$total_consumption_rc,
           sb54 = policy_data$total_consumption_sb54,
           comp = policy_data$total_consumption_comp
    )
  }
  
  get_implement_year <- function(policy_code) {
    switch(policy_code,
           sr   = as.numeric(input$implement_year_sr),
           rr   = as.numeric(input$implement_year_rr),
           rc   = as.numeric(input$implement_year_rc),
           sb54 = as.numeric(input$implement_year_54),
           comp = as.numeric(input$implement_year_sr_comp)
    )
  }
  
  
  get_ghg_data <- function(policy_code, policy_data) {
    switch(policy_code,
           sr   = policy_data$ghg_sr_data,
           rr   = policy_data$ghg_rr_data,
           rc   = policy_data$ghg_rc_data,
           sb54 = policy_data$ghg_sb54_data,
           comp = policy_data$ghg_comp_data
    )
  }
  
  get_consum_data <- function(policy_code, policy_data) {
    df <- switch(policy_code,
                 sr   = policy_data$consum_sr_data,
                 rr   = policy_data$consum_rr_data,
                 rc   = policy_data$consum_rc_data,
                 sb54 = policy_data$consum_sb54_data,
                 comp = policy_data$consum_comp_data
    )
    
    # rr and rc use BAU consumption since they do not alter production (run_policy_rr and _rc do not run calc_consum_rr)
    if (policy_code %in% c("rr", "rc")) {
      df <- df |> rename(mt_plastic_sr = mt_plastic_bau) #renaming column names of these policies to _sr for graphing to be consistent with naming in graphing function.
    }
    df
  }
  
  ## calculating the difference in avoid ghg and putting it as an output
  
  output$comparison_ghg_diff <- renderUI({
    res <- comparison_results() #uses "comparison_results" reactive
    
    avoid_ghg_a <- get_avoid_ghg(input$policy_a, res$policy_a_data)
    avoid_ghg_b <- get_avoid_ghg(input$policy_b, res$policy_b_data)
    
    val <- avoid_ghg_a - avoid_ghg_b #calculate the difference between avoided production after pulling results
    
    tagList(
      div(
        get_arrow_icon(val),
        tags$span(
          style = "font-size: 40px; font-weight: bold; font-family: 'Epilogue', serif;",
          format(abs(round(val)))
        )
      )
    ) #end tag list
    
  })
  
  #plots:
  
  ##RUNNING PLOTS:
  
  ## building the avoid prod comparison bar chart
  
  output$avoid_prod_comparison_bar <- renderPlot({
    res <- comparison_results()
  
    build_avoid_prod_comparison_bar(
      avoid_prod_a = get_avoid_prod(input$policy_a, res$policy_a_data),
      avoid_prod_b = get_avoid_prod(input$policy_b, res$policy_b_data),
      scenario_a_name = policy_labels[[input$policy_a]],
      scenario_b_name = policy_labels[[input$policy_b]],
      scenario_a_color = "#687E03",
      scenario_b_color = "#967DA1"
    )
  })
    
  ## building the avoid ghg comparison bar chart
  
  output$avoid_ghg_comparison_bar <- renderPlot({
    res <- comparison_results()
    
    build_avoid_ghg_comparison_bar(
      avoid_ghg_a = get_avoid_ghg(input$policy_a, res$policy_a_data),
      avoid_ghg_b = get_avoid_ghg(input$policy_b, res$policy_b_data),
      scenario_a_name = policy_labels[[input$policy_a]],
      scenario_b_name = policy_labels[[input$policy_b]],
      scenario_a_color = "#687E03",
      scenario_b_color = "#967DA1"
    )
  })
  
    ## building the EOL comparison lollipop plot
    
    output$comparison_lollipop_plot <- renderPlot({
      res <- comparison_results()
      
      
      implement_year_a <- get_implement_year(input$policy_a)
      implement_year_b <- get_implement_year(input$policy_b)
      
      total_consumption_bau <- consum_bau() |> 
        filter(sector == "all_sec", year >= min(implement_year_a, implement_year_b)) |> 
        summarise(total = sum(mt_plastic_bau, na.rm = TRUE)) |> 
        pull(total)
      
      
      
      build_comparison_lollipop(
        eol_bau_data = bau_results()$eol_bau,
        eol_a_data = get_eol_data(input$policy_a, res$policy_a_data),
        eol_b_data = get_eol_data(input$policy_b, res$policy_b_data),
        total_consumption_bau = total_consumption_bau,
        total_consumption_a = get_total_consumption(input$policy_a, res$policy_a_data),
        total_consumption_b = get_total_consumption(input$policy_b, res$policy_b_data),
        scenario_a_name = policy_labels[[input$policy_a]],
        scenario_b_name = policy_labels[[input$policy_b]],
        implement_year_a = implement_year_a,
        implement_year_b = implement_year_b,
        scenario_a_color = "#687E03",
        scenario_b_color = "#967DA1"
      )
    })
    
    
  
  ## GHG line plot. Currently not displayed in UI, but leaving in server incase we want to display it again

    
    output$comparison_ghg_line_chart <- renderPlot({
      res <- comparison_results()
      
      build_ghg_comparison_line_chart(
        ghg_bau_data = bau_results()$ghg_bau,
        ghg_a_data = get_ghg_data(input$policy_a, res$policy_a_data),
        ghg_b_data = get_ghg_data(input$policy_b, res$policy_b_data),
        scenario_a_name = policy_labels[[input$policy_a]],
        scenario_b_name = policy_labels[[input$policy_b]],
        implement_year_a = get_implement_year(input$policy_a),
        implement_year_b = get_implement_year(input$policy_b),
        scenario_a_color = "#687E03",
        scenario_b_color = "#967DA1"
      )
    })
  
    
    ## comparison consumption/production line chart, not shown in UI but leaving here incase we want to display it again
    
    output$comparison_consum_line_chart <- renderPlot({
      
      #pulling needed data
      
      res <- comparison_results()
      scenario_a_name <- policy_labels[[input$policy_a]]
      scenario_b_name <- policy_labels[[input$policy_b]]
      implement_year_a <- get_implement_year(input$policy_a)
      implement_year_b <- get_implement_year(input$policy_b)
      
      
      #uses current build_consum_line_chart function to build the comparison between BAU policy A
      
      comparison_consum_line_chart <- build_consum_line_chart(consum_bau = consum_bau(),
                                                              scenario_data = get_consum_data(input$policy_a, res$policy_a_data),
                                                              implement_year = implement_year_a,
                                                              scenario_label = scenario_a_name)
      
      #adding a third line with scenario B ontop of current plot values
      comparison_consum_line_chart <- comparison_consum_line_chart +
        geom_line(
          data = get_consum_data(input$policy_b, res$policy_b_data) |> 
            filter(sector == "all_sec") |> 
            mutate(year = as.numeric(year)),
          aes(x = year, y = mt_plastic_sr, color = scenario_b_name),
          linetype = "dashed"
        ) +
        #there is already a vline for implement A, however overwriting with color
        geom_vline(xintercept = implement_year_a, color = "#687E03", linetype = "dotted" ) + 
        geom_vline(xintercept = implement_year_b, color = "#967DA1", linetype = "dotted" ) +
        scale_color_manual(values = c(
          "Business as Usual" = "black",
          setNames("#687E03", scenario_a_name),
          setNames("#967DA1", scenario_b_name)
        ))
      
      #joining reactive and default sb54 scenarios to build a ribbon between them
      comparison_compare_data <- get_consum_data(input$policy_a, res$policy_a_data) |> 
        filter(sector == "all_sec") |> 
        mutate(year = as.numeric(year)) |> 
        select(year, mt_plastic_sr_a = mt_plastic_sr) |> 
        left_join(
          get_consum_data(input$policy_b, res$policy_b_data) |> 
            filter(sector == "all_sec") |> 
            mutate(year = as.numeric(year)) |> 
            select(year, mt_plastic_sr_b = mt_plastic_sr),
          by = "year"
        )
      
      #adding a ribbon between the reactive and default sb54 lines
      comparison_consum_line_chart +
        geom_ribbon(
          data = comparison_compare_data,
          aes(x = year, ymin = pmin(mt_plastic_sr_a, mt_plastic_sr_b),
              ymax = pmax(mt_plastic_sr_a, mt_plastic_sr_b)),
          fill = "#967DA1",
          alpha = 0.2,
          inherit.aes = FALSE
        )
      
    })
  
  
 
  
 
  
  
  
} # END SERVER







# Create the shiny app ----------------------------------------------------

shinyApp(ui = ui, server = server)



