# Load dependencies necessary for shiny dashboard
lapply(c("shinydashboard", "shiny", "ggplot2",
         "gridExtra", "DT", "dplyr",
         "tidyr", "stringr", "pbapply",
         "lubridate", "purrr", "formattable",
         "rsconnect", "scales"), function(x){
  require(x, character.only = TRUE)
})

library(shinydashboard)
library(scales)

print(sessionInfo())

# Coercing installation of required dependency for shiny dashboard
# devtools::install_github("rstudio/httpuv")

# Read-in most recently prepared data
#read_data <- read.csv("Surgery/ME9005_TOTAL_JOINT_ENCOUNTERS_(5-4-2021).csv",
                      #skip = 3) %>%
  #cleansing(.)

#read_data <- read_data %>% group_by(field) %>% nest() %>% ungroup()

ui <- dashboardPage(
  
  dashboardHeader(), 
  dashboardSidebar(
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem(text = "CSV Upload", tabName = "filedata", icon = icon("file-upload")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Visualizations", icon = icon("bar-chart-o"), startExpanded = TRUE,
       menuSubItem("Pie Charts", tabName = "pie_charts"),
       menuSubItem("Bar Graphs", tabName = "bar_graphs")
      ),
      menuItem(selectInput("practice_input", 
                           "Practice Area",
                           choices = unique(c("Podiatry", "Orthopaedic",
                                              "General", "Hand", "Gynecology",
                                              "Pain Management", "Spine")) %>%
                             as.character()), tabName = "Practice Area")
    ),
    textOutput("res")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              strong("MONTHLY DATA SUMMARY"),
              fluidRow(box(DT::dataTableOutput("summary"))),
              br(), br(),
              strong("MONTHLY DATA DETAIL"),
              DT::dataTableOutput("detail")),
      tabItem(tabName = "filedata", 
              fluidRow(
                box(width = 12, 
                       fileInput(inputId = "filedata", 
                                 label = "File upload:",
                                 accept = ".csv")
                    ))),
      tabItem(tabName = "pie_charts",
              fluidRow(
                tabBox(title = "Pie Charts", id = "pie_tabset",
                       tabPanel("Billed $", shiny::plotOutput(outputId = "pie_plot")),
                       tabPanel("Case Qty", shiny::plotOutput(outputId = "pie_plot2"))
              ))),
      tabItem(tabName = "subitem2", "Sub-item 2 tab content") 
    )
  )
)

server <- function(input, output, session) {
  output$res <- renderText({
    if (input$sidebarCollapsed) {
      "Sidebar is collapsed"
    } else {
      "Sidebar is expanded"
    }
  })
  
  # Enabling data to be read from csv
  data <- reactive({
    req(input$filedata)
    test <- read.csv(input$filedata$datapath, skip = 3)
    
    print(head(test))
    
    cleansing <- function(data){
      
      #data <- read_data
      
      temp_tbl <- data %>%
        dplyr::rename_all(., function(x){tolower(x)}) %>%
        mutate_all(., function(x){stringr::str_squish(x)}) %>%
        inner_join(tibble(physician = c('102 - Vancourt, Robert B', 
                                        '103 - Wilcox, Joseph F', 
                                        '104 - Bernacki, Walter L', 
                                        '105 - Mathis, Jeremy R', 
                                        '106 - Philbin, Terrence M',
                                        '107 - Logan, Daniel B', 
                                        '108 - Lowery, Lisa K',
                                        '112 - Comisar, Bruce R', 
                                        '113 - Hyer, Christopher F', 
                                        '115 - Berlet, Gregory C', 
                                        '122 - Westerheide, Kenneth J', 
                                        '140 - Millard, Gary M', 
                                        '144 - Krantz, Carl A', 
                                        '145 - Moeller, Donnalyn', 
                                        '147 - Cook, Tracy L', 
                                        '156 - Jones, Stuart R',
                                        '197 - Rouse Scharschmidt, Angela G', 
                                        '204 - Davanzo, Mark E', 
                                        '223 - Long, Nathaniel K', 
                                        '265 - Figg, Gregory M', 
                                        '286 - Bull, Patrick E', 
                                        '313 - Thomas, Randall C', 
                                        '323 - Quisno, Amanda L', 
                                        '326 - Prissel, Mark A', 
                                        '334 - Stephens, Scott P', 
                                        '335 - Griffith, Corey J', 
                                        '350 - Kovack, Thomas J', 
                                        '355 - Cassandra, James C', 
                                        '357 - Skeels, Michael', 
                                        '368 - Eichenseer, Paul H', 
                                        '369 - Gould, Robert E'),
                          field = c("Podiatry", "Orthopaedic", "General",
                                    "Orthopaedic", "Podiatry", "Podiatry",
                                    "Orthopaedic", "Orthopaedic", "Podiatry",
                                    "Podiatry", "Orthopaedic", "Hand",
                                    "Gynecology", "Podiatry", "Gynecology",
                                    "Gynecology", "Gynecology", "General",
                                    "Orthopaedic", "Pain Management", "Podiatry",
                                    "Podiatry", "Podiatry", "Podiatry", "Orthopaedic",
                                    "Podiatry", "Orthopaedic", "Orthopaedic",
                                    "Pain Management", "Spine", "Pain Management"))) %>%
        mutate(date = stringr::str_split(dateofservice, "/")) %>%
        mutate(date = pbapply::pblapply(date, function(x){
          
          #x <- tbl_data$date[1]
          
          term <- x %>% unlist()
          
          return(paste0(term[3],"-", term[1], "-", term[2]) %>% as.Date())
          
        })) %>% as_tibble() %>% mutate(date = unlist(date)) %>% 
        mutate(date = lubridate::as_date(date)) %>%
        mutate(month = month(date)) %>%
        group_by(field) %>% tidyr::nest() %>% ungroup() %>%
        mutate(data_cleansed = pbapply::pblapply(data, function(x){
          
          #x <- tibble(temp_tbl$data[2])
          
          temp_tbl <- x %>% tidyr::unnest() %>% group_by(month) %>% tidyr::nest() %>%
            ungroup() %>%
            mutate(data = pbapply::pblapply(data, function(x){
              
              #x <- tibble(temp_tbl$data[1])
              
              temp_temp_tbl <- x %>% tidyr::unnest() %>%
                mutate_at(vars(staffcost:billedamount,
                               srgymin:groupbilledamount,
                               grouppatientcount, groupsurgeryminutes),
                          function(x){extract_numeric(x)}) %>%
                mutate(groupby = stringr::str_split(groupby, ":")) %>%
                mutate(groupby = pbapply::pblapply(groupby, function(x){
                  
                  #x <- tbl_data$date[1]
                  
                  term <- x %>% unlist()
                  
                  return(paste0(term[1]))})) %>% 
                dplyr::select(., -starts_with("total")) %>%
                mutate(groupsurgeryminutes = groupsurgeryminutes * 10000) %>%
                mutate_if(is.numeric, function(x){ifelse(is.na(x), 0, x)})
              
              metrics <- tibble(num_cases = nrow(temp_temp_tbl),
                                avg_supplycost = round(mean(temp_temp_tbl$supplycost),
                                                       digits = 2),
                                total_billedamount = round(sum(temp_temp_tbl$billedamount),
                                                           digits = 2),
                                avg_srgymin = round(mean(temp_temp_tbl$srgymin),
                                                    digits = 2)) %>%
                mutate(cases_sample = round(num_cases * 0.05, digits = 0)) %>%
                mutate(cases_sample = ifelse(cases_sample <=1, 1, cases_sample))
              
              return(metrics)
            })) %>% tidyr::unnest()
          
          return(temp_tbl)
          
        }))
      
      data_viz <- temp_tbl %>% 
        dplyr::select(-c(data_cleansed)) %>% tidyr::unnest() %>%
        inner_join(test <- temp_tbl %>% dplyr::select(-c(data)) %>%
                     tidyr::unnest())
      
      return(data_viz)
      
    }
    
    cleansing(test)
    
  })
  
  output$summary <- DT::renderDataTable({
    DT::datatable(sum_results <- data() %>%
                    filter(field == input$practice_input) %>%
                    dplyr::select(c(month:cases_sample)) %>%
                    distinct() %>%
                    pivot_longer(., cols = c(!month), names_to = "measure_name",
                                 values_to = "amount") %>%
                    mutate(month = tolower(as.character(month(ymd(010101) + 
                                                                months(month-1),
                                                              label=TRUE,
                                                              abbr=TRUE)))) %>%
                    pivot_wider(., names_from = month, values_from = amount) %>%
                    group_by(measure_name) %>% tidyr::nest() %>% ungroup() %>%
                    mutate(data = map2(measure_name, data, function(x, y){
                      
                      if(x %in% c('avg_supplycost', 'total_billedamount') == TRUE){
                        
                        y %>% tidyr::unnest() %>%
                          mutate_all(., function(x){formattable::currency(x, digits = 2L) %>%
                              as.character()})
                      }else{y %>% tidyr::unnest() %>% mutate_all(., function(x){format(x, nsmall = 2)})}
                      
                    })) %>% tidyr::unnest() %>%
                    mutate(measure_name = case_when(
                      measure_name == "num_cases" ~ "Number of Cases",
                      measure_name == "avg_supplycost" ~ "Avg Supply Cost $", 
                      measure_name == "total_billedamount" ~ "Total Billed $",
                      measure_name == "avg_srgymin" ~ "Avg Surgery Minutes",
                      TRUE ~ "Number of Cases (sample)"
                    )),
                  extensions = "FixedColumns",
                  options = list(
                    paging = TRUE, searching = TRUE, info = FALSE,
                    sort = TRUE, scrollX = TRUE))    
  })
  
  output$detail <- DT::renderDataTable({DT::datatable(data() %>%
                                                    filter(field == input$practice_input) %>%
                                                    dplyr::select(-c(month:cases_sample)) %>%
                                                    distinct() %>%
                                                    arrange(dateofservice),
                                                  extensions = "FixedColumns",
                                                  options = list(autoWidth = TRUE,
                                                                 paging = TRUE, searching = TRUE, info = FALSE,
                                                                 sort = TRUE, scrollX = TRUE, 
                                                                 fixedColumns = list(leftColumns = 3)))})

  output$pie_plot <- renderPlot({
    
    # Development of Pie Chart Data
    test_data <- data() %>%
      dplyr::select(field, total_billedamount, num_cases) %>%
      distinct() %>% 
      mutate(cum_billed = sum(total_billedamount),
             cum_cases = sum(num_cases)) %>%
      mutate(field_perc = round(total_billedamount/cum_billed, digits = 3),
             field_case_perc = round(num_cases/cum_cases, digits = 3)) %>%
      dplyr::select(-c(cum_billed, cum_cases)) %>%
      mutate(field_perc_2 = label_percent()(field_perc),
             field_case_perc_2 = label_percent()(field_case_perc)) %>%
      mutate(total_billed_abbrev = round(total_billedamount/1000000,
                                         digits = 0)) %>%
      mutate(total_cases_abbrev = round(num_cases,
                                        digits = 0)) %>%
      mutate(field_perc_2 = paste0(field, " - ",
                                   paste0("$",total_billed_abbrev,
                                          "M "), '(', field_perc_2, ")"),
             field_cases_perc_2 = paste0(field, " - ",total_cases_abbrev,
                                         ' (', field_case_perc_2, ")")) %>%
      as.data.frame()
    
    pie(test_data$field_perc, labels = test_data$field_perc_2,
        col=rainbow(length(test_data$field_perc_2)), 
        main="Total Billed Amount ($)")
  
  })
  
  output$pie_plot2 <- renderPlot({
    
    # Development of Pie Chart Data
    test_data <- data() %>%
      dplyr::select(field, total_billedamount, num_cases) %>%
      distinct() %>% 
      mutate(cum_billed = sum(total_billedamount),
             cum_cases = sum(num_cases)) %>%
      mutate(field_perc = round(total_billedamount/cum_billed, digits = 3),
             field_case_perc = round(num_cases/cum_cases, digits = 3)) %>%
      dplyr::select(-c(cum_billed, cum_cases)) %>%
      mutate(field_perc_2 = label_percent()(field_perc),
             field_case_perc_2 = label_percent()(field_case_perc)) %>%
      mutate(total_billed_abbrev = round(total_billedamount/1000000,
                                         digits = 0)) %>%
      mutate(total_cases_abbrev = round(num_cases,
                                        digits = 0)) %>%
      mutate(field_perc_2 = paste0(field, " - ",
                                   paste0("$",total_billed_abbrev,
                                          "M "), '(', field_perc_2, ")"),
             field_cases_perc_2 = paste0(field, " - ",total_cases_abbrev,
                                         ' (', field_case_perc_2, ")")) %>%
      as.data.frame()
    
    pie(test_data$field_case_perc, labels = test_data$field_cases_perc_2,
        col=rainbow(length(test_data$field_cases_perc_2)), 
        main="Total Case Amount")
    
  })
  
}

shinyApp(ui, server)
