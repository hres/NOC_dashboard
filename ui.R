ui<-dashboardPage(
  dashboardHeader(title='Notice of Compliance (NOC) Dashboard',
                  titleWidth=400),
  
  dashboardSidebar(
                   sidebarMenu(id='sidebarmenu',
                     menuItem(tabName = 'overview','NOC submission overview'),
                     menuItem(tabName = 'explore','NOC submission history'),
                     menuItem(tabName = 'intro','About')
                   ),
                   conditionalPanel("input.sidebarmenu=='overview'",
                                    
                                    selectizeInput('time',"Select a Year",
                                                   choices=c('All',years)),
                                    actionLink("remove", "Remove detail tabs")
                   ),
                   conditionalPanel("input.sidebarmenu=='explore'",
                     
                     selectizeInput('product',"Select a drug ingredient",
                                    choices=c("start typing to search"="",ingredients))
                   )
                   
    
  ),
  dashboardBody(
    tabItems(
    tabItem(tabName='overview',
    tabsetPanel(id = "tabs",
                tabPanel(
                  title = "Main Dashboard",
                  value = "page1",
                  
                  fluidRow(
                    valueBoxOutput("total_noc")
                  ),
                  fluidRow(
                    column(width = 12,
                           p(textOutput("annual")),
                           highchartOutput("group_totals")),
                    column(width = 6,
                           tags$b("Product Category"),
                           tags$p("Click on a category in the plot to see the details"),
                           highchartOutput("product_type")),
                    column(width=6,
                           tags$b("Submission Type"),
                           tags$p("Click on a category in the plot to see the details"),
                           highchartOutput('submission_type'))
                  ),
                  fluidRow(
                    column(width = 8,
                           tags$b("Top 10 manufacturers"),
                           highchartOutput("manufacturer"))
                    
                    # column(width=6,
                    #        tags$b("Therapeutic Class"),
                    #        highchartOutput('therapeutic_class'))
                  )
                )
        )
    ),
    tabItem(
      tabName='explore',
      fluidRow(
        valueBoxOutput("total_crp")
      ),
      fluidRow(
        tags$h3('List of Notice of Compliance with the selected ingredient'),
        DT::dataTableOutput('noc_output')
      ),
      fluidRow(
        tags$h3('Information about innovator products'),
        DT::dataTableOutput('innovator_output')
      )
     ),
    tabItem(
      tabName="intro",
      box(
        width=12,status='info',
        h2("About"),
        tags$p("A Notice of Compliance is a notification, issued pursuant to paragraph C.08.004(1)(a), indicating that a manufacturer has complied with sections C.08.002 or C.08.003 and C.08.005.1 of the Food and Drug Regulations. 
                 Notices of Compliance are issued to a manufacturer following the satisfactory review of a submission."),
        tags$p("This Dashboard enables the user to visualize some of the information from NOC database")
                
      ),
      tags$strong("Authors:"),
      fluidRow(
      box(
        "Daniel Buijs, MSc", br(),
        "Data Science Manager", br(),
        "Data Science Unit / Business Informatics / Resource Management and Operations Directorate / Health Porducts and Food Branch / Health Canada", br(),
        "daniel.buijs@canada.ca",
        width = 3,status='warning'
      ),
      box(
        "Nanqing Zhu, MSc", br(),
        "Data Scientist", br(),
        "Data Science Unit / Business Informatics / Resource Management and Operations Directorate / Health Porducts and Food Branch / Health Canada", br(),
        "nanqing.zhu@canada.ca",
        width = 3,status='warning'
      )
    )
    )
    )
  )
)