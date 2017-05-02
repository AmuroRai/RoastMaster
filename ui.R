library(shiny)

shinyUI (
  
  pageWithSidebar(
    headerPanel("Roasting Curve Generator"),
    
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
      
      tabsetPanel(
        tabPanel("Date",uiOutput("Date"),actionLink("alldate","Select/Unselect All Date")),
        tabPanel("Bean",uiOutput("Bean"),actionLink("allbean","Select/Unselect All Bean")),
        tabPanel("Batch",uiOutput("Batch"),actionLink("allbatch","Select/Unselect All Batch")),
        tabPanel("Color Chart",uiOutput("ColorChart"),actionLink("allcolor","Select/Unselect All Color"))
      ),
      
      br(),br(),
      actionButton("view", "View Original File",width='100%'),
      br(),br(),
      actionButton("go", "Choose Data",width='100%'),
      br(),br(),
      actionButton("confirm","Confirm and Plot",width='100%'),
      br(),br(),
      actionButton("reset","Reset",width='100%'),
#For R-portable only
#      br(),br(),
#      actionButton("quit","Quit",width='100%'),
      width=3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("View Original Data",tableOutput("origin")),
        tabPanel("Chosen Data",tableOutput("d")),
        tabPanel("Plot", plotOutput("plot"))
      )
    )
    
  )
  
)
