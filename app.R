# Load the required packages
library(bs4Dash)
library(shiny)
library(dplyr)
library(highcharter)

# load the dataset
project_dataset<-readxl::read_xlsx("./project_details_dataset.xlsx")

# Start the shiny app
shinyApp(
  
  
  ui = dashboardPage( 
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    # ),
    #includeCSS("www/style.css"),
    # Start of the UI
    title = "Basic Dashboard",
    fullscreen = TRUE,
    header = dashboardHeader(
      title = dashboardBrand(
        title = tags$strong("Projects Dashboard"),
        color = "navy"
        #image = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg",
      ),
      skin = "light",
      status = "white",
      border = TRUE,
      sidebarIcon = icon("bars"),
      controlbarIcon = icon("th"),
      fixed = FALSE,
      
      rightUi = dropdownMenu(
        badgeStatus = "danger",
        type = "messages",
        messageItem(
          inputId = "triggerAction1",
          message = "message 1",
          from = "Ramesh Konaru",
          image = "image.jpg",
          time = "today",
          color = "lime"
        )
      )
    ),
    sidebar = dashboardSidebar(
      skin = "light",
      status = "primary",
      elevation = 4,
      width = 5,
      sidebarUserPanel(
        image = "image.jpg",
        name = "Welcome to Akond"
      ),
      sidebarMenu(id="accdemic_sidebar",
                  menuItem(
                    tabName = "year_2023",
                    text = "Academic year 2023",
                    icon = icon("calendar"),
                    selected = TRUE
                  ),
                  menuItem(
                    text = "Academic Projects",
                    tabName = "second_menu",
               
                    icon = icon("bars")
                    
                  ),
                  menuItem(
                    text = "Profile",
                    tabName = "profile_name",
                    icon = icon("person")
                  )
                  )
      
    ),
    
    
    body = dashboardBody(
      tabItems(
        tabItem(
          tabName = "year_2023",
        uiOutput("projects_ui")
        ),
        tabItem(
          tabName = "second_menu",
          fluidRow(column(width = 2,
          selectInput("select_project","Select Year",choices = c("2023","2022","2021","2020"))),
          column(width = 2,
                 selectizeInput("select_branch", "Select Branch",
                                c("ECE","CSE","DATA SCIENCE","IT")),
                 
                 
                 #selectizeInput("select_branch","Select Branch",choices = C("ECE","EEE","IT","CSE"))
                 )
          ),
          
        fluidRow(
          column(width = 6,
                 box(
                   width = 12,
                   title = "Line Graph",
                   highchartOutput("bar_plot",width = "80%",height = "300px"),
                collapsible = FALSE
                 )),
          
          column(width = 6,
                 
                 box(width = 12,
                     title = "Scatter Plot",
                     collapsible = FALSE,
                     highchartOutput("scatter_plot",width = "80%",height = "300px")
                     )
                 )
        )  
        ),
        tabItem(
          tabName = "profile_name",
          fluidRow(
            column(width = 6,
          userBox(width = 12,
            title = userDescription(
              title = "Nirvaan Simha",
              subtitle = "Senior Software Engineer",
              image = "image.jpg",
              backgroundImage = "https://cdn.statically.io/img/wallpaperaccess.com/full/1119564.jpg",
            ),
            status = "olive",
            closable = TRUE,
            maximizable = TRUE,
            "Academic Year 2016-2018",
            footer = " Project management is the process of leading the wos within the given constraints. The primary constraints are scope, time, and budget.[2] Thjectives."
          )),
          column(width = 6,
                 socialBox(width = 12,
                   title = userBlock(
                     image = "https://adminlte.io/themes/AdminLTE/dist/img/user4-128x128.jpg",
                     title = "Social Box",
                     subtitle = "example-01.05.2018"
                   ),
                   "Some text here!",
                   attachmentBlock(
                     image = "https://adminlte.io/themes/v3/dist/img/user1-128x128.jpg",
                     title = "Test",
                     href = "https://google.com",
                     "This is the content"
                   ),
                   lapply(X = 1:10, FUN = function(i) {
                     boxComment(
                       image = "image.jpg",
                       title = paste("Comment", i),
                       date = "01.05.2018",
                       paste0("The ", i, "-th comment")
                     )
                   }),
                   footer = "The footer here!"
                 )
                 )
        )
      ))
    )
  ),  # End of the UI
  server = function(input, output) {
    
    
    # vals<-reactiveValues()
  
    
    output$projects_ui<-renderUI({
      fluidRow(
        
        lapply(project_dataset$Project_Name, function(i) {
          
          input_dataset <<- project_dataset %>% filter(grepl(paste0("^",i,"$"), Project_Name))
          
          box(
            title = 
              
          
              fluidRow(
                column(width = 9,
                              tags$strong("Project Name : "),
                       (input_dataset$Project_Name)
              ),
              tags$hr(),
              column(width = 9,
                     tags$strong("Domain : "),
                     (input_dataset$domain)),
              tags$hr(),
              column(width = 9,
                     tags$strong("Guide : "),
                     (input_dataset$project_guide)

              )),
            
            
            width = 4,
            collapsible = FALSE,
            actionButton(paste0("view_details", i), "More Details",style="color: #fff; background-color: #337ab7")
           
          )
        })  
      )
    }) #end of the render ui
    

        
        
        observeEvent(project_dataset$Project_Name, {
      
      lapply(project_dataset$Project_Name, function(i) {
        
        input_dataset_more <- project_dataset %>% filter(grepl(paste0("^",i,"$"), Project_Name))
        print(input_dataset_more)
       # browser()
        observeEvent(input[[paste0("view_details", i)]], {
    showModal(
      modalDialog(
        size = "l",
        easyClose = TRUE,
        footer = tagList(bs4Dash::actionButton("popupclose_button", "Close")),
        fluidPage(
          includeCSS("www/style.css"),
          fluidRow(
            column(width = 5,
                   tags$strong("Objective")
                   ),
            column(width = 2,
                   (":")
                   ),
            column(
              width = 5,
              input_dataset_more$Object
            )
          ),
          tags$hr(),
          fluidRow(
            column(width = 5,
                   tags$strong("Team Size")),
            column(width = 2,(":")),
            column(width = 5,input_dataset_more$Team_size)
          ),
          tags$hr(),
          fluidRow(
            column(width = 5,
                   tags$strong("Team Members")),
            column(width = 2,(":")),
            column(width = 5,input_dataset_more$team_members)
          ),
          tags$hr(),
          fluidRow(
            column(width = 5,
                   tags$strong("Contact Mails")),
            column(width = 2,(":")),
            column(width = 5,input_dataset_more$contact_mail)
          ),
          tags$hr(),
          fluidRow(
            column(width = 5,
                   tags$strong("Project Duration")),
            column(width = 2,(":")),
            column(width = 5,input_dataset_more$project_duration)
          )
        )
      )
    )  
    })
        })
      })
        
  observeEvent(input$popupclose_button,{
    removeModal()
  })      
    
  output$bar_plot<-renderHighchart({
    pokemon%>%
      count(type_1)%>%
      arrange(n)%>%
      hchart(type = "bar", hcaes(x = type_1, y = n))
  })
  output$scatter_plot<-renderHighchart({
    highchart()%>%
      hc_add_series(pokemon, "scatter", hcaes(x = height, y = weight))
  })
  
  } # End of the Server Function
)
# End of the Shiny App 