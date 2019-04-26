##############################################
# Code author: erin r stearns
# Code objective: us counties geodisparities shiny mock up ui script
# Date: 4.25.2019
#############################################

######################################################################################################
# -------------------------------------- ui -------------------------------------------------------- #
######################################################################################################
ui <- bootstrapPage(
  dashboardPage(
    skin = "green",
    dashboardHeader(title = "US County GeoDisparities"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("About", tabName = "about", icon = icon("leanpub")),
        menuItem("Data explorer", tabName = "indata", icon = icon("database"))
      )
    ),
    
    dashboardBody(
      tabItems(
        #About MoD Geodisparities ----------------------------------------------------
        tabItem(tabName = "about",
                fluidRow(
                  box(includeHTML("app/about.html"), width = 9)
                )

        ), # ------------------------------------------------------- end About tab
        # Data explorer --------------------------------------------------------------
        tabItem(tabName = "indata",
                fluidRow(
                  box(
                    # -- user input options -- 
                    title = "Controls",
                    #select geography
                    # -> source - parent of:
                    selectInput('state', label = "State", choices = c("All",state_names), selected = "All"),
                    #select year
                    # -> source - parent of:
                    sliderInput('year', label = "Year", value = 2007, min = 2007, max = 2017, step=1, sep = "",
                                animate = animationOptions(interval = 750)),
                    #select xvar
                    # -> endpoint - child of: renderUI -- not currently
                    # -> source - parent of:
                    uiOutput('xvar'),
                    #selectInput('xvar',label = 'x Var', choices = var_choices, selected = "blackwhite_ratio"),
                    #select yvar
                    # -> endpoint - child of: renderUI -- not currently
                    # -> source - parent of:
                    uiOutput('yvar'),
                    #selectInput('yvar',label = 'y Var', choices = var_choices, selected = "edu_collegeplus"),
                    #select color var (for map)
                    # -> endpoint - child of: renderUI -- not currently
                    # -> source - parent of:
                    uiOutput('cvar'),
                    #selectInput('cvar',label = 'Color Var', choices = var_choices, selected = "blackwhite_ratio"),
                    
                    #grab ui output
                    uiOutput("ui"),
                    width = 3
                    
                  ),
                  # map
                  # >- endpoint - child of:
                  box(width = 9,status = 'warning',
                      leafletOutput("map",height = 445)
                  )
                  
                ), #end fluidrow 1
                
                fluidRow(
                  # -- bivariate scatter plot -- 
                  box(plotOutput("biscatter", 
                                 brush = brushOpts(id="bibrush")), 
                      width = 6, status = 'warning'),
                  #setting row height
                  #style = 'height:40vh'
                  box(plotOutput("xscatter"),
                      width = 3, status = 'warning'),
                  box(plotOutput("yscatter"), 
                      width = 3, status = 'warning')
                ) #end fluidrow2
                
        ) # -------------------------------------------------------- end Data explorer
        
      ) #close tabitems
    ) #close dashboard body
  ) #close dashboard page
) #close bootstrap page
