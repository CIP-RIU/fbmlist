#first shinySky
 
library(shinysky)
library(shiny)
#To avoid problems with dplyr and shiny. This package overlap actionButton from Shiny
library(DT)  ##agregar la nueva version de github
library(shinydashboard)
library(DBI) #agregar a hidap2 app
library(RMySQL) #agregar a hidap2 app
library(spsurvey) #agregar a hidap2 app
library(foreign) #agregar a hidap2 app
library(rhandsontable)
library(shinyBS)
library(tools)
library(shinyjs) #agregar a hidap2 app
library(stringr)
library(dplyr)
library(data.table)
library(countrycode) ##agregar al hidap2 app
library(openxlsx)
library(fbglobal)
##test (crear listas usando pedregree book)
##test (crear listas usando passport book)
#test combinar listas pedgree book (n>1)
#test combinar listas passport book (n>1)
#test combinar listas de pedegree y passport book


tabNameS <- "generateList"
tabNameS2 <- "generateList"

server <- function(input, output, session,values) {
  values = shiny::reactiveValues()
  #fbmlist::server_generate(input, output, session, values = values)
  fbmlist::server_managerlist(input, output, session, values = values)
  #fbmlist::server_createlist(input, output, session, values = values)
}

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Germoplasm List"),
                    dashboardSidebar(width = 200,
                                     menuItem("Resources",
                                              sidebarMenu(id = "menu",
                                                          menuSubItem("Management", icon = icon("star"),
                                                                      tabName = tabNameS)
                                              )
                                     )
                    ),
                    dashboardBody(
                      
                      tabItems(
                        
                        #fbmlist::generate_ui(name = tabNameS)#,
                        #fbmlist::createlist_ui(name = tabNameS)
                        fbmlist::managerlist_ui(name = tabNameS)
                        
                      )
                    )
)

shinyApp(ui = ui, server = server)

