#' UI Side for parental of material list
#' 
#' Returns user friendly ui
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title diaply title name
#' @param name UI TabName
#' @export
#' 

parent_ui <- function(type = "tab", title = "Parental List", name = "parentList"){
  
  
  shinydashboard::tabItem(tabName = name, # begin data_processing tabItem
                          h2(title),   
  
                          shinyjs::useShinyjs(), #refresh functions
                          shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"), #refresh functions
 
##### Begin Select Buttons: crop, database list and Connect ####

                          fluidRow( #Buttons for database connection
                            box(
                              title = " ", width = 12, status = "primary", height = "250px",
                              
                              fluidRow(
                                column(6, selectizeInput(inputId = "fbmlist_sel_crop_parent", label = "Select crop", width="100%",
                                                         choices = c("potato","sweetpotato"),
                                                         options = list(
                                                           placeholder = 'Please select the crop',
                                                           onInitialize = I('function() { this.setValue(""); }')
                                                         )
                                         )#,
                                ),
                                
                               column(6, selectizeInput("fbmlist_sel_type_parent", "Select database", width="100%", selected = 2,
                                                         choices = c("Institutional","Local")))
                              ),
                              
                              fluidRow(
                                column(6, uiOutput("sel_list_parent_btn")),
                                column(6, actionButton("fbmlist_connect_parent", "Connect", icon("fa fa-database"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4", width = 150))
                              ),
                              
                              tags$style(type='text/css', "#fbmlist_sel_list_parent { width:100%; margin-top: 25px;}"),
                              tags$style(type='text/css', "#fbmlist_connect_parent  { width:100%; margin-top: 25px;}")
                              
                            )#,
                            
                          ) , #End of #Buttons for database connection
#### End Select Buttons: crop, database list and Connect
                          
                              
                          

#### Begin conditional Panel  input.fbmlist_sel_type_parent=='Institutional' #####
    conditionalPanel( condition = "input.fbmlist_sel_type_parent=='Institutional'",                          

                      
# Conditional Panel for search and selection of parentals  ---------------------------------


                      #conditionalPanel( condition = "input.fbmlist_sel_type_parent=='Institutional'",
                        conditionalPanel( condition = "output.show_mtable_parent",  ##conditional Panel
                                            fluidRow(
                                              box(
                                                #"Ingrese una lista de familias o clones", width = 4, status = "primary", height = "730px",
                                                title = "Paste a list of clones", width = 4, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                                br(),
                                                br(),
                                                
                                                #tags$textarea(id="fbmlist_txtarea_parent", rows=30, cols=31, "")#,
                                                
                                                conditionalPanel( condition = "input.fbmlist_sex_parent=='Female'",
                                                
                                                  tags$textarea(id="fbmlist_txtarea_parent_fem", rows=30, cols=31, "")#,
                                                ),
                                                
                                                conditionalPanel( condition = "input.fbmlist_sex_parent=='Male'",
                                                  tags$textarea(id="fbmlist_txtarea_parent_male", rows=30, cols=31, "")#,
                                                ),
                                                
                                                
                                                shiny::wellPanel(
                                                  shiny::HTML("<b>Observations </b>"),
                                                  shiny::textOutput("fbmlist_foundclones_parent")
                                                ),
                                                
                                                br(),
                                                br(),
                                                br()
                                                #actionButton("fbmlist_search", "Search", icon("fa fa-search"), style="color: #fff; background-color: #51a351; border-color: #51a351", width = 150)
                                                
                                              ),
                                              box(
                                                #"Resultados de busqueda", width = 8, status = "primary", height = "730px",
                                                title = "Search Results", width = 8, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                                br(),
                                                br(),
                                                
                                                conditionalPanel( condition = "input.fbmlist_sex_parent=='Female'",
                                                                  div(dataTableOutput("fbmlist_table_parent_fem"), style = "font-size:85%")#,
                                                ),
                                                #div(dataTableOutput("fbmlist_table_parent"), style = "font-size:85%"),
                                                conditionalPanel( condition = "input.fbmlist_sex_parent=='Male'",
                                                                  div(dataTableOutput("fbmlist_table_parent_male"), style = "font-size:85%")#,
                                                ),
                                                
                                                
                                                
                                                br(),
                                                
                                                actionButton("fbmlist_select_parent", "Select marked", icon("fa-a farrow-down"), style="color: #fff; background-color: #51a351; border-color: #51a351"),
                                                shiny::radioButtons("fbmlist_sex_parent",label = "Sex of the parent", choices = c("Female", "Male"), selected = "Female", inline = TRUE),
                                                
                                                br(),
                                                br(),
                                                br()
                                              ),
                                              br(),
                                              br(),
                                              br()
                                            ),
                                            br(),
                                            br()
                          ), ##fin conditional Panel
                          
                                                    
                        # Conditional Panel for Select and Save button ---------------------------------
                        div( #begin div
                          id = "form-parent",
                          conditionalPanel( condition = "output.show_mtable_parent",
                                            
                                            fluidRow(
                                              box(
                                                #"Fill your Material List Information", width = 4, status = "primary", height = "600px",
                                                title = "Fill your parental list information", width = 4, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                                br(),
                                                br(),
                                                uiOutput("create_parent_name")#,
                                                #shiny::selectInput(inputId = "parent_type_trial",label = "Type of procedure", c("Standard","PVS"), selected = 1 )
                                                
                                                #textInput("text", label = h3("Text input"), value = "Enter text..."),
                                                #textInput("text", label = h3("Text input"), value = "Enter text...")
                                                
                                              ),
                                              box(
                                                #width = 8, status = "primary", height = "600px",
                                                title = "Parentals Selected",  width = 8, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                                br(),
                                                h2("Females"),
                                                DT::dataTableOutput('fbmlist_choosen_table_parent_fem'),
                                                
                                                h2("Males"),
                                                DT::dataTableOutput('fbmlist_choosen_table_parent_male'),
                                                
                                                #actionButton("plot1_dl", "Save list", icon("fa fa-floppy-o"), style="color: #fff; background-color: #51a351; border-color: #51a351", width = 150)
                                                uiOutput("savelist_parent_btn"),
                                                #shinyBS::bsAlert("alert_fbmlist_on"),
                                                shinysky::shinyalert("alert_fbmlist_parent", FALSE, auto.close.after = 4),
                                                
                                                br()
                                              ),
                                              br(),
                                              br(),
                                              br()
                                            ),
                                            
                                            br()#,
                                            
                          )#,
                        )#,#end div


), #End conditional Panel  input.fbmlist_sel_type_parent=='Institutional'


###### Begin sel_type_parent == local ##############
conditionalPanel( condition = "input.fbmlist_sel_type_parent=='Local'",                          
                  
                  fluidRow(
                    box(
                      
                      title = "Parent Table", width = 12, status = "success", solidHeader = TRUE, collapsible = TRUE,
                      br(),
                      br(),
                      #Display table when user select "Local" database
                      dataTableOutput("fbmlist_table_parent_local")
                      ),
                  box(width = 12, 
                      fluidRow(
                        box(
                          title = "Female Parent Information", width = 6, status = "success", solidHeader = TRUE, collapsible = TRUE,
                          dataTableOutput("fbmlist_table_female_local")
                          ),
                        box(
                          title = "Male Parent Information", width = 6, status = "success", solidHeader = TRUE, collapsible = TRUE,
                          dataTableOutput("fbmlist_table_male_local")
                          )
                      )
                  )
                  )

),
#######End sel_type_parent == "local" #

                        br(),
                        br(),
                        br()        

                          
  )
  
 

  
  
}