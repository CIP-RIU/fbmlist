#' UI Side for Creation Material List from Scratch
#' 
#' Returns user friendly ui
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title diaply title name
#' @param name UI TabName
#' @export
#' 

createlist_ui <- function(type = "tab", title = "Family List", name = "createList"){
  
  shinydashboard::tabItem(tabName = name, 
                          h2(title),   
                          
                          shinyjs::useShinyjs(),
                          shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
                          
                          fluidRow(
                            box(
                              title = " ", width = 12, status = "primary", height = "250px",
                              #p("Seleccione un cultivo y una base de datos"),
                               fluidRow(
                                column(6, selectizeInput(inputId = "fbmlist_sel_crop_new", label = "Select crop", width="100%",
                                                         choices = c("potato","sweetpotato"),
                                                         options = list(
                                                           placeholder = 'Please select the crop',
                                                           onInitialize = I('function() { this.setValue(""); }')
                                                         )
                                )#,

                                ),
                                
                                column(6, selectizeInput("fbmlist_sel_type_new", "Select database", width="100%", selected = 2,
                                                         choices = c("Institutional"))
                                )
                              ),
                              
                              fluidRow(
                                #column(6, selectizeInput("fbmlist_sel_list", "Select data base", width="100%",
                                #                         choices = db_files_choices )),
                                column(6, uiOutput("sel_list_new_btn")),
                                column(6, shiny::actionButton("fbmlist_connect_new", "Connect", icon("fa fa-database"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4", width = 150))
                              ),
                              
                              tags$style(type='text/css', "#fbmlist_sel_list_new { width:100%; margin-top: 25px;}"),
                              tags$style(type='text/css', "#fbmlist_connect_new  { width:100%; margin-top: 25px;}")
                              
                            )#,
                     
                          ),

                     #      ---------------------------------
                          conditionalPanel( condition = "output.show_mtable_new",  ##conditional Panel
                                            
                                            fluidRow(
                                              box(
                                                #"Ingrese una lista de familias o clones", width = 4, status = "primary", height = "730px",
                                                title = "Paste a list of families", width = 4, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                                br(),
                                                br(),
                                                tags$textarea(id="fbmlist_txtarea_new", rows=30, cols=31, ""),
                                                
                                                shiny::wellPanel(
                                                  shiny::HTML("<b>Observations </b>"),
                                                  shiny::textOutput("fbmlist_foundclones_new")
                                                ),
                                                
                                                
                                                br(),
                                                br(),
                                                br()
                                                #actionButton("fbmlist_search", "Search", icon("fa fa-search"), style="color: #fff; background-color: #51a351; border-color: #51a351", width = 150)
                                                
                                              ),
                                              box(id= "search-results",
                                                #"Resultados de busqueda", width = 8, status = "primary", height = "730px",
                                                title = "Search Results", width = 8, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                                br(),
                                                br(),
                                                div(dataTableOutput("fbmlist_table_new"), style = "font-size:85%"),
                                                #DT::dataTableOutput('fbmlist_table'),
                                                br(),
                                                actionButton("fbmlist_select_new", "Select marked", icon("fa fa-arrow-down"), style="color: #fff; background-color: #51a351; border-color: #51a351", width = 150),
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
                          ),##fin conditional Panel

                     # Conditional Panel for Connect DB button ---------------------------------
                     
                     div(
                       id = "form",
                          conditionalPanel( condition = "output.show_mtable_new",
                                            
                                            fluidRow(
                                              box(
                                                #"Fill your Material List Information", width = 4, status = "primary", height = "600px",
                                                title = "Fill your Material List Information", width = 4, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                                uiOutput("create_new_name"),
                                                uiOutput("researcher_new_name"),
                                                uiOutput("continent_new_name"),
                                                uiOutput("country_new_name"),
                                                uiOutput("breedercode_new_name"),
                                                shiny::selectInput(inputId = "new_type_trial",label = "Type of procedure", c("Standard","Varietal Selection"), selected = 1),
                                                br()
                                              ),
                                              
                                              div( #begin div
                                                id = "form2",
                                              box(
                                                #width = 8, status = "primary", height = "600px",
                                                title = "Material Selected", width = 8, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                                uiOutput("family_new_list"),
                                                
                                                #DT::dataTableOutput('fbmlist_choosen_table_new'),
                                                #actionButton("plot1_dl", "Save list", icon("fa fa-floppy-o"), style="color: #fff; background-color: #51a351; border-color: #51a351", width = 150)
                                                uiOutput("savelist_new_btn"),
                                                shinysky::shinyalert("alert_fbmlist_new", FALSE, auto.close.after = 4),
                                                #shinyBS::bsAlert("alert_fbmlist_new"),
                                                br()
                                              )#,
                                              ),#fin div
                                              br(),
                                              br(),
                                              br()
                                            ),
                                            
                                            br()#,
                                            
                          )#,
                     ),#end div
                          br(),
                          br(),
                          br()
                          
  )#End data_processing tabItem
  
}




