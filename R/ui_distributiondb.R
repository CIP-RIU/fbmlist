#' UI for Distribution DataBase
#' 
#' Returns user friendly ui
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title diaply title name
#' @param name UI TabName
#' @export
#' 

distribution_ui <- function(type = "tab", title = "Distribution Database", name = "distributionDB"){
  
  
  
  shinydashboard::tabItem(tabName = name, # begin data_processing tabItem
                          h2(title),   
                          
                          shinyjs::useShinyjs(),
                          shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
                          
                          fluidRow( #Buttons for database connection
                            box(
                              title = " ", width = 12, status = "primary", height = "250px",
                              #p("Seleccione un cultivo y una base de datos"),
                              
                              fluidRow(
                                column(6, selectizeInput(inputId = "fbmlist_sel_crop_dist", label = "Select crop", width="100%",
                                                         choices = c("potato","sweetpotato"),
                                                         options = list(
                                                           placeholder = 'Please select the crop',
                                                           onInitialize = I('function() { this.setValue(""); }')
                                                         )
                                                         
                                )#,
                                
                                
                                ),
                                
                                column(6, selectizeInput("fbmlist_sel_type_dist", "Select database", width="100%", selected = 2,
                                                         choices = c("Institutional")))
                              ),
                              
                              fluidRow(
                                #column(6, selectizeInput("fbmlist_sel_list", "Select data base", width="100%",
                                #                         choices = db_files_choices )),
                                column(6, uiOutput("sel_list_dist_btn")),
                                column(6, actionButton("fbmlist_connect_dist", "Connect", icon("fa fa-database"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4", width = 150))
                              ),
                              
                              tags$style(type='text/css', "#fbmlist_sel_list_dist { width:100%; margin-top: 25px;}"),
                              tags$style(type='text/css', "#fbmlist_connect_dist  { width:100%; margin-top: 25px;}")
                              
                            )#,
                            
                          ), #End of #Buttons for database connection
                          
                          
                          # Conditional Panel for Selection of material and Search of material ---------------------------------
                          
                          
                          conditionalPanel( condition = "output.show_mtable_dist",  ##conditional Panel
                                            
                                            fluidRow(
                                              
                                              column(8, offset = 3,
                                              box(
                                                #"Ingrese una lista de familias o clones", width = 4, status = "primary", height = "730px",
                                                title = "Paste a list of clones", width = 8, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                                br(),
                                                br(),
                                                tags$textarea(id="fbmlist_txtarea_dist", rows=5, cols=50, ""),

                                                shiny::wellPanel(
                                                  shiny::HTML("<b>Observations </b>"),
                                                  shiny::textOutput("fbmlist_foundclones_dist")
                                                ),

                                                br(),
                                                br()
                                                #actionButton("fbmlist_search", "Search", icon("fa fa-search"), style="color: #fff; background-color: #51a351; border-color: #51a351", width = 150)

                                              )  
                                                
                                              ),
                                              
                                              
                                              box(
                                                #"Resultados de busqueda", width = 8, status = "primary", height = "730px",
                                                title = "Search Results", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                                br(),
                                                br(),
                                                div(dataTableOutput("fbmlist_table_dist"), style = "font-size:85%"),
                                                #DT::dataTableOutput('fbmlist_table'),
                                                br(),
                                                #actionButton("fbmlist_select_dist", "Select marked", icon("fa fa-arrow-down"), style="color: #fff; background-color: #51a351; border-color: #51a351", width = 150),
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
                          
                          # Conditional Panel for Select and Save button ---------------------------------
                          div( #begin div
                            id = "form-dist",
                            conditionalPanel( condition = "output.show_mtable_dist",
                                              
                                              fluidRow(
                                                # box(
                                                #   #"Fill your Material List Information", width = 4, status = "primary", height = "600px",
                                                #   title = "Fill your material list information", width = 4, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                                #   br(),
                                                #   br(),
                                                #   uiOutput("create_dist_name"),
                                                #   shiny::selectInput(inputId = "gen_type_trial_dist",label = "Type of procedure", c("Standard","Varietal Selection"), selected = 1 )
                                                #    
                                                # ),
                                                # box(
                                                #   #width = 8, status = "primary", height = "600px",
                                                #   title = "Material Selected", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                                #   br(),
                                                #   DT::dataTableOutput('fbmlist_choosen_table_dist'),
                                                #   #uiOutput("savelist_dist_btn"),
                                                #   #shinyBS::bsAlert("alert_fbmlist_on"),
                                                #   #shinysky::shinyalert("alert_fbmlist_dist", FALSE, auto.close.after = 4),
                                                #   
                                                #   br()
                                                # ),
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
