#' UI Side for generation of Material List
#' 
#' Returns user friendly ui
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title diaply title name
#' @param name UI TabName
#' @export
#' 
#generateList createList manageList
# db_files_choices <- mtl_files()
# db_files_choices <- db_files_choices$short_name

generate_ui <- function(type = "tab", title = "Clone List", name = "generateList"){
  

  
  shinydashboard::tabItem(tabName = name,
                           h2(title),   
                          
                          shinyjs::useShinyjs(),
                          shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
                          
                          fluidRow(
                            box(
                              title = " ", width = 12, status = "primary", height = "250px",
                              #p("Seleccione un cultivo y una base de datos"),
                              
                              fluidRow(
                                column(6, selectizeInput(inputId = "fbmlist_sel_crop", label = "Select crop", width="100%",
                                                         choices = c("potato","sweetpotato"),
                                                         options = list(
                                                           placeholder = 'Please select the crop',
                                                           onInitialize = I('function() { this.setValue(""); }')
                                                         )
                                                         
                                                         )#,
                                                        
                                       
                                       ),

                                column(6, selectizeInput("fbmlist_sel_type", "Select database", width="100%", selected = 2,
                                                         choices = c("Institutional","Local")))
                              ),
                              
                              fluidRow(
                                #column(6, selectizeInput("fbmlist_sel_list", "Select data base", width="100%",
                                #                         choices = db_files_choices )),
                                column(6, uiOutput("sel_list_on_btn")),
                                column(6, actionButton("fbmlist_connect", "Connect", icon("fa fa-database"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4", width = 150))
                              ),
                              
                              tags$style(type='text/css', "#fbmlist_sel_list { width:100%; margin-top: 25px;}"),
                              tags$style(type='text/css', "#fbmlist_connect  { width:100%; margin-top: 25px;}")
                              
                            )#,
#                             box(
#                               title = "Manage lists", width = 6, status = "primary", height = "250px",
#                               p("Seleccione una o varias listas"),
#                               
#                               fluidRow(
#                                 #column(6, selectizeInput('e5', 'Select list', list("List 1" = 1, "List 2" = 2, "List 3" = 3, "List 4" = 4, "List 5" = 5),
#                                 #                         multiple = TRUE, options = list(maxItems = 3), width = "100%")),
#                                 column(6, uiOutput("sel_genlist_on_btn")),
#                                 column(6, actionButton("fbmlist_selectgenlist", "Select list", icon("fa fa-list"), style="color: #fff; background-color: #51a351; border-color: #51a351", width = 150))
#                               )
#                             )
                          ),
                        

# Conditional Panel for Connect DB button ---------------------------------


                conditionalPanel( condition = "output.show_mtable",  ##conditional Panel

                            fluidRow(
                              box(
                                #"Ingrese una lista de familias o clones", width = 4, status = "primary", height = "730px",
                                title = "Paste a list of clones", width = 4, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                br(),
                                br(),
                                tags$textarea(id="fbmlist_txtarea", rows=30, cols=31, ""),
                                
                                shiny::wellPanel(
                                  shiny::HTML("<b>Observations </b>"),
                                  shiny::textOutput("fbmlist_foundclones_gen")
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
                                div(dataTableOutput("fbmlist_table"), style = "font-size:85%"),
                                #DT::dataTableOutput('fbmlist_table'),
                                br(),
                                actionButton("fbmlist_select", "Select marks", icon("fa fa-arrow-down"), style="color: #fff; background-color: #51a351; border-color: #51a351", width = 150),
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
                id = "form-gen",
               conditionalPanel( condition = "output.show_mtable",
                                 
                          fluidRow(
                            box(
                              #"Fill your Material List Information", width = 4, status = "primary", height = "600px",
                              title = "Fill your material list information", width = 4, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                              br(),
                              br(),
                              uiOutput("create_on_name"),
                              shiny::selectInput(inputId = "gen_type_trial",label = "Type of trial", c("Normal","PVS"), selected = 1 )
                              
                              #textInput("text", label = h3("Text input"), value = "Enter text..."),
                              #textInput("text", label = h3("Text input"), value = "Enter text...")
                              
                            ),
                            box(
                              #width = 8, status = "primary", height = "600px",
                              title = "Material Selected", width = 8, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                              br(),
                              DT::dataTableOutput('fbmlist_choosen_table'),
                              #actionButton("plot1_dl", "Save list", icon("fa fa-floppy-o"), style="color: #fff; background-color: #51a351; border-color: #51a351", width = 150)
                              uiOutput("savelist_on_btn"),
                              #shinyBS::bsAlert("alert_fbmlist_on"),
                              shinysky::shinyalert("alert_fbmlist_on", FALSE, auto.close.after = 4),
                              
                              br()
                            ),
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
