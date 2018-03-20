#' UI for open books in HiDAP
#' Returns user friendly ui
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title diaply title name
#' @param name UI TabName
#' @export
#' 

managerlist_ui <- function(type = "tab", title = "Manage List", name = "manageList"){
  
  shinydashboard::tabItem(tabName = name,
                          h2(title),   
                          shinyjs::useShinyjs(),
                          shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
                          
                          
                          box(
                            title = " ", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE, width = 12,
                            #tabsetPanel(
                            tabBox(width = 10,
                                   tabPanel("Material List", #begin tabset "CHECK"
                                            
                                            fluidRow(
                                              
                                              column(width = 12, 
                                                     
                                                     br(),
                                                     #HTML('<div style="float: right; margin: 0 5px 5px 10px;">'),
                                                     shiny::actionButton("fbmlist_syncronize", "Synchronize Material Lists", icon("refresh"), 
                                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                     shinysky::shinyalert("alert_fbmlist_syncronize", FALSE, auto.close.after = 300),
                                                     
                                                     
                                                     br(),
                                                     br()
                                                  )

                                            ),
  
                                            fluidRow( 
                                              #
                                              column(width = 12, DT::dataTableOutput('x2'))#,
                                             
                                            ), #end fluidow
                                            
                                            fluidRow(
                                              HTML('<div style="float: right; margin: 0 15px 18px 0px;">'),
                                              shiny::downloadButton(outputId = "fbmlist_Export", label = "Export"),
                                              shiny::actionButton(inputId = "fbmlist_fileDelete" ,label = "Delete", icon("file"),
                                                                  style="color: #fff; background-color: #51a351; border-color: #51a351"
                                              ),
                                              shiny::actionButton("fbmlist_refresh", "Refresh", icon("refresh"), 
                                                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                              ),
                                              
                                              
                                              HTML('</div>')
                                            ),
                                            br(),
                                            br()
                                   )#,#end tab Panel "CHECK"
                                   
                            )
                          ),
                          br(),
                          br(),
                          br()
                          
                          
  )#End data_processing tabItem
  
}