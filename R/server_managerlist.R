#' Server openbooks
#' 
#' @param input shinyserver input 
#' @param output shinyserver output
#' @param session shinyserver session
#' @param values reactive values
#' @author Omar Benites
#' @export

server_managerlist <- function(input,output,session, values){
  
  #fb_file_list <- shiny::reactive({
#   tablita <- function(){#
#     
#     #fb_file_list <- list.files(getwd(),full.names = TRUE)
#     fb_file_list <- list.files(getwd(), full.names = TRUE, pattern = ".xlsx")
#     ignore_temps <- grepl(pattern = "~\\$",x = fb_file_list)
#     
#     fb_file_list <- fb_file_list[!ignore_temps]
#     #fb_file_list <- basename(fb_file_list)
#     
#     files <- data.frame(fb_file_list)
#     names(files) <- "Files_Direction"
#     files
#   }#new
  # })
  
  mtl_files_react <- eventReactive(input$fbmlist_refresh,{   
    
    path <- fbglobal::get_base_dir()
    
    dbf_file_list <- list.files(path, full.names = TRUE, pattern = ".rds")
    
    
    lg_dbf <- length(dbf_file_list)
    
    if(lg_dbf == 0){ gmtfiles <- "" }
    if(lg_dbf>0)   { 
      ignore_temps <- grepl(pattern = "~\\$",x =  dbf_file_list)
      dbf_file_list <-  dbf_file_list[!ignore_temps]
      short_name <- basename(dbf_file_list)
      gmtfiles <- data.frame(short_name, dbf_file_list, stringsAsFactors = FALSE)
      names(gmtfiles) <- c("short_name","full_name")
      

      out_list <- c("dspotatotrials_dpassport.rds", "dssweettrials_dpassport.rds", "potato_pedigree.rds", "sweetpotato_pedigree.rds", "table_sites.rds")
      gmtfiles <- dplyr::filter(.data = gmtfiles, !(short_name %in% out_list))

      gmtfiles
    }
    
    mtl_files <- gmtfiles
    mtl_files
    
  })
  
  
  values <- reactiveValues(mtl_data = NULL)
  
  
  shiny::observeEvent(input$fbmlist_syncronize , {
    shiny::withProgress(message = "Syncronizing material list from DB...", detail = 'This may take a while...', value= 0,
                        {
                          
                          try({ #begin of Try
                            #mtl_files_react()
                            incProgress(3/15)
                            fbmlist_data()
                            
                          })# end of Try
                        })
  })
  
  selected_file <- reactive({
    
    index  <- input$x2_rows_selected
    n_pos <- length(index) #You select the last row selected. By default it will storage all positions
    index <- index[n_pos] 
    index <- as.numeric(index)
    
    #table_files <- mtl_files() #from utils.R
    
    table_files <- mtl_files_react() #from react
    selected_file <- table_files[index,"full_name"]
    selected_file <- as.character(selected_file)
    
  })
  
  download_data <- reactive({

    path <-  selected_file()
    downtbl <- readRDS(path)
    downtbl
    
  })

  observe({
   # input$fbmlist_refresh
    values$mtl_data <- mtl_files_react()
  })
  
  
#   output$x2  <-  DT::renderDataTable({
#     
#     input$fbmlist_refresh
#     tablita <- mtl_files()
#     DT::datatable(tablita, selection = c("single"))
#     
#       # }) 
#   })
  
  output$x2  <-  DT::renderDataTable({
      
      #input$fbmlist_refresh
      #tablita <- mtl_files()
      DT::datatable(values$mtl_data, selection = c("single"))
      
        # }) 
    })
  
  output$fbmlist_Export <- downloadHandler(
    
    
    
    filename = function() {
      matlist_filename <- selected_file() %>% basename() %>% tools::file_path_sans_ext()
      #rint(matlist_filename)
      #paste(matlist_filename, '.xlsx', sep='')
      #paste("Material_list", '.xlsx', sep='')
    },
    content = function(file) {
 
      # print(mtl_files_react())
      # print(selected_file())
      # print(download_data())
      # print(fbglobal::get_base_dir())
      
      
      
      tbl_list_data <- download_data()
      
      hs <- openxlsx::createStyle(fontColour = "#000000", fontSize=12,
                        fontName="Calibri", fgFill = "orange")
      
      openxlsx::write.xlsx(tbl_list_data, file, headerStyle = hs, sheetName="Material_List", colWidths="auto")
    }
  )
 
   shiny::observeEvent(input$fbmlist_fileDelete, {
    
    withProgress(message = "Deleting Material List..",value= 0,
                 {
                   index  <- input$x2_rows_selected
                   n_pos <- length(index) #You select the last row selected. By default it will storage all positions
                   index <- index[n_pos] 
                   index <- as.numeric(index)
                   
                   temp <- values$mtl_data[-index, ]
                   values$mtl_data <- temp
                   
                   path <- selected_file()
                   file.remove(path)
                   
                   
                 })
  })
  
  
} 