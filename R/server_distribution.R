#' Server side for distribution database
#' 
#' @param input shinyserver input 
#' @param output shinyserver output
#' @param session shinyserver session
#' @param values reactive values
#' @author Omar Benites
#' @export

server_distribution <- function(input,output,session, values){
  
  #Reactive data after connecting to database or local lists  
  gmtl_data <- shiny::eventReactive(input$fbmlist_connect_dist, {
    
    
    dbf_file <- input$fbmlist_sel_list_dist
    n <- length(input$fbmlist_sel_list_dist)
    
    if(n==1){
      
      
      #path <- Sys.getenv("LOCALAPPDATA")
      path <- fbglobal::get_base_dir()
      path <- file.path(path,dbf_file)
      
      germlist_db <- readRDS(path)
      
      
    }
    #germlist_db <- foreign::read.dbf(file = dbf_file, as.is = TRUE)
    if(n > 1){
      combine <- list() 
      for(i in 1:n){  
        
        path <- fbglobal::get_base_dir()
        path <- paste(path, dbf_file,sep = "\\")
        combine[[i]] <- readRDS(file = dbf_file[i])
        #combine[[i]] <- readRDS(file = dbf_file[i]) 
      } 
      join_books <- data.table::rbindlist(combine,fill = TRUE)
      join_books <- as.data.frame(join_books)
      germlist_db <- join_books
    }
    
    n_row <- nrow(germlist_db)
    germlist_db <-  dplyr::mutate(germlist_db, IDX = 1:n_row)
    names(germlist_db) <- c("Accession_Number", "Male", "Female", "Distribution_status", "Health_status", "Accession_storage", "Distribution_Number",
                                 "Year", "Request", "Form_of_Distribution", "Crop", "Consignee", "Institute_Center", "Country", "Order_status", "DistributionData", "IDX") 
    
    germlist_db <-  germlist_db
    
  }) 
  
  
  #reactive value for displaying box and panels
  output$show_mtable_dist <- shiny::reactive({
    return(!is.null(gmtl_data()))
  })
  
  
  #reactive value for save button
  output$show_save_dist <- shiny::reactive({
    return(length(input$fbmlist_select_dist[1]))
  })
  
  
  #set options for show_mtable
  outputOptions(output, 'show_mtable_dist', suspendWhenHidden=FALSE)
  #outputOptions(output, 'show_save', suspendWhenHidden=FALSE)
  
  
  #selectInput button for selection of local lists or databases
  output$sel_list_dist_btn <- shiny::renderUI({
   
    crop <- input$fbmlist_sel_crop_dist
    type_db <- input$fbmlist_sel_type_dist
    mtl_db_sel <- mtl_files()$short_name
    
    if(crop == "") {
      
      db_files_choices  <-  "" 
      sel_multiple <- FALSE 
      
    }   
    
    if(crop == "potato") { 
      
      if(type_db=="Institutional"){ 
        
        #db_files_choices <- list("dspotatotrials_dpassport.dbf", "potato_pedigree.dbf")
        db_files_choices <- list("potato_db_distribution.rds")
        sel_multiple <- FALSE                         
      }
      if(type_db=="Local"){ 
        
        db_files_choices <- mtl_db_sel[str_detect(mtl_db_sel , "PT")] 
        sel_multiple <- TRUE
      }
    }
    
    if(crop == "sweetpotato") {
      
      if(type_db=="Institutional") {     
        db_files_choices <- list("dssweettrials_dpassport.rds", "sweetpotato_pedigree.rds")
        sel_multiple <- FALSE   
      }
      if(type_db=="Local"){ 
        db_files_choices <- mtl_db_sel[str_detect(mtl_db_sel , "SP")]
        sel_multiple <- TRUE 
      }
    }
    
    db_files_choices <- db_files_choices
    
    shiny::selectizeInput(inputId ="fbmlist_sel_list_dist", label = "Select list", 
                          multiple =  sel_multiple, width="100%", choices = db_files_choices,
                          options = list(
                            placeholder = 'Please select an option below',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
  })
  
  
  #TextInput space to write list's name
  output$create_dist_name <- renderUI({
    
    req(input$fbmlist_select_dist)
    textInput("fbmlist_create_dist_name", label = h3("New list name"), value = "", placeholder = "Write a list name")
  })
  
  
  #save button to store information
  output$savelist_dist_btn <- renderUI({
    
    req(input$fbmlist_select_dist)
    #shiny::actionButton("fbmlist_save", label = "Save List", icon = icon("save"))
    shinysky::actionButton2("fbmlist_save_dist", label = "Save list", icon = "save", icon.library = "bootstrap")
  })
  
  
  ##########################################
  
  output$sel_country_dist <- renderUI({
  
    country_choices <- dplyr::select(gmtl_data(), Country)[[1]] %>% unique() %>% sort()
    
    shiny::selectizeInput(inputId ="fbmlist_sel_country_dist", label = "Select country", 
                          multiple =  FALSE, width=NULL, choices = country_choices,
                          options = list(
                            placeholder = 'Please select country',
                            onInitialize = I('function() { this.setValue(""); }')
                          ))
  
  })
  
  
  output$sel_year_dist <- renderUI({
    
    year_choices <- dplyr::select(gmtl_data(), Year)[[1]] %>% unique() %>% sort()
    
    shiny::selectizeInput(inputId ="fbmlist_sel_year_dist", label = "Select year", 
                          multiple =  FALSE, width=NULL, choices =  year_choices,
                          options = list(
                            placeholder = 'Please select year',
                            onInitialize = I('function() { this.setValue(""); }')
                          ))
    
  })
  
  
  output$sel_request_dist <- renderUI({
    
    req(input$fbmlist_sel_country_dist)
    req(input$fbmlist_sel_year_dist)
    
    country <- input$fbmlist_sel_country_dist
    year <- as.numeric(input$fbmlist_sel_year_dist)
    
    
    # print(year)
    # print(gmtl_data())
    request_choices <- dplyr::filter(gmtl_data(), Country == country, Year == year ) #%>% dplyr::select(., Request)[[1]] %>% unique() %>% sort()
     #print(request_choices)
    # 
    request_choices <-  dplyr::select(request_choices, Request)[[1]] %>% unique() %>% sort()
    #print(request_choices)
    
    shiny::selectizeInput(inputId ="fbmlist_sel_request_dist", label = "Select request", 
                          multiple =  FALSE, width=NULL, choices = request_choices,
                          options = list(
                            placeholder = 'Please select request',
                            onInitialize = I('function() { this.setValue(""); }')
                          ))
    
  })
  
  
  #############################################
  
  #Clones founded using textArea
  output$fbmlist_foundclones_dist <- renderText({
    
    mtl_table <- gmtl_data()
    # mtl_headers <- c("Accession_Number", "Female_AcceNumb", "Female_codename", "Male_AcceNumb", 
    #                  "Male_codename", "Population", "Cycle", "Date_Created", "IDX") 
    # 
    mtl_table <- mtl_table[,1:6]
    temp_mtl_table <- mtl_table
    
    if(input$fbmlist_txtarea_dist!=""  || !str_detect(input$fbmlist_txtarea_dist, "[[:space:]]") ){
      
      #trimming search filter
      search_filter <- str_split(input$fbmlist_txtarea_dist,"\\n")[[1]]
      search_filter <- stringr::str_trim(search_filter,side = "both")
      search_filter <- as.character(search_filter)
      
      #extracting columns Accesion Number and Accesion Name
      material_db_accnum <- as.character(temp_mtl_table$Accession_Number)
      material_db_accname <- as.character(temp_mtl_table$Accession_Name) #in case of generate (clone list), it has Accession Name
      
      material_acc_union <- union(material_db_accnum, material_db_accname)
      
      out_dbacc_search <- setdiff(search_filter, material_acc_union) #find the element which are NOT in the inserction
      out_dbacc_search <- out_dbacc_search[!is.na(out_dbacc_search)]
      out_dbacc_search <- out_dbacc_search[out_dbacc_search!=""]
      n_search <- length(out_dbacc_search)
      print(n_search)
      
      # Show messages according to accesion founder in accesion number or accesion name
      
      if(n_search>0){ #for accession number, flag =1
        out <- paste(out_dbacc_search, collapse = ", ")
        out <- paste("N= ", n_search, " accession(s) were not found: ", out, sep="")
      } else {
        out <- paste("", sep = "")
      }
      out
      
    }
    
    
  })
  
  
  # Selection on generataed material list button ----------------------------------------------------
  output$fbmlist_table_dist  <-  DT::renderDataTable({
    
    shiny::req(input$fbmlist_sel_list_dist)
    shiny::req(input$fbmlist_connect_dist)
    #shiny::req(input$fbmlist_selectgenlist)
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                          
                          
                          row_click <- NULL
                          
                          mtl_table <- gmtl_data()
                          
                          n_row <- nrow(mtl_table)
                          mtl_table <-  mutate(mtl_table)
                          
                          #print(names(mtl_table))
                          
                          
                          mtl_table <- dplyr::select_(mtl_table, "Accession_Number", "Male", "Female", "Country", "Year", "Request", "Distribution_status", 
                                                      "Health_status", "Accession_storage", "Distribution_Number",
                                                      "Form_of_Distribution", "Crop", "Consignee", "Institute_Center",  "Order_status", "DistributionData", "IDX")
                          

                          if(input$fbmlist_txtarea_dist!=""){
                            
                            #Deprecated IDX 
                            #mtl_table <-  mutate(mtl_table, IDX = 1:n())
                            #End deprecated IDX
                            
                            
                            search_filter <- str_split(input$fbmlist_txtarea_dist,"\\n")[[1]]
                            search_filter <- stringr::str_trim(search_filter, side = "both")
                            
                            mtl_table_f <- filter(mtl_table, Accession_Number %in% search_filter)
                            
                            
                            # Additional Code in case of search in others columns
                            
                            # if(nrow(mtl_table_f)==0 &&  is.element("Year",names(mtl_table_f))) {
                            #   
                            #   mtl_table_f <- dplyr::filter(mtl_table, Year %in% search_filter)
                            #   
                            # }
                            # 
                            # if(nrow(mtl_table_f)==0 &&  is.element("Request",names(mtl_table_f))) {
                            #   
                            #   mtl_table_f <- dplyr::filter(mtl_table, Request %in% search_filter)
                            #   
                            # }
                            # 
                            #  if(nrow(mtl_table_f)==0 &&  is.element("Consignee",names(mtl_table_f))) {
                            # 
                            #     mtl_table_f <- dplyr::filter(mtl_table, Consignee %in% search_filter)
                            # 
                            #  }
                            # 
                            #   
                            # if(nrow(mtl_table_f)==0 &&  is.element("Country",names(mtl_table_f))) {
                            #   
                            #   mtl_table_f <- dplyr::filter(mtl_table, Country %in% search_filter)
                            #   
                            # }
                            # 
                          
                            
                            # SEARCH ACCESSION BY DIFFERENTE PEDRIGREE ATRIBUTES (Temporary disable)   
                            
                            #   if(nrow(mtl_table_f)==0 &&  is.element("Accession_Code",names(mtl_table_f))) {
                            #     
                            #     mtl_table_f <- dplyr::filter(mtl_table, Accession_Code %in% search_filter)
                            # 
                            #   }
                            # 
                            #   if(nrow(mtl_table_f)==0 &&  is.element("Female_AcceNumb",names(mtl_table_f))) {
                            #     
                            #     mtl_table_f <- dplyr::filter(mtl_table, Female_AcceNumb %in% search_filter)
                            # 
                            #   } 
                            #   
                            #   if(nrow(mtl_table_f)==0 &&  is.element("Female_codename",names(mtl_table_f))) {
                            #     
                            #     mtl_table_f <- dplyr::filter(mtl_table, Female_codename %in% search_filter)
                            # 
                            #   }  
                            #   
                            #   if(nrow(mtl_table_f)==0 &&  is.element("Male_AcceNumb",names(mtl_table_f))) {
                            #     
                            #     mtl_table_f <- dplyr::filter(mtl_table, Male_AcceNumb %in% search_filter)
                            # 
                            #   }    
                            #   
                            #   if(nrow(mtl_table_f)==0 &&  is.element("Male_codename",names(mtl_table_f))) {
                            #     
                            #     mtl_table_f <- dplyr::filter(mtl_table, Male_codename %in% search_filter)
                            # 
                            #   }  
                            #   
                            #   if(nrow(mtl_table_f)==0  &&  is.element("Population",names(mtl_table_f))) {
                            #  
                            #    mtl_table_f <- dplyr::filter(mtl_table, Population %in% search_filter)
                            # 
                            # }
                            #   
                            # if(nrow(mtl_table_f)==0  &&  is.element("Cycle",names(mtl_table_f))) {
                            #     
                            #     mtl_table_f <- dplyr::filter(mtl_table, Cycle %in% search_filter)
                            # 
                            # }  
                            # 
                            
                            # END SEARCH ACCESSION BY DIFFERENTE PEDRIGREE ATRIBUTES (Temporary disable)       
                            
                            if(nrow(mtl_table_f)>0){ 
                              row_click <- as.numeric(mtl_table_f$IDX)
                              Search <- rownames(mtl_table_f) %>% as.numeric(.)
                            } 
                            
                            
                            DT::datatable( mtl_table_f, rownames = FALSE, 
                                           extensions = 'FixedColumns',
                                           options = list(scrollX = TRUE, scroller = TRUE, pageLength = 8, fixedColumns = list(leftColumns = 5)),
                                           #selection = list( mode= "multiple",  selected =  rownames(mtl_table)), 
                                           selection = list( mode = "multiple", selected = Search), 
                                           filter = 'bottom'#,
                                           # extensions = 'Buttons', options = list(
                                           #   dom = 'Bfrtip',
                                           #   buttons = 
                                           #     list(list(
                                           #       extend = 'collection',
                                           #       buttons = c('csv', 'excel'),
                                           #       text = 'Download'
                                           #     ))
                                           #   
                                           # )
                            )
                            
                          } 
                          
                          else if(input$fbmlist_sel_country_dist!="" && input$fbmlist_sel_year_dist!="" ){
                                    
                           country <-  input$fbmlist_sel_country_dist
                           year <- as.numeric(input$fbmlist_sel_year_dist)
                           request <- input$fbmlist_sel_request_dist
                            
                           if(request!=""){
                           
                            mtl_table_f <- filter(mtl_table, Country == country, Year== year, Request == request)
                            
                           } else {
                             
                             mtl_table_f <- filter(mtl_table, Country == country, Year== year)
                             
                           }
                           
                              mtl_table_f <- mtl_table_f
                              
                              
                              DT::datatable( mtl_table_f, rownames = FALSE, 
                                             extensions = 'FixedColumns',
                                             options = list(scrollX = TRUE, scroller = TRUE, pageLength = 8, fixedColumns = list(leftColumns = 5)),
                                             #selection = list( mode= "multiple",  selected =  rownames(mtl_table)), 
                                             #selection = list( mode = "multiple", selected = Search), 
                                             filter = 'bottom'#,
                                             #options = list(searching = FALSE,  pageLength = 5, scrollX = TRUE, scroller = TRUE,   fixedColumns = list(leftColumns = 4) )  
                                             # extensions = 'Buttons', options = list(
                                             #   dom = 'Bfrtip',
                                             #   buttons = 
                                             #     list(list(
                                             #       extend = 'collection',
                                             #       buttons = c('csv', 'excel'),
                                             #       text = 'Download'
                                             #     ))
                                             #   
                                             # )
                              )
                            
                          } 
                          
                          else {
                            
                            DT::datatable(mtl_table, rownames = FALSE,
                                          extensions = 'FixedColumns',
                                          options = list(scrollX = TRUE, scroller = TRUE, pageLength = 8,  fixedColumns = list(leftColumns = 5)),
                                          #selection = list( mode= "multiple",  selected =  rownames(mtl_table)),
                                          selection = list( mode = "multiple"),
                                          filter = 'bottom'#,
                                          #  extensions = 'Buttons', options = list(
                                          #   dom = 'Bfrtip',
                                          #   buttons =
                                          #     list(list(
                                          #       extend = 'collection',
                                          #       buttons = c('csv', 'excel'),
                                          #       text = 'Download'
                                          #     ))
                                          # 
                                          # )
                            )
                            
                          }
                          
                        }) #end of Progress
    
  }  )
  
  
  # #the index of selection material
  # gmtl_row_index <- eventReactive(input$fbmlist_select_dist,{
  #   
  #   row_click <- NULL
  #   mtl_table <- gmtl_data()
  #   
  #   mtl_table <- mtl_table[,1:6]
  #   
  #   n_row <- nrow(mtl_table)
  #   mtl_table <-  mutate(mtl_table, IDX = 1:n_row)
  #   
  #   print(input$fbmlist_txtarea_dist)
  #   
  #   if(input$fbmlist_txtarea_dist!=""){
  #     
  #     #Deprecated IDX 
  #     #mtl_table <-  mutate(mtl_table, IDX = 1:n())
  #     #End deprecated IDX
  #     
  #     
  #     search_filter <- str_split(input$fbmlist_txtarea_dist,"\\n")[[1]]
  #     search_filter <- stringr::str_trim(search_filter,side = "both")
  #     
  #     mtl_table_f <- filter(mtl_table, Accession_Number %in% search_filter)
  #     #row_click <- as.numeric(rownames(mtl_table_f))
  #     #                               row_click <- dplyr::select(mtl_table_f, IDX)[[1]]
  #     #                               print(row_click)
  #     #print(row_click)
  #     
  #     # if(nrow(mtl_table_f)==0 &&  is.element("Year",names(mtl_table_f))) {
  #     #   
  #     #   mtl_table_f <- dplyr::filter(mtl_table, Year %in% search_filter)
  #     #   #row_click <- as.numeric(rownames(mtl_table_f))
  #     #   # row_click <- dplyr::select(mtl_table_f, IDX)[[1]]
  #     # }
  #     # 
  #     # if(nrow(mtl_table_f)==0 &&  is.element("Request",names(mtl_table_f))) {
  #     #   
  #     #   mtl_table_f <- dplyr::filter(mtl_table, Request %in% search_filter)
  #     #   #row_click <- as.numeric(rownames(mtl_table_f))
  #     #   # row_click <- dplyr::select(mtl_table_f, IDX)[[1]]
  #     # }
  #     # 
  #     # if(nrow(mtl_table_f)==0 &&  is.element("Consignee",names(mtl_table_f))) {
  #     #   
  #     #   mtl_table_f <- dplyr::filter(mtl_table, Consignee %in% search_filter)
  #     #   #row_click <- as.numeric(rownames(mtl_table_f))
  #     #   # row_click <- dplyr::select(mtl_table_f, IDX)[[1]]
  #     # } 
  #     # 
  #     # if(nrow(mtl_table_f)==0 &&  is.element("Country",names(mtl_table_f))) {
  #     #   
  #     #   mtl_table_f <- dplyr::filter(mtl_table, Country %in% search_filter)
  #     #   
  #     # }  
  #     # 
  #     
  #     if(nrow(mtl_table_f)>0){ 
  #       
  #       row_click <- as.numeric(mtl_table_f$IDX)
  #       
  #     } else {
  #       row_click <- NULL
  #     }
  #     
  #   } 
  #   
  #   else {
  #     
  #     row_select <- input$fbmlist_table_dist_rows_selected #comand to get selected values		
  #     #row_filter <- input$fbmlist_table_new_rows_all #comand to get filtered values		    
  #     #row_mtlist_selection <- dplyr::intersect(row_select,row_filter)		     
  #     
  #     row_mtlist_selection <- sort(row_select)
  #     row_click <- row_mtlist_selection
  #   }
  #   
  #   #print(row_select)
  #   print(row_click)
  #   row_click
  #   
  # })
  # 
  # 
  
  
  # #table of selected clones after pressing "Select marked"
  # output$fbmlist_choosen_table_dist  <- DT::renderDataTable({
  #   
  #   #print(input$foo)
  #   
  #   index <- gmtl_row_index()
  #   mtl_table <- gmtl_data()
  #   
  #   mtl_table_temp <- mtl_table #temporal table for visualizing
  #   chosen_gmtl_table <-  mtl_table_temp[index, ]
  #   
  #   
  #   chosen_gmtl_table <- dplyr::select_(chosen_gmtl_table, "Accession_Number", "Male", "Female", "Country", "Year", "Request", "Distribution_status", "Health_status", "Accession_storage", "Distribution_Number",
  #      "Form_of_Distribution", "Crop", "Consignee", "Institute-Center",  "Order_status", "DistributionData", "IDX")
  #   
  #   
  #   chosen_gmtl_table 
  #   
  #   
  #   
  #   
  #   
  #   
  # }, options = list(searching = FALSE,  pageLength = 5, scrollX = TRUE, scroller = TRUE,   fixedColumns = list(leftColumns = 4) ))
  # 
  # 
  
  
  
  # Observers of fbmlist ----------------------------------------------------
  # shiny::observeEvent( input$fbmlist_save_dist, {
  #   
  #   index <- gmtl_row_index()
  #   mtl_table <- gmtl_data()
  #   chosen_gmtl_table <-  mtl_table[index, ]
  #   fbmlist_name_dbf  <- str_trim(string = input$fbmlist_create_dist_name, side = "both")
  #   fbmlist_name_dbf  <- gsub("\\s+", "_", fbmlist_name_dbf)
  #   
  #   fbmlist_name_dbf_temp <- fbmlist_name_dbf  #This variable is for control when user do not type empty names
  #   
  #   #Adding the crop notation 
  #   crop <- input$fbmlist_sel_crop_dist
  #   if(crop=="potato")      { fbmlist_name_dbf <- paste("PT","clon", fbmlist_name_dbf,sep = "_") }
  #   if(crop=="sweetpotato") { fbmlist_name_dbf <- paste("SP","clon", fbmlist_name_dbf,sep = "_") } 
  #   #End of crop notation
  #   
  #   
  #   #All the files names
  #   db_files  <- file_path_sans_ext(mtl_files()$short_name)
  #   
  #   if(fbmlist_name_dbf %in% db_files) {
  #     
  #     shinysky::showshinyalert(session, "alert_fbmlist_dist", paste("WARNING: This list already exists"),
  #                              styleclass = "warning")
  #     
  #   }
  #   else if(fbmlist_name_dbf_temp==""){  #use of the temporary variable to control empty names given by users. in line 261
  #     
  #     shinysky::showshinyalert(session, "alert_fbmlist_dist", paste("WARNING: Please Type a Material List Name"), 
  #                              styleclass = "warning")
  #   } 
  #   else {
  #     
  #     gen_headers <- c("Numeration","Is_control", "Scale_audpc", "Family_AcceNumb", 
  #                      "Cycle"	, "Seed_source", "Simultanious_trials", "Previous_trials")
  #     
  #     if(all(is.element(gen_headers,names(chosen_gmtl_table)))){  
  #       
  #       gen_list_tbl <- chosen_gmtl_table
  #       
  #     } else {  
  #       
  #       chosen_gmtl_table_list <- as.list(chosen_gmtl_table)
  #       extra_parameters <- list(
  #         Numeration = 1:nrow(chosen_gmtl_table),
  #         Is_control	= NA,
  #         Scale_audpc	= NA,
  #         Family_AcceNumb = NA,
  #         Cycle	 = NA,
  #         Seed_source = NA,	
  #         Simultaneous_trials = NA,
  #         list_name= fbmlist_name_dbf,
  #         Previous_trials = NA,
  #         Date_Created = format(Sys.Date(), "%d %m %Y")
  #       )
  #       
  #       gen_list_tbl <- c(chosen_gmtl_table_list, extra_parameters)
  #       gen_list_tbl <- as.data.frame(gen_list_tbl, stringsAsFactors = FALSE) 
  #     }
  #     #foreign::write.dbf(dataframe = chosen_gmtl_table, file = fbmlist_name_dbf, factor2char = FALSE)
  #     gen_list_tbl <- gen_list_tbl
  #     
  #     
  #     if(input$gen_type_trial_dist=="Standard"){ #normal columns by default
  #       gen_list_tbl <- gen_list_tbl
  #     } else { #remove columns Is_Control, "Scale_Audpc"
  #       gen_list_tbl <- dplyr::select(gen_list_tbl, -Is_control, -Scale_audpc)
  #       gen_list_tbl <- as.data.frame(gen_list_tbl)
  #     }
  #     
  #     gen_list_tbl <- gen_list_tbl
  #     
  #     crop <- input$fbmlist_sel_crop_dist
  #     
  #     fbmlist_name_dbf <- paste(fbmlist_name_dbf,".rds",sep = "")
  #     
  #     
  #     ## using fbglobal
  #     path <- fbglobal::get_base_dir()
  #     path <- paste(path,  fbmlist_name_dbf, sep="\\")
  #     
  #     saveRDS(gen_list_tbl, file = path)
  #     
  #     mtl_files()
  #     
  #     shinyjs::reset("fbmlist_sel_list_dist")
  #     shinyjs::reset("form-dist")
  #     shinyjs::reset("fbmlist_sel_type_dist")
  #     #to do
  #     #Refresh a the textArea
  #     
  #     shinysky::showshinyalert(session, "alert_fbmlist_dist", paste("Material List successfully created!"), 
  #                              styleclass = "success")
  #     
  #     
  #     #for(i in 1:75000){print(i)}
  #     
  #     #shinyjs::js$refresh() 
  #     
  #   }
  #   
  #   
  # })
  # 
  
  #ToDo
  #Visualize route maps. This link shows how to create route and give ideas for distribution data.
  #http://r.prevos.net/create-air-travel-route-maps/
  
  #Table for the most requiered genotype for all year. Also by year, by country (separar a PERU)
  #Table of the country which has asked the vast majority of genotypes.
  #Country which has asked differents genotypes
  
  #year calendar to heatmap: el anhho que pidieron mas genotipos.
  #el mejorador que pidio mas genotipos.
  
  # ToDo: With leaflet
#   library(leaflet)
#   library(geosphere)
#   gcIntermediate(p1 = c(5,52), p2 =  c(-120,37),
#                  n=100,
#                  addStartEnd=TRUE,
#                  sp=TRUE) %>%
#     leaflet() %>%
#     addTiles() %>%
#     addPolylines()
# #   
#   
#   
 }



