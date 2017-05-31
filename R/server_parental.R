#' Server Side for parental of material list
#' 
#' @param input shinyserver input 
#' @param output shinyserver output
#' @param session shinyserver session
#' @param values reactive values
#' @author Omar Benites
#' @export

server_parentlist <- function(input,output,session, values){
  
  #Reactive data after local lists or database connection.
  gmtl_data <- eventReactive(input$fbmlist_connect_parent, {
    
    #stype <- length(input$fbmlist_sel_type)
    #print(length(input$fbmlist_connect))
    dbf_file <- input$fbmlist_sel_list_parent
    
    n <- length(input$fbmlist_sel_list_parent)
 
    
    if(input$fbmlist_sel_type_parent=="Institutional"){
    
    #print(dbf_file)
    #dbf_sel <- input$fbmlist_sel_type
    #if(is.null(dbf_file) && is.null(dbf_sel)){ return(NULL) }
    if(n==1){
      
      
      #path <- Sys.getenv("LOCALAPPDATA")
      path <- fbglobal::get_base_dir()
      path <- paste(path, dbf_file,sep = "\\")
      
      #print(path)
      print(path)
      #germlist_db <- readRDS(dbf_file)
      germlist_db <- readRDS(path)
      
      # if(stringr::str_detect(path, "parent")){
      #   germlist_db <- germlist_db$parental_table
      # }
      
    }
    #germlist_db <- foreign::read.dbf(file = dbf_file, as.is = TRUE)
    if(n > 1){
      combine <- list() 
      for(i in 1:n){  
        
        path <- fbglobal::get_base_dir()
        path <- paste(path,dbf_file,sep = "\\")
        combine[[i]] <- readRDS(file = dbf_file[i])
        #combine[[i]] <- readRDS(file = dbf_file[i]) 
      } 
      join_books <- data.table::rbindlist(combine,fill = TRUE)
      join_books <- as.data.frame(join_books)
      germlist_db <- join_books
    }
      
      n_row <- nrow(germlist_db)
      #germlist_db <-  mutate(IDX = 1:n_row, germlist_db)
      germlist_db <-  cbind(IDX = 1:n_row, germlist_db)
      
      
    }
    if(input$fbmlist_sel_type_parent=="Local"){
      
      path <- fbglobal::get_base_dir()
      path <- paste(path, dbf_file,sep = "\\")
      print(path)
      germlist_db <- readRDS(path)
      
      if(stringr::str_detect(path, "parent")){
        germlist_db <- germlist_db
      }
      
    }
    
    
    germlist_db
    
  }) 

  #Reactive data for parents 
  output$show_mtable_parent <- reactive({
    return(!is.null(gmtl_data()))
  })

  #Reactive values. It works in conditionalPanels to show the table after pressing 'connect'
  output$show_save_parent <- reactive({
    return(length(input$fbmlist_select_parent[1]))
  }) #podria ser retirado
  
  #setting options
  outputOptions(output, 'show_mtable_parent', suspendWhenHidden=FALSE)
  
  #selectInput button for local lists and databases
  output$sel_list_parent_btn <- renderUI({
    #mtl_files()
    #db_files_choices <- mtl_files()
    #db_files_choices <- db_files_choices$short_name
    
    #db_files_choices <- list("dspotatotrials_dpassport.dbf", "dssweettrials_dpassport.dbf" ,"potato_pedigree.dbf" ,"sweetpotato_pedigree.dbf")
    crop <- input$fbmlist_sel_crop_parent
    type_db <- input$fbmlist_sel_type_parent
    mtl_db_sel <- mtl_files()$short_name
    
    if(crop == "") {
      
      db_files_choices  <-  "" 
      sel_multiple <- FALSE 
      
    }   
    
    if(crop == "potato") { 
      
      if(type_db=="Institutional"){ 
        
        #db_files_choices <- list("dspotatotrials_dpassport.dbf", "potato_pedigree.dbf")
        db_files_choices <- list("dspotatotrials_dpassport.rds")
        sel_multiple <- FALSE                         
      }
      if(type_db=="Local"){ 
        
        db_files_choices <- mtl_db_sel[str_detect(mtl_db_sel , "PT")] 
        sel_multiple <- TRUE
      }
    }
    
    if(crop == "sweetpotato") {
      
      if(type_db=="Institutional") {     
        db_files_choices <- list("dssweettrials_dpassport.rds")
        sel_multiple <- FALSE   
      }
      if(type_db=="Local"){ 
        db_files_choices <- mtl_db_sel[str_detect(mtl_db_sel , "SP")]
        sel_multiple <- TRUE 
      }
    }
    
    db_files_choices <- db_files_choices
    
    shiny::selectizeInput(inputId ="fbmlist_sel_list_parent", label = "Select list", 
                          multiple =  FALSE, width="100%", choices = db_files_choices,
                          options = list(
                            placeholder = 'Please select an option below',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
  })
  
  # Parent list's name
  output$create_parent_name <- renderUI({
    
    req(input$fbmlist_select_parent)
    textInput("fbmlist_create_parent_name", label = h3("New list name"), value = "", placeholder = "Write a list name")
  })
  
  # Render ui Button for saveing local lists
  output$savelist_parent_btn <- renderUI({
    
    req(input$fbmlist_select_parent)
    #shiny::actionButton("fbmlist_save", label = "Save List", icon = icon("save"))
    shinysky::actionButton2("fbmlist_save_parent", label = "Save list", icon = "save", icon.library = "bootstrap")
  })
  
  
  # Local DataTable (DT) ----------------------------------------------------
  
  #This table serves only for local list created by users. It's for select parents (binding of females and males)
  output$fbmlist_table_parent_local  <- DT::renderDataTable({
    
    
    #shiny::req(input$fbmlist_sel_list_parent)
    #shiny::req(input$fbmlist_connect_parent)
    germlist_db <- gmtl_data()
    print(gmtl_data())
    germlist_parent_db <- germlist_db$parental_table
    
    req(input$fbmlist_sel_type_parent)
    
    if(input$fbmlist_sel_type_parent=="Local" ){
    
      
      mtl_table <- germlist_parent_db
      print("1")
      print(mtl_table)
      mtl_table
      
    } else {
      
      mtl_table <- data.frame() 
      print("2")
      print(mtl_table)
      mtl_table
    }
    
    mtl_table <- mtl_table
    
  }, options = list(searching = FALSE, pageLength = 5, scrollY = 200, scrollX = TRUE, scroller = TRUE),  rownames= TRUE,  selection = 'none' )
  
  #This table serves only for local list created by users. It's for select female
  output$fbmlist_table_female_local  <- DT::renderDataTable({
    
    req(input$fbmlist_sel_type_parent)
    germlist_db <- gmtl_data()
    germlist_female_local_db <- germlist_db$female
    
    if(input$fbmlist_sel_type_parent=="Local" ){
      
      mtl_table <- germlist_female_local_db
      mtl_table
      
    } else {
      
      mtl_table <- data.frame() 
      mtl_table
    }
    
    mtl_table <- mtl_table
    
  }, options = list(searching = FALSE, pageLength = 5, scrollY = 200, scrollX = TRUE, scroller = TRUE),  rownames= FALSE,  selection = 'none' )
  
  #This table serves only for local list created by users. It's for select male
  output$fbmlist_table_male_local  <-   DT::renderDataTable({
    
    germlist_db <- gmtl_data()
    germlist_male_local_db <- germlist_db$male

    req(input$fbmlist_sel_type_parent)
    
    if(input$fbmlist_sel_type_parent=="Local" ){
      
      
      mtl_table <- germlist_male_local_db
      # print("1")
      # print(mtl_table)
      mtl_table
      
    } else {
      
      mtl_table <- data.frame() 
      print("2")
      print(mtl_table)
      mtl_table
    }
    
    mtl_table <- mtl_table
    
  }, options = list(searching = FALSE, pageLength = 5, scrollY = 200, scrollX = TRUE, scroller = TRUE),  rownames= FALSE,  selection = 'none' )
  
  # End local DataTable (DT) --
  
  
  # New ---------------------------------------------------------------------
  
  # Female ------------------------------------------------------------------
  
  #Entire table for female genotypes. In this table we select the female accessions.
  output$fbmlist_table_parent_fem  <-  DT::renderDataTable({
    
    
    shiny::req(input$fbmlist_connect_parent)
    #shiny::req(input$fbmlist_selectgenlist)
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                          
                          row_click <- NULL
                          
                          mtl_table <- gmtl_data()
                          
                          #col_headers <- c("Accession_Number", "Female_AcceNumb","Female_codename", "Male_AcceNumb" ,"Male_codename", "Population" ,"IDX")
                          mtl_table <- mtl_table[,1:6]
                          
                          n_row <- nrow(mtl_table)
                          mtl_table <-  mutate(mtl_table, IDX = 1:n_row)
                          
                          #col_names <- c("ACCNum", "ACCNam", "COLLNUMB", "POP", "PEDIGREE") 
                          #mtl_table <- mtl_table[col_names] #show cols selected
                          
                          #print(input$fbmlist_txtarea)
                          
                          
                          if(input$fbmlist_txtarea_parent_fem!=""){
                            
                            #Deprecated IDX 
                            #mtl_table <-  mutate(mtl_table, IDX = 1:n())
                            #End deprecated IDX
                            
                            
                            search_filter <- str_split(input$fbmlist_txtarea_parent_fem,"\\n")[[1]]
                            search_filter <- stringr::str_trim(search_filter, side = "both")
                            
                            mtl_table_f <- filter(mtl_table, Accession_Number %in% search_filter)
                            
                            if(nrow(mtl_table_f)==0 &&  is.element("Accession_Name",names(mtl_table_f))) {
                              
                              mtl_table_f <- dplyr::filter(mtl_table, Accession_Name %in% search_filter)
                              
                            }
                            
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
                                           #selection = list( mode= "multiple",  selected =  rownames(mtl_table)), 
                                           selection = list( mode = "multiple", selected = Search), 
                                           filter = 'bottom'#,
                            )
                            
                          } else {
                            
                            DT::datatable(mtl_table, rownames = FALSE,
                                          #selection = list( mode= "multiple",  selected =  rownames(mtl_table)),
                                          selection = list( mode = "multiple"),
                                          filter = 'bottom'#,
                            )
                            
                          }
                          
                        }) #end of Progress
    
  }, options = list(scrollX = TRUE, scroller = TRUE))
  
  
  #textarea input for females
  output$fbmlist_foundclones_parent_fem <- renderText({
    
    mtl_table <- gmtl_data()
    # mtl_headers <- c("Accession_Number", "Female_AcceNumb", "Female_codename", "Male_AcceNumb", 
    #                  "Male_codename", "Population", "Cycle", "Date_Created", "IDX") 
    # 
    mtl_table <- mtl_table[,1:6]
    temp_mtl_table <- mtl_table
    
    if(input$fbmlist_txtarea_parent_fem!=""|| !str_detect(input$fbmlist_txtarea_parent_fem, "[[:space:]]") ){
      
      #trimming search filter
      search_filter <- str_split(input$fbmlist_txtarea_parent_fem,"\\n")[[1]]
      search_filter <- stringr::str_trim(search_filter,side = "both")
      search_filter <- as.character(search_filter)
      
      #extracting columns Accesion Number and Accesion Name
      material_db_accnum <- as.character(temp_mtl_table$Accession_Number)
      material_db_accname <- as.character(temp_mtl_table$Accession_Name) #in case of generate (clone list), it has Accesion Name
      
      material_acc_union <- union(material_db_accnum, material_db_accname)
      
      out_dbacc_search <- setdiff(search_filter, material_acc_union) #find the element which are NOT in the inserction
      out_dbacc_search <- out_dbacc_search[!is.na(out_dbacc_search)]
      out_dbacc_search <- out_dbacc_search[out_dbacc_search!=""]
      n_search <- length(out_dbacc_search)
      print(n_search)
      
      # Show messages according to accesion founder in accesion number or accesion name
      
      if(n_search>0){ #for accession number, flag =1
        out <- paste(out_dbacc_search, collapse = ", ")
        out <- paste("N= ", n_search, " accesion(s) were not found: ", out, sep="")
      } else {
        out <- paste("", sep = "")
      }
      out
      
    }
    
    
  })
  
  
  #indeces for selected females
  gmtl_row_index_fem <- eventReactive(input$fbmlist_select_parent,{
    
    row_click <- NULL
    mtl_table <- gmtl_data()
    
    mtl_table <- mtl_table[,1:6]
    
    n_row <- nrow(mtl_table)
    mtl_table <-  mutate(mtl_table, IDX = 1:n_row)
    
    print(input$fbmlist_txtarea_parent_fem)
    
    if(input$fbmlist_txtarea_parent_fem!=""){
      
      
      search_filter <- str_split(input$fbmlist_txtarea_parent_fem,"\\n")[[1]]
      search_filter <- stringr::str_trim(search_filter,side = "both")
      
      mtl_table_f <- filter(mtl_table, Accession_Number %in% search_filter)
      #row_click <- as.numeric(rownames(mtl_table_f))
      #                               row_click <- dplyr::select(mtl_table_f, IDX)[[1]]
      #                               print(row_click)
      #print(row_click)
      
      if(nrow(mtl_table_f)==0 &&  is.element("Accession_Name",names(mtl_table_f))) {
        
        mtl_table_f <- dplyr::filter(mtl_table, Accession_Name %in% search_filter)
        #row_click <- as.numeric(rownames(mtl_table_f))
        # row_click <- dplyr::select(mtl_table_f, IDX)[[1]]
      }
      
      if(nrow(mtl_table_f)==0 &&  is.element("Accession_Code",names(mtl_table_f))) {
        
        mtl_table_f <- dplyr::filter(mtl_table, Accession_Code %in% search_filter)
        #row_click <- as.numeric(rownames(mtl_table_f))
        # row_click <- dplyr::select(mtl_table_f, IDX)[[1]]
      }
      
      if(nrow(mtl_table_f)==0 &&  is.element("Female_AcceNumb",names(mtl_table_f))) {
        
        mtl_table_f <- dplyr::filter(mtl_table, Female_AcceNumb %in% search_filter)
        #row_click <- as.numeric(rownames(mtl_table_f))
        # row_click <- dplyr::select(mtl_table_f, IDX)[[1]]
      } 
      
      if(nrow(mtl_table_f)==0 &&  is.element("Female_codename",names(mtl_table_f))) {
        
        mtl_table_f <- dplyr::filter(mtl_table, Female_codename %in% search_filter)
        
      }  
      
      if(nrow(mtl_table_f)==0 &&  is.element("Male_AcceNumb",names(mtl_table_f))) {
        
        mtl_table_f <- dplyr::filter(mtl_table, Male_AcceNumb %in% search_filter)
        
      }    
      
      if(nrow(mtl_table_f)==0 &&  is.element("Male_codename",names(mtl_table_f))) {
        
        mtl_table_f <- dplyr::filter(mtl_table, Male_codename %in% search_filter)
        #row_click <- as.numeric(rownames(mtl_table_f))
        
      }  
      
      if(nrow(mtl_table_f)==0  &&  is.element("Population",names(mtl_table_f))) {
        
        mtl_table_f <- dplyr::filter(mtl_table, Population %in% search_filter)
        
      }
      
      if(nrow(mtl_table_f)==0  &&  is.element("Cycle",names(mtl_table_f))) {
        
        mtl_table_f <- dplyr::filter(mtl_table, Cycle %in% search_filter)
        #row_click <- as.numeric(rownames(mtl_table_f))
        #                                 row_click <- dplyr::select(mtl_table_f, IDX)[[1]]
        #                                 print(row_click)
      }  
      
      if(nrow(mtl_table_f)>0){ 
        
        row_click <- as.numeric(mtl_table_f$IDX)
        
      } else {
        row_click <- NULL
      }
      
    } 
    
    else {
      
      row_select <- input$fbmlist_table_parent_fem_rows_selected #comand to get selected values		
      #row_filter <- input$fbmlist_table_new_rows_all #comand to get filtered values		    
      #row_mtlist_selection <- dplyr::intersect(row_select,row_filter)		     
      
      row_mtlist_selection <- sort(row_select)
      row_click <- row_mtlist_selection
    }
    
    #print(row_select)
    print(row_click)
    row_click
    
  })
  
  
  
  #Table of selected female genotypes
  output$fbmlist_choosen_table_parent_fem  <- DT::renderDataTable({
    
    #print(input$foo)
    
    
    index <- gmtl_row_index_fem()
    mtl_table <- gmtl_data()
    
    mtl_table_temp <- mtl_table #temporal table for visualizing
    chosen_gmtl_table <-  mtl_table_temp[index, ]
    chosen_gmtl_table 
    
  }, 
                                                                  options = list(searching = FALSE, pageLength = 5, scrollX = TRUE, scroller = TRUE), rownames= FALSE, selection = 'none' )
  
  # End Female ---------------------------------------------------------------
  
  
  # Male ---------------------------------------------------------------
  
  #Entire table for male genotypes. In this table we select the male accessions.
  output$fbmlist_table_parent_male  <-  DT::renderDataTable({
    
    
    shiny::req(input$fbmlist_connect_parent)
    #shiny::req(input$fbmlist_selectgenlist)
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                          
                          row_click <- NULL
                          
                          mtl_table <- gmtl_data()
                          
                          #col_headers <- c("Accession_Number", "Female_AcceNumb","Female_codename", "Male_AcceNumb" ,"Male_codename", "Population" ,"IDX")
                          mtl_table <- mtl_table[,1:6]
                          
                          n_row <- nrow(mtl_table)
                          mtl_table <-  mutate(mtl_table, IDX = 1:n_row)
                          
                          #col_names <- c("ACCNum", "ACCNam", "COLLNUMB", "POP", "PEDIGREE") 
                          #mtl_table <- mtl_table[col_names] #show cols selected
                          
                          #print(input$fbmlist_txtarea)
                          
                          
                          if(input$fbmlist_txtarea_parent_male!=""){
                            
                            #Deprecated IDX 
                            #mtl_table <-  mutate(mtl_table, IDX = 1:n())
                            #End deprecated IDX
                            
                            
                            search_filter <- str_split(input$fbmlist_txtarea_parent_male,"\\n")[[1]]
                            search_filter <- stringr::str_trim(search_filter, side = "both")
                            
                            mtl_table_f <- filter(mtl_table, Accession_Number %in% search_filter)
                            
                            if(nrow(mtl_table_f)==0 &&  is.element("Accession_Name",names(mtl_table_f))) {
                              
                              mtl_table_f <- dplyr::filter(mtl_table, Accession_Name %in% search_filter)
                              
                            }
                            
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
                                           #selection = list( mode= "multiple",  selected =  rownames(mtl_table)), 
                                           selection = list( mode = "multiple", selected = Search), 
                                           filter = 'bottom'#,
                            )
                            
                          } else {
                            
                            DT::datatable(mtl_table, rownames = FALSE,
                                          #selection = list( mode= "multiple",  selected =  rownames(mtl_table)),
                                          selection = list( mode = "multiple"),
                                          filter = 'bottom'#,
                            )
                            
                          }
                          
                        }) #end of Progress
    
  }, options = list(scrollX = TRUE, scroller = TRUE))
  
  
  
  #textarea input for males
  output$fbmlist_foundclones_parent_male <- renderText({
    
    mtl_table <- gmtl_data()
    # mtl_headers <- c("Accession_Number", "Female_AcceNumb", "Female_codename", "Male_AcceNumb", 
    #                  "Male_codename", "Population", "Cycle", "Date_Created", "IDX") 
    # 
    mtl_table <- mtl_table[,1:6]
    temp_mtl_table <- mtl_table
    
    if(input$fbmlist_txtarea_parent_male!=""|| !str_detect(input$fbmlist_txtarea_parent_male, "[[:space:]]") ){
      
      #trimming search filter
      search_filter <- str_split(input$fbmlist_txtarea_parent_male,"\\n")[[1]]
      search_filter <- stringr::str_trim(search_filter,side = "both")
      search_filter <- as.character(search_filter)
      
      #extracting columns Accesion Number and Accesion Name
      material_db_accnum <- as.character(temp_mtl_table$Accession_Number)
      material_db_accname <- as.character(temp_mtl_table$Accession_Name) #in case of generate (clone list), it has Accesion Name
      
      material_acc_union <- union(material_db_accnum, material_db_accname)
      
      out_dbacc_search <- setdiff(search_filter, material_acc_union) #find the element which are NOT in the inserction
      out_dbacc_search <- out_dbacc_search[!is.na(out_dbacc_search)]
      out_dbacc_search <- out_dbacc_search[out_dbacc_search!=""]
      n_search <- length(out_dbacc_search)
      print(n_search)
      
      # Show messages according to accesion founder in accesion number or accesion name
      
      if(n_search>0){ #for accession number, flag =1
        out <- paste(out_dbacc_search, collapse = ", ")
        out <- paste("N= ", n_search, " accesion(s) were not found: ", out, sep="")
      } else {
        out <- paste("", sep = "")
      }
      out
      
    }
    
    
  })
  
  
  
  #indeces for selected males
  gmtl_row_index_male <- eventReactive(input$fbmlist_select_parent,{
    
    row_click <- NULL
    mtl_table <- gmtl_data()
    
    mtl_table <- mtl_table[,1:6]
    
    n_row <- nrow(mtl_table)
    mtl_table <-  mutate(mtl_table, IDX = 1:n_row)
    
    print(input$fbmlist_txtarea_parent_male)
    
    if(input$fbmlist_txtarea_parent_male!=""){
      
      
      search_filter <- str_split(input$fbmlist_txtarea_parent_male,"\\n")[[1]]
      search_filter <- stringr::str_trim(search_filter,side = "both")
      
      mtl_table_f <- filter(mtl_table, Accession_Number %in% search_filter)
      #row_click <- as.numeric(rownames(mtl_table_f))
      #                               row_click <- dplyr::select(mtl_table_f, IDX)[[1]]
      #                               print(row_click)
      #print(row_click)
      
      if(nrow(mtl_table_f)==0 &&  is.element("Accession_Name",names(mtl_table_f))) {
        
        mtl_table_f <- dplyr::filter(mtl_table, Accession_Name %in% search_filter)
        #row_click <- as.numeric(rownames(mtl_table_f))
        # row_click <- dplyr::select(mtl_table_f, IDX)[[1]]
      }
      
      if(nrow(mtl_table_f)==0 &&  is.element("Accession_Code",names(mtl_table_f))) {
        
        mtl_table_f <- dplyr::filter(mtl_table, Accession_Code %in% search_filter)
        #row_click <- as.numeric(rownames(mtl_table_f))
        # row_click <- dplyr::select(mtl_table_f, IDX)[[1]]
      }
      
      if(nrow(mtl_table_f)==0 &&  is.element("Female_AcceNumb",names(mtl_table_f))) {
        
        mtl_table_f <- dplyr::filter(mtl_table, Female_AcceNumb %in% search_filter)
        #row_click <- as.numeric(rownames(mtl_table_f))
        # row_click <- dplyr::select(mtl_table_f, IDX)[[1]]
      } 
      
      if(nrow(mtl_table_f)==0 &&  is.element("Female_codename",names(mtl_table_f))) {
        
        mtl_table_f <- dplyr::filter(mtl_table, Female_codename %in% search_filter)
        
      }  
      
      if(nrow(mtl_table_f)==0 &&  is.element("Male_AcceNumb",names(mtl_table_f))) {
        
        mtl_table_f <- dplyr::filter(mtl_table, Male_AcceNumb %in% search_filter)
        
      }    
      
      if(nrow(mtl_table_f)==0 &&  is.element("Male_codename",names(mtl_table_f))) {
        
        mtl_table_f <- dplyr::filter(mtl_table, Male_codename %in% search_filter)
        #row_click <- as.numeric(rownames(mtl_table_f))
        
      }  
      
      if(nrow(mtl_table_f)==0  &&  is.element("Population",names(mtl_table_f))) {
        
        mtl_table_f <- dplyr::filter(mtl_table, Population %in% search_filter)
        
      }
      
      if(nrow(mtl_table_f)==0  &&  is.element("Cycle",names(mtl_table_f))) {
        
        mtl_table_f <- dplyr::filter(mtl_table, Cycle %in% search_filter)
        #row_click <- as.numeric(rownames(mtl_table_f))
        #                                 row_click <- dplyr::select(mtl_table_f, IDX)[[1]]
        #                                 print(row_click)
      }  
      
      if(nrow(mtl_table_f)>0){ 
        
        row_click <- as.numeric(mtl_table_f$IDX)
        
      } else {
        row_click <- NULL
      }
      
    } 
    
    else {
      
      row_select <- input$fbmlist_table_parent_male_rows_selected #comand to get selected values		
      #row_filter <- input$fbmlist_table_new_rows_all #comand to get filtered values		    
      #row_mtlist_selection <- dplyr::intersect(row_select,row_filter)		     
      
      row_mtlist_selection <- sort(row_select)
      row_click <- row_mtlist_selection
    }
    
    #print(row_select)
    print(row_click)
    row_click
    
  })
  
  
  
  #Table of selected males genotypes
  output$fbmlist_choosen_table_parent_male  <- DT::renderDataTable({
    
    #print(input$foo)
    
    index <- gmtl_row_index_male()
    mtl_table <- gmtl_data()
    
    mtl_table_temp <- mtl_table #temporal table for visualizing
    chosen_gmtl_table <-  mtl_table_temp[index, ]
    chosen_gmtl_table 
    
  }, 
                                                                    options = list(searching = FALSE, pageLength = 5, scrollX = TRUE, scroller = TRUE), rownames= FALSE, selection = 'none')
  
  # End Male ---------------------------------------------------------------
  
  
  #Invalidate save button 
  observe({
    toggleState(id = "fbmlist_save_parent", condition =  !is.null(input$fbmlist_create_parent_name) && str_trim(input$fbmlist_create_parent_name, side = "both")!= ""
         )
  })
  
  
  
  #ActionButton
  shiny::observeEvent(input$fbmlist_save_parent, {
  
    
    index_fem <- gmtl_row_index_fem()
    index_male <- gmtl_row_index_male()
    
    mtl_table_parent <- gmtl_data()
    
    chosen_gmtl_table_fem <-  mtl_table_parent[index_fem, ]
    chosen_gmtl_table_male <-  mtl_table_parent[index_male, ]
    
    
    fbmlist_name_dbf_parent  <- str_trim(string = input$fbmlist_create_parent_name, side = "both")
    fbmlist_name_dbf_parent  <- gsub("\\s+", "_", fbmlist_name_dbf_parent)
    fbmlist_name_dbf_temp_parent <- fbmlist_name_dbf_parent  #This variable is for control when user do not type empty names
    
    #Adding the crop notation 
    crop <- input$fbmlist_sel_crop_parent
    if(crop=="potato")      { fbmlist_name_dbf_parent <- paste("PT","parent", fbmlist_name_dbf_parent,sep = "_") }
    if(crop=="sweetpotato") { fbmlist_name_dbf_parent <- paste("SP","parent", fbmlist_name_dbf_parent,sep = "_") } 
    #End of crop notation
    
    
    #All the files names
    db_files_parent  <- file_path_sans_ext(mtl_files()$short_name)
    #print(db_files_parent)
    #print(fbmlist_name_dbf_parent)
    #print(fbmlist_name_dbf_parent %in% db_files_parent)
    
    
    if(fbmlist_name_dbf_parent %in% db_files_parent) {
      
      #print("paso1")
          shinysky::showshinyalert(session, "alert_fbmlist_parent", paste("WARNING: This list already exists"),
                               styleclass = "warning")
      
    } 
    
      else if(fbmlist_name_dbf_temp_parent==""){  #use of the temporary variable to control empty names given by users. in line 261

          shinysky::showshinyalert(session, "alert_fbmlist_parent", paste("WARNING: Please Type a Material List Name"),
                               styleclass = "warning")

    }
    # 
      else if(is.null(input$fbmlist_create_parent_name) || str_trim(input$fbmlist_create_parent_name, side = "both")== "" ) {

          shinysky::showshinyalert(session, "alert_fbmlist_parent", paste("WARNING: Please fill the information"),
                                 styleclass = "warning")

    }
    # 
    else if(is.null(index_fem)){

          shinysky::showshinyalert(session, "alert_fbmlist_parent", paste("Warning: Please select FEMALE parentals"),
                               styleclass = "warning")

    }
    # 
     else if(is.null(index_male)){
     
           shinysky::showshinyalert(session, "alert_fbmlist_parent", paste("Warning: Please select MALE parentals"),
                                styleclass = "warning")
     
     } else if(is.null(index_fem) && is.null(index_male)){
     
           shinysky::showshinyalert(session, "alert_fbmlist_parent", paste("Warning: Please select FEMALE and MALE parentals"),
                                styleclass = "warning")
     } 
    
     else {

       #db <- list(female = chosen_gmtl_table_fem, male = chosen_gmtl_table_male)
      
       nrow_male   <- nrow(chosen_gmtl_table_male)
       nrow_female <- nrow(chosen_gmtl_table_fem)
       
       
       if(nrow_male>nrow_female){

         n <-  nrow_male-nrow_female
         vec_na <- rep(NA, n)
         female_parents <- c(chosen_gmtl_table_fem$Accession_Number, vec_na)
         parental_table <- data.frame(Female_AcceNumb = female_parents, Male_AcceNumb = chosen_gmtl_table_male$Accession_Number, stringsAsFactors = FALSE)
         
         
       } else if (nrow_female>nrow_male){
         
         n <-  nrow_female-nrow_male
         vec_na <- rep(NA, n)
         male_parents <- c(chosen_gmtl_table_male$Accession_Number, vec_na)
         parental_table <- data.frame(Female_AcceNumb = chosen_gmtl_table_fem$Accession_Number, Male_AcceNumb =  male_parents, stringsAsFactors = FALSE)
         
       
       } else {
         
         parental_table <- data.frame(Female_AcceNumb = chosen_gmtl_table_fem$Accession_Number, 
                                         Male_AcceNumb = chosen_gmtl_table_male$Accession_Number, stringsAsFactors = FALSE)
         
       }
       
      
           parental_table <- parental_table
           db_parent <- list(female = chosen_gmtl_table_fem, male = chosen_gmtl_table_male, parental_table = parental_table)
           
           path <- fbglobal::get_base_dir()
           file_parent <- paste(fbmlist_name_dbf_parent,".rds",sep = "")
           
           path <- file.path(path, file_parent)
           saveRDS( db_parent, file = path)
           
           shinysky::showshinyalert(session, "alert_fbmlist_parent", paste("Material List successfully created!"), 
                                     styleclass = "success")
           print("paso el filtro")
    }
    
  })
  
}  


