#' Server Side for generation of Material List
#' 
#' @param input shinyserver input 
#' @param output shinyserver output
#' @param session shinyserver session
#' @param values reactive values
#' @author Omar Benites
#' @export

server_generate <- function(input,output,session, values){
  
   #Reactive data after connecting to database or local lists  
   gmtl_data <- eventReactive(input$fbmlist_connect, {
    dbf_file <- input$fbmlist_sel_list
   
    n <- length(input$fbmlist_sel_list)
    if(n==1){
        path <- fbglobal::get_base_dir()
        path <- file.path(path, dbf_file)
        germlist_db <- readRDS(path)
    }
    if(n > 1){
      combine <- list() 
        for(i in 1:n){  
          
          path <- fbglobal::get_base_dir()

          path <- file.path(path, dbf_file)
          combine[[i]] <- readRDS(file = dbf_file[i])

          #path <- paste(path,dbf_file,sep = "\\")
          combine[[i]] <- readRDS(file = dbf_file[i])
          #combine[[i]] <- readRDS(file = dbf_file[i]) 

        } 
      join_books <- data.table::rbindlist(combine,fill = TRUE)
      join_books <- as.data.frame(join_books)
      germlist_db <- join_books
    }
    
    n_row <- nrow(germlist_db)
    germlist_db <-  mutate(germlist_db, IDX = 1:n_row)
    
    germlist_db
    
  }) 
  
   #reactive value for displaying box and panels
   output$show_mtable <- reactive({
     return(!is.null(gmtl_data()))
   })
   
   #reactive value for save button
   output$show_save <- reactive({
     return(length(input$fbmlist_select[1]))
   })
   
   #set options for show_mtable
   outputOptions(output, 'show_mtable', suspendWhenHidden=FALSE)

  

   #selectInput button for selection of local lists or databases
   output$sel_list_on_btn <- renderUI({
    #mtl_files()
    #db_files_choices <- mtl_files()
    #db_files_choices <- db_files_choices$short_name
    
    #db_files_choices <- list("dspotatotrials_dpassport.dbf", "dssweettrials_dpassport.dbf" ,"potato_pedigree.dbf" ,"sweetpotato_pedigree.dbf")

    crop <- input$fbmlist_sel_crop
    type_db <- input$fbmlist_sel_type
    mtl_db_sel <- mtl_files()$short_name
   
    if(crop == "") {
     
     db_files_choices  <-  "" 
     sel_multiple <- FALSE 
   
   }   
    
   if(crop == "potato") { 
      
      if(type_db=="Institutional"){ 
        
              #db_files_choices <- list("dspotatotrials_dpassport.dbf", "potato_pedigree.dbf")
              db_files_choices <- list("dspotatotrials_dpassport.rds", "potato_pedigree.rds")
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
      
    shiny::selectizeInput(inputId ="fbmlist_sel_list", label = "Select list", 
                            multiple =  sel_multiple, width="100%", choices = db_files_choices,
                            options = list(
                              placeholder = 'Please select an option below',
                              onInitialize = I('function() { this.setValue(""); }')
                            )
                          )
  })
  
   
  #TextInput space to write list's name
  output$create_on_name <- renderUI({
    
    req(input$fbmlist_select)
    textInput("fbmlist_create_on_name", label = h3("New list name"), value = "", placeholder = "Write a list name")
  })
  
  
  #save button to store information
  output$savelist_on_btn <- renderUI({
    
    req(input$fbmlist_select)
    #shiny::actionButton("fbmlist_save", label = "Save List", icon = icon("save"))
    shinysky::actionButton2("fbmlist_save", label = "Save list", icon = "save", icon.library = "bootstrap")
  })

  
  #Clones founded using textArea
  output$fbmlist_foundclones_gen <- renderText({
    
    mtl_table <- gmtl_data()
    # mtl_headers <- c("Accession_Number", "Female_AcceNumb", "Female_codename", "Male_AcceNumb", 
    #                  "Male_codename", "Population", "Cycle", "Date_Created", "IDX") 
    # 
    mtl_table <- mtl_table[,1:6]
    temp_mtl_table <- mtl_table
    
    if(input$fbmlist_txtarea!=""  || !str_detect(input$fbmlist_txtarea, "[[:space:]]") ){
      
      #trimming search filter
      search_filter <- str_split(input$fbmlist_txtarea,"\\n")[[1]]
      search_filter <- stringr::str_trim(search_filter,side = "both")
      search_filter <- as.character(search_filter)

      #extracting columns Accesion Number and Accesion Name
      material_db_accnum <- as.character(temp_mtl_table$Accession_Number)
      material_db_accname <- as.character(temp_mtl_table$Accession_Name) #in case of generate (clone list), it has Accession Name
      
      material_acc_union <- union(material_db_accnum, material_db_accname)

      out_dbacc_search <- setdiff(search_filter, material_acc_union) #find the element which are NOT in the intesection
      out_dbacc_search <- out_dbacc_search[!is.na(out_dbacc_search)]
      out_dbacc_search <- out_dbacc_search[out_dbacc_search!=""]
      n_search <- length(out_dbacc_search)
      print(n_search)
      
      # Show messages according to accesion founder in accession number or accesion name
      
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
  output$fbmlist_table  <-  DT::renderDataTable({
    
    shiny::req(input$fbmlist_sel_list)
    shiny::req(input$fbmlist_connect)
    #shiny::req(input$fbmlist_selectgenlist)
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                         
                          row_click <- NULL
                         
                          mtl_table <- gmtl_data()
                         
                          #col_headers <- c("Accession_Number", "Female_AcceNumb","Female_codename", "Male_AcceNumb" ,"Male_codename", "Population" ,"IDX")
                          mtl_table <- mtl_table[,1:6]
                           
                          n_row <- nrow(mtl_table)
                          mtl_table <-  mutate(mtl_table, IDX = 1:n_row)
                         
                          
                          if(input$fbmlist_txtarea!=""){
                            
                            #Deprecated IDX 
                            #mtl_table <-  mutate(mtl_table, IDX = 1:n())
                            #End deprecated IDX
                            
                            
                            search_filter <- str_split(input$fbmlist_txtarea,"\\n")[[1]]
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
                                          options = list(scrollX = TRUE, scroller = TRUE),
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

                          } else {

                          DT::datatable(mtl_table, rownames = FALSE,
                                        #selection = list( mode= "multiple",  selected =  rownames(mtl_table)),
                                        options = list(scrollX = TRUE, scroller = TRUE),
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
  
  })
  
  
  #the index of selection material
  gmtl_row_index <- eventReactive(input$fbmlist_select,{
    
    row_click <- NULL
    mtl_table <- gmtl_data()
    
    mtl_table <- mtl_table[,1:6]
    
    n_row <- nrow(mtl_table)
    mtl_table <-  mutate(mtl_table, IDX = 1:n_row)
    
    print(input$fbmlist_txtarea)
    
    if(input$fbmlist_txtarea!=""){
      
      #Deprecated IDX 
      #mtl_table <-  mutate(mtl_table, IDX = 1:n())
      #End deprecated IDX
      
      
      search_filter <- str_split(input$fbmlist_txtarea,"\\n")[[1]]
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
      
      row_select <- input$fbmlist_table_rows_selected #comand to get selected values		
      #row_filter <- input$fbmlist_table_new_rows_all #comand to get filtered values		    
      #row_mtlist_selection <- dplyr::intersect(row_select,row_filter)		     
     
      row_mtlist_selection <- sort(row_select)
      row_click <- row_mtlist_selection
    }
    
    #print(row_select)
    print(row_click)
    row_click
 
})
  
  
  #table of selected clones after pressing "Select marked"
  output$fbmlist_choosen_table  <- DT::renderDataTable({
    
    #print(input$foo)
    
    index <- gmtl_row_index()
    mtl_table <- gmtl_data()
    
    mtl_table_temp <- mtl_table #temporal table for visualizing
    chosen_gmtl_table <-  mtl_table_temp[index, ]
    chosen_gmtl_table 
    
  }, options = list(searching = FALSE, scrollX = TRUE, scroller = TRUE) )
  
  
  # Observers of fbmlist ----------------------------------------------------
  shiny::observeEvent( input$fbmlist_save, {
    
    index <- gmtl_row_index()
    mtl_table <- gmtl_data()
    chosen_gmtl_table <-  mtl_table[index, ]
    fbmlist_name_dbf  <- str_trim(string = input$fbmlist_create_on_name, side = "both")
    fbmlist_name_dbf  <- gsub("\\s+", "_", fbmlist_name_dbf)
    
    fbmlist_name_dbf_temp <- fbmlist_name_dbf  #This variable is for control when user do not type empty names
   
    #Adding the crop notation 
    crop <- input$fbmlist_sel_crop
    if(crop=="potato")      { fbmlist_name_dbf <- paste("PT","clon", fbmlist_name_dbf,sep = "_") }
    if(crop=="sweetpotato") { fbmlist_name_dbf <- paste("SP","clon", fbmlist_name_dbf,sep = "_") } 
    #End of crop notation
    
    
    #All the files names
    db_files  <- file_path_sans_ext(mtl_files()$short_name)

    if(fbmlist_name_dbf %in% db_files) {
    
      shinysky::showshinyalert(session, "alert_fbmlist_on", paste("WARNING: This list already exists"),
                               styleclass = "warning")
      
    }
    else if(fbmlist_name_dbf_temp==""){  #use of the temporary variable to control empty names given by users. in line 261
      
      shinysky::showshinyalert(session, "alert_fbmlist_on", paste("WARNING: Please Type a Material List Name"), 
                               styleclass = "warning")
    } 
    else {
      
      gen_headers <- c("Numeration","Is_control", "Scale_audpc", "Family_AcceNumb", 
                        "Cycle"	, "Seed_source", "Simultanious_trials", "Previous_trials")
      
      if(all(is.element(gen_headers,names(chosen_gmtl_table)))){  
      
              gen_list_tbl <- chosen_gmtl_table
        
      } else {  
        
        chosen_gmtl_table_list <- as.list(chosen_gmtl_table)
        extra_parameters <- list(
                                  Numeration = 1:nrow(chosen_gmtl_table),
                                  Is_control	= NA,
                                  Scale_audpc	= NA,
                                  Family_AcceNumb = NA,
                                  Cycle	 = NA,
                                  Seed_source = NA,	
                                  Simultaneous_trials = NA,
                                  list_name= fbmlist_name_dbf,
                                  Previous_trials = NA,
                                  Date_Created = format(Sys.Date(), "%d %m %Y")
                              )
      
        gen_list_tbl <- c(chosen_gmtl_table_list, extra_parameters)
        
        gen_list_tbl <- as.data.frame(gen_list_tbl, stringsAsFactors = FALSE) 
        
        
      }
      #foreign::write.dbf(dataframe = chosen_gmtl_table, file = fbmlist_name_dbf, factor2char = FALSE)
      gen_list_tbl <- gen_list_tbl
      
      header_order <- c("Numeration", "Accession_Number",	"Accession_Name", "Accession_Code", "Is_control",	"Scale_audpc",	"Family_AcceNumb", "Female_AcceNumb",
                        "Female_codename",	"Male_AcceNumb",  "Male_codename",
                        "Population",	"Cycle"	,"Seed_source", "Simultaneous_trials",	"Previous_trials", "list_name",	"Previous_trials",	"Date_Created")
      
      header_found <- dplyr::intersect(header_order , colnames(gen_list_tbl))
      
      gen_list_tbl <- gen_list_tbl[, header_found]
      
      if(input$gen_type_trial=="Standard"){ #normal columns by default
        gen_list_tbl <- gen_list_tbl
      } else {   #PVS #remove columns Is_Control, "Scale_Audpc" 
        gen_list_tbl <- dplyr::select(gen_list_tbl, -Is_control, -Scale_audpc)
        gen_list_tbl <- as.data.frame(gen_list_tbl)
      }
      
      gen_list_tbl <- gen_list_tbl
      
      crop <- input$fbmlist_sel_crop

      fbmlist_name_dbf <- paste(fbmlist_name_dbf,".rds",sep = "")
     
      
      ## using fbglobal
      path <- fbglobal::get_base_dir()
      path <- file.path(path,  fbmlist_name_dbf)
      
      saveRDS(gen_list_tbl, file = path)
       
      mtl_files()
      
      shinyjs::reset("fbmlist_sel_list")
      shinyjs::reset("form-gen")
      shinyjs::reset("fbmlist_sel_type")
      #to do
      #Refresh a the textArea
      
      shinysky::showshinyalert(session, "alert_fbmlist_on", paste("Material List successfully created!"), 
                               styleclass = "success")
     
      
      #for(i in 1:75000){print(i)}
      
      #shinyjs::js$refresh() 
      
    }

    
    })
  
  
}



