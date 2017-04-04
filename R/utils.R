#' Stablish conection to Material List Data Base
#' @describeIn Read the table from M.List DB an write into the computer
#' 
#' 
  fbmlist_data <- function(){
  
    locos <- Sys.info()["sysname"][[1]]
    
    path <- fbglobal::get_base_dir()
    
    if(locos == "Windows") {
    
    
   
    m <- dbDriver("MySQL");
    con <- dbConnect(m,user='dspotatotrials',password='ca7H=j~$V+p2G0715',host='176.34.248.121',dbname='datacippotato_martpot_trials');
    res <- dbSendQuery(con, "SELECT `CIPNUMBER`, `CULTVRNAME`, `COLNUMBER`, `FEMALE`, `MALE`, `PopulationGroup` FROM `dspotatotrials__dpassport__main`")
    dspotatotrials_dpassport <- fetch(res, n = -1)
    names(dspotatotrials_dpassport) <- c("Accession_Number","Accession_Name","Accession_Code","Female_AcceNumb","Male_AcceNumb","Population")
    #write.dbf(dspotatotrials_dpassport,"dspotatotrials_dpassport.dbf")
 
    #path <- fbglobal::get_base_dir()
    #path <- Sys.getenv("LOCALAPPDATA")
    #print("path")
    path_file <- file.path(path, "dspotatotrials_dpassport.rds")
    
    saveRDS(dspotatotrials_dpassport, file = path_file)
    #saveRDS(dspotatotrials_dpassport,file = "dspotatotrials_dpassport.rds")
    dbDisconnect(con)
 
    
    m <- dbDriver("MySQL");
    con <- dbConnect(m,user='dssweettrials',password='c42=gFf8AfZnS0715',host='176.34.248.121',dbname='datacipsweet_martsweet_trials');
    res <- dbSendQuery(con, "SELECT `CIPNUMBER`, `CULTVRNAME`, `COLNUMBER`, `FEMALE`, `MALE`, `PopulationGroup` FROM `dssweettrials__dpassport__main`")
    dssweettrials_dpassport <- fetch(res, n = -1)
    names(dssweettrials_dpassport) <- c("Accession_Number","Accession_Name","Accession_Code","Female_AcceNumb","Male_AcceNumb","Population")
    #write.dbf(dssweettrials_dpassport,"dssweettrials_dpassport.dbf")
    #path <- fbglobal::get_base_dir()
    #path_file <- paste(path, "dssweettrials_dpassport.rds", sep = "\\")
    path_file <- file.path(path, "dssweettrials_dpassport.rds")
   
    saveRDS(dssweettrials_dpassport,file =  path_file)
    #saveRDS(dssweettrials_dpassport,file = "dssweettrials_dpassport.rds")
    dbDisconnect(con)
    
    
    m <- dbDriver("MySQL");
    con <- dbConnect(m,user='cippedigree',password='cF6Jr<tVW]dU60713',host='176.34.248.121',dbname='cippedigree');
    res <- dbSendQuery(con, "SELECT ped_family.pedNameCipnumber, ped_family.pedFemaleCipnumber, ped_family.pedFemale, ped_family.pedMaleCipnumber, ped_family.pedMale, ped_population.PedPopName, ped_family.pedCycle FROM ped_family INNER JOIN ped_population ON ped_family.ped_population_PedPopId = ped_population.PedPopId WHERE ped_family.Crop_CropId = 'SO'")
    potato_pedigree <- fetch(res, n = -1)
    names(potato_pedigree) <- c("Accession_Number","Female_AcceNumb","Female_codename","Male_AcceNumb","Male_codename","Population", "Cycle")
    #write.dbf(potato_pedigree,"potato_pedigree.dbf")
    
    #path <- fbglobal::get_base_dir()
    path_file <- file.path(path, "potato_pedigree.rds")
    
    #saveRDS(potato_pedigree,file = "potato_pedigree.rds")
    saveRDS(potato_pedigree, file =  path_file)
    
    
    dbDisconnect(con)

    m <- dbDriver("MySQL");
    con <- dbConnect(m,user='cippedigree',password='cF6Jr<tVW]dU60713',host='176.34.248.121',dbname='cippedigree');
    res <- dbSendQuery(con, "SELECT ped_family.pedNameCipnumber, ped_family.pedFemaleCipnumber, ped_family.pedFemale, ped_family.pedMaleCipnumber, ped_family.pedMale, ped_population.PedPopName, ped_family.pedCycle FROM ped_family INNER JOIN ped_population ON ped_family.ped_population_PedPopId = ped_population.PedPopId WHERE ped_family.Crop_CropId = 'IP'")
    sweetpotato_pedigree <- fetch(res, n = -1)
    names(sweetpotato_pedigree) <- c("Accession_Number","Female_AcceNumb","Female_codename","Male_AcceNumb","Male_codename","Population", "Cycle")

    #path <- fbglobal::get_base_dir()
    path_file <- file.path(path, "sweetpotato_pedigree.rds")
    saveRDS(sweetpotato_pedigree,file = path_file)
    
    
    #saveRDS(sweetpotato_pedigree,file = "sweetpotato_pedigree.rds")
    dbDisconnect(con)
    #foreign::write.dbf(mtl_data, "mlist_biomart.dbf") 
    } else {
      files <- c("sweetpotato_pedigree.rds", "potato_pedigree.rds",
                 "dssweettrials_dpassport.rds", "dspotatotrials_dpassport.rds")
      fp <- paste0("https://github.com/c5sire/drat/blob/gh-pages/inst/db/", files)
      for(i in 1:length(fp)) download.file(fp[i], path)
    }
    
}


#' List of the dbf files
#' @describeIn Read the table from M.List DB an write into the computer
#' 
#' 
  mtl_files <- function(){
  
     ##sin rsimon
      #dbf_file_list <- list.files(getwd(), full.names = TRUE, pattern = ".rds")
    
     ##con fbglbal
      path <- fbglobal::get_base_dir()
      #path <- Sys.getenv("LOCALAPPDATA")
      dbf_file_list <- list.files(path, full.names = TRUE, pattern = ".rds")
      #dbf_file_list <- list.files(getwd(), full.names = TRUE, pattern = ".rds")
      
      lg_dbf <- length(dbf_file_list)
      if(lg_dbf == 0){ gmtfiles <- "" }
      if(lg_dbf>0)   { 
          ignore_temps <- grepl(pattern = "~\\$",x =  dbf_file_list)
          dbf_file_list <-  dbf_file_list[!ignore_temps]
          short_name <- basename(dbf_file_list)
          gmtfiles <- data.frame(short_name, dbf_file_list, stringsAsFactors = FALSE)
          names(gmtfiles) <- c("short_name","full_name")
          gmtfiles
      }
      
      mtl_files <- gmtfiles
      mtl_files
  
}

#' Cipnumber Creation
#' @describeIn According to every cipnumber and repetition, it creates new cipnumber
#
  cipnumber_creation <- function(header, nrep){
     
     #cipnumber structure: cipnumber_new = cipnumber_family.cipnumber_tail
     cipnumber_family <- header
     cipnumber_tail <- formatC(1:nrep, width = 3, format = "d", flag = "0")
     cipnumbers_new <- paste(cipnumber_family, cipnumber_tail, sep = ".")
     
    
     cipnumbers_nrep <- rep( cipnumber_family, nrep)
     
     
     
     out <- list(cipnumbers_new = cipnumbers_new, cipnumbers_rep = cipnumbers_nrep )
   }
   
# #' Cipnumber Repetition
# #' @describeIn 
# #' 
# # cipnumber repetitions: A repetition vector according to the nrep
#    cipnumber_rep <- function(cipnumber, nrep){
#       cipnumbers_nrep <- rep(cipnumber, nrep)
#    #new cipnumbers and repetitive cipnumber for database join
#    }
  
#' Accession Name Creation
#' @describeIn Creation of accession names according to the number of repetitions
#'
       
  accessname_code <- function(accession_name, accession_number, nrep){
     
     #cipnumber structure: cipnumber_new = cipnumber_family.cipnumber_tail
     accen_name <- accession_name
     
     accen_tail <- formatC(1:nrep, width = 3, format = "d", flag = "0")
     
     temp_nrep <-  stringr::str_sub(accession_number,-3,-1)
     
     accen_new <- paste(accen_name, temp_nrep, sep = "")
     
     accen_new <- paste(accen_new, accen_tail, sep = ".")
     
}
  
  headers_new_list <-  function(){
  
  out <- c("Numeration", "Accession_Number", "Accession_Name", "Accession_code", 
                                "Female_AcceNumb", "Female_codename", "Male_AcceNumb", "Male_codename", "Population",
                                "Cycle", "Is_control", "Scale_audpc", "Material_list_name" , "Researcher_Name" ,    "Continent"   ,       
                                "Country", "Seed_source",  "Simultanious_trials", "Previous_trials","Date_Created" ) 
}





