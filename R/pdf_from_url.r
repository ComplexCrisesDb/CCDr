# function to download pdf files from urls

pdf_from_url = function(urls, export_path, overwrite = T) {
    # download from a a dataframe containing the url of the files
  
    urls=data.frame(urls)
    ref_colnames = c("title","pdf", "name_file")
    
    if (!dir.exists(export_path)) {
        # create new folder if not existing
        dir.create(export_path, recursive = T)
    }
    log = list()
    if (any(names(urls) %in% ref_colnames)) {
        # make sure the files has the proper format with at least the name of the
        # file and a url link to pdf
        count = 0
        progress = dplyr::progress_estimated(dim(urls)[1])
        lapply(1:dim(urls)[1], function(i) {
            count <<- count + 1
            filename = urls[i, "name_file"]
            tictoc::tic(paste0(urls[i, "name_file"], " : ", count, "/", dim(urls)[1]))  #progress bar to follow the evolution of the downloads
            # Download every documents and overwrite existing
            if (overwrite == T) {
                file <- try(download.file(urls[i, "pdf"], destfile = paste0(export_path, 
                  "/", urls[i, "name_file"], ".pdf")), silent = T)
                if ("try-error" %in% class(file)) {
                  # handle errors in the url or errors in the download of the file
                  cat(crayon::red(paste(urls[i, "name_file"], ": Error in path file: ", 
                    urls[i, "pdf"], sep = "")))
                  log[urls[i, "name_file"]] = "Error on download"
                  file = NA
                } else {
                  log[urls[i, "name_file"]] = "Download completed"
                  cat(crayon::green(paste(urls[i, "name_file"], ": succesfully downloaded", 
                    sep = "")))
                }
            } else {
                # Download only missing documents
                if (!file.exists(paste0(export_path, "/", urls[i, "name_file"], 
                  ".pdf"))) {
                  file <- try(download.file(urls[i, "pdf"], destfile = paste0(export_path, 
                    "/", urls[i, "name_file"], ".pdf")), silent = T)
                  if ("try-error" %in% class(file)) {
                    # handle errors in the url or errors in the download of the file
                    cat(crayon::red(paste(urls[i, "name_file"], ": Error in path file: ", 
                      urls[i, "pdf"], sep = "")))
                    log[urls[i, "name_file"]] = "Error on download"
                    file = NA
                  } else {
                    log[urls[i, "name_file"]] = "Download completed"
                    cat(crayon::green(paste(urls[i, "name_file"], ": succesfully downloaded \n", 
                      sep = "")))
                  }
                } else {
                  log[urls[i, "name_file"]] = "Old version kept"
                  cat(crayon::blue(paste(urls[i, "name_file"], ": already downloaded, keep existing \n", 
                    sep = "")))
                }
            }
            progress$pause(0.01)$tick()$print()
            tictoc::toc()
            file
        })
        # export a log on the details of the extraction
        rio::export(log, paste0("log_downloads_", Sys.time(), ".RData"))
        cat(crayon::green(paste0("urls succesfully downloaded in '", export_path, 
            "\n'")))
        
        # print('urls succesfully downloaded')
    } else {
        cat(crayon::red("Please provide a valid data.frame of with at least two columns: name_file (name of your file) and pdf (the url link) \n"))
        
    }
    
}
