rootDir <- "docDB"

# Main Function
main <- function(){
  
  #1. Testing with populated folder (copying and reseting DB)
  test_folder_name <- "test_folder"
  if (!dir.exists(test_folder_name)) {
    dir.create(test_folder_name)
  }
  file.create(file.path(test_folder_name, "CampusAtNight.jpg#Northeastern#ISEC"))
  file.create(file.path(test_folder_name, "foo#bar.jpg"))
  file.create(file.path(test_folder_name, "CampusAtNight#Northeastern#ISEC.jpg"))
  file.create(file.path(test_folder_name, "CampusAtDay#ISEC#2022.png"))
  file.create(file.path(test_folder_name, "CampusAtDay.png#Northeastern#2022"))
  file.create(file.path(test_folder_name, "CampusNotes#ISEC#2022.txt"))
  file.create(file.path(test_folder_name, "CampusSounds.mp3#Northeastern#Soundtracked"))
  
  #setup DB
  configDB(rootDir,"")
  #setup tag folders and copy files
  storeObjs(test_folder_name, rootDir, TRUE)
  #reset DB
  clearDB(rootDir)
  
  #2. Testing with populated folder (only copying)
  test_folder_name <- "test_folder2"
  if (!dir.exists(test_folder_name)) {
    dir.create(test_folder_name)
  }
  file.create(file.path(test_folder_name, "CampusAtNight.jpg#Northeastern#ISEC"))
  file.create(file.path(test_folder_name, "CampusAtNight#Northeastern#ISEC.jpg"))
  file.create(file.path(test_folder_name, "CampusAtDay#ISEC#2022.png"))
  file.create(file.path(test_folder_name, "CampusAtDay.png#Northeastern#2022"))
  file.create(file.path(test_folder_name, "CampusNotes#ISEC#2022.txt"))
  file.create(file.path(test_folder_name, "CampusSounds.mp3#Northeastern#Soundtracked"))
  
  #setup tag folders and copy files
  storeObjs(test_folder_name, rootDir, TRUE)
  
  #3. Testing with empty folder
  # warning printed "No files to copy"
  test_folder_name <- "test_folder3"
  if (!dir.exists(test_folder_name)) {
    dir.create(test_folder_name)
  }

  storeObjs(test_folder_name, rootDir, TRUE)
  
  #4. Testing moving files to non-existent directory
  #execution will stop with a warning
  test_folder_name <- "test_folder4"
  if (!dir.exists(test_folder_name)) {
    dir.create(test_folder_name)
  }
  file.create(file.path(test_folder_name, "CampusAtNight.jpg#Northeastern#ISEC"))
  
  rootDir2 <- "docDB2"
  storeObjs(test_folder_name, rootDir2, TRUE)
}

# Sets up folders and database related structure
# @param root Directory in which all tag folders will be stored
# @param path Project folder path
configDB <- function(root, path){
  #checks if parent path exists
  #if blank create DB in project folder
  #if not blank create DB under the provided parent path
  directory_path <- ifelse(path == "", root, file.path(path,root))
  if(!dir.exists(directory_path)){
    dir.create(directory_path)
  }
}

# Returns the correctly generated path to a tag folder
# @param root Parent directory
# @param tag Filename tag
# @return Path to a tag folder
genObjPath <- function(root, tag){
  
  if (!dir.exists(root)) {
    str <- sprintf("The directory %s does not exist!\n", root)
    cat(str)
    quit()
  }
  
  tag_directory_path <- file.path(root, tag)
  if(!dir.exists(tag_directory_path)){
    dir.create(tag_directory_path)
    }
  return(tag_directory_path)
}

# Returns a vector of tags in the file name
# @param filename Filename
# @return Vector of tags
getTags <- function(filename){
  #call pruneFileName function and redact the first keyword (filename with extension)
  return(pruneFileName(filename)[-1])
}

#' Returns filename in the file name
#' @param filename Filename
#' @return Filename string
getFileName <- function(filename){
  #call pruneFileName function and return only first keyword (filename with extension)
  return(pruneFileName(filename)[1])
}

# Helper function for getTags and getFileName
# @param filename Filename
# @return Vector of filename keywords
pruneFileName <- function(filename){
  #split file path keywords by / and flatten list into vector
  file_path_keywords <- unlist(strsplit(filename, split="/"))
  #retrieve filename from vector (last keyword in file path)
  name <- tail(file_path_keywords, n=1)
  #remove whitespaces from filename
  name <- gsub("\\s", "", name)
  #retrieve file extension from filename
  extension <- tools::file_ext(name)
  #split filename by # into seperated name and tags
  filename_keywords <- unlist(strsplit(name, "#"))
  #store seperated filename in variable
  file <- filename_keywords[1]
  #remove extension after dot for keyword with extension
  filtered_keywords <- gsub("\\.[a-zA-Z0-9]+$", "", filename_keywords)
  #recreate filename with previously stored extension
  file_with_extension <- paste0(file, ".", extension)
  #if filename keyword already has extension then dont replace but if filename keyword
  # does not have extension then replace with newly created keyword with extension
  filtered_keywords[[1]] <- ifelse(!endsWith(file, extension), file_with_extension, file)
  # return full filename keywords (filename with extension, tags)
  return(filtered_keywords)
}

# Copies all files in the specified folder to their correct folders under the root folder
# @param folder Source folder
# @param root Destination parent folder
# @param print Logging boolean
# @return Vector of filename keywords
storeObjs <- function(folder, root, print){
  #retrieve files from folder
  #retrieve full file path for help with moving file between dirs
  files <- list.files(folder, full.names=TRUE)
  
  #emtpy folder check
  if(length(files) == 0){
    str <- sprintf("No files to copy.\n")
    cat(str)
  }else{
    #iterate through all files
    for(file in files){
      #retrieve file tags
      file_tags = getTags(file)
      #retrieve file name
      file_name = getFileName(file)
      
      #handle logging if boolean arg passed
      if(print){
        str <- sprintf("Copying %s to %s.\n", file_name,paste(file_tags, collapse=","))
        cat(str)
      }
      
      #iterate through a files associated tags
      for (tag in file_tags){
        #generates path to a tag folder from parent root folder
        tag_directory <- genObjPath(root, tag)
        #full tag folder path
        tag_directory_path <- normalizePath(tag_directory)
        #file source 
        source_path <- file
        #file destination (with tag folder path and filename)
        destination_path <- file.path(tag_directory_path, file_name)
        #copy file from source to destination
        success <- file.copy(source_path, destination_path)
      }
    }
  }
}

# Removes all folders and files in the folder specified by root
# @param root Source folder
clearDB <- function(root){
  #checks files in directory
  #full names argument retrieves full file paths
  #recursive argument handles nested files
  files <- list.files(root, full.names=TRUE, recursive=TRUE)
  
  #empty directory check before delete operation
  if(length(files) > 0){
    invisible(file.remove(files))
  }
}

main()
quit()
