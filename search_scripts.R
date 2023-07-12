files <- list.files('./scripts',pattern = "\\.R$")



keyword <- "night"  # Replace with your desired keyword

folder_path <- "/Users/najah/work/cpr"

# Call the recursive function to get a list of R scripts in the folder and subfolders
files <- list_files_recursive(folder_path)
files

# Now you can perform actions on the files
for (file in files) {
  script_contents <- readLines(file)
  
  if (any(grepl(keyword, script_contents))) {
    # Print or perform actions on the file containing the keyword
    print(file)
  }
}

# File path
file_path <- "/Users/najah/work/cpr/cpr-pol/static/dynasties/old-posts/families-cat2.Rmd"

# Open the file in RStudio
file.edit(file_path)

