# --- 

create_data_list <- function(source_file){
  sources <- read_yaml(source_file)
  return(sources)
}

# exemple
# a <- create_data_list("sources.yml")
