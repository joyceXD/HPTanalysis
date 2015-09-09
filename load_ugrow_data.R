library("xlsx")

#----------------------------------------------------------------------------------#
#                            import baby info data                                 #
#----------------------------------------------------------------------------------#


# define baby information file name
baby.file.path <- "./data/ID_Birth data baby.xlsx"
BabyAge <- read.xlsx(baby.file.path, 
                     sheetIndex = 1,
                     as.data.frame = TRUE,
                     stringsAsFactors = FALSE,
                     header = TRUE)

# remove all the redundant data entries
BabyAge <- BabyAge[-c(23:nrow(BabyAge)),]
colnames(BabyAge) <- c("UserIDExport", 
                       "FirstTimeParents", 
                       "BornWeek",
                       "BabyID")

rm(baby.file.path)


#----------------------------------------------------------------------------------#
#                              import tracker data                                 #
#----------------------------------------------------------------------------------#

# set feeding data directory path
# can be replaced by database connections in the future
filepath <- "./data/"

# retrieve all feeding data csv files in this folder
read_file_list <- function(filepath){
  filelist <- list.files(path = filepath, 
                         all.files = FALSE,
                         full.names = TRUE,
                         pattern = "*.csv",
                         include.dirs = FALSE)
  return(filelist)
}

filelist <- ReadFileList(filepath)


# read different tracker data into different data frames
# the name of the data frame is the same as the file name

for(i in 1:length(filelist)){
  dfname = strsplit(basename(filelist[i]), '[.]')[[1]][1]
  assign(dfname, 
         read.csv(filelist[i], 
                  stringsAsFactors = FALSE,
                  header = TRUE))
}

# remove the redundant variables
rm(dfname, filelist, filepath, i)

