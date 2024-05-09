## Code modified from http://spatialreasoning.com/wp/20170307_1244_r-reading-filtering-weather-data-from-the-global-historical-climatology-network-ghcn
data_dir <- here("data", "raw", "precip", "ghcnd_nyc")
fns <- list.files(data_dir, full.names=TRUE, pattern=".dly")
n_files <- length(fns)

for (i in 1:n_files) {
  infile <- fns[[i]]
  ## substitute the file extension with .csv
  outfile <- gsub(".dly", ".csv", infile)
 
#Each record in a file contains one month of daily data.  The variables on each
#line include the following:
#
#------------------------------
#Variable   Columns   Type
#------------------------------
#ID            1-11   Character
#YEAR         12-15   Integer
#MONTH        16-17   Integer
#ELEMENT      18-21   Character
#VALUE1       22-26   Integer
#MFLAG1       27-27   Character
#QFLAG1       28-28   Character
#SFLAG1       29-29   Character
#VALUE2       30-34   Integer
#MFLAG2       35-35   Character
#QFLAG2       36-36   Character
#SFLAG2       37-37   Character
#  .           .          .
#  .           .          .
#  .           .          .
#VALUE31    262-266   Integer
#MFLAG31    267-267   Character
#QFLAG31    268-268   Character
#SFLAG31    269-269   Character
#------------------------------

    
    cols <- c( "A11", "I4", "I2", "A4",
            rep( c( "I5", "A1", "A1", "A1"), 31) )
    df <- read.fortran(infile, cols, na.strings="-9999") # -9999 indicates missing data

    # next, fill in the column names
    tmp <- c("Val","xxM","xxQ","xxS") # xx so we can ditch them later
    vhdrs <- paste(   rep(tmp,31),   rep(1:31,each=4), sep="")
    hdrs <- c("ID", "year", "month", "element", vhdrs)
    names(df) <- hdrs
    df <- df[df$year >= 1936 & df$year <= 2024,]
    df_out <- dplyr::select(df, -matches("xx*")) # get rid of M, Q, S 
    write.csv(df_out, outfile)
}
