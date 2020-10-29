# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   17/05/2010
# *

#' Read observed data from file
#' 
#' The codes will skip first two lines of the observed data file.
#' The third line of this file should be the name of each column.
#' The fourth line of this file should be the unit of each column.
#' The observed data must be started from the fifth line. 
#' The missing data should be indicated by * not "0".
#' @param obsFile The absolute or relative file path to the observed data
#' @return A data frame of observed data
readobs <- function( obsFile = NULL )
{
    if ( is.null( obsFile[1] ) )
    {
        warning( "Please specify the file path of observed data. A NULL object will be retured.", 
                call. = FALSE )
        return( NULL )
    }
    if ( !file.exists( obsFile[1] ) )
    {
        warning( paste( "Can not find file with name ", obsFile,
                        ". A NULL object will be retured.", 
                        sep = "" ), 
                call. = FALSE )
        return( NULL )
    }
    
    temp <- readLines( obsFile, n = 100 )
    start.line <- grep( "ExpID", temp )
    if ( start.line < 1 )
    {
        warning( paste( "Can not find ExpID in the observed file. ",
                        "Please check your data.", 
                        sep = "" ), 
                call. = FALSE )
        return( NULL )
    }
    obs <- read.table( obsFile, sep = "", header = FALSE, as.is = TRUE, skip = start.line + 1,
            col.names = scan( obsFile, "", sep = "", skip = start.line - 1, nline = 1 ) )

    return( obs )
}

#' Read simulated data of APSIM from file
#' 
#' The head of the simulated could contain some comment information (less than 100 lines).
#' The comment line must format as "variable = value". The codes read all lines, 
#' then add to simulated data.
#' The first line after comments should be the name of each column.
#' The second line after comments should be the unit of each column.
#' The simulated data must be started from the third line after comments. 
#' If simFolders was specified, the file name of data must be like this 
#' "file_name-suffix.out"
#' @param simFiles A vector of files of simulated results of APSIM
#' @param simFolders A vector of folders of simulated results of APSIM
#' @param simSuffix The suffix of files of simulated results of APSIM
#' @return A data frame of simulated data
readsim <- function( simFiles = NULL, simFolders = NULL, simSuffix = "outputfile_daily" )
{
    fileList <- NULL
    if ( !is.null( simFiles ) )
    {
        for ( i in 1:length( simFiles ))
        {
            if ( !file.exists( simFiles[i] ) )
            {
                warning( paste( "Can not find file with name ", simFiles[i], sep = "" ), 
                        call. = FALSE )
            }
            fileList <- c( fileList, simFiles[i] )
        }
    }
    if ( !is.null( simFolders ) )
    {
        for ( i in 1:length( simFolders ))
        {
            
            list.file <- list.files( simFolders[i], pattern = ".out$" )
            list.file <- list.file[ grep( ( simSuffix ), list.file ) ]
            if ( length( list.file ) == 0 )    
            {
                warning( paste( "Can not find and simulated files of APSIM in folder ", simFolders[i], sep = "" ), 
                        call. = FALSE )
                next()
            }
            fileList <- c( fileList, file.path( simFolders[i], list.file ) )
        }
    }
    if ( is.null( fileList ) )
    {
        warning( paste( "Can not find any simulated files of APSIM. ",
                        "A NULL object will be retured.", 
                        sep = "" ), 
                call. = FALSE )
        return( NULL )
    }
    
    sim <- NULL
    c <- 0
    for ( f in fileList )
    {
        c <- c + 1
        n <- sub( " outputfile_daily.out", "", f )
        
        temp <- readLines( f, n = 100 )
        start.line <- grep( "DaysAfterSowing", temp )
        
        if ( start.line < 1 )
        {
            warning( paste( "Can not find DaysAfterSowing in the observed file. ",
                            "Please check your data.", 
                            sep = "" ), 
                    call. = FALSE )
            next()
        }
        
        intermediaire <- read.table( f, sep = "", skip = start.line + 1, header = FALSE,
                col.names = scan( f, "", sep = "", skip = start.line - 1, nline = 1 ), as.is = TRUE )

        intermediaire$filename <- basename( f )
        
        for ( i in 1:100 )
        {
            pos <- searchChar( temp[i], "=" )
            if ( is.null( pos ) )
            {
                break()
            }
            com <- splitEqual( temp[i] )
            
            intermediaire[[com$name]] <- com$value
        }
        
        if ( is.null( intermediaire$site ) | is.null( intermediaire$genotype ) )
        {
            warning( paste( "Can not find the site and/or genotype in the file \n",
                            f, ".\n Please double check the output data. \n",
                            "This simulated file was ommited.",sep = "" ),
                    call. = FALSE )
        }
        else
        {
            sim <- rbind( sim, intermediaire )
        }
    }
    return( sim )
}

