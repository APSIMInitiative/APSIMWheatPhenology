# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   12/08/2010
# *


#' Generate the template file for REMS from APSIM simulation file
#' 
#' This function is used to convert the apsim file to REMS template
#' file according to the description of a command file. The command
#' file is the csv format. In the command file, first column is the 
#' line number which need to deal with; the second column is the old
#' value in the apsim file; the third column is the new value in the 
#' REMS template file. If the old and new value are empty, this row
#' will be deleted from output file. If the old value is empty, but 
#' the new value is not empty, a new line will be added before this
#' line. If the old and new value are not empty, the old value will
#' be replaced by the new value. 
#' @param sim The file name of apsim
#' @param com The file name of commands
#' @param output The output file name 
#' @examples
#' \dontrun{ sim2tem("test.apsim","test.csv") }
sim2tem <- function( sim, com, output = "output.tem" )
{    
    commands <- read.table( com, 
            header = FALSE, 
            as.is = TRUE, 
            skip = 1,
            sep = ",",
            comment.char = "",
            na.strings = "NA",
            col.names = scan( com, "", 
                    sep = ",", 
                    skip = 0, nline = 1 ) )
    clen <- 0
    newcommands <- NULL
    for ( i in 1:length( commands$line ) )
    {
        str <- strsplit( as.character( commands$line[i] ), "-" )
        str <- str[[1]]
        if ( length( str ) > 1 )
        {
            start <- str[1]
            end <- str[2]
            for ( j in start:end )
            {
                clen <- clen + 1
                newcommands$line[clen] <- j
                newcommands$old[clen] <- ""
                newcommands$new[clen] <- ""
            }
        }
        else
        {
            clen <- clen + 1
            newcommands$line[clen] <- commands$line[i]
            newcommands$old[clen] <- commands$old[i]
            newcommands$new[clen] <- commands$new[i]
            
        }
    }
    commands <- newcommands
    commands <- as.data.frame( commands )
    commands$line <- as.numeric(as.vector(commands$line))
    commands$old <- as.character(as.vector(commands$old))
    commands$new <- as.character(as.vector(commands$new))
    apsim <- readLines( sim, warn = FALSE )
    results <- NULL
    rownum <- 1
    for ( i in 1:length(apsim) )
    {
        pos <- commands$line %in% i 
        command <- commands[pos,]
        commands <- commands[!pos,]
        if ( length(command$line) > 0 )
        {
            for ( j in 1:sum( pos ) )
            {
                if ( command$old[j] == "" & !( command$new[j] == "" ) )
                {
                    if ( sum( pos ) == 1 )
                    {
                        results[rownum] <- apsim[i]
                        rownum <- rownum + 1
                    }
                    results[rownum] <- command$new[j]
                    rownum <- rownum + 1                    
                }
                else if ( !( command$old[j] == "" ) & !( command$new[j] == "" ) )
                {
                    results[rownum]  <- sub( command$old[j], command$new[j], apsim[i], fixed = TRUE )
                    rownum <- rownum + 1
                }
            }
        }
        else
        {
            results[rownum] <- apsim[i]
            rownum <- rownum + 1
        }
    }
    write.table( results, file = output, 
            quote = FALSE,
            row.names = FALSE,
            col.names = FALSE )
}
