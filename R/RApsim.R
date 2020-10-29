# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   17/05/2010
# *

#' This the main function to compare the simulated and observed data.
#' 
#' This package was originally designed to compare the observed data
#' and simulated results of Apsim. But you could use this package to 
#' compare any observed and simulated data. All figures created by 
#' RApsim are flexible. You can design them through several parameters.
#' only columns, which were indicated in "identifiers" and "variables", 
#' would be used. Any other data would been ignored. "graph.combinations" 
#' would be used to design the graph which compare simulated and observed data.
#' You can used "xvar" to set which data from "identifiers" was drawn in the 
#' x-axis.
#' @param ... The parameters of setpara functions
#' @param para The parameter list. See defaultPara for more detailed information.
#' @param obsFile The absolute or relative file path to the observed data
#' @param simFiles A vector of files of simulated results of APSIM
#' @param simFolders A vector of folders of simulated results of APSIM
#' @param simSuffix The suffix of files of simulated results of APSIM
#' @param xvar The variable will be put into the x-axis
#' @examples
#' # Set met files and/or met folders
#' \dontrun{ RApsim( simFolders = "path-to-simfolder", obsFile = "path-to-obsfiles" ) }
#' # Set a specific parameter
#' \dontrun{ RApsim( simFolders = "path-to-simfolder", obsFile = "path-to-obsfiles", outputPrefix = "output" ) }
#' # Using thermal time which was drawn in x-axis
#' \dontrun{ RApsim( simFolders = "path-to-simfolder", obsFile = "path-to-obsfiles", xvar = "cumTT" ) }
#' @seealso See \code{\link{setpara}} for avaiable parameters of RApsim. 
#' \code{\link{readobs}} for the file format of observed files.
#' \code{\link{readsim}} for the file format of simulated files. 
RApsim <- function( 
        para = NULL, 
        obsFile = NULL, 
        simFiles = NULL, 
        simFolders = NULL,
        simSuffix = "outputfile_daily",
        xvar = "DaysAfterSowing", ... )
{    
    para <- setpara( p = para, ... )

    # Create a new folder if the para$outputPrefix is a folder name
    if ( !is.null( para$outputPrefix ) && ( right( para$outputPrefix ) == "/" | right( para$outputPrefix ) == "\\" ) )
    {
        dir.create( para$outputPrefix, showWarnings = FALSE )
    }    
    
    # Read data
    sim <- readsim( simFiles, simFolders, simSuffix )
    obs <- readobs( obsFile )
    
    # Combine Data
    sim.obs <- combineObsSim( obs, sim, 
            para$variables, 
            para$identifiers,
            para$isOutputObsData )
    
    # Plot the data
    all.plots <- NULL
    all.plots[[length( all.plots ) + 1]] <- 
            plotObsSimCompare( sim.obs, para$identifiers, para$variables )
    
    com.plots <- NULL
    if ( !is.null( sim.obs[[xvar]] ) & xvar %in% para$identifiers$sim )
    {
        xlab <- "Days After Sowing (d)"
        com.plots <- plotGraphCombinations( sim.obs, 
                graph.combinations = para$graph.combinations, 
                identifiers = para$identifiers,
                variables = para$variables,
                xvar = xvar,
                isOutputObsData = para$isOutputObsData )
        
        plots.num <- length( all.plots )
        for ( i in 1:length( com.plots ) )
        {
            plots.num <- plots.num + 1
            all.plots[[plots.num]] <- com.plots[[i]]
        }
    }

    filename <- NULL
    if ( para$outputPrefix == "" )
    {
        filename <- paste( "graph.simul-observed", ".", para$outputFormat, sep = "" )
    }
    else
    {
        filename <- paste( para$outputPrefix, ".", "graph.simul-observed", ".", para$outputFormat, sep = "" )
    }

    if ( para$outputFormat == "pdf" )
    {
        pdf( filename, paper = "a4", width = 8, height = 11, onefile = TRUE )
        for ( i in 1:length( all.plots ) )
        {
            print( all.plots[[i]] )
        }
        dev.off()
    }

}
