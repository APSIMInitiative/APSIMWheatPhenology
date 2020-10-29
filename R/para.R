# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   17/05/2010
# *

#' Get the all/a default parameter value
#' 
#' @param para.name The paramer with this name will be returned
#' @return The all default parameter will be returned if the para.name is not specified, or
#' just the default paramter whose name is para.name
defaultPara <- function( para.name = NULL )
{
    dp <- list(
            outputFormat = "pdf",
            outputPrefix = "",
            isOutputObsData = FALSE,
            identifiers = list(
                    sim = c( "genotype", "site", "date", 
                            "DaysAfterSowing", "cumTT", "filename", 
                            "year" ),
                    label = c( "genotype", "site", "date", 
                            "Days After Sowing", "Thermal Time", "filename", 
                            "year" ),
                    unit = c( "(-)", "(-)", "(-)", "(d)",                             
                            ~"("*degree*"C)", "(-)", "(-)" ) ),
            variables = paste(.find.package("RApsim"),"/data/variables.csv",sep=""),
            
            graph.combinations = list(
                    graph = list( axis = list( y1 = "grain_size",
                                    y2 = c( "grain_no" ),
                                    y3 = c( "stage" ) ),
                            title = "Yield Components" ),
                    graph = list( axis = list( y1 = c( "n_uptake" ), 
                                    y3 = "stage" ),
                            title = "N Status" ),
                    graph = list( axis = list( y1 = c( "biomass", "yield" ), 
                                    y2 = c( "leafgreenwt", 
                                            "leafsenescedwt", 
                                            "roottotalwt",
                                            "stemtotalwt",
                                            "paniclewt" ),
                                    y3 = c( "stage" ) ) ,
                            title = "Biomass Partitionning" ),
                    graph = list( axis = list( y1 = "sdr", 
                                    y2 = c( "irrigation", "rain" ),
                                    y3 = c( "stage" ) ) ,
                            title = "Water Status" ),
                    graph = list( axis = list( y1 = "lai",
                                    y3 = "stage" ),
                            title = "Phenology" ),
                    graph = list( axis = list( y1 = c( "maxt", "mint" ),
                                    y2 = c( "radn" ),
                                    y3 = c( "stage" )),
                            title = "Weather Status" )
            ), 
            
            
            placeholder = NA # a placeholder to conveniently add new parameters
    )
    if ( !is.null( para.name ) )
    {
        return( dp[[para.name]] )
    }
    else
    {
        return( dp )
    }
}

#' Set/Change some parameters
#' 
#' The validity of some parameters have been checked. only columns, which 
#' were indicated in "identifiers" and "variables", would be used. Any other
#' data would been ignored. "graph.combinations" would be used to design the 
#' graph which compare simulated and observed data.
#' @param p The old parameter list. The default parameter would be used
#' if p was not specified.
#' @param identifiers A list of the column names to identify each data.
#' This parameter must have 3 elements: "sim, label, unit", and the three
#' element must have the same length. The unit element could be expression
#' for the complicated unit of variables. The sim should be from the simulated
#' files and the observed file should contain the same columns. 
#' @param variables the file path to a csv format file which lists 
#' the compared column names of observed and simulated data.
#' This file must have 6 column: "sim, obs, label, unit, col, pch" 
#' and the six column must have the same length. The unit column could be 
#' expression for the complicated unit of variables.
#' @param graph.combinations A list to describe the graph which compare
#' the simulated and observed data.
#' @param outputFormat The format of output files. Currently only pdf format is supported
#' @param outputPrefix The prefix of output files. A new folder will be created if the last character
#' @param isOutputObsData Whether output observed data
#' is "/" or "\" 
setpara <- function( p = NULL, 
        outputFormat = defaultPara( "outputFormat" ), 
        outputPrefix = defaultPara( "outputPrefix" ),
        isOutputObsData = defaultPara( "isOutputObsData" ),
        identifiers = defaultPara( "identifiers" ),
        variables = defaultPara( "variables" ),
        graph.combinations = defaultPara( "graph.combinations" ) )
{
    
    if ( is.null( p ) )
    {
        p <- defaultPara()
    }
    
    # for outputFormat
    if ( is.null( p$outputFormat ) )
    {
        p$outputFormat <- defaultPara( "outputFormat" )
    }    
    if ( checkPara( "character", "outputFormat", outputFormat, support = c( "pdf" ) ) )
    {
        p$outputFormat <- outputFormat[1]
    }
    
    # for outputPrefix
    if ( is.null( p$outputPrefix ) )
    {
        p$outputPrefix <- defaultPara( "outputPrefix" )
    }    
    if ( checkPara( "path", "outputPrefix", outputPrefix ) )
    {
        p$outputPrefix <- outputPrefix[1]
    }
    
    # for isOutputObsData
    if ( is.null( p$isOutputObsData ) )
    {
        p$isOutputObsData <- defaultPara( "isOutputObsData" )
    }    
    if ( !is.na( as.logical( isOutputObsData ) ) )
    {
        p$isOutputObsData <- as.logical( isOutputObsData[1] )
    }
    
    # for identifiers
    if ( is.null( p$identifiers ) )
    {
        p$variables <- defaultPara( "identifiers" )
    }    
    if ( !is.null( identifiers ) )
    {
        w <- FALSE
        if ( length( identifiers ) != 3 )
        {
            w <- TRUE
        }
        else if ( is.null( identifiers$sim ) |
                is.null( identifiers$label ) | 
                is.null( identifiers$unit ) )
        {
            w <- TRUE
        }
        else
        {
            len <- length( identifiers$sim )
            if ( length( identifiers$label ) != len |
                    length( identifiers$unit ) != len )
            {
                w <- TRUE
            }
        }
        
        if ( w == TRUE )
        {
            warning( paste( "variables: \"",  
                            vector2string( identifiers ), 
                            "\" was not supported. The default value \"", 
                            vector2string( defaultPara( "identifiers" ) ), 
                            "\" or \"original\" value was used.", sep = "" ), 
                    call. = FALSE )
        }
        else
        {
            p$identifiers <- identifiers
        }
    }
    
    
    # for variables
    if ( is.null( p$variables ) )
    {
        p$variables <- defaultPara( "variables" )
    }
    if ( !file.exists( variables ) )
    {
        warning( paste( "variables: \"",  
                        vector2string( variables ), 
                        "\" was not supported. The default value \"", 
                        vector2string( defaultPara( "variables" ) ), 
                        "\" or \"original\" value was used.", sep = "" ), 
                call. = FALSE )
        if ( !file.exists( variables ) )
        {
            stop( paste( "\" was not supported. The default value of variables\"", 
                            vector2string( defaultPara( "variables" ) ), 
                            "\" is not existed.", sep = "" ), 
                    call. = FALSE )
        }
        else
        {
            p$variables <- defaultPara( "variables" )
        }
    }
    else
    {
        p$variables <- variables
    }
    
    p$variables <- read.table( p$variables, 
            header = FALSE, 
            as.is = TRUE, 
            skip = 1,
            sep = ",",
            comment.char = "",
            na.strings = "NA",
            col.names = scan( p$variables, "", 
                    sep = ",", 
                    skip = 0, nline = 1 ) )
    
    # for graph.combinations
    if ( is.null( p$graph.combinations ) )
    {
        p$graph.combinations <- defaultPara( "graph.combinations" )
    }    
    else if ( !is.null( graph.combinations ) )
    {
        p$graph.combinations <- graph.combinations
    }
    return( p )
}

#' Check parameter value whether is legal for the request of this library
#'
#' Check parameter value whether is legal for the request of this library
#' @param type The parameter type
#' @param key The parameter name
#' @param value The parameter value which could be any type
#' @param support A vector of the supported value of parameter key
#' @param prange The range of parameter key if value is a numeric and the length of value equal to one
#' @param len The legal length of parameter key if the length of value is more than one  
#' @return True if the value is legal for parameter key
checkPara <- function( type, key, value, 
        support = NULL, 
        prange = NULL,
        len = 2 )
{
    if ( is.null( value ) )
    {
        return( FALSE )
    }
    if ( type == "normal" )
    {
        return( checkParaNormal( key, value, support, prange, len ) )
    }
    else if ( type == "character" )
    {
        return( checkParaCharacter( key, value, support ) )
    }
    else if ( type == "list" )
    {
        return( checkParaList( key, value, support ) )
    }
    else if ( type == "path" )
    {
        return( checkParaPath( key, value ) )
    }
    else if ( type == "vector" )
    {
        return( checkParaVector( key, value, len ) )
    }
    
}

#' Check parameter value of normal type whether is legal for the request of this library
#'
#' Check parameter value whether is legal for the request of this library
#' @param key The parameter name
#' @param value The parameter value which could be any type
#' @param support A vector of the supported value of parameter key
#' @param prange The range of parameter key if value is a numeric and the length of value equal to one
#' @param len The legal length of parameter key if the length of value is more than one  
#' @return True if the value is legal for parameter key
checkParaNormal <- function( key, value, 
        support = NULL, 
        prange = NULL,
        len = 2 )
{
    is.newvalue <- FALSE
    if ( is.numeric( value ) )
    {
        if ( length( value ) == 1 )
        {
            if ( length( prange ) %% 2 != 0 )
            {
                warning( "The length of range should be even. The last number of range was omitted.", call. = FALSE )
            }
            ranged <- FALSE
            for ( i in 1:( length( prange ) / 2 ) )
            {
                if ( value >= prange[ 2 * i - 1 ] & value <= prange[ 2 * i ] )
                {
                    ranged <- TRUE
                    break
                }
            }
            if ( ranged )
            {
                is.newvalue <- TRUE
            }
        }
        else
        {
            if ( length( value ) == len )
            {
                is.newvalue <- TRUE
            }
        }
    }
    else
    {
        if ( sum( value == support ) )
        {
            is.newvalue <- TRUE
        }
    }
    if ( !is.newvalue )
    {
        warning( paste( key, ": \"",  vector2string( value ), "\" was not supported. The default value \"", vector2string( defaultPara( key ) ), "\" or \"original\" value was used.", sep = "" ), call. = FALSE )
    }
    return( is.newvalue )    
}

#' Check parameter value of list type whether is legal for the request of this library
#'
#' Check parameter value whether is legal for the request of this library
#' @param key The parameter name
#' @param value The parameter value which could be any type
#' @param support A vector of the supported value of parameter key
#' @return True if the value is legal for parameter key
checkParaList <- function( key, value, 
        support = NULL )
{
    is.newvalue <- TRUE
    
    if ( length( value ) != length(support) )
    {
        is.newvalue <- FALSE
    }
    
    for ( i in 1:length( support ) )
    {
        if ( is.null( value[[support[i]]]) )
        {
            is.newvalue <- FALSE
            break
        }
    }
    if ( !is.newvalue )
    {
        warning( paste( key, ": \"",  vector2string( value ), "\" was not supported. The default value \"", vector2string( defaultPara( key ) ), "\" or \"original\" value was used.", sep = "" ), call. = FALSE )
    }
    return( is.newvalue )    
}

#' Check parameter value of character type whether is legal for the request of this library
#'
#' Check parameter value whether is legal for the request of this library
#' @param key The parameter name
#' @param value The parameter value which could be any type
#' @param support A vector of the supported value of parameter key 
#' @return True if the value is legal for parameter key
checkParaCharacter <- function( key, value, 
        support = NULL )
{
    is.newvalue <- FALSE
    if ( sum( value == support ) )
    {
        is.newvalue <- TRUE
    }
    if ( !is.newvalue )
    {
        warning( paste( key, ": \"",  vector2string( value ), "\" was not supported. The default value \"", vector2string( defaultPara( key ) ), "\" or \"original\" value was used.", sep = "" ), call. = FALSE )
    }
    return( is.newvalue )    
}


#' Check parameter value of path type whether is legal for the request of this library
#'
#' Check parameter value whether is legal for the request of this library
#' @param key The parameter name
#' @param value The parameter value which could be any type
#' @return True if the value is legal for parameter key
checkParaPath <- function( key, value )

{
    is.newvalue <- FALSE
    
    nvalue <- value
    if ( !is.null( nvalue ) && ( right( nvalue ) == "/" | right( nvalue ) == "\\" ) )
    {
        nvalue <- left( nvalue, len( nvalue ) - 1 )
    }
    randpath <- paste( runif( 1, 100000.0, 999999.9 ), nvalue, sep = "" )
    s <- file.create( randpath, showWarnings = FALSE )
    if ( s )
    {
        is.newvalue <- TRUE
        file.remove( randpath, recursive = TRUE )
    }
    else
    {
        warning( paste( key, ": \"",  vector2string( value ), "\" was not supported. The default value \"", vector2string( defaultPara( key ) ), "\" or \"original\" value was used.", sep = "" ), call. = FALSE )
    }
    return( is.newvalue )    
}

#' Check parameter value of vector type whether is legal for the request of this library
#'
#' Check parameter value whether is legal for the request of this library
#' @param key The parameter name
#' @param value The parameter value which could be any type
#' @param len The legal length of parameter key if the length of value is more than one  
#' @return True if the value is legal for parameter key
checkParaVector <- function( key, value, len = 2 )
{
    is.newvalue <- FALSE
    if ( len == Inf | length( value ) == len )
    {
        is.newvalue <- TRUE
    }
    
    if ( !is.newvalue )
    {
        warning( paste( key, ": \"",  vector2string( value ), "\" was not supported. The default value \"", vector2string( defaultPara( key ) ), "\" or \"original\" value was used.", sep = "" ), call. = FALSE )
    }
    return( is.newvalue )    
}

