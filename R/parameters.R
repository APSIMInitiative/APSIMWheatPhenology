# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   20/03/2011
# *

cacheEnv <- new.env()

#' read a parameter from sim or module file
#'
#' @param name The parameter name
#' @param ... Other arguments to specify sim file and module file
#' @export
readPara <- function( name, ... )
{
    # if rapsim.glb.para is not existed, create a new global variable
    other.args <- list( ... )
    sim <- other.args$sim

    if ( !exists( "rapsim.glb.para", envir = cacheEnv ) | !is.null( sim ) )
    {
        if ( !file.exists( sim ) )
        {
            stop( paste( sim, " not existed.", sep = "" ) )
        }
        rapsim.glb.para <- XML::xmlInternalTreeParse( sim )
        assign( "rapsim.glb.para", rapsim.glb.para, envir = cacheEnv )
    } else {
        rapsim.glb.para <- get("rapsim.glb.para", envir=cacheEnv)
    }

    # if ( !file.exists( sim ) )
    # {
    #     stop( paste( sim, " not existed.", sep = "" ) )
    # }
    # assign( "rapsim.glb.para", XML::xmlInternalTreeParse( sim ), envir = .GlobalEnv )

    # Check name
    if ( is.null( name ) )
    {
        return( NULL )
    }

    cultivar_n <- other.args$cultivar
    if (!is.null(cultivar_n))
    {
        max_len <- max(length(name), length(cultivar_n))
        cultivar_n <- rep(cultivar_n, length = max_len)
        name <- rep(name, length = max_len)
    }
    res <- NULL

    for( i in seq( along = name ) )
    {
        t.name <- name[i]
        temp <- XML::xpathApply( rapsim.glb.para, paste( "//", t.name, sep = "" ) )

        if ( length( temp ) == 0 )
        {
            # warning( paste( t.name," can not be found.", sep = "" ) )
            res <- c( res, as.character( NA ) )
            next
        }
        c.res <- XML::xmlValue( temp[[1]] )
        # for genotype parameters
        if ( length( temp ) > 1 )
        {
            parent <-  XML::xmlParent( temp[[1]])
            if ( XML::xmlName( parent ) == "base_cultivar" )
            {
                # find cultivar
                if (is.null(cultivar_n))
                {
                    cultivar <- XML::xpathApply( rapsim.glb.para, paste( "//", "cultivar", sep = "" ) )
                    if ( length( cultivar ) == 0 )
                    {
                        stop( "Cultivar node can not be find" )
                    }
                    cultivar <- XML::xmlValue( cultivar[[1]] )
                } else
                {
                    cultivar <- cultivar_n[i]
                }
                cultivar <- XML::xpathApply( rapsim.glb.para, paste( "//",  cultivar, sep = "" ) )
                c.res <- getPara4Genotype( t.name, cultivar )
            }
        }
        res <- c( res, c.res )
    }
    return( res )
}

# Find parameter for genotype
getPara4Genotype <- function( name, cultivar )
{
    rapsim.glb.para <- get("rapsim.glb.para", envir = cacheEnv)
    temp <- cultivar[[1]][name]
    if ( length( temp ) > 0 )
    {
        return( XML::xmlValue( temp[[1]] ) )
    } else
    {
        cultivar <- cultivar[[1]]["derived_from"]
        cultivar <- XML::xpathApply( rapsim.glb.para, paste( "//", XML::xmlValue( cultivar[[1]] ) , sep = "" ) )
        return( getPara4Genotype( name, cultivar ) )
    }
    return ( NULL )
}
