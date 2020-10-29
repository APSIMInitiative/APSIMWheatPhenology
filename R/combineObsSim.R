# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   18/05/2010
# *

#' Combine the simulated and observed data
#' 
#' Calculate the mean and standard deviations of observed data.
#' Insert the observed mean and standard deviations into the table of simulated data
#' @param obs A data.frame of the observed data
#' @param sim A data.frame of the simulated data
#' @param variables A list of intersting variable name
#' @param identifier A vector of the column names of identifier
#' @param isOutputObsData Whether output observed data
#' @return A data.frame contian all simulated and observed data. 
#' The several column at the beginning are the identifier of data/
#' The other columns were the simulated value, the mean of observed value and the 
#' standard deviations of observed value for each interesting variable. 
combineObsSim <- function( obs, sim, variables, identifier = NULL, isOutputObsData = FALSE )
{
    res <- NULL
    sim.len <- 0
    identifier.sim <- identifier$sim
    for ( i in 1:length( identifier.sim ) )
    {
        if ( !is.null( sim[[identifier.sim[i]]] ) )
        {
            res[[identifier.sim[i]]] <- sim[[identifier.sim[i]]]
            res[[identifier.sim[i]]][res[[identifier.sim[i]]] == "*"] <- NA
            sim.len <- sim.len + 1
        }
    }
    
    variables.sim <- variables$sim
    variables.obs <- variables$obs

    for ( i in 1:length( variables.sim ) )
    {
        res[[variables.sim[i]]] <- as.numeric( sim[[variables.sim[i]]] )
        sim.len <- sim.len + 1
    }
    res <- as.data.frame( res )

    obs.iden <- NULL
    if ( !is.null( obs$DaysAfterSowing ) )
    {
        obs.iden <- "DaysAfterSowing"
    }
    else if ( !is.null( obs$date ) )
    {
        obs.iden <- "date"
    }
    
    if ( is.null( obs.iden ) )
    {
        warning( paste( "The Observed data must contain one of ",
                        " \"DayAfterSowing\" or \"date\" item.", 
                        " The observed data will be ommited.", 
                        sep = "" ),
                call. = FALSE )
        return( res )
    }
    
    if ( is.null( obs$site ) )
    {
        obs$site <- "TEMPSITE"
    }
    if ( is.null( obs$genotype ))
    {
        obs$genotype <- "TEMPGENOTYPE"
    }
    if ( is.null( sim$site ) )
    {
        sim$site <- "TEMPSITE"
    }
    if ( is.null( sim$genotype ))
    {
        sim$genotype <- "TEMPGENOTYPE"
    }
    
    obs.com <- NULL
    
    site <- as.vector( tapply( obs$site, 
            list( obs$site, obs$genotype, obs[[obs.iden]] ), 
            FUN = function(x) x[1] ) )
    genotype <- as.vector( tapply( obs$genotype, 
            list( obs$site, obs$genotype, obs[[obs.iden]] ), 
            FUN = function(x) x[1] ) )
    obs.com <- cbind( site = site, genotype = genotype )
    obs.com <- as.data.frame( obs.com )
    obs.com[[obs.iden]] <- as.vector( tapply( obs[[obs.iden]], 
            list( obs$site, obs$genotype, obs[[obs.iden]] ), 
            FUN = function(x) x[1] ) )

    res.len <- length( res[[1]] )
    
    for ( i in 1:length( variables.obs ) )
    {
        if ( is.null( obs[[variables.obs[i]]] ) )
        {
            next()
        }
        index.name.obs.mean <- paste( variables.sim[i], ".obs.mean", sep = "" )
        index.name.obs.sd <- paste( variables.sim[i], ".obs.sd", sep = "" )
        obs[[variables.obs[i]]][obs[[variables.obs[i]]] == "*"] <- NA
        obs[[variables.obs[i]]] <- as.numeric( obs[[variables.obs[i]]] )
        obs.mean <- as.vector( tapply( obs[[variables.obs[i]]], 
                list( obs$site, obs$genotype, obs[[obs.iden]] ), 
                FUN = function(x) mean( x, na.rm = TRUE ) ) )
        
        obs.com[[index.name.obs.mean]] <- obs.mean
        
        obs.sd <- as.vector( tapply( obs[[variables.obs[i]]], 
                        list( obs$site, obs$genotype, obs[[obs.iden]] ), 
                        FUN = function(x) sd( x, na.rm = TRUE ) ) )
        
        obs.com[[index.name.obs.sd]] <- obs.sd
        res[[index.name.obs.mean]] <- rep( NA, res.len )
        res[[index.name.obs.sd]] <- rep( NA, res.len )
    }
    obs.com <- obs.com[!is.na(obs.com$site),]
    # for replicate, only 5 replicated was supuported
    if ( isOutputObsData )
    {
        for ( i in 1:length( variables.obs ) )
        {
            if ( is.null( obs[[variables.obs[i]]] ) )
            {
                next()
            }
            temp <- unlist( tapply( obs[[variables.obs[i]]],
                            list( obs$site, obs$genotype, obs[[obs.iden]] ), 
                            FUN = function(x) x[1:5] ) )
            dim(temp)<-c( 5, length(temp) / 5 )
            temp <- t( temp )
            for ( j in 1:5 )
            {
                index.name.obs.rep <- paste( variables.sim[i], ".obs.rep.", j, sep = "" )
                obs.com[[index.name.obs.rep]] <- temp[,j]
                res[[index.name.obs.rep]] <- rep( NA, res.len )
            }        
        }
    }

    obs.com.len <- length( obs.com[1,] )
    for ( k in 1:length( obs.com[[1]] ) )
    {
        site <- obs.com$site[k]
        genotype <- obs.com$genotype[k]
        obs.id <- obs.com[[obs.iden]][k]
        
        pos <- res$site == site & res$genotype == genotype
        if ( obs.iden == "DaysAfterSowing" )
        {
            pos <- pos & res$DaysAfterSowing == obs.id
        }
        else if ( obs.iden == "date" )
        {
            obs.data <- as.Date( obs.id, "%d/%m/%Y" )
            pos <- pos & as.Date( res$date, "%d/%m/%Y" ) == obs.data
        }
    
        if ( !sum( pos ) )
        {
            temp.pos <- res$site == site & res$genotype == genotype & res$DaysAfterSowing == 1
            temp <- res[temp.pos,]
            obs.id <- NULL
            temp.date <- as.Date( temp$date, "%d/%m/%Y" )
            if ( obs.iden == "DaysAfterSowing" )
            {
                temp$DaysAfterSowing <- obs.com$DaysAfterSowing[k]
                temp$date <- format( temp.date + temp$DaysAfterSowing - 1, "%d/%m/%Y" )
            }
            else if ( obs.iden == "date" )
            {
                obs.data <- as.Date( obs.com$date[k], "%d/%m/%Y" )
                print(paste(site, genotype, obs.data))
                temp$DaysAfterSowing <- as.numeric( obs.data - temp.date + 1 )
                temp$date <- obs.com$date[k]
            }
            
            for ( i in 1:length( variables.sim ) )
            {
                temp[[variables.sim[i]]] <- NA
            }
            temp <-  cbind(temp[,1:sim.len], obs.com[k,4:obs.com.len] )
            res <- rbind( res, temp )
            next()
        }
        res[pos,] <- cbind(res[pos,1:sim.len], obs.com[k,4:obs.com.len] )
    }
    
    if ( res$site[1] == "TEMPSITE" )
    {
        res$site <- NULL
    }
    if ( res$genotype[1] == "TEMPGENOTYPE" )
    {
        res$genotype <- NULL
    }
    return( res )
}