# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   17/05/2010
# *

#' Plot the figure to compare the observed and simulated variables
#' 
#' @param sim.obs The simulated values which combine the observed values
#' for each interesting variable.
#' @param identifiers A vector of the column names of identifier
#' @param variables A vector of the column names of interesting variables
#' @return The object of this figure
plotObsSimCompare <- function( sim.obs, identifiers, variables )
{
    sim.obs.names <- names( sim.obs )
    identifier <- identifiers$sim[ identifiers$sim %in% sim.obs.names ]
    
    variable <- variables$sim[ variables$sim %in% sim.obs.names ]
    
    pos <- variable %in% variables$sim
    label <- variables$label[pos]
    unit <- variables$unit[pos]

    if ( is.null( sim.obs$site ) )
    {
        sim.obs$site <- "TEMPSITE"
    }
    if ( is.null( sim.obs$genotype ))
    {
        sim.obs$genotype <- "TEMPGENOTYPE"
    }
    
    # Generate the date
    pd <- NULL
    
    pd.range <- NULL
    strips <- NULL
    
    temp <- NULL
    temp <- sim.obs[, names( sim.obs ) %in% identifier ]
    temp <- as.data.frame( temp )
    k <- 0
    for ( i in 1:length( variable ) )
    {
        index.name.obs.mean <- paste( variable[i], ".obs.mean", sep = "" )
        index.name.obs.sd <- paste( variable[i], ".obs.sd", sep = "" )
        if ( is.null( sim.obs[[index.name.obs.mean]] ) )
        {
            next()
        }
        
        temp$sim <- sim.obs[[variable[i]]]
        temp$obs.value.mean <- sim.obs[[index.name.obs.mean]]
        temp$obs.value.sd <- sim.obs[[index.name.obs.sd]]
        temp$index <- i
        
        pd <- rbind( pd, temp )
        k <- k + 1
        pd.range[[k]] <- range( temp$sim , temp$obs.value.mean - temp$obs.value.sd,
                temp$obs.value.mean + temp$obs.value.sd, na.rm = TRUE )
        
        strips <- c( strips, substitute( 
                        paste( t1, " ", t2 ), 
                        list( t1 = label[i], t2 = unit[[i]] ) ) )
    }
    
    if ( is.null( pd$index ) )
    {
        return( NULL )
    }
    
    strips <- as.expression( strips )
    site.level <- levels( as.factor( sim.obs$site ) )
    genotype.level <- levels( as.factor( sim.obs$genotype ) )

    site.level.num <- length( site.level )
    genotype.level.num <- length( genotype.level )
    
    site.level.colors <- blue2red(site.level.num)
    if ( site.level.num == 1 )
    {
        site.level.colors <- rgb(0,0,1)
    }
    genotype.level.colors <- magenta2green(genotype.level.num)

    k <- 1    
    for ( i in 1:site.level.num )
    {
        pos <- pd$site == site.level[i]
        pd$site.col[pos] <- site.level.colors[i]
        k <- k + 1
    }
    
    for ( j in 1:genotype.level.num )
    {
        pos <- pd$genotype == genotype.level[j]
        pd$genotype.col[pos] <- genotype.level.colors[j]
        pd$genotype.pch[pos] <- j - 1
        k <- k + 1
    }
    key.lines.colors <- NULL
    key.points.colors <- NULL
    key.lab <- NULL
    lty <- NULL
    pch <- NULL
    if ( site.level.num > 1 )
    {
        key.lines.colors <- c( rgb( 1, 1, 1 ), site.level.colors ) 
        key.points.colors <- c( rep( rgb( 1, 1, 1 ), site.level.num + 1 ) ) 
        key.lab <- c( "Site", site.level)
        lty <- c( 0, rep( 1, site.level.num ) )
        pch <- c( rep( 0, site.level.num + 1 ) )
    }
    if ( genotype.level.num > 1 )
    {
        key.lines.colors <- c( key.lines.colors, rgb( 1, 1, 1 ), rgb( 1, 1, 1 ), genotype.level.colors ) 
        key.points.colors <- c( key.points.colors, rep( rgb( 1, 1, 1 ), 2 ), genotype.level.colors ) 
        key.lab <- c( key.lab, "", "Genotype", genotype.level )
        lty <- c( lty, 0, 0, rep( 0, genotype.level.num ) )
        pch <- c( pch, rep( 0, 2 ), seq( from = 0, to = genotype.level.num - 1 ) )
    }
    
    key <- list(lines = list( col = key.lines.colors, lty  = lty ),
            points = list( col = key.points.colors, pch = pch ),
            text = list(lab = key.lab),
            columns = 3,
            space = "bottom" )
    

    this.plot <- xyplot( sim ~ obs.value.mean | factor( index ),
            data = pd,
            groups = pd$obs.value.sd,
            layout = c( 2, 3 ),
            scales = list( y = list( rot = 0, relation = "free" ),
                    x = list( rot = 0, relation = "free" )),
            ylim = pd.range,
            xlim = pd.range,
            xlab = "Observed",
            ylab = "Simulated",
            main = "Comparison between simulated and observed value",
            key = key,
            page = function( n )
            {
                notes <- paste( "Generated at ", date(), sep = "" )
                grid.text( notes, 
                        x = .99, y = 0.01, 
                        default.units = "npc", 
                        just = c( "right", "bottom" ),
                        gp = gpar( col = "grey", fontsize = 8 ) )
            },
            strip = strip.custom( factor.levels = strips ),
            subscripts = TRUE,
            panel = function( x, y, subscripts, groups )
            {
                panel.xyplot( x, y, type = "p", 
                        pch = pd$genotype.pch[subscripts],
                        col = pd$genotype.col[subscripts] )
                panel.xyplot( c(-100000,100000),c(-100000,100000), type = "l" )
                
                panel.segments( pd$obs.value.mean[subscripts] + groups[subscripts], pd$sim[subscripts],
                        pd$obs.value.mean[subscripts] - groups[subscripts], pd$sim[subscripts],
                        col = pd$site.col[subscripts] )
                                
                panel.segments( pd$obs.value.mean[subscripts] - groups[subscripts], unit( pd$sim[subscripts], "native" ) - unit( 0.2, "cm" ), 
                        pd$obs.value.mean[subscripts] - groups[subscripts], unit( pd$sim[subscripts], "native" ) + unit( 0.2, "cm" ), 
                        lend = 2, col = pd$site.col[subscripts] )
                
                panel.segments( pd$obs.value.mean[subscripts] + groups[subscripts], unit( pd$sim[subscripts], "native" ) - unit( 0.2, "cm" ), 
                        pd$obs.value.mean[subscripts] + groups[subscripts], unit( pd$sim[subscripts], "native" ) + unit( 0.2, "cm" ), 
                        lend = 2, col = pd$site.col[subscripts] )
            }
    )

    return( this.plot )
    
    
}