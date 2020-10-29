# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   21/05/2010
# *

#' Plot figures by combinations for all treatments
#' 
#' @param sim.obs The simulated values which combine the observed values
#' @param graph.combinations A list to describe how to combime figures.
#' @param identifiers A vector of the column names of identifier.
#' @param variables The compared column names of observed and simulated data.
#' @param xvar The variable which will be drawn in x axis.
#' @param isOutputObsData Whether output observed data.
#' @return A vector of all figures
plotGraphCombinations <- function( sim.obs, 
        graph.combinations = defaultPara( "graph.combinations" ), 
        identifiers = defaultPara( "identifiers" ),
        variables = defaultPara( "variables" ),
        xvar = NULL,
        isOutputObsData = FALSE )
{
    
    identifier <- identifiers$sim[ identifiers$sim %in% names( sim.obs ) ]
    pos <- identifiers$sim %in% xvar

    xlab <- substitute( 
            paste( t1, " ", t2 ), 
            list( t1 = identifiers$label[pos], t2 = identifiers$unit[pos][[1]] ) )
    
    if ( is.null( sim.obs$site ) )
    {
        sim.obs$site <- "TEMPSITE"
    }
    if ( is.null( sim.obs$genotype ) )
    {
        sim.obs$genotype <- "TEMPGENOTYPE"
    }
    site.level <- levels( as.factor( sim.obs$site ) )
    genotype.level <- levels( as.factor( sim.obs$genotype ) )
    
    yaxis.range.allplots <- NULL
    
    # Calcualte the y axis range
    for ( a in 1:length( site.level ) )
    {
        for ( b in 1:length( genotype.level) )
        {
            pos <- sim.obs$site == site.level[a] & sim.obs$genotype == genotype.level[b]
            pd <- sim.obs[pos,]
            
            yaxis.range.figure <- NULL
            
            # for each graph
            for ( i in 1:length( graph.combinations ) )
            {
                graph.axis.des <- graph.combinations[[i]]$axis
                # for each y axis, current only two axis supported
                for ( j in 1:min( 3, length( graph.axis.des ) ) )
                {                    
                    yaxis.des <- graph.axis.des[[j]]
                    # for each data
                    range.yaxis <- NULL
                    for ( m in 1:length( yaxis.des ) )
                    {                        
                        y.vari <- yaxis.des[m]
                        if ( !is.null( pd[[y.vari]] ) )
                        {    
                            range.yaxis <- range( range.yaxis, pd[[y.vari]],
                                    na.rm = TRUE  )
                            
                            index.name.obs.mean <- paste( y.vari, ".obs.mean", sep = "" )
                            index.name.obs.sd <- paste( y.vari, ".obs.sd", sep = "" )
                            
                            if ( !is.null( pd[[index.name.obs.mean]] ) )
                            {
                                range.yaxis <- range( range.yaxis, 
                                    pd[[index.name.obs.mean]] + pd[[index.name.obs.sd]],
                                    pd[[index.name.obs.mean]] - pd[[index.name.obs.sd]],
                                    na.rm = TRUE )
                            }
                        }
                    }    
                    yaxis.range.figure <- c( yaxis.range.figure, range.yaxis )
                }
                if ( length( graph.axis.des ) < 3 )
                {
                    yaxis.range.figure <- c( yaxis.range.figure, rep( c( NA, NA ), 3 - length( graph.axis.des ) ) )
                }            
            }
            yaxis.range.allplots <- rbind( yaxis.range.allplots, yaxis.range.figure )
        }
    }
    
    # Combine range of all plots
    yaxis.range.allplots.actual <- NULL
    for ( i in 1:( length(yaxis.range.allplots[1,] ) / 2 ) )
    {
        temp <- c( yaxis.range.allplots[, 2 * i - 1], 
                yaxis.range.allplots[, 2 * i] )
        rng <- NULL
        if ( sum( is.na( temp ) ) == length( temp ) )
        {
            rng <- c( NA, NA )
        }
        else
        {
            rng <- range( temp, na.rm = TRUE )
        }
        yaxis.range.allplots.actual <- rbind( yaxis.range.allplots.actual, rng )
    }
    
    # Rescale the yaxis
    pd.allplots <- NULL
    variable.app <- as.data.frame( list( name = "temp", col = 1, pch = 1 ) )
    for ( a in 1:length( site.level ) )
    {
        for ( b in 1:length( genotype.level) )
        {
            pos <- sim.obs$site == site.level[a] & sim.obs$genotype == genotype.level[b]
            pd <- sim.obs[pos,]
            
            pd.figure <- NULL
            
            temp <- NULL
            temp <- pd[, names( pd ) %in% identifier ]
            temp <- as.data.frame( temp )
            total.yaxis.num <- 1
            # for each graph
            for ( i in 1:length( graph.combinations ) )
            {
                temp$graph.index <- i
                pd.graph <- NULL
                
                graph.des <- graph.combinations[[i]]
                graph.axis.des <- graph.des$axis
                # for each y axis, current only two axis supported
                for ( j in 1:min( 3, length( graph.axis.des ) ) )
                {
                    temp$yaxis.pos <- j
                    pd.yaxis <- NULL
                    
                    range.yaxis <- yaxis.range.allplots.actual[total.yaxis.num + j - 1,]
                    pars.yaxis <- scales.pars( range.yaxis )
                    
                    yaxis.des <- graph.axis.des[[j]]
                    data.group <- 0
                    yaxis.lab <- NULL
                    # for each data
                    for ( m in 1:length( yaxis.des ) )
                    {
                        y.vari <- yaxis.des[m]
                        if ( !is.null( pd[[y.vari]] ) )
                        {    
                            data.group <- data.group + 1
                            temp$sim <- rescale( pd[[y.vari]], pars.yaxis )
                            index.name.obs.mean <- paste( y.vari, ".obs.mean", sep = "" )
                            index.name.obs.sd <- paste( y.vari, ".obs.sd", sep = "" )
                            temp$obs.value.mean <- NA
                            temp$obs.value.sd <- NA
                            
                            if ( isOutputObsData )
                            {
                                for ( d in 1:5 )
                                {
                                    index.name.rep <- paste( "obs.value.rep", d, sep = "" )
                                    temp[[index.name.rep]] <- NA
                                }
                            }
                            if ( !is.null( pd[[index.name.obs.mean]] ) )
                            {
                                temp$obs.value.mean <- rescale( pd[[index.name.obs.mean]], pars.yaxis )
                                temp$obs.value.sd <- rescale( pd[[index.name.obs.sd]], pars.yaxis )
                                if ( isOutputObsData )
                                {
                                    for ( d in 1:5 )
                                    {
                                        index.name.obs.rep <- paste( y.vari, ".obs.rep.", d, sep = "" )
                                        index.name.rep <- paste( "obs.value.rep", d, sep = "" )
                                        
                                        if ( !is.null( pd[[index.name.obs.rep]] ) )
                                        {
                                            temp[[index.name.rep]] <- rescale( pd[[index.name.obs.rep]], pars.yaxis ) 
                                        }
                                    }
                                }
                                
                            }
                            temp$data.group <- data.group
                            
                            # for y label
                            y.vari.pos <- NULL
                            for ( c in 1:length( variables$sim ) )
                            {
                                if ( variables$sim[c] == y.vari )
                                {
                                    y.vari.pos <- c
                                    break()
                                }
                            }
                            if ( is.null( yaxis.lab ) )
                            {
                                yaxis.lab <- variables$label[y.vari.pos]
                            }
                            else
                            {
                                if ( m %% 2 == 0 )
                                {
                                    yaxis.lab <- paste( yaxis.lab, variables$label[y.vari.pos], sep = " & " )
                                }
                                else
                                {
                                    yaxis.lab <- paste( yaxis.lab, variables$label[y.vari.pos], sep = "\n" )
                                }
                            }
                            
                            # Set color and pch
                            if ( y.vari %in% variable.app$name )
                            {
                                temp$col <- variable.app$col[ variable.app$name == y.vari ]
                                temp$pch <- variable.app$pch[ variable.app$name == y.vari ]
                            }
                            else
                            {
                                num <- length( variable.app$name ) + 1
                                variable.app <- rbind( variable.app, 
                                        as.data.frame( list( name = y.vari, col = variables$col[y.vari.pos], pch = variables$pch[y.vari.pos] ) ) )
                                temp$col <- variables$col[y.vari.pos]
                                temp$pch <- variables$pch[y.vari.pos]
                            }
                            
                            pd.yaxis <- rbind( pd.yaxis, temp )
                        }
                    }
                    pd.yaxis$yaxis.lab <- yaxis.lab
                    pd.graph <- rbind( pd.graph, pd.yaxis )
                }
                
                total.yaxis.num <- total.yaxis.num + 3
                pd.figure <- rbind( pd.figure, pd.graph )                
            }
            pd.allplots <- rbind( pd.allplots, pd.figure )
        }
    }

    # for xrange
    xrange <- range( pd.allplots[[xvar]] )
    # for strips
    strips <- NULL
    for ( i in 1:length( graph.combinations ) )
    {
        graph.des <- graph.combinations[[i]]
        if ( !is.null( graph.des$title ) )
        {
            strips <- c( strips, graph.des$title )
        }
        else
        {
            strips <- c( strips, paste( "Graph", i ) )
        }
    }
    
    # for key
    variable.app <- variable.app[2:length( variable.app$name ),]
    
    key.lab <- NULL
    k <- 0
    for ( i in 1:length( variable.app$name ) )
    {
        for ( j in 1:length( variables$sim ) )
        {
            if ( variable.app$name[i] == variables$sim[j] )
            {
                k <- k + 1
                key.lab[[k]] <- substitute( 
                        paste( t1, " ", t2 ), 
                        list( t1 = variables$label[j], t2 = variables$unit[[j]] ) )    
                
            }
        }
    }
    key.lab <- as.expression(key.lab)
    
    key <- NULL
    key.num <- length( variable.app$col )
    key.row.num <- 10
    start.num <- key.row.num
    end.num <- key.row.num * 2 - key.num
    if ( key.num > key.row.num * 2 )
    {
        total.key.num <- ceiling( key.num / 2 ) * 3
        start.num <- total.key.num / 3
        end.num <- total.key.num - start.num - key.num
    }
    
    key <- list( lines = list( col = c( rep( rgb( 1, 1, 1 ), start.num ), 
                            variable.app$col,
                            rep( rgb( 1, 1, 1 ), end.num)),
                    cex = 0.7 ),
            points = list( col = c( rep( rgb( 1, 1, 1 ), start.num ), 
                            variable.app$col,
                            rep( rgb( 1, 1, 1 ), end.num ) ), 
                    pch = c( rep( 0, start.num), 
                            variable.app$pch,
                            rep( 0, end.num) ),
                    cex = 0.7 ),
            text = list( lab = c( rep( "", start.num ), 
                            key.lab,
                            rep( "", end.num) ),
                    cex = 0.7 ),
            columns = 3,
            space = "bottom" )
    
    all.plots <- NULL
    plots.num <- 0
     
    for ( a in 1:length( site.level ) )
    {
        for ( b in 1:length( genotype.level) )
        {
            figure.pos <- pd.allplots$site == site.level[a] & pd.allplots$genotype == genotype.level[b]
            figure.pd <- pd.allplots[figure.pos,]
            
            stage <- NULL
            if ( !is.null( sim.obs$stage ) )
            {
                stage <- cbind ( xvar = sim.obs[[xvar]][sim.obs$site == site.level[a] & sim.obs$genotype == genotype.level[b]],
                        stage = sim.obs$stage[sim.obs$site == site.level[a] & sim.obs$genotype == genotype.level[b]] )
                stage <- as.data.frame( stage )
                for ( j in 1:length( variables$sim ) )
                {
                    if ( "stage" == variables$sim[j] )
                    {
                        stage$col <- variables$col[j]
                    }
                }
            }
            
            notes <- NULL
            main <- NULL
            year <- NULL
            if ( !is.null( figure.pd$year ) )
            {
                year <- figure.pd$year[1]
                notes <- paste( notes, "Year: \"", year, "\". ", sep = "" )
                main <- paste( main, "Year: ", year, "    ", sep = "" )
            }
            if ( site.level[a] != "TEMPSITE" )
            {
                main <- paste( main, "Site: ", site.level[a], "    ", sep = "" )
            }
            if ( genotype.level[b] != "TEMPGENOTYPE" )
            {
                main <- paste( main, "Genotype: ", genotype.level[b], sep = "" )
            }
            
            if ( is.null( main ) )
            { 
                main <- "Comparison between observed and simulated data"
            }
            
            filename <- NULL
            if ( !is.null( figure.pd$filename ) )
            {
                filename <- figure.pd$filename[1]
                notes <- paste( notes, "Filename \"", filename, "\". ", sep = "" )
            }
            notes <- paste(  notes, "Time: \"", date(), "\". ", sep = "" )
            
            this.plot <- plotGraphCombinationsByTreatment(
                    pd = figure.pd,
                    graph.combinations = graph.combinations,
                    xvar = xvar, xlab = xlab,
                    xrange = xrange, 
                    yrange = yaxis.range.allplots.actual,
                    strips = strips,
                    main = main,
                    key = key,
                    notes = notes,
                    isOutputObsData = isOutputObsData,
                    stage = stage )
            
            plots.num <- plots.num + 1
            all.plots[[plots.num]] <- this.plot
        }
    }

    return( all.plots )
}

#' Plot figures by combinations for each treatment
#' 
#' @param pd The simulated values which combine the observed values
#' @param graph.combinations A list to describe how to combime figures
#' @param xvar The variable which will be drawn in x axis
#' @param xlab the label of x axis
#' @param xrange The range of x variable.
#' @param yrange The range of y variable. 
#' @param strips A vector of strips.
#' @param main The main title of the figure.
#' @param key A list to describe key.
#' @param notes A string which put in the footnoot of pages.
#' @param isOutputObsData Whether output observed data.
#' @param stage A vector to describe the stage 
#' @return A object of this figure
plotGraphCombinationsByTreatment <- function( pd, 
        graph.combinations = NULL,
        xvar = NULL,
        xlab = NULL,
        xrange = NULL,
        yrange = NULL,
        strips = NULL,
        main = NULL,
        key = NULL,
        notes = NULL,
        isOutputObsData = FALSE,
        stage = NULL )
{
    
    yscale.components <- function(lim, ...)
    {
        pa.num <- panel.number()

        if ( length( pa.num ) == 0 )
        {
            return( yscale.components.default( c( 0, 1 ), ...) )
        }

        y.range.left <- yrange[ 3 * ( pa.num - 1 ) + 1, ]
        y.range.right <- yrange[ 3 * ( pa.num - 1 ) + 2, ]

        y.axis.left <-NULL
        y.axis.right <-NULL
        if ( sum( is.na( y.range.left ) ) == length( y.range.left ) ) 
        {
            y.axis.left$left <- FALSE
        }
        else
        {
            y.axis.left <- yscale.components.default( y.range.left, ...)
            y.axis.left$left$ticks$at <-
                    rescale( y.axis.left$left$ticks$at, scales.pars( y.range.left ) )
            y.axis.left$left$labels$at <-
                    rescale( y.axis.left$left$labels$at, scales.pars( y.range.left ) )
        }

        if ( sum( is.na( y.range.right ) ) == length( y.range.right ) ) 
        {
            y.axis.right$left <- FALSE
        }
        else
        {
            y.axis.right <- yscale.components.default( y.range.right, ...)
            y.axis.right$left$ticks$at <-
                    rescale( y.axis.right$left$ticks$at, scales.pars( y.range.right ) )
            y.axis.right$left$labels$at <-
                    rescale( y.axis.right$left$labels$at, scales.pars( y.range.right ) )
        }
        
        ans <- yscale.components.default(lim, ...)
        ans$left <- y.axis.left$left
        ans$right <- y.axis.right$left
        
        return( ans )
    }
    
    xdiff <- diff( xrange )
    
    this.plot <- xyplot( sim ~ DaysAfterSowing | factor(graph.index),
            data = pd,
            xlim =  xrange,
            ylim = c( 0, 1 ),
            xlab = xlab,
            ylab = "",
            main = main,
            layout = c( 2, 3 ),
            subscripts = TRUE,
            cex.lab = 0.7,
            yscale.components = yscale.components,
            scales = list( 
                    x = list( alternating = 3, relation = "free", cex = 0.8 ), 
                    y = list( relation = "free", rot = 0, cex = 0.7 ) ),
            key = key,
            page = function( n )
            {
                grid.text( notes, 
                        x = .99, y = 0.01, 
                        default.units = "npc", 
                        just = c( "right", "bottom" ),
                        gp = gpar( col = "grey", fontsize = 8 ) )
                pushViewport( viewport( x = unit( 0.05, "npc" ),
                                y = unit( 0.06, "npc" ),
                                height = unit( 0.1, "npc" ),
                                width = unit( 0.2, "npc" ),
                                clip = "off",
                                just = c( "left", "bottom" ) ) ) 
                
                xaxis <- yscale.components.default( xrange + c (-0.05, 0.05 ) * xdiff )
                
                grid.xaxis( at = rescale( xaxis$left$ticks$at, scales.pars( xrange + c (-0.05, 0.05 ) * xdiff ) ),
                        label = xaxis$left$labels$labels,
                        gp = gpar( fontsize = 8 )) 
                grid.text( "Stage", x = unit( 0, "npc") - unit( 2.4, "lines"), 
                        y = unit( 0.5,  "npc"),
                        just = "centre", rot = 90,
                        gp = gpar( fontsize = 8 ))
                grid.text( xlab, x = unit( 0.5, "npc") , 
                        y = unit( 0,  "npc") - unit( 2.8, "lines"),
                        just = "centre",
                        gp = gpar( fontsize = 8 ))
                grid.rect()
                
                
                if ( !is.null( stage ) )
                {
                    stage <- stage[order(stage$xvar),]
                    stage.range <- range( stage$stage, na.rm = TRUE )
                    stage.range <- stage.range + c (-0.05, 0.05 ) * diff( stage.range, na.rm = TRUE )
                    yaxis <- yscale.components.default( stage.range )
                    grid.yaxis( at = rescale( yaxis$left$ticks$at, scales.pars( stage.range ) ),
                            label = yaxis$left$labels$labels,
                            gp = gpar( fontsize = 8 ))
                    grid.lines( rescale( stage$xvar, scales.pars( xrange + c (-0.05, 0.05 ) * xdiff ) ),
                            rescale( stage$stage, scales.pars( stage.range ) ), 
                            gp = gpar( col = stage$col ) )
                }
                else
                {
                    grid.yaxis( at = seq( from = 0, to = 1, by = 0.2  ),
                            label = seq( from = 0, to = 10, by = 2  ),
                            gp = gpar( fontsize = 8 ))
                }
                
                popViewport()
            },
            between = list( x = 6 ),
            par.settings = list( 
                    layout.widths = 
                            list( left.padding = 1, 
                                    right.padding = 8 ),
                    layout.heights = 
                            list( bottom.padding = 2 ) 
            ),
            strip = strip.custom( factor.levels = strips,
                    par.strip.text = list( cex = 0.8 ) ),
            panel = function( x, y, subscripts, groups )
            {
                panel.data <- pd[subscripts,]
                panel.data <- panel.data[order(panel.data[[xvar]]),]
                yaxis.num <- nlevels( factor( panel.data$yaxis.pos ) )
                for ( i in 1:yaxis.num )
                {
                    yaxis.pos <- panel.data$yaxis.pos == i
                    yaxis.data <- panel.data[yaxis.pos,]
                    data.num <- nlevels( factor( yaxis.data$data.group ) )
                    for ( j in 1:data.num )
                    {
                        group.pos <- yaxis.data$data.group == j
                        group.data <- yaxis.data[group.pos,]
                        panel.xyplot( group.data[[xvar]], group.data$sim, type = "l",
                                col = group.data$col )
                        if ( !is.null( group.data$obs.value.mean ) )
                        {
                            panel.xyplot( group.data[[xvar]], group.data$obs.value.mean, type = "p",
                                    col = group.data$col, pch = group.data$pch )
                            panel.segments( group.data[[xvar]], group.data$obs.value.mean + group.data$obs.value.sd,
                                    group.data[[xvar]], group.data$obs.value.mean - group.data$obs.value.sd,
                                    col = group.data$col )
                            panel.segments( group.data[[xvar]] - xdiff * 0.02, group.data$obs.value.mean + group.data$obs.value.sd,
                                    group.data[[xvar]] + xdiff * 0.02, group.data$obs.value.mean + group.data$obs.value.sd,
                                    col = group.data$col )
                            panel.segments( group.data[[xvar]] - xdiff * 0.02, group.data$obs.value.mean - group.data$obs.value.sd,
                                    group.data[[xvar]] + xdiff * 0.02, group.data$obs.value.mean - group.data$obs.value.sd,
                                    col = group.data$col  )
                            if ( isOutputObsData )
                            {
                                for ( a in 1:5 )
                                {
                                    index.name.rep <- paste( "obs.value.rep", a, sep = "" )
                                    if ( !is.null( group.data[[index.name.rep]] ) )
                                    {
                                        if ( sum( is.na( group.data[[index.name.rep]] ) ) != length( group.data[[index.name.rep]] ) )
                                        {
                                            panel.xyplot( group.data[[xvar]], 
                                                    group.data[[index.name.rep]], 
                                                    type = "p", 
                                                    cex = 0.9,
                                                    col = "gray90" )
                                        }
                                    }
                                }
                            }
                        }
                    }
                    ylab <- yaxis.data$yaxis.lab[1]
                    pushViewport( viewport( clip = "off" ) ) 
                    if ( i == 1 )
                    {
                        grid.text( ylab, x = unit( 0, "npc") - unit( 4.5, "lines"), 
                                y = unit( 0.5,  "npc"),
                                just = "centre", rot = 90,
                                gp = gpar( fontsize = 8 ) )
                    }
                    else if ( i == 2 )
                    {
                        grid.text( ylab, x = unit( 1, "npc") +  unit( 4.5, "lines"), 
                                y = unit( 0.5,  "npc"),
                                just = "centre", rot = 270,
                                gp = gpar( fontsize = 8 ) )
                    }
                    popViewport()                     
                }
                
                # for x axis - major mark line
                panel.grid( h = 0, v = -1, lty = 3, col = "black" )
            } 
    )
    

    return( this.plot )
    
}

#' Calculate the minimum values and iterated differences
#' 
#' @param x A numeric vector or matrix containing the values to be differenced.
#' @return A list contain the minimum values and iterated differences
scales.pars <- function( x )
{
    return( c( mx = min( x, na.rm = TRUE ), dx = diff( range( x, na.rm = TRUE ) ) ) )
}

#' Rescale the x to ( 0, 1 )
#' 
#' @param x A numeric vector to rescale
#' @param pars The minimum values and iterated differences 
#' @return The new vector which all value in ( 0, 1 )
rescale <- function( x, pars = scales.pars( x ) )
{
     return( ( x - pars["mx"] ) / pars["dx"] )
}

