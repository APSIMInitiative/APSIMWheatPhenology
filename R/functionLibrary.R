# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   20/03/2011
# *

# A library for common functions


#' Transfer of sign - from FORTRAN.
#' The result is of the same type and kind as a. Its value is the abs(a) of a,
#' if b is greater than or equal positive zero; and -abs(a), if b is less than
#' or equal to negative zero.
#' Example a = sign_apsim (30,-2) ! a is assigned the value -30
#'
#' @param a value 1
#' @param b value 2
sign_apsim <- function( a, b )
{
    if ( b >= 0 )
    {
        return( abs( a ) )
    } else
    {
        return( -abs(a) )
    }
}


#' Crown temperature from nwheat
#'
#' @param maxt Maximum temperature (oC)
#' @param mint Minimum temperature (oC)
#' @param snow Snow depth (cm)
crown_temp_nwheat <- function( maxt, mint, snow = 0 )
{
    cx <- NULL
    cx <- maxt
    pos <- !is.na(maxt) & maxt < 0
    cx[pos] <- 2.0 + maxt[pos] * ( 0.4 + 0.0018 * ( snow - 15 ) ^ 2 )
    cn <- mint
    pos <- !is.na(mint) & mint < 0
    cn[pos] <- 2.0 + mint[pos] * ( 0.4 + 0.0018 * ( snow - 15 ) ^ 2 )

    return( ( cn + cx ) / 2.0 )
}



#' Photoperiod factor
#'
#' @param photoperiod photoperiod
#' @param p_photop_sen p_photop_sen
wheat_photoperiod_effect <- function( photoperiod, p_photop_sen )
{
    photop_eff <- 1. - p_photop_sen * 0.002 * ( 20. - photoperiod ) ^ 2
    photop_eff[photop_eff < 0] <- 0
    photop_eff[photop_eff > 1] <- 1
    return( photop_eff )
}

#' Calculate daily vernalisation and accumulate to g_cumvd
#'
#' @param maxt Daily maximum Temperature
#' @param mint Daily minimum temperature
#' @param tempcr Crown temperature
wheat_vernaliz_days <- function( maxt, mint, tempcr )
{
    dlt_cumvd <- rep( 0.0, times = length( maxt ) )
    ver <- function( maxt, mint, tempcr )
    {
        vd1 <- 1.4 - 0.0778 * tempcr
        vd2 <- 0.5 + 13.44 / ( maxt - mint + 3.) ^ 2 * tempcr
        vd <- pmin ( vd1, vd2 )
        vd[vd<0] <- 0
        return( vd )
    }
    pos1 <- !is.na(maxt) & !is.na(mint) & mint < 15.0 & maxt > 0.0
    dlt_cumvd[pos1] <- ver( maxt[pos1], mint[pos1], tempcr[pos1])

    return( dlt_cumvd )
}

#' Calculate daily vernalisation and accumulate to g_cumvd
#'
#' @param maxt Daily maximum Temperature
#' @param mint Daily minimum temperature
#' @param tempcr Crown temperature
#' @param g_cumvd cumulative vernalisation days till yesterday
#' @param dlt_cumvd delta cummulative vernalisation days
wheat_de_vernaliz_days <- function( maxt, mint, tempcr, g_cumvd, dlt_cumvd )
{
    pos2 <- maxt > 30. & g_cumvd + dlt_cumvd < 10. & !is.na(maxt)
    dlt_cumvd[pos2] <- - 0.5 * ( maxt[pos2] - 30. )
    dlt_cumvd <- - pmin(-(dlt_cumvd), g_cumvd)
    return( dlt_cumvd )
}


#' Vernalisation factor
#'
#' @param p_vern_sens p_vern_sens
#' @param cumvd cumvd
#' @param dlt_cumvd dlt_cumvd
#' @param reqvd reqvd
wheat_vernaliz_effect <- function( p_vern_sens, cumvd, dlt_cumvd, reqvd )
{
    if ( reqvd < 0.0 ) { reqvd = 50.0 }
    vern_sens_fac <-  p_vern_sens * 0.0054545 + 0.0003
    vfac <- 1. - vern_sens_fac * ( reqvd - ( cumvd + dlt_cumvd ) )
    vfac[vfac < 0 ] <- 0
    vfac[vfac > 1 ] <- 1
    return( vfac )
}



#' lower limit
#'
#' @param x x
#' @param limit limit
l_bound <- function( x, limit )
{
    if ( x > limit )
    {
        return( x )
    }
    return( limit )
}

#' boundt
#'
#' @param x x
#' @param min min
#' @param max max
bound <- function( x, min, max )
{
    if ( x > max )
    {
        return( max )
    }
    if ( x < min )
    {
        return( min )
    }
    return( x )
}

#'Return a y value from a linear interpolation function
#'
#' @param x x
#' @param y y
#' @param values values
#' @param split split
#' @export
interpolationFunction <- function( x, y, values, split = '\\s+' )
{
    if (is.character(x) & length(x) == 1)
    {
        x <- as.numeric(strsplit(x, split)[[1]])
    }
    if (is.character(y) & length(y) == 1)
    {
        y <- as.numeric(strsplit(y, split)[[1]])
    }
    res <- rep(NA, length(values))

    pos <- values < x[1]
    res[pos] <- y[1]

    for (i in seq(length = length(x) - 1))
    {
        pos <- values >= x[i] & values < x[i + 1]
        slope <- (y[i+1] - y[i] ) / (x[i+1] - x[i])
        res[pos] <- y[i] + slope * (values[pos] - x[i])
    }
    pos <- values >= x[length(x)]
    res[pos] <- y[length(y)]
    return ( res )
}

#' The time for sunrise and sunset
#' from 90 degree in am and pm. +ve above the horizon, -ve below the horizon.
#' @param  doy day of year number
#' @param lat latitude of site (deg)
#' @param long longitude of site (deg)
#' @param timezone local time
#' @param  angle angle to measure time between, such as twilight (deg).
#' angular distance between 90 deg and end of twilight - altitude of sun. +ve up, -ve down.
#' @param flag rise or set
#' @export
sunRiseSet <- function(doy, lat, long, timezone = 0, angle = 6, flag = 'rise')
{
    doy_len <- length(doy)
    lat <- rep(lat, length.out = doy_len)
    long <- rep(long, length.out = doy_len)
    dg2rdn <- pi / 180
    rdn2dg <- 180 / pi
    lngHour <- long / 15
    if (flag == 'rise')
    {
        tt <- doy + ((6 - lngHour) / 24)
    } else
    {
        tt <- doy + ((18 - lngHour) / 24)
    }
    MM <- (0.9856 * tt) - 3.289
    LL <- MM + (1.916 * sin(MM * dg2rdn)) + (0.020 * sin(2 * MM * dg2rdn)) + 282.634
    LL <- (LL + 360 * 100) %% 360
    RA <- rdn2dg * atan(0.91764 * tan(LL * dg2rdn))
    RA <- (RA + 360 * 100) %% 360
    Lquadrant <- (floor(LL/90)) * 90
    RAquadrant <- (floor(RA/90)) * 90
    RA <- RA + (Lquadrant - RAquadrant)
    RA <- RA / 15
    sinDec <- 0.39782 * sin(LL * dg2rdn)
    cosDec <- cos(asin(sinDec))
    cosH <- (cos((angle + 90)* dg2rdn) - (sinDec * sin(lat * dg2rdn))) / (cosDec * cos(lat * dg2rdn))

    if (sum(cosH > 1) > 0)
    {
        stop('the sun never rises on this location (on the specified date)')
    }
    if (sum(cosH < -1) > 0)
    {
        stop('the sun never sets on this location (on the specified date)')
    }

    if (flag == 'rise')
    {
        HH <- 360 - rdn2dg * acos(cosH)
    } else
    {
        HH <- rdn2dg * acos(cosH)
    }
    HH <- HH / 15
    TT <- HH + RA - (0.06571 * tt) - 6.622
    UT <- TT - lngHour
    # UT <- (UT + 24 * 100) %% 24
    localT <- UT + timezone
    localT <- (localT + 24 * 100) %% 24

    return(localT)
}
