# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   20/03/2011
# *


#' APSIM-Wheat module
#' @param sim The path to sim file.
#' @param stages The stages of outputs
#' @param stageNames The stage names of outputs
#' @param model The model of Phenology. APSIM for default method in APSIM,
#' V1 and V2 change the minimum function to multiple. V2 extend photoperiod
#' effect of flowering
#' @param met The weather data with weaana class.
#' @param daily whether output daily stages
#' @param ... Other arguments
#' @export
wheatPhenology <- function(sim, stages = 6,
    stageNames = 'flow', model = 'APSIM',
    met = NULL, daily = FALSE, ...)
{
    # Check sim file
    sim <- sim[1]
    if (!file.exists(sim))
    {
        stop(paste(sim, " not existed.", sep = ""))
    }

    max_stage <- ifelse(daily, 9, max(stages))

    # Read parameters
    readPara(NULL, sim = sim)


    # Find start and end date
    date <- readPara("date")
    startDate <- as.Date(readPara("start_date"), format = "%d/%m/%Y")
    endDate <- as.Date(readPara("end_date"), format = "%d/%m/%Y")

    startYear <- as.numeric(format(startDate, "%Y"))
    endYear <- as.numeric(format(endDate, "%Y"))
    if (endYear < startYear)
    {
        stop("End year is not less than start year.")
    }

    # Phenology parameter
    phenology <- NULL
    phenology$stageCodes <- as.numeric(strsplit(readPara("stage_code_list"), "\\s+")[[1]])
    phenology$phaseNames <- strsplit(readPara("phase_names"), "\\s+")[[1]]
    phenology$phaseType <- strsplit(readPara("phase_type"), "\\s+")[[1]]
    phenology$phaseTT <- as.numeric(readPara(paste("tt_", phenology$phaseNames, sep = "")))
    phenology <- as.data.frame(phenology, stringsAsFactors = FALSE)

    shoot_lag <- as.numeric(readPara("shoot_lag"))
    shoot_rate <- as.numeric( readPara("shoot_rate"))
    sowing_depth <- as.numeric(readPara("depth"))
    phenology$phaseTT[2] <- shoot_lag + sowing_depth * shoot_rate

    eme2ej <- strsplit(readPara("eme2ej"), "\\s+")[[1]]
    eme2ej <- phenology[phenology$phaseNames %in% eme2ej, ]
    if (model == 'V2')
    {
        eme2fl <- c(eme2ej$stageCodes, max(eme2ej$stageCodes) + 1)
    }
    # if (model == 'V1')
    # {
        # eme2fl <- 5
    # }
    vernalisation <- strsplit(readPara("vernalisation"), "\\s+")[[1]]
    vernalisation <- phenology[phenology$phaseNames %in% vernalisation, ]

    vern_sens <- as.numeric(readPara("vern_sens"))
    photop_sens <- as.numeric(readPara("photop_sens"))
    twilight <- as.numeric(readPara("twilight"))

    fun_tt_x <- as.numeric(strsplit(readPara("x_temp"), "\\s+")[[1]])
    fun_tt_y <- as.numeric(strsplit(readPara("y_tt"), "\\s+")[[1]])

    # read met file
    if (is.null(met))
    {
        met <- readWeatherRecords(readPara("filename"))
    }
    met_infor <- siteInfor(met)

    wcal(met, FUN = dayLength, var.args = "day",
            lat = met_infor$Latitude,
            angle = twilight,
            var.name = "photoperiod")
    wcal(met, FUN = crown_temp_nwheat, var.args = c("maxt", "mint"),
            var.name = "tempcr")

    wcal(met, FUN = interpolationFunction, var.args = "tempcr",
        x = fun_tt_x, y = fun_tt_y,
        var.name = "dlt_tt")
    wcal(met, FUN = wheat_vernaliz_days, var.args = c("maxt", "mint", "tempcr"),
            var.name = "dlt_cumvd")
    # for multiple factors
    other_args <- list(...)
    factors <- other_args$factors
    if (is.null(factors))
    {
        factors <- as.data.frame(NA)
    }

    # Create target thermal time
    f_length <- nrow(factors)
    vars_names <- names(factors)
    phe_target <- as.list(NULL)
    for (i in seq(length = nrow(phenology)))
    {
        if ("cultivar" %in% vars_names & i != 2 )
        {
            phe_target[[phenology$phaseNames[i]]] <-
                as.numeric(readPara(paste("tt_", phenology$phaseNames[i], sep = ""),
                    cultivar = factors$cultivar))
        } else
        {
            phe_target[[phenology$phaseNames[i]]] <-
                rep(phenology$phaseTT[i], times = f_length)
        }
    }

    if ("cultivar" %in% vars_names)
    {
        vern_sens <- as.numeric(readPara("vern_sens", cultivar = factors$cultivar))
        photop_sens <- as.numeric(readPara("photop_sens", cultivar = factors$cultivar))
    }

    for (n in seq(along = vars_names))
    {
        if (vars_names[n] == "NA")
        {
            next
        } else if (substr(vars_names[n], 1, 3)  == "tt_")
        {
            phe_target[[substr(vars_names[n], 4, nchar(vars_names[n]))]] <-
                    factors[[n]]
        } else
        {
            assign(vars_names[n], factors[[n]])
        }
    }

    vern_sens <- rep(vern_sens, length = f_length)
    photop_sens <- rep(photop_sens, length = f_length)
    date <- rep(date, length = f_length)

    phe_target <- as.data.frame(phe_target)
    extra <- met_infor
    if (vars_names[1] != "NA")
    {
        if (is.null(other_args$extra))
        {
            extra <- data.frame(extra, factors,
                    row.names = seq(length = f_length))
        }else
        {
            extra <- data.frame(extra, factors, other_args$extra,
                    row.names = seq(length = f_length))
        }
    }
    res_sim <- NULL
    for (sowingYear in startYear:endYear)
    {
        # Calculate sowing doy for these year
        sowingDOY <- as.numeric(as.Date(paste(date, "-", sowingYear, sep = ""), format = "%d-%b-%Y") -
                        as.Date(paste("1-jan-", sowingYear, sep = ""), format = "%d-%b-%Y") + 1) - 1
        # Get weather records
        growthMET <- getWeatherRecords(met, yrange = c(sowingYear, sowingYear + 1))

        ttInTarget <- phe_target[[1]]
        ttInStage <- rep(0, times = f_length)
        ttNextStage <- rep(0, times = f_length)

        cStage <- rep(1, times = f_length)
        cumvd <- rep(0, times = f_length)
        res_year <- NULL

        if (daily == FALSE)
        {
            res_year$year <- rep(sowingYear, times = f_length)
            res_year$sow_doy <- sowingDOY
            for (j in seq(along = stages))
            {
                res_year[[paste(stageNames[j], '_doy', sep = '')]] <- rep(NA, times = f_length)
                res_year[[paste(stageNames[j], '_das', sep = '')]] <- rep(NA, times = f_length)
            }
        }

        sowing_year_start <- as.Date(paste(sowingYear - 1, '-12-31', sep = ''),
            format = '%Y-%m-%d')
        for (j in 1:300)
        {
            r_pos <- cStage < max_stage & !is.na(cStage)
            j_length <- sum(r_pos)
            j_stage <- cStage[r_pos]

            doy <- j + sowingDOY


            doy <- doy[r_pos]
            doy_date <- as.Date(doy, origin = sowing_year_start)
            doy_pos <- match(doy_date, growthMET$date)
            if (sum(!is.na(doy_pos)))
            {
                maxt <- growthMET$maxt[doy_pos]
                mint <- growthMET$mint[doy_pos]
                photoperiod <- growthMET$photoperiod[doy_pos]
                tempcr <- growthMET$tempcr[doy_pos]
                dlt_tt <- growthMET$dlt_tt[doy_pos]
                dlt_cumvd_ver <- growthMET$dlt_cumvd[doy_pos]
                c_cumvd <- cumvd[r_pos]
                c_vern_sens <- vern_sens[r_pos]
                r_photop_sens <- photop_sens[r_pos]

                r_ttInTarget <- ttInTarget[r_pos]
                r_ttInStage <- ttInStage[r_pos]
                r_ttNextStage <- ttNextStage[r_pos]

                cstage_index <- floor(j_stage)

                dlt_cumvd <- rep(0, times = j_length)

                pos_ver <- cstage_index %in% vernalisation$stageCodes
                dlt_cumvd[pos_ver] = wheat_de_vernaliz_days(maxt[pos_ver], mint[pos_ver],
                        tempcr[pos_ver], c_cumvd[pos_ver], dlt_cumvd_ver[pos_ver])

                vern_eff <- rep(1, times = j_length)
                photop_eff <- rep(1, times = j_length)

                pos <- cstage_index %in% eme2ej$stageCodes
                vern_eff[pos] <- wheat_vernaliz_effect(c_vern_sens[pos], c_cumvd[pos], dlt_cumvd[pos], 50.0)
                if (model %in% c('V2'))
                {
                    pos <- cstage_index %in% eme2fl
                    photop_eff[pos] <- wheat_photoperiod_effect(photoperiod[pos], r_photop_sens[pos])
                } else
                {
                    photop_eff[pos] <- wheat_photoperiod_effect(photoperiod[pos], r_photop_sens[pos])
                }
                c_cumvd[pos_ver] <- c_cumvd[pos_ver] + dlt_cumvd[pos_ver]
                cumvd[r_pos] <- c_cumvd

                # Calculate stage increase
                if (model %in% c('V1' , 'V2'))
                {
                    dlt_tt_phenol <- dlt_tt *  vern_eff * photop_eff
                } else
                {
                    dlt_tt_phenol <- dlt_tt * pmin(vern_eff, photop_eff)
                }
                phase_devel <- rep(0, times = j_length)

                pos <- cstage_index == 1
                if (j == 1)
                {
                    phase_devel[pos] = rep(0.999, times = j_length)
                } else
                {
                    phase_devel[pos] = rep(1.999, times = j_length)
                }

                phase_devel[!pos] = (dlt_tt_phenol[!pos] +
                            r_ttInStage[!pos]) / r_ttInTarget[!pos]
                new_stage <- cstage_index + phase_devel
                dltStage <- new_stage - j_stage

                index_devel <- j_stage - cstage_index + dltStage

                pos <- index_devel >= 1.0 & !is.na(index_devel)
                if (sum(pos) > 0)
                {
                    cStage1 <- j_stage[pos]
                    dltStage1 <- dltStage[pos]
                    index_devel1 <- index_devel[pos]
                    dlt_tt_phenol1 <- dlt_tt_phenol[pos]
                    c_ttInStage1 <- r_ttInStage[pos]

                    new_index <- floor(cStage1 + pmin (1.0, dltStage1))
                    fract_in_old <- rep(0, sum(pos))
                    portion_in_old <- rep(0, sum(pos))

                    pos1 <- cStage1 %% 1  == 0
                    fract_in_old[pos1] <- 1.0 - (index_devel1[pos1] - 1.0) / dltStage1[pos1]
                    portion_in_old[pos1] <- fract_in_old[pos1] * (dlt_tt_phenol1[pos1] +
                                c_ttInStage1[pos1]) - c_ttInStage1[pos1]

                    fract_in_old[!pos1] <- 1.0 - (index_devel1[!pos1] - 1.0) / dltStage1[!pos1]
                    portion_in_old[!pos1] <- fract_in_old[!pos1] * dlt_tt_phenol1[!pos1]

                    portion_in_new <- dlt_tt_phenol1 - portion_in_old
                    r_ttInStage[pos] <- r_ttInStage[pos] + portion_in_old
                    r_ttNextStage[pos] <- r_ttNextStage[pos] + portion_in_new
                }
                r_ttInStage[!pos] <- r_ttInStage[!pos] + dlt_tt_phenol[!pos]



                pos <- phase_devel >= 1.0 & !is.na(phase_devel)
                j_stage[pos] <- floor(j_stage[pos] + 1.0)
                r_ttInStage[pos] <- r_ttNextStage[pos]
                r_ttNextStage[pos] <- 0

                r_index <- seq(length = f_length)[r_pos]
                for (mm in seq(along = pos))
                {
                    if (pos[mm])
                    {
                        r_ttInTarget[mm] <- phe_target[[cstage_index[mm]+1]][r_index[mm]]
                    }
                }

                ttInTarget[r_pos] <- r_ttInTarget
                ttInStage[r_pos] <- r_ttInStage
                ttNextStage[r_pos] <- r_ttNextStage


                j_stage[!pos] <- new_stage[!pos]

                cStage[r_pos] <- j_stage

                if (daily == FALSE)
                {
                    for (k in seq(along = stages))
                    {
                        pos <- round(cStage,5) < stages[k] & !is.na(cStage)
                        res_year[[paste(stageNames[k], '_doy', sep = '')]][pos] <- (j + sowingDOY)[pos]
                        res_year[[paste(stageNames[k], '_das', sep = '')]][pos] <- j
                        pos <- is.na(cStage)
                        res_year[[paste(stageNames[k], '_doy', sep = '')]][pos] <- NA
                        res_year[[paste(stageNames[k], '_das', sep = '')]][pos] <- NA
                    }
                    if (sum (cStage >= max_stage, na.rm = TRUE) == f_length)
                    {
                        break
                    }
                    if (j > 299)
                    {
                        for (k in seq(along = stages))
                        {
                            pos <- round(cStage,5) < stages[k] & !is.na(cStage)
                            res_year[[paste(stageNames[k], '_doy', sep = '')]][pos] <- NA
                            res_year[[paste(stageNames[k], '_das', sep = '')]][pos] <- NA
                        }

                    }
                } else
                {
                    res_year[[j]] <- as.data.frame(list(year = sowingYear,
                        sow_doy = sowingDOY,
                        das = j,
                        stage = cStage))
                }
            }
            else
            {
                if (daily == FALSE)
                {
                    for (k in seq(along = stages))
                    {
                        pos <- round(cStage,5) < stages[k] & !is.na(cStage)
                        res_year[[paste(stageNames[k], '_doy', sep = '')]][pos] <- NA
                        res_year[[paste(stageNames[k], '_das', sep = '')]][pos] <- NA
                    }
                }
                break
            }

        }
        if (daily & !is.null(res_year))
        {
            res_year <- do.call(rbind, res_year)
        }
        res_sim <- rbind(res_sim, as.data.frame(res_year))
    }
    if (nrow(res_sim) > 0) {
        res_sim <- data.frame(extra, res_sim,
                row.names = seq(length = nrow(res_sim)))
    }
#    pos <- res_sim$flow_doy > 365
#    res_sim$flow_doy[pos] <- NA
#    res_sim$flow_das[pos] <- NA
    return(res_sim)
}
