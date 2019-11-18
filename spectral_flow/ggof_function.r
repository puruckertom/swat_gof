sim=mpeflow.log.zoo
obs=obsflow.log.zoo

function (sim, obs, na.rm = TRUE, dates, date.fmt = "%Y-%m-%d",
    pt.style = "ts", ftype = "o", FUN, gof.leg = TRUE, digits = 2,
    legend = c("Sim", "Obs"), leg.cex = 1, tick.tstep = "months",
    lab.tstep = "years", lab.fmt, cal.ini = NA, val.ini = NA,
    main, xlab = "Time", ylab = c("Q, [m3/s]"), col = c("blue",
        "black"), cex = c(0.5, 0.5), cex.axis = 1.2, cex.lab = 1.2,
    lwd = c(1, 1), lty = c(1, 3), pch = c(1, 9), ...)
{
    require(hydroTSM)
    if (is.na(match(class(sim), c("zoo", "numeric", "integer"))))
        stop("Invalid argument: 'class(sim)' must be in c('zoo', 'numeric', 'integer')")
    if (is.na(match(class(obs), c("zoo", "numeric", "integer"))))
        stop("Invalid argument: 'class(obs)' must be in c('zoo', 'numeric', 'integer')")
    if (length(sim) != length(obs))
        stop(paste("Invalid argument: 'obs' and 'sim' must have the same length ! (",
            length(obs), "vs", length(sim), ")", sep = " "))
    require(hydroTSM)
    if (!is.na(match(class(obs), c("zoo")))) {
        if (sfreq(sim) != sfreq(obs)) {
            stop(paste("Invalid argument: 'obs' and 'sim' have different sampling frequency ! (",
                sfreq(obs), "vs", sfreq(sim), ")", sep = " "))
        }
    }
    if (is.na(match(ftype, c("o", "dm", "ma", "dma"))))
        stop("Invalid argument: 'ftype' must be in c('o', 'dm', 'ma, 'dma')")
    if (!is.na(match(ftype, c("dm", "ma", "dma"))) & missing(FUN))
        stop("Missing argument: 'FUN' must be provided when 'ftype' is in c('dm', 'ma, 'dma')")
    if (missing(main))
        main <- "Observations vs Simulations"
    require(zoo)
    if (!missing(dates)) {
        if (length(dates) != length(sim))
            stop("Invalid argument: 'dates' and 'sim' must have the same length")
        if (is.na(match(class(dates), c("character", "factor",
            "Date"))))
            stop("Invalid argument: 'class(dates)' must be in c('character', 'factor', 'Date')")
        if (!is.na(match(class(dates), c("factor", "character"))))
            dates <- as.Date(dates, format = date.fmt)
        if (is.zoo(obs)) {
            time(obs) <- dates
        }
        if (is.zoo(sim)) {
            time(sim) <- dates
        }
    }
    else if (!is.zoo(obs))
        print("Note: You didn't provide dates, so only a numeric index will be used in the time axis.",
            quote = FALSE)
    require(hydroTSM)
    if (!is.zoo(obs) & !missing(dates)) {
        obs <- vector2zoo(x = obs, dates = dates, date.fmt = date.fmt)
    }
    else if (is.zoo(obs) & missing(dates)) {
        if (class(time(obs)) == "Date") {
            dates <- time(obs)
        }
        else if (class(time(obs)) == "character") {
            dates <- as.Date(time(obs), format = "%Y")
        }
    }
    if (!is.zoo(sim) & !missing(dates)) {
        sim <- vector2zoo(x = sim, dates = dates, date.fmt = date.fmt)
    }
    else if (is.zoo(sim) & is.zoo(obs) & missing(dates)) {
        if (class(time(sim)) == "Date") {
            dates <- time(obs)
        }
        else if (class(time(sim)) == "character") {
            dates <- as.Date(time(sim), format = "%Y")
        }
    }
    if (missing(lab.fmt)) {
        if (lab.tstep == "days") {
            lab.fmt <- "%Y-%m-%d"
        }
        else if (lab.tstep == "months") {
            lab.fmt <- "%b"
        }
        else if (lab.tstep == "years") {
            lab.fmt <- "%Y"
        }
    }
    if (ftype == "o") {
        plot2(x = sim, y = obs, plot.type = "single", main = main,
            col = col, lwd = lwd, lty = lty, pch = pch, xlab = xlab,
            ylab = ylab, pt.style = pt.style, add = FALSE, tick.tstep,
            lab.tstep, cex = cex, cex.axis = cex.axis, cex.lab = cex.lab,
            gof.leg = gof.leg, gof.digits = digits, legend = legend,
            leg.cex = leg.cex, cal.ini = cal.ini, val.ini = val.ini,
            date.fmt = date.fmt, ...)
    }
    else if (ftype == "dm") {
        if (sfreq(sim) != "daily") {
            stop("Invalid argument: 'sim' has to have a 'daily' sampling frequency")
        }
        else {
            obs.monthly <- daily2monthly(obs, FUN, na.rm)
            sim.monthly <- daily2monthly(sim, FUN, na.rm)
            def.par <- par(no.readonly = TRUE)
            on.exit(par(def.par))
            if (gof.leg) {
                layout(matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2,
                  2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4), ncol = 11,
                  byrow = TRUE))
            }
            else {
                par(mfrow = c(2, 1))
            }
            par(mar = c(5, 4, 4, 0) + 0.1)
            plot2(x = sim, y = obs, plot.type = "single", main = paste("Daily",
                main, sep = " "), tick.tstep = tick.tstep, lab.tstep = lab.tstep,
                cex = cex, cex.axis = cex.axis, cex.lab = cex.lab,
                col = col, lwd = lwd, lty = lty, pch = pch, xlab = xlab,
                ylab = ylab, pt.style = "ts", add = TRUE, gof.leg = gof.leg,
                gof.digits = digits, legend = legend, leg.cex = leg.cex,
                cal.ini = cal.ini, val.ini = val.ini, date.fmt = date.fmt,
                ...)
            par(mar = c(5, 4, 4, 0) + 0.1)
            plot2(x = sim.monthly, y = obs.monthly, plot.type = "single",
                main = paste("Monthly", main, sep = " "), tick.tstep = tick.tstep,
                lab.tstep = lab.tstep, cex = cex, cex.axis = cex.axis,
                cex.lab = cex.lab, col = col, lwd = lwd, lty = lty,
                pch = pch, xlab = xlab, ylab = ylab, pt.style = "ts",
                add = TRUE, gof.leg = gof.leg, gof.digits = digits,
                legend = legend, leg.cex = leg.cex, cal.ini = cal.ini,
                val.ini = val.ini, date.fmt = date.fmt, ...)
        }
    }
    else if (ftype == "ma") {
        if (is.na(match(sfreq(sim), c("daily", "monthly")))) {
            stop("Invalid argument: the sampling frequency of 'sim' has to be in c('daily', 'monthly'")
        }
        else {
            if (sfreq(sim) == "daily") {
                obs <- daily2monthly(obs, FUN, na.rm)
                sim <- daily2monthly(sim, FUN, na.rm)
            }
            obs.annual <- monthly2annual(obs, FUN, na.rm, out.fmt = "%Y-%m-%d")
            sim.annual <- monthly2annual(sim, FUN, na.rm, out.fmt = "%Y-%m-%d")
            def.par <- par(no.readonly = TRUE)
            on.exit(par(def.par))
            if (gof.leg) {
                layout(matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2,
                  2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4), ncol = 11,
                  byrow = TRUE))
            }
            else {
                par(mfrow = c(2, 1))
            }
            par(mar = c(5, 4, 4, 0) + 0.1)
            plot2(x = sim, y = obs, plot.type = "single", main = paste("Monthly",
                main, sep = " "), tick.tstep = tick.tstep, lab.tstep = lab.tstep,
                cex = cex, cex.axis = cex.axis, cex.lab = cex.lab,
                col = col, lwd = lwd, lty = lty, pch = pch, xlab = xlab,
                ylab = ylab, pt.style = "ts", add = TRUE, gof.leg = gof.leg,
                gof.digits = digits, legend = legend, leg.cex = leg.cex,
                cal.ini = cal.ini, val.ini = val.ini, date.fmt = date.fmt,
                ...)
            par(mar = c(5, 4, 4, 0) + 0.1)
            plot2(x = sim.annual, y = obs.annual, plot.type = "single",
                main = paste("Annual", main, sep = " "), tick.tstep = "years",
                lab.tstep = "years", cex = cex, cex.axis = cex.axis,
                cex.lab = cex.lab, col = col, lwd = lwd, lty = lty,
                pch = pch, xlab = xlab, ylab = ylab, pt.style = pt.style,
                add = TRUE, gof.leg = gof.leg, gof.digits = digits,
                legend = legend, leg.cex = leg.cex, cal.ini = cal.ini,
                val.ini = val.ini, date.fmt = date.fmt, ...)
        }
    }
    else if (ftype == "dma") {
        if (sfreq(sim) != "daily") {
            stop("Invalid argument: the 'sim' has to have a 'Daily' sampling frequency")
        }
        else {
            obs.monthly <- daily2monthly(obs, FUN, na.rm)
            sim.monthly <- daily2monthly(sim, FUN, na.rm)
            obs.annual <- daily2annual(obs, FUN, na.rm, out.fmt = "%Y-%m-%d")
            sim.annual <- daily2annual(sim, FUN, na.rm, out.fmt = "%Y-%m-%d")
            def.par <- par(no.readonly = TRUE)
            on.exit(par(def.par))
            if (gof.leg) {
                layout(matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2,
                  2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 5, 5, 5,
                  5, 5, 5, 5, 5, 5, 6, 6), ncol = 11, byrow = TRUE))
            }
            else {
                par(mfrow = c(3, 1))
            }
            par(mar = c(5, 4, 4, 0) + 0.1)
            plot2(x = sim, y = obs, plot.type = "single", main = paste("Daily",
                main, sep = " "), tick.tstep = tick.tstep, lab.tstep = lab.tstep,
                cex = cex, cex.axis = cex.axis, cex.lab = cex.lab,
                col = col, lwd = lwd, lty = lty, pch = pch, xlab = xlab,
                ylab = ylab, pt.style = "ts", add = TRUE, gof.leg = gof.leg,
                gof.digits = digits, legend = legend, leg.cex = leg.cex,
                cal.ini = cal.ini, val.ini = val.ini, date.fmt = date.fmt,
                ...)
            par(mar = c(5, 4, 4, 0) + 0.1)
            plot2(x = sim.monthly, y = obs.monthly, plot.type = "single",
                main = paste("Monthly", main, sep = " "), tick.tstep = tick.tstep,
                lab.tstep = lab.tstep, cex = cex, cex.axis = cex.axis,
                cex.lab = cex.lab, col = col, lwd = lwd, lty = lty,
                pch = pch, xlab = xlab, ylab = ylab, pt.style = "ts",
                add = TRUE, gof.leg = gof.leg, gof.digits = digits,
                legend = legend, leg.cex = leg.cex, cal.ini = cal.ini,
                val.ini = val.ini, date.fmt = date.fmt, ...)
            par(mar = c(5, 4, 4, 0) + 0.1)
            plot2(x = sim.annual, y = obs.annual, plot.type = "single",
                main = paste("Annual", main, sep = " "), tick.tstep = "years",
                lab.tstep = "years", cex = cex, cex.axis = cex.axis,
                cex.lab = cex.lab, col = col, lwd = lwd, lty = lty,
                pch = pch, xlab = xlab, ylab = ylab, pt.style = pt.style,
                add = TRUE, gof.leg = gof.leg, gof.digits = digits,
                legend = legend, leg.cex = leg.cex, cal.ini = cal.ini,
                val.ini = val.ini, date.fmt = date.fmt, ...)
        }
    }
}
