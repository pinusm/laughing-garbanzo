#Helper function to provide some basic stats
mysum <- function(x) {
        c(
                n = sum(!is.na(x)),
                M = round(mean(x, na.rm=TRUE),3),
                SD = round(sd(x, na.rm=TRUE),3),
                med = round(median(x, na.rm=TRUE),3)
        )
}

#Helper to provide mean and N
meanN <- function(x) {
        c(
                n = sum(!is.na(x)),
                M = mean(x, na.rm=TRUE)
        )
}


##Creates a correlation matrix table
corstarsl <- function(x, silent = F){
        require(Hmisc)
        x <- as.matrix(x, silent = silent)
        rc <- rcorr(x)
        R <- rc$r
        p <- rc$P
        n <- rc$n

        ## define notions for significance levels; spacing is important.
#        ps <- ifelse(p < .001, "< .001", as.character(round(p,3)))
        ps <- ifelse(p < .001, "< .001", ifelse(p < .01, "< .01", ifelse(p < .05, "< .05", as.character(round(p,3)))))

        ## trunctuate the matrix that holds the correlations to two decimal
        R <- format(round(cbind(rep(-1.11, ncol(x)), R), 3))[,-1]

        ## build a new matrix that includes the correlations with their apropriate stars
        Rnew <- matrix(paste(R, ps, n, sep="\n"), ncol=ncol(x))
        diag(Rnew) <- paste(diag(R), " ", sep="")
        rownames(Rnew) <- colnames(x)
        colnames(Rnew) <- paste(colnames(x), "", sep="")

        ## remove upper triangle
        Rnew <- as.matrix(Rnew)
        Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
        Rnew <- as.data.frame(Rnew)

        ## remove last column and return the matrix (which is now a data frame)
        Rnew <- cbind(Rnew[1:length(Rnew)-1])
        return(Rnew)
}
##For creating a correlaiton matrix plot for the corrgram function
panel.shadeNtext <- function (x, y, corr = NULL, col.regions, ...)
{
        #corr <- cor(x, y, use = "pair")
        rc <- rcorr(as.matrix(x), as.matrix(y))
        corr <- rc$r[1,2]
        n <- rc$n[1,2]
        #results <- cor.test(x, y, alternative = "two.sided")
        #est <- round(results$p.value, 3)
        est <- round(rc$P[1,2], 3)
        stars <- ifelse(est < .001, "< .001", as.character(est))
        ncol <- 14
        pal <- col.regions(ncol)
        col.ind <- as.numeric(cut(corr, breaks = seq(from = -1, to = 1,
                                                     length = ncol + 1), include.lowest = TRUE))
        usr <- par("usr")
        rect(usr[1], usr[3], usr[2], usr[4], col = pal[col.ind],
             border = NA)
        box(col = "lightgray")
        on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r <- formatC(corr, digits = 3, format = "f")
        #cex.cor <- .8/strwidth("-X.xxx")
        cex.cor <- .6/strwidth("-X.xxx")
        fonts <- ifelse(stars != "", 2,1)
        # option 1: stars:
        #text(0.5, 0.4, paste0(r,"\n", stars), cex = cex.cor)
        text(0.5, 0.7, r, cex = cex.cor*0.6)
        text(0.5, 0.4, paste0( '(', stars, ')' ), cex = cex.cor*0.3)
        text(0.5, 0.2, paste0( '(', n, ')' ), cex = cex.cor*0.3)
        # option 2: bolding:
        #text(0.5, 0.5, r, cex = cex.cor, font=fonts)
}

##For providing basic descriptives
mysum <- function(x, shouldPvalue = FALSE) {
        ret <-
                c(
                        n = sum(!is.na(x)),
                        M = round(mean(x, na.rm=TRUE),3),
                        SD = round(sd(x, na.rm=TRUE),3),
                        SE = round(sqrt(var(x, na.rm=TRUE)/length(!is.na(x))),3),
                        med = round(median(x, na.rm=TRUE),3)
                )
        if (shouldPvalue)
        {
                ttt <-  t.test(x, mu=0)
                pVal <- ifelse(ttt$p.value <= .0001, .0001, ttt$p.value)
                ret <- c(ret, p = pVal)
        }
        return(ret)
}

#For ggplot2: creates a few statistics that we can use for graphing
min.mean.sd.max <- function(x) {
        #r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
        xsd <- sd(x, na.rm=TRUE)
        xn <- sum(!is.na(x))
        #xerr <- qnorm(0.975)*xsd/sqrt(xn)
        xmin = min(x, na.rm = TRUE)
        xmax = max(x, na.rm = TRUE)
        xmean =  mean(x, na.rm=TRUE)
        r <- c(xmin, xmean-xsd, xmean, xmean + xsd, xmax)
        names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
        r
}

#For ggplot2: creates a few statistics that we can use for graphing (I often use this one for creating box-plots)
min.median.sd.max <- function(x) {
        #r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
        xsd <- sd(x, na.rm=TRUE)
        xn <- sum(!is.na(x))
        #xerr <- qnorm(0.975)*xsd/sqrt(xn)
        xmin = min(x, na.rm = TRUE)
        xmax = max(x, na.rm = TRUE)
        xmean =  mean(x, na.rm=TRUE)
        xmedian =  median(x, na.rm=TRUE)
        r <- c(xmin, xmean-xsd, xmedian, xmean + xsd, xmax)
        names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
        r
}

#Get the bottom and top of a confidence interval of a mean (not in the context of ANOVA). Used for creating box-plot.
##YBYB: Cannot use for publications, unless your test was a t-test because you had only two groups.
getCI <- function(x) {
        #r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
        xsd <- sd(x, na.rm=TRUE)
        xn <- sum(!is.na(x))
        xerr <- qnorm(0.975)*xsd/sqrt(xn)
        xmean <-  mean(x, na.rm=TRUE)
        xmin <- xmean-xerr
        xmax <- xmean+xerr
        r <- c(xmin, xmax)
        names(r) <- c("ymin", "ymax")
        r
}

n_fun <- function(x){
        return(data.frame(y = max(x)*1.1, label = paste0("n = ",sum(!is.na(x)))))
}

##A function for creating a matrix of boxplot graphs for a 2 by 2 design.
#The inData argument is the row-per-participant data.
#That data must have these variable names:
#DV - the variable for the y-axis
#xCond - the factor for the x-axis
#facetCond - the factor for separating the graphs.
twoByTwoBoxPlot <- function(inData, title='A 2 by 2 graph',
                            xTitle = 'IV', yTitle = 'DV', jitter=TRUE) {
        forplot <- ddply(.data=inData,
                         .variables = .(xCond, facetCond),
                         summarise,
                         xsd = sd(DV, na.rm=TRUE),
                         xn = sum(!is.na(DV)),
                         xmin = min(DV, na.rm = TRUE),
                         xmax = max(DV, na.rm = TRUE),
                         xtop = max(DV, na.rm = TRUE)+0.3,
                         xmean =  mean(DV, na.rm=TRUE),
                         xmedian =  median(DV, na.rm=TRUE),
                         xlower = mean(DV, na.rm=TRUE) - sd(DV, na.rm=TRUE),
                         xupper = mean(DV, na.rm=TRUE) + sd(DV, na.rm=TRUE),
                         xerrlow = mean(DV, na.rm=TRUE) - (qnorm(0.975)*sd(DV, na.rm=TRUE)/sqrt(length(!is.na(DV)))),
                         xerrup = mean(DV, na.rm=TRUE) + (qnorm(0.975)*sd(DV, na.rm=TRUE)/sqrt(length(!is.na(DV))))
        )


        geomPts <- geom_count(data = inData, aes(y=DV, x=factor(xCond), facet=factor(facetCond)))
        if (jitter)
        {
                geomPts <- geom_jitter(data = inData, aes(y=DV, x=factor(xCond), facet=factor(facetCond)), position=position_jitter(width=.8, height=0))
        }

        outBox <- ggplot(data=forplot, aes(x=factor(xCond), facet=factor(facetCond)))+
                geom_boxplot(aes(lower = xlower, upper = xupper, middle = xmedian, ymin = xmin, ymax = xmax), stat = "identity") +
                geom_errorbar(aes(ymin = xerrlow, ymax = xerrup), width = 0.5, linetype = 'longdash', colour = 'black') +
                facet_grid(.~facetCond)+
                #facet_grid(facetCond ~ .)+
                geom_point(aes(y=xmean),color='grey', size=4, shape=17) +
                ggtitle(title) +
                labs(x=xTitle, y=yTitle)+
                #xlab("Sorting condition") +
                #ylab("IAT Score") +

                scale_fill_brewer(palette="Greys")+
                geomPts+
                #ifelse(jitter,
                #  geom_jitter(data = inData, aes(y=DV, x=factor(xCond), facet=factor(facetCond)), position=position_jitter(width=.4, height=0))+
                #  geom_count(data = inData, aes(y=DV, x=factor(xCond), facet=factor(facetCond)))+
                #)+
                scale_size_continuous(range = c(1,3))+
                geom_text(aes(y = xmax,label = paste('n =', xn, sep=' ')), vjust = 0, nudge_y = 0.05, nudge_x = 0.05)

        return(outBox)
}

##A function for creating one boxplot (one factor design).
#The inData argument is the row-per-participant data.
#That data must have these variable names:
#DV - the variable for the y-axis
#xCond - the factor for the x-axis
oneFactorBoxPlot <- function(inData, title='one factor graph',
                             xTitle = 'IV', yTitle = 'DV', jitter=TRUE) {
        forplot <- ddply(.data=inData,
                         .variables = .(xCond),
                         summarise,
                         xsd = sd(DV, na.rm=TRUE),
                         xn = sum(!is.na(DV)),
                         xmin = min(DV, na.rm = TRUE),
                         xmax = max(DV, na.rm = TRUE),
                         xtop = max(DV, na.rm = TRUE)+0.3,
                         xmean =  mean(DV, na.rm=TRUE),
                         xmedian =  median(DV, na.rm=TRUE),
                         xlower = mean(DV, na.rm=TRUE) - sd(DV, na.rm=TRUE),
                         xupper = mean(DV, na.rm=TRUE) + sd(DV, na.rm=TRUE),
                         xerrlow = mean(DV, na.rm=TRUE) - (qnorm(0.975)*sd(DV, na.rm=TRUE)/sqrt(length(!is.na(DV)))),
                         xerrup = mean(DV, na.rm=TRUE) + (qnorm(0.975)*sd(DV, na.rm=TRUE)/sqrt(length(!is.na(DV))))
        )

        geomPts <- geom_count(data = inData, aes(y=DV, x=factor(xCond)))
        if (jitter)
        {
                geomPts <- geom_jitter(data = inData, aes(y=DV, x=factor(xCond)), position=position_jitter(width=.8, height=0))
        }

        outBox <- ggplot(data=forplot, aes(x=factor(xCond)))+
                geom_boxplot(aes(lower = xlower, upper = xupper, middle = xmedian, ymin = xmin, ymax = xmax), stat = "identity") +
                geom_errorbar(aes(ymin = xerrlow, ymax = xerrup), width = 0.5, linetype = 'longdash', colour = 'black') +
                #facet_grid(.~facetCond)+
                geom_point(aes(y=xmean),color='grey', size=4, shape=17) +
                ggtitle(title) +
                labs(x=xTitle, y=yTitle)+
                #xlab("Sorting condition") +
                #ylab("IAT Score") +
                scale_fill_brewer(palette="Greys")+
                geomPts+
                scale_size_continuous(range = c(1,3))+
                geom_text(aes(y = xmax,label = paste('n =', xn, sep=' ')), vjust = 0, nudge_y = 0.05, nudge_x = 0.05)

        return(outBox)
}

##A function for creating many boxplot (one factor design) - one for each variable in the data.
#The inData argument is a "melted" dataset. It must have the variables:
#value - the variable for the y-axis
#facetCond - the name of the DV variable (usually outputs as the column 'variable' from the melt function)
#xCond - the variable for the x-axis.
manyBoxes <- function(inData, title='one factor graph',
                      xTitle = 'IV', yTitle = 'DV', jitter=TRUE, freeY=TRUE)
{

        geomPts <- geom_count(shape=1)
        if (jitter)
        {
                geomPts <- geom_jitter(position=position_jitter(width=.8, height=0), shape=1)
        }

        facetW <- facet_wrap( ~ facetCond)
        if (freeY)
        {
                facetW <- facet_wrap( ~ facetCond, scales="free_y")
        }

        outG <- ggplot(data = inData, aes(x=xCond, y=value, facet=facetCond)) +
                #geom_boxplot(aes(fill=sortCond))+
                scale_fill_brewer(palette="Greys")+
                stat_summary(fun.data = min.median.sd.max, geom = "boxplot") +
                stat_summary(fun.data = getCI, geom = "errorbar", width=0.5, linetype = 'longdash', colour = 'black') +
                stat_summary(fun.y="mean", geom="point", shape=17, size=4, color="grey") +

                #So far, I couldn't figure out how to add the n of each bar above the bar.
                stat_summary(fun.data = n_fun, geom = "text", size=2, color="black") +
                #geom_jitter(position=position_jitter(width=.8, height=0), shape=1)+
                #geom_text(aes(y = "max",label = getN), vjust = 0, nudge_y = 0.05, nudge_x = 0.05)+
                geomPts+
                ggtitle(title) +
                labs(x=xTitle, y=yTitle)+
                facetW

        return(outG)
}

