[1mdiff --git a/R/ec-evaluation.r b/R/ec-evaluation.r[m
[1mindex 2562661..dd0e16c 100644[m
[1m--- a/R/ec-evaluation.r[m
[1m+++ b/R/ec-evaluation.r[m
[36m@@ -575,17 +575,6 @@[m [mfind_dynlag <- function(x, dyn) {[m
 	maxis <- ind[which.max(abs(x[ind] - avg))][m
 	c(index = maxis, tau = maxis - m)[m
 }[m
[31m-fixlag_index <- function(x, fix) {[m
[31m-	n <- length(x)[m
[31m-	if (n %% 2) {[m
[31m-		# ungerade[m
[31m-		m <- (n + 1) / 2[m
[31m-	} else {[m
[31m-		# gerade[m
[31m-		m <- n / 2 + 1[m
[31m-	}[m
[31m-    m + fix[m
[31m-}[m
 [m
 # reduce cospecta "resolution" for plotting[m
 reduce_cospec <- function(cospec,freq,length.out=100){[m
[36m@@ -1208,7 +1197,6 @@[m [mprocess_ec_fluxes <- function([m
         , high_cont_sec = 2[m
         , cont_pts = 5[m
         , subintervals = TRUE[m
[31m-        , subint_prefix = 'subint_'[m
         , subint_n = 5[m
         , subint_detrending = c(u = 'linear', v = 'linear', w = 'linear', T = 'linear', nh3_ppb = 'linear', nh3_ugm3 = 'linear', h2o_mmolm3 = 'linear', co2_mmolm3 = 'linear')[m
         , oss_threshold = 0[m
[36m@@ -1357,12 +1345,7 @@[m [mprocess_ec_fluxes <- function([m
         damping_reference <- fix_defaults(damping_reference, covariances[scalar_covariances])[m
         damping_lower <- fix_defaults(damping_lower, covariances[scalar_covariances])[m
         damping_upper <- fix_defaults(damping_upper, covariances[scalar_covariances])[m
[31m-        # sub intervals (4 to 8 are valid)[m
[31m-        if (!(is.numeric(subint_n) && (subint_n %% 1 == 0) && subint_n >= 4 &&[m
[31m-            subint_n <= 8)) {[m
[31m-            stop('argument "subint_n" must be an integer number between 3 and 10')[m
[31m-        }[m
[31m-        subint_detrending <- fix_defaults(subint_detrending, variables)[m
[32m+[m[32m        # subint_detrending <- fix_defaults(subint_detrending, variables)[m
         ts_vars <- variables[m
         if (!ht_null) {[m
             # add ht8700 quality parameters (oss) to plotting[m
[36m@@ -2868,8 +2851,109 @@[m [mogive_model <- function(fx, m, mu, A0, f = freq) {[m
                 }[m
             }, simplify = FALSE))[m
 [m
[32m+[m[32m            # sub-intervals:[m
[32m+[m[32m            # -----------------------------------------------------------------------[m
[32m+[m[32m            if (subintervals) {[m
[32m+[m
[32m+[m[32m                browser()[m
[32m+[m
[32m+[m[32m                # copy original .SD for sub-interval processing[m
[32m+[m[32m                SDsub <- copy(.SD)[m
[32m+[m[32m                # detrending sonic data[m
[32m+[m[32m                cat("~~~\ndeterending sonic data...\n")[m
[32m+[m[32m                wind <- detrend_sonic_data(SDsub, detrending, rec_Hz)[m
[32m+[m
[32m+[m[32m                # calculate some turbulence parameters and collect some wind parameters[m
[32m+[m[32m                # --------------------------------------------------------------------------[m[41m [m
[32m+[m[32m                wind_stats <- wind_statistics(wind, z_canopy[[1]], z_ec[[1]],[m[41m [m
[32m+[m[32m                    ustar_method = ustar_method)[m
[32m+[m[32m                # detrending scalars[m
[32m+[m[32m                if (length(scalars)) {[m
[32m+[m[32m                    scalar_list <- SDsub[, I(lapply(.SD, na.omit)),[m[41m [m
[32m+[m[32m                        .SDcols = scalars][m
[32m+[m[32m                    # detrend scalars[m
[32m+[m[32m                    # --------------------------------------------------------------[m[41m [m
[32m+[m[32m                    cat("~~~\ndetrending scalars...\n")[m
[32m+[m[32m                    detrended_scalars <- mapply(trend, y = scalar_list, method =[m[41m [m
[32m+[m[32m                        detrending[scalars], MoreArgs = list(Hz_ts = rec_Hz),[m[41m [m
[32m+[m[32m                        SIMPLIFY = FALSE[m
[32m+[m[32m                    )[m
[32m+[m[32m                    # assign to SDsub[m
