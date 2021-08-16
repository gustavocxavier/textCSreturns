## Monthly Text-based Forecast -------------------------------------------------

## The code below predicts the expected investment growth (EIG) of
## Hou, Moe, Xue, and Zhang (2021), which is the change in investment-to-assets.
##
## In other words, it is :
##   
##   [(A_{FY+1} - A_{FY}) / A_{FY}] - [(A_{FY} - A_{FY-1}) / A_{FY-1}] - ,
## 
## where A_{FY} is the total assets of the firm in the current fiscal year.

## Load library ----------------------------------------------------------------
library(dplyr)
library(data.table)
library(glmnet)
library(tidytext)
library(lubridate)
library(purrr)

## Set path --------------------------------------------------------------------
myPath <- "C:/Dropbox/Code/investmentPlan/GitHub/EIGtext/2_pipeline/"

## Set Y and X -----------------------------------------------------------------
## Load EIG benchmark measure predicted as Hou, Moe, Xue, and Zhang (2021) (hereafter HMXZ)

hmxz <- readRDS(paste0(myPath,"4a_hmxz.rds"))
db_YX <- hmxz %>% select(permno:fiscaldate, Y = d1_ia, cop, q, dROE, me, Y_hat = EIG)
rm(hmxz)

## Include fiscal year in db_YX
ccm_a <- readRDS(paste0(myPath,"1b_ccm_a.rds"))
ccm_a <- ccm_a %>% select(gvkey, permno, fiscaldate = datadate, fyear)
db_YX <- db_YX %>% left_join(ccm_a)
rm(ccm_a)


## Include cik in db_YX
sccm_a <- readRDS(paste0(myPath,"3a_sccm_a.rds"))
sccm_a <- sccm_a %>%
  mutate(fyear = filing.year - 1) %>%
  ungroup %>% 
  select(permno, gvkey, fyear, cik)
db_YX <- db_YX %>% left_join(sccm_a) %>% na.omit
rm(sccm_a)


### Include return in db_YX and save as db_YX2
crsp_m <- readRDS(paste0(myPath,"1a_cleaned_crsp_m2.rds"))
crsp_m <- crsp_m %>% select(permno, date, return=retadj)
db_YX2 <- db_YX %>% left_join(crsp_m, by = c("permno", "date"))

### Load word data -------------------------------------------------------------
DT <- fread(file = "~/DT_AllSample.csv",
            select=c(year_gvkey="numeric",
                     cik = "numeric",
                     filing.year="numeric", term= "character",
                     ngrams="numeric",
                     wFreq="numeric",
                     # tf="numeric", idf="numeric", tf_idf="numeric",
                     sentiment="factor"),
            key = "year_gvkey", na.strings = "")

# Require at least 250 words to appear in the MD&A section (Loughran)
DT[, totalWodsPerDocument := sum(wFreq), by=c("year_gvkey")]
nrow(DT[totalWodsPerDocument<250])/nrow(DT) # Remove x% of documents
DT <- DT[totalWodsPerDocument>=250]
DT[,totalWodsPerDocument := NULL]

# remove stop words
data("stop_words")
colnames(stop_words)[1] <- "term"
DT <- DT %>% anti_join(stop_words, by = "term")
rm(stop_words)

# remove numbers
nums <- DT %>% filter(stringr::str_detect(term, "^[0-9]")) %>% select(term) %>% unique()
nums
DT <- DT %>% anti_join(nums, by = "term")
rm(nums)

# ## require each word and phrase to be included in the MD&A of at least 10
# ## firms in each year. (Frankel)
# DT[, n_MDA_with_this_term := .N, by=c("filing.year","term")]
# DT[n_MDA_with_this_term<10,]
# DT <- DT[n_MDA_with_this_term>=10,]
# DT[, n_MDA_with_this_term := NULL]

## Include date.filed in DT
sec_index <- readRDS("data/sec_index.rds")
sec_index <- select(sec_index, cik, date.filed, filing.year=year)
DT
# left_join(x, y, by = "Id")
# y[x, on = "Id"]
setDT(sec_index)
DT <- sec_index[DT, on = c("cik", "filing.year")]
setkey(DT, date.filed, year_gvkey)
# TODO: Resolver date.filed que ficou NA
# unique(DT[is.na(date.filed)]$filing.year)

## Set date_range --------------------------------------------------------------

range_x <- unique(db_YX$date)
range_x <- sort(range_x)
range_x <- c(first(range_x),last(range_x))

range_z <- unique(DT$date.filed)
range_z <- sort(range_z)
range_z <- c(first(range_z),last(range_z))

range_all <- c(
  last(sort(c(range_x[1], range_z[1]))),
  first(sort(c(range_x[2], range_z[2])))
)
range_all[1] <- range_all[1] %m+% months(12)

range_all <- seq(ymd(range_all[1]),ymd(range_all[2]), by = '1 month')
range_all <- seq(ymd("1996-01-01"),ymd("2018-12-01"), by = '1 month')
range_all <- paste0(format(range_all, format="%Y-%m"),"-", days_in_month(range_all))

## Set db_Z --------------------------------------------------------------------
# Zt_12 => most recent filing date between month t-24 and t-13
# Zt    => most recent filing date between month t-12 and t-1
#   Select all filing.date between month t-24 and t-13
#     Select the most recent of each cik
#       tf-idf by month { tf_idf if n>=1, otherwise 0 }
#       normalize by month { normalize if n>=1, otherwise 0 }
#       Create a Column t = t-12 | as.factor(t, t-12)
#   Select all filing.date between month t-12 and t-1
#     Select the most recent of each cik
#       tf-idf by month { tf_idf if n>=1, otherwise 0 }
#       normalize by month { normalize if n>=1, otherwise 0 }
#       Create a Column t = t | as.factor(t, t-12)
#   db_Zt <- rbind(db_Zt_12, db_Zt)
#   Z <- sparsematrix (db_Zt)
#   Zt    <- Z[db_Zt$t=="t",]
#   Zt_12 <- Z[db_Zt$t=="t-12",]

# Organize Z

# firms <- db_YX[date=="1999-03-31", .(cik, gvkey, permno, date)]

hmxz <- readRDS(paste0(myPath,"4a_hmxz.rds"))
recursive_fun <- function() {
  for (cs_month in range_all) {
    print(cs_month)
    # cs_month <- c("1996","01")
    # cs_month <- paste0(cs_month[1], "-", cs_month[2], "-",
    #                    days_in_month(as.numeric(cs_month[2])))
    cs_month <- ymd(cs_month)
    
    YX <- db_YX[date==cs_month, .(cik, gvkey, permno, date, me, Y, cop, q, dROE)]
    
    # What is the difference between the FYEAR and the DATADATE. When we extract
    # the year from the datadate, it is not always the same as the fyear. Why is
    # this? When examining across time periods, which should we use?
    # http://www.stat.rice.edu/~dobelman/courses/482/faq/FAQ00044.htm
    
    
    db_Z <- DT[between(date.filed,cs_month %m-% months(24), cs_month %m-% months(1))]
    
    # Align same firms for db_YX and db_Z
    db_Z <- db_Z[cik %in% unique(YX$cik)]
    YX <- YX[cik %in% unique(db_Z$cik)]
    if (nrow(db_Z)<50 | nrow(YX)<50) { next }
    db_Z[, t := factor(NA, levels = c("t", "t-12"))]
    db_Z[date.filed < cs_month %m-% months(12), t := "t-12"]
    db_Z[date.filed >= cs_month %m-% months(12), t := "t"]
    
    ## Remove older document if there is more than one
    remove_doc <- matrix(NA, nrow=2)
    while (nrow(remove_doc)>0) {
      remove_doc <- db_Z %>% select(cik, t, year_gvkey, date.filed) %>% unique %>% 
        group_by(t, cik) %>%
        summarise(n = n(), date.filed=first(date.filed) ) %>% filter(n>1)
      db_Z <- db_Z %>% left_join(remove_doc) %>% filter(is.na(n)) %>% select(-n)
    }
    rm(remove_doc)
    
    ## Compute tf-idf ----------------------------------------------------------
    
    ## Compute TF
    
    # Number of times term w appears in a document
    # divided by the total number of terms in the document
    db_Z[, tf := (1+log(wFreq)) / ( 1+log(mean(wFreq)) ), by=year_gvkey]
    
    ### Compute IDF
    ## Do total de documentos em quantos a palavra aparece.
    ## Quanto mais documentos ela aparece mais o idf tende a zero.
    # by month t
    db_Z[, Nd := uniqueN(year_gvkey), by=c("t")]  # Total number of documents
    db_Z[, df := .N, by=c("t", "term")]      # Number of documents with term w in it
    
    db_Z[, idf := log(Nd/df) ]
    db_Z[, tf_idf := tf*idf]
    
    ## Set Z Matrix ----------------------------------------------------------------
    db_Z <- db_Z[order(t, cik)]
    Z <- tidytext::cast_sparse(db_Z, row = cik, column = term, value = tf_idf)
    Zt    <- Z[as.character(unique(db_Z[t=="t", .(cik)])$cik),]
    Zt_12 <- Z[as.character(unique(db_Z[t=="t-12", .(cik)])$cik),]
    # unique(db_Z[t=="t", .(cik)])
    # head(Z[as.character(unique(db_Z[t=="t", .(cik)])$cik),1:7])
    # tail(Z[as.character(unique(db_Z[t=="t", .(cik)])$cik),1:7])
    # unique(db_Z[t=="t-12", .(cik)])
    # head(Z[as.character(unique(db_Z[t=="t-12", .(cik)])$cik),1:7])
    # tail(Z[as.character(unique(db_Z[t=="t-12", .(cik)])$cik),1:7])
    
    ## organize Yt, Xt_12 and Zt_12 -----------------------------------------------
    
    ## Merge Y, X and Z matrix -----------------------------------------------------
    # YXt_12 => most recent filing date between month t-24 and t-13
    # YXt    => most recent filing date between month t-12 and t-1
    #   Select all filing.date between month t-24 and t-13
    #     Select the most recent of each cik/gvkey
    #       Create a Column t = t-12 | as.factor(t, t-12)
    #   Select all filing.date between month t-12 and t-1
    #     Select the most recent of each cik/gvkey
    #       Create a Column t = t | as.factor(t, t-12)
    #   Xt    <- db_YX[t=="t",]
    #   Xt_12 <- db_YX[t=="t-12",]
    #   Yt    <- db_YX[t=="t",]
    #   Yt_12 <- db_YX[t=="t-12",]
    dim(Zt_12)
    YX[cik %in% rownames(Zt_12)][order(cik)]
    YX <- YX %>%
      select(cik, Y, cop, q, dROE) %>% 
      filter(cik %in% rownames(Zt_12)) %>% distinct(cik, .keep_all = T) %>% 
      arrange(cik)
    
    ## Set Yt ----------------------------------------------------------------------
    Yt <- as.matrix(YX[,.(Y)])
    
    ## Set Xt_12 -------------------------------------------------------------------
    Xt_12 <- as.matrix(YX[,.(cop, q, dROE)])
    
    ## Normalize Zt_12 -------------------------------------------------------------
    # mu <- colMeans(Zt_12)
    # sigma <- apply(Zt_12, 2, sd)
    # Zt_12 <- Zt_12b - mu / sigma
    # Zt_12[is.na(Zt_12)] <- 0
    
    ## Estimate glmnet Yt = Xt_12 + Zt_12 -----------------
    # (alpha=0.5, lambda=AICc) 
    
    # Yt = Xt_12 + Zt_12
    
    library(glmnet)
    # X <-  matrix(runif(50000), nrow=100)
    # Z <- matrix(rnorm(300), ncol=3)
    # Y <- matrix(rnorm(100),ncol=1)
    fit <- glmnet(x = cbind(Xt_12, as.matrix(Zt_12)), y = Yt, family = "gaussian",
                  penalty.factor = c(  rep(0, ncol(Xt_12)),
                                       rep(1, ncol(Zt_12))  ),
                  # weights = trainSample$me,
                  alpha = 0.5,
                  lambda =  seq(from = 0.01, to = 0.99, by = 0.01))
    glmnet_aicc <- function(fit){
      with(fit,
           {
             tLL <- nulldev - deviance(fit)
             # tLL <- nulldev - nulldev * (1 - dev.ratio)
             k   <- df   # The number of nonzero coefficients
             n   <- nobs # Number of observations
             AIC <- - tLL + 2 * k 
             AICc <- AIC + 2 * k * (k + 1) / (n - k - 1)
             out <-  list()
             out$bestAICc <- AICc[which.min(AICc)]
             out$nonzero_coef <- k[which.min(AICc)]
             out$lambda <- lambda[which.min(AICc)]
             print(out)
             return(lambda[which.min(AICc)])
           })
    }
    lambda <- glmnet_aicc(fit)
    data.table(variable = rownames(coef(fit, s = lambda)),
               coefficient = as.vector(coef(fit, s = lambda))) %>%
      filter(coefficient != 0)
    
    
    
    
    
    
    
    
    ## forecast Yt+12 using Xt and Zt as newx ----------------------------------------
    ## Use the model to forecast Y h=1 (t+12)
    ## Forecast Yt 12 months ahead
    ## Yh1 = Xt + Zt
    
    # h=12, h=24, h=36
    
    # YX one hear ahead (h=12)
    YX1 <- db_YX[date==(cs_month %m+% months(12)), .(cik, gvkey, permno, date, me, Y, cop, q, dROE)]
    YX1
    
    ## Merge Y, X and Z matrix
    dim(Zt)
    YX1[cik %in% rownames(Zt)][order(cik)]
    YX1 <- YX1 %>%
      select(date, cik, Y, cop, q, dROE) %>% 
      filter(cik %in% rownames(Zt)) %>% distinct(cik, .keep_all = T) %>% 
      arrange(cik)
    Zt <- Zt[rownames(Zt) %in% YX1$cik,]
    if (nrow(YX1)<50 | nrow(Zt)<50) { next }
    
    # ## Normalize Zt -------------------------------------------------------------
    # mu <- colMeans(Zt)
    # sigma <- apply(Zt, 2, sd)
    # Zt <- Zt - mu / sigma
    # Zt[is.na(Zt)] <- 0
    
    
    ## Set Yt+12 
    Yt12 <- as.matrix(YX1[,.(Y)])
    
    ## Set Xt 
    Xt <- as.matrix(YX1[,.(cop, q, dROE)])
    
    
    f <- as.vector(predict(fit,
                           newx = cbind(Xt, as.matrix(Zt)),
                           s = lambda))
    mean((Yt12 - f)^2)
    
    # Export Results -------------------------------------------------------------
    out <- list()
    out$fit <- fit
    out$lambda <- lambda
    out$words <- data.table(variable = rownames(coef(fit, s = lambda)),
                            coefficient = as.vector(coef(fit, s = lambda))) %>%
      filter(coefficient != 0)
    out$serie <- YX1 %>% mutate(f_enet = f) %>% 
      left_join(
        hmxz %>% select(date, Y=d1_ia, f_bench=EIG)
      )
    # %>% 
    #   mutate(e_enet = Y - f_enet, e_bench = Y - f_bench) %>% 
    #   summarise(MSE = mean(e_enet^2), MSE_bench=mean(e_bench^2))
    saveRDS(out, file = paste0("~/monthly_hmxz_1/", cs_month,".rds"))
  }
  
}
# debug(recursive_fun)
recursive_fun()

## Functions for Statistical Tests ---------------------------------------------
CW_test <- function(obs,mod,bench){
  MSPE2 <- (obs-bench)^2 - (obs-mod)^2 + (bench-mod)^2
  eq <- t.test(MSPE2, alternative="greater", conf.level=0.90)
  
  # pvalue2 <- eq$p.value
  # return(round(pvalue2,2))
  
  # t.stat2 <- eq$statistic
  # return(round(t.stat2,2))
  
  return(mean(MSPE2))
}
r2csos <- function(y, y_mean, f, f_mean) {
  n <- length(y)
  MSFE  <- sum( ((y - y_mean) - (f - f_mean))^2 ) / n
  sigma <- sum( (y - y_mean)^2 ) / n
  R2 <- 1 - (MSFE / sigma)
  R2 <- round(R2*100,2)
  return(R2)
}
NW <- function(sample1, sample2=0) {
  ## Performs one and two sample t-tests adjusted for NeweyWest standard errors
  
  require(sandwich)
  require(lmtest)
  
  fit <- lm( (sample1-sample2) ~ 1 )
  
  # ## Show selected lag by automatic selection procedure
  # print(NeweyWest(fit, verbose = T))
  
  ## Show estimates, t-value and p-value
  print(coeftest(fit, vcov=NeweyWest(fit))[1,])
  
  t.value <- coeftest(fit, vcov=NeweyWest(fit))[1,"t value"]
  return(t.value)
}


## Check results ---------------------------------------------------------------
files <- dir(path = "~/monthly_hmxz_1", pattern = "*.rds", full.names = T)

## Checking files
head(files)
tail(files)
(files %>% map(readRDS))[[1]] %>% str

# ## List of months
# files %>% map(readRDS) %>% map(~ .x[["serie"]]) %>% bind_rows %>% 
#   select(date) %>% unique %>% as.matrix %>%  as.vector -> month_list
(1996:2018)[1:11]
(1996:2018)[12:23]
## Best Words
Map(cbind,
    files %>% map(readRDS) %>% map(~ .x[["words"]]),
    month = ymd(stringr::str_sub(files, -14, -5))) %>% bind_rows %>%
  filter( !(variable %in% c("(Intercept)", "cop", "q", "dROE")) ) %>%
  mutate(year = year(month)) %>%
  mutate(s = if_else(year<2006, 1, 2)) %>%
  group_by(variable) %>% 
  summarise(coef = abs(round( mean(coefficient),5))) %>%
  arrange(-abs(coef)) %>% data.frame

## Evaluation All Serie
serie <- files %>%
  map(readRDS) %>%
  map(~ .x[["serie"]]) %>%
  bind_rows %>%
  mutate(e_bench = Y - f_bench,
         e_enet  = Y - f_enet)

# Evaluation by cross-section
serie %>% group_by(date) %>%
  mutate(
    y_mean = mean(Y),
    f_enet_mean = mean(f_enet),
    f_bench_mean = mean(f_bench),
  ) %>%
  summarise(
    MSE_bench = mean(e_bench^2),
    MSE_enet  = mean(e_enet^2),
    # MSFE Rapach
    MSFE_bench = sum( ( (Y-y_mean) - (f_bench - f_bench_mean) )^2 )/n(),
    MSFE_enet = sum( ( (Y-y_mean) - (f_enet - f_enet_mean) )^2 )/n(),
    ## RELATIVE MEASURES
    RMSE = MSE_enet / MSE_bench,
    # Gu, Kelly and Xiu modified Diebold-Mariano for cross-section 
    # + sign indicates difference (e_bench > e_enet)
    DMcs = mean((e_bench^2) - (e_enet^2)), 
    R2_CSOS = r2csos(Y, y_mean, f = f_enet, f_enet_mean),
    # R2_CSOSb = r2csos(f_bench, f_bench_mean, f = f_enet, f_enet_mean),
    R2_OOS = 1 - MSFE_enet / MSFE_bench,
    RMSFE = MSFE_enet / MSFE_bench, # Relative MSFE
    CW = CW_test(Y, f_enet, f_bench) # Clark West Test for Nested Models
  ) %>% as.data.table -> x
x
round(colMeans(x[,-1]),4)
round(NW(x$CW),4)
round(NW(x$R2_OOS),4)
# NW(1-x$RMSFE)
# NW(x$R2_CSOS)
# NW(1-x$RMSE)
# NW(x$DMcs)
# t.test(1-x$RMSFE, alternative = "greater", conf.level = 0.9)
# t.test(x$CW, alternative = "greater", conf.level = 0.9)
# t.test(x$DMcs, alternative = "greater", conf.level = 0.9)

# Use time-series average mean of cross-section Diebold-Mariano test and
# Newey-West standard error, which is more likely to satisfy the mild regularity conditions neeeded
# for asymptotic normality, and in turn provides appropriate p-values for model
# comparison tests.
# Gui Kelly


# Compute Encompass Model ------------------------------------------------------
library(purrr)
library(broom)
library(tidyr)
serie %>%
  nest(data = -date) %>% 
  mutate(
    # eA = a + b(eA - eB) + e
    fit = map(data, ~ lm(e_bench ~ (e_bench - e_enet), data=.x ) ),
    tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied) %>% 
  select(date, estimate) -> encompass_test 

plot(ts(encompass_test$estimate, start = c(1997,1), frequency = 12))
encompass_test <- encompass_test %>%
  mutate(theta = if_else(estimate<0, 0, estimate)) %>% 
  mutate(theta = if_else(estimate>1, 1, estimate))
plot(ts(encompass_test$theta, start = c(1997,1), frequency = 12))

NW(encompass_test$estimate) # null hypothesis theta != 0
NW(1, encompass_test$estimate) # null hypothesis theta != 1

# When theta = 0, A encompasses B on average over the forecast evaluation period,
# meaning that B does not contain useful information for forecasting
# crosssectional returns (in terms of cross-sectional MSFE) beyond the
# information already contained in A; alternatively, when theta > 0, A does not
# encompass B, so that B does provide useful information beyond that already
# contained in A. Analogously, if theta = 1, then B encompasses A on average;
# if  theta < 1, then B does not encompass A.


## Economic Value --------------------------------------------------------------
# DT %>% group_by(date) %>% count %>% filter(year(date)>1975) %>% arrange(n)

crsp_m <- readRDS(paste0(myPath,"7b_crsp_m.rds"))
crsp_m <- crsp_m %>% select(permno, date, retadj, me)



portf <- serie %>%
  left_join(select(db_YX, permno, gvkey, cik, date, Y), by = c("date", "cik", "Y")) %>% 
  select(cik, permno, gvkey, date, everything())
portf <- portf %>% left_join(crsp_m)

## get return


setwd("C:/Dropbox/Code/investmentPlan/GitHub/investmentGrowthLC")
qmodel <- read.csv("0_data/q5_factors_monthly_2019a.csv", sep = ",") %>%
  as.data.table

qmodel$rn <- NULL
qmodel$R_F <- NULL
qmodel

ff <- read.csv("0_data/F-F_Research_Data_5_Factors_2x3.csv", sep = ",",skip = 3) %>% 
  as.data.table

ff <- ff %>%
  mutate(year  = as.numeric(substr(X, 1, 4))) %>% 
  mutate(month = as.numeric(substr(X, 5, 6))) %>% 
  select(year, month, everything()) %>% 
  select(-X)


setwd("~/")
n_port <- 5
portf[, characteristic := Y ]
portf[, p := cut(characteristic, quantile(characteristic, probs = 0:n_port/n_port),
                 labels = FALSE, include.lowest = TRUE), by=date]
portf
as_tibble(portf) %>% group_by(p, date) %>%
  summarise(ewret = mean(retadj),
            vwret = weighted.mean(retadj, w = me),
            IG = mean(Y),
            EIG = mean(f_enet)) %>%
  arrange(date) %>% as.data.table %>% arrange(p, date) -> portf_10
portf_10
inner_join(portf_10 %>% ungroup %>% filter(p==n_port) %>% select(date, high=vwret),
           portf_10 %>% ungroup %>% filter(p==1     ) %>% select(date, low=vwret),
           by = "date") %>%
  mutate(year = year(date),
         month = month(date),
         high = round(high*100,2),
         low = round(low*100,2),
         ls = high - low) %>% 
  select(year, month, high, low, ls) -> longshort
db_return <- longshort %>% left_join(ff) %>%
  mutate(LS = ls) %>% 
  mutate(HIGH = high - RF) %>% 
  mutate(LOW = low - RF) # %>% select(LS, HIGH, LOW, Mkt.RF:CMA)
summary(lm(HIGH ~ Mkt.RF + SMB + HML + RMW + CMA, data = db_return))
summary(lm(LOW ~ Mkt.RF + SMB + HML + RMW + CMA, data = db_return))
summary(lm(LS ~ Mkt.RF + SMB + HML + RMW + CMA, data = db_return))

db_return %>%
  # mutate(r = (Mkt.RF)/100) %>%
  mutate(r = (LS)/100) %>%
  summarise(
    # HIGH_r = ((1+mean(HIGH)/100)^12-1)*100,
    # HIGH_SR = HIGH_r*12 / sd(HIGH*12),
    # LOW_r = ((1+mean(LOW)/100)^12-1)*100,
    # LOW_SR = LOW_r*12 / sd(LOW*12),
    ann_return = ( (1+mean(r))^12 - 1 ) * 100,
    ann_sd = sd(r) * sqrt(12) * 100,
    ann_SR = ann_return / ann_sd
  )

# db_return <- longshort %>% left_join(ff) %>% select(ls:CMA)
# summary(lm(ls ~ ., data = db_return))


# db_return <- longshort %>% left_join(qmodel) %>% select(ls:R_ROE)
# summary(lm(ls ~ ., data = db_return))

## Best words ------------------------------------------------------------------
(files %>% map(readRDS))[[1]] %>% str

Map(cbind,
    files %>% map(readRDS) %>% map(~ .x[["words"]]),
    month = ymd(stringr::str_sub(files, -14, -5))) %>% bind_rows %>%
  filter( !(variable %in% c("(Intercept)", "cop", "q", "dROE")) ) %>%
  mutate(year = year(month)) %>%
  #mutate(s = if_else(year>2006, 1, 2)) %>%
  # group_by(year, variable) %>% 
  group_by(variable) %>% 
  summarise(coef = (round( mean(coefficient),5))) %>%
  arrange(-(coef)) %>% data.frame -> best_words

best_words %>% arrange(year)

best_words %>% left_join(
  DT %>% select(variable=term, sentiment) %>% na.omit %>% unique  
) -> best_words

best_words %>% 
  mutate(sent_charged = if_else(!is.na(sentiment), TRUE, FALSE)) %>%
  group_by(sent_charged) %>% count

x <- cbind(best_words %>% slice_head(coef, n=30),
           best_words %>%
             filter(!(variable %in% c("tejas", "tci"))) %>% 
             slice_tail(coef, n=30) %>% arrange(coef))
x
rownames(x) <- NULL

xtable::xtable(
  as.matrix(x)
)

wplot <- unique(DT[term=="patent", .(year=filing.year, term, nword=tf_idf)])
wplot %>% group_by(year) %>% summarise(n=sum(nword)) %>% 
  select(n) %>% ts(start = c(1994)) %>% plot

Map(cbind,
    files %>% map(readRDS) %>% map(~ .x[["words"]]),
    month = ymd(stringr::str_sub(files, -14, -5))) %>% bind_rows %>%
  filter( !(variable %in% c("(Intercept)", "cop", "q", "dROE")) ) %>%
  mutate(year = year(month)) %>%
  #mutate(s = if_else(year>2006, 1, 2)) %>%
  group_by(year, variable) %>% 
  # group_by(variable) %>% 
  summarise(coef = (round( mean(coefficient),5))) %>%
  arrange(-(coef)) %>% data.frame -> best_words

best_words %>% arrange(year, variable)

## Forecast Returns ------------------------------------------------------------

## Load CRSP/Compustat data

## Load Text Data

## Estimate Models

## Forecast

## Evaluation
