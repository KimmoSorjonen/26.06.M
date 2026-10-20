
############################# BIENVENUE #############################

############# I SOLEMNLY SWEAR THAT I AM UP TO NO GOOD ##############

#################################
## Loading packages

library(lavaan)

#################################

n <- 504 ## sample size

rm <- matrix(c( ## correlations reported by Wang et al. (2026)
  
  1.000, 0.115, 0.263, 0.230,
  0.115, 1.000, 0.636, 0.737,
  0.263, 0.636, 1.000, 0.622,
  0.230, 0.737, 0.622, 1.000), nrow=4)

colnames(rm) <- rownames(rm) <- c("DO","SE","AC","PR") ## names of variables

#################################
## Alternative model

altmod <- "

## Loadings

CSE =~ DO+1*SE+AC+PR

## (Error) variances

DO ~~ DO
SE ~~ SE
AC ~~ AC
PR ~~ PR

CSE ~~ CSE

"

fit.alt <- lavaan(altmod, sample.cov=rm, ## fitting model to data 
            sample.nobs=n, meanstructure=F)

summary(fit.alt, fit.measures=T, ci=T, standardized=T, rsq=T) ## the results

mod2 <- paste(altmod,"\nDO~~SE") ## addinf correlated residuals

fit2 <- lavaan(mod2, sample.cov=rm, ## fitting model to data 
        sample.nobs=n, meanstructure=F)

summary(fit2, fit.measures=T, ci=T, standardized=T, rsq=T) ## the results


########################## MISCHIEF MANAGED #########################

############################# AU REVOIR #############################

