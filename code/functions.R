##---------------------------------------------------##
## Functions for synthetic data summary
##---------------------------------------------------##

## Risk measure: DCR, NNDR, DUPI
dist_euclid_ft <- function(orig, synt){
  # variable order & identify categorical vars.
  synt <- synt[,names(orig)]
  is_cat <- sapply(orig, is.factor)
  
  # scaling numeric variables
  vec_mean <- apply(orig[,!is_cat], 2, mean)
  vec_sd   <- apply(orig[,!is_cat], 2, sd)
  
  vec_sd[vec_sd==0] <- 1
  
  orig_scale <- orig
  synt_scale <- synt
  
  orig_scale[,!is_cat] <- sweep(orig_scale[,!is_cat], 2, vec_mean, "-")
  orig_scale[,!is_cat] <- sweep(orig_scale[,!is_cat], 2, vec_sd  , "/")
  
  synt_scale[,!is_cat] <- sweep(synt_scale[,!is_cat], 2, vec_mean, "-")
  synt_scale[,!is_cat] <- sweep(synt_scale[,!is_cat], 2, vec_sd  , "/")

  # Dummy-encode
  orig_dummy <- orig_scale
  synt_dummy <- synt_scale
  
  if(any(is_cat)){
    orig_dummy[,is_cat] <- lapply(orig_scale[,is_cat], function(s) addNA(factor(s, ordered = FALSE)))
    synt_dummy[,is_cat] <- lapply(synt_scale[,is_cat], function(s) addNA(factor(s, ordered = FALSE)))
    
    orig_dummy <- model.matrix(~ . - 1, data = orig_dummy)
    synt_dummy <- model.matrix(~ . - 1, data = synt_dummy)
  }

  dist_mat <- as.matrix( dist( rbind(orig_dummy, synt_dummy), method = "euclidean" ) )
  
  n1 <- nrow(orig_dummy)
  n2 <- nrow(synt_dummy)
  
  dist_mat_results <- list()
  
  dist_mat_results$dist_os <- dist_mat[1:n1, (n1+1):(n1+n2)]
  dist_mat_results$dist_oo <- dist_mat[1:n1, 1:n1]
  dist_mat_results$dist_ss <- dist_mat[(n1+1):(n1+n2), (n1+1):(n1+n2)]
  return(dist_mat_results)
}


DCR_ft <- function(dist_mat){ ## row: original(n1) / col: synthetic(n2) n1*n2 matirix
  dcr <- apply(dist_mat, 2, min)
  return(dcr)
}

nndr_ft <- function(dist_mat){ ## row: original(n1) / col: synthetic(n2) n1*n2 matirix
  nndr <- apply(dist_mat, 2, function(x){
    xs <- sort(x)
    xs[1] / xs[2]
  })
  
  return(nndr)
}

DUPI_ft <- function(dist_os, dist_oo){
  ## original - original
  dist_o_o <- t(vapply(seq_len(nrow(dist_oo)), \(i) dist_oo[i, -i], numeric(nrow(dist_oo)-1)))
  
  minOS <- apply(dist_os, MARGIN=1, min)
  minOO <- apply(dist_o_o, MARGIN=1, min)
  
  result <- list()
  result$dupi <- mean(minOS<minOO)
  result$minOS <- minOS
  result$minOO <- minOO

  return(result)
}


pMSE_cart_ft <- function(df_all, index, ps_base = mean(index), 
                         cp = 0.001, maxdepth = 10, minsplit = 20){
  df_all <- as.data.frame(df_all)
  df_all$.index <- factor(index, levels = 0:1) ## orig -> 0 / synt -> 1
  
  fit <- rpart::rpart(.index ~ ., data = df_all, method = 'class',
                      control = rpart::rpart.control(
                        cp = cp,
                        maxdepth = maxdepth,
                        minsplit = minsplit
                      ))
  
  ps_vec <- predict(fit, type = "prob")[,2] ## probability of synthetic records
  
  pmse_vec <- (ps_vec - ps_base)^2
  pMSE <- mean(pmse_vec)
  
  return(list(
    pMSE = pMSE,
    pmse_vec = pmse_vec,
    ps_vec = ps_vec,
    ps_base = ps_base
  ))
}


s_pMSE_cart_ft <- function(orig, synt, B = 300, 
                           cp = 0.001, maxdepth = 10, minsplit = 20, seedNum = 99){
  set.seed(seedNum)
  
  orig <- as.data.frame(orig)
  synt <- as.data.frame(synt)
  synt <- synt[, names(orig), drop = FALSE]
  
  df_all <- rbind(orig, synt)
  index <- c(rep(0, nrow(orig)), rep(1, nrow(synt)))
  
  # observed pMSE
  obs_res <- pMSE_cart_ft( df_all = df_all, index = index,
                           cp = cp, maxdepth = maxdepth, minsplit = minsplit )
  pMSE_obs <- obs_res$pMSE
  
  # permutation null distribution
  null_dist <- numeric(B)
  
  for (b_rep in seq_len(B)) {
    index_perm <- sample(index)
    
    null_dist[b_rep] <- pMSE_cart_ft(
      df_all = df_all,
      index = index_perm,
      cp = cp,
      maxdepth = maxdepth,
      minsplit = minsplit
    )$pMSE
  }
  
  null_mean <- mean(null_dist)
  null_sd <- sd(null_dist)
  
  s_pMSE <- (pMSE_obs - null_mean) / null_sd
  
  list(
    s_pMSE = s_pMSE,
    pMSE = pMSE_obs,
    null_mean = null_mean,
    null_sd = null_sd,
    null_dist = null_dist,
    ps_vec = obs_res$ps_vec,
    pmse_vec = obs_res$pmse_vec,
    ps_base = obs_res$ps_base
  )
}


pMSE_logistic_ft <- function(df_all, index, ps_base = mean(index)){
  df_all <- as.data.frame(df_all)
  
  ## factor NA
  is_cat <- sapply(df_all, is.factor)
  if (any(is_cat)) {
    df_all[, is_cat] <- lapply(df_all[, is_cat, drop = FALSE], addNA)
  }
  
  df_all$.index <- index
  
  ## ps
  fit <- glm(.index ~ ., data = df_all, family = binomial())
  ps_vec <- predict(fit, type = "response") ## probability of synthetic records
  
  pmse_vec <- (ps_vec - ps_base)^2
  pMSE <- mean(pmse_vec)
  
  k <- length(coef(fit))
  
  return(list(
    pMSE = pMSE,
    pmse_vec = pmse_vec,
    ps_vec = ps_vec,
    ps_base = ps_base, 
    k = k
  ))
}


s_pMSE_logistic_ft <- function(orig, synt){
  orig <- as.data.frame(orig)
  synt <- as.data.frame(synt)
  synt <- synt[, names(orig), drop = FALSE]
  
  df_all <- rbind(orig, synt)
  index <- c(rep(0, nrow(orig)), rep(1, nrow(synt)))
  
  pmse_res <- pMSE_logistic_ft(df_all = df_all, index = index)
  
  k <- pmse_res$k
  N <- length(index)
  
  null_mean <- ((k - 1) * (1 - pmse_res$ps_base)^2 * pmse_res$ps_base) / N
  null_sd <- sqrt(2 * (k - 1)) * ((1 - pmse_res$ps_base)^2 * pmse_res$ps_base) / N
  
  s_pMSE <- (pmse_res$pMSE - null_mean) / null_sd
  
  list(
    s_pMSE = s_pMSE,
    pMSE = pmse_res$pMSE,
    null_mean = null_mean,
    null_sd = null_sd,
    ps_vec = pmse_res$ps_vec,
    pmse_vec = pmse_res$pmse_vec,
    ps_base = pmse_res$ps_base,
    k = k
  )
}

