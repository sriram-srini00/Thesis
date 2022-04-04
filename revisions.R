#### SETUP ####
source('timetrader.R')
source('intervaltrader.R')

#### FUNCTIONS ####
build_ts_exp <- function(dates, sym, m) {
  week_df <- as.data.frame(matrix(nrow=0,ncol=4))
  colnames(week_df) <- c('time', 'rt', 'xt', 'xbar')
  for (day in dates) {
    q1 <- paste0('tqmergeT[', day,'; `', sym, '; ', toString(7*hr), '; ', toString(16*hr), ' ]')
    tmp1 <- execute(h, q1)
    
    indices <- which(!is.na(tmp1$siz))
    revisions <- c(1)*0
    for (t in indices[-1]) {
      # eq1: rt = (qt_b+qt_a)/2 - (qt-1_b+qt-1_a)/2
      revisions[length(revisions) + 1] <- (tmp1[t+1, 'bid'] + tmp1[t+1, 'ask'])/2 - (tmp1[t-1, 'bid'] + tmp1[t-1, 'ask'])/2
    }
    tmp1 <- tmp1[!is.na(tmp1$siz),]
    tmp1$rt <- revisions
    tmp1$sign <- rep(NA, nrow(tmp1))
    tmp1[tmp1$aggr == 'S', ][, 'sign'] <- -1
    tmp1[tmp1$aggr == 'B', ][, 'sign'] <- 1
    tmp1$xt <- tmp1$sign*tmp1$siz
    tmp1 <- na.omit(subset(tmp1, select = c(time, rt, xt)))
    week_df <- rbind(week_df, tmp1)
  }
  week_df$xbar <- calc_xbar(week_df$xt, m)

  rt_ts <- ts(week_df$rt)
  xt_ts <- ts(week_df$xbar)
  return (list(rt=rt_ts, xt=xt_ts))
}

build_var <- function(rt_ts, xt_ts, lag) {
  # rt = xt-1 + xt-2 + ...
  VAR_rt <- dynlm(rt_ts ~ 0+L(xt_ts, 1:lag))
  coef_names <- c()
  for (i in 1:lag) {
    coef_names[length(coef_names) + 1] <- paste0('x_t-',i)
  }
  names(VAR_rt$coefficients) <- coef_names
  return (coef(VAR_rt))
}

calc_xbar <- function(trades, m) {
  xbars <- rep(0, length(trades))
  A = 0
  B = 0
  for (i in seq_along(trades)) {
    A = exp(-1/m)*A + trades[i]
    B = exp(-1/m)*B + 1
    xbars[i] <- A/B
  }
  return (xbars)
}

calc_revisions <- function(xbars, varmod, m, lag) {
  revisions <- c()
  for (k in seq(lag+1, length(xbars))) {
    revisions[length(revisions)+1] <- 0
    for (l in 1:length(varmod)) {
      revisions[length(revisions)] <- revisions[length(revisions)] + varmod[l]*xbars[k-l]
    }
  }
  return (revisions)
}

setup_kernel <- function(dates, sym, K) {
  xbars_k = matrix(nrow=length(K),ncol=0)
  r_t = NULL
  formula = paste0('r_t ~ 0')
  for (i in seq_along(K)) {
    data = build_ts_exp(dates, sym, K[i])
    if (i == 1) {
      xbars_k = matrix(xbars_k, nrow = length(K), ncol=length(data$xt))
      r_t = data$rt
    }
    xbars_k[i,] = data$xt
    formula = paste0(formula, '+ xbars_k[', i, ',]')
  }
  model <- dynlm(as.formula(formula))
  b <- as.double(coef(model))
  info <- list(B=b, n_trades=dim(xbars_k)[2])
  return (info)
}

calc_kernel <- function(tsmax, b, K) {
  denom = 1/(1-exp(-1/K)) # this is sum(w[t-s;k], s=0,...,t)
  a = (b[1]*exp(-(0:tsmax)/K[1])/denom[1]) + (b[2]*exp(-(0:tsmax)/K[2])/denom[2]) + (b[3]*exp(-(0:tsmax)/K[3])/denom[3])
  return (a)
}

simulate_and_cumrev <- function(side, day, start_t, V, n, t, sym, m, lag, varmod, interval, trade_type) {
  if (trade_type == 'interval') info = protocol_interval(side, day, start_t, V, n, sym, interval) else info = protocol(side, day, start_t, V, n, t, sym)
  
  agent_times <- info$t
  trades_all <- info$tr
  xbars_all <- calc_xbar(trades_all, m)
  revisions_all <- calc_revisions(xbars_all, varmod, m, lag)
  
  trades_market <- info$tr
  trades_market[agent_times] = 0
  xbars_market <- calc_xbar(trades_market, m)
  revisions_market <- calc_revisions(xbars_market, varmod, m, lag)
  
  rev_diff <- revisions_all - revisions_market
  rev_diff <- c(rep(0, lag), rev_diff)
  rev_cum_agent <- cumsum(rev_diff)[agent_times]
  
  adjusted_prices <- info$p + rev_cum_agent
  slippages <- execute_lot(adjusted_prices, info$arr_p, side)
  return (slippages)
}