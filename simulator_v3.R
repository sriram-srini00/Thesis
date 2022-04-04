#### SETUP ####
source('timetrader.R')
source('intervaltrader.R')
source('revisions.R')
h <- open_connection('hfm.princeton.edu', 6008)

#### Ch 3: Plot ####
day = '2020.10.21'
q1 <- paste0('tqmergeT[', day,'; `', sym, '; ', toString(7*hr), '; ', toString(16*hr), ' ]')
tmp <- execute(h, q1)
tmp$trade_v <- rep(0, nrow(tmp))
indices = which(!is.na(tmp$siz))
tmp$trade_v[indices] <- seq.int(1,length(indices))
for (i in seq_along(indices[-1])) {
  tmp$trade_v[indices[i]+1] = tmp$trade_v[indices[i]]
}
deleted = which(tmp$trade_v == 0)
tmp = tmp[-deleted,]
yax = seq(0, 30000, 5000)
ylabels = paste(yax/1000, 'k', sep="")
xax = seq(7*hr, 16*hr, 1*hr)
xlabels = c('7:00', '8:00', '9:00', '10:00', '11:00', '12:00', '13:00', '14:00', '15:00', '16:00')
plot(tmp$time, tmp$trade_v, type='s', col='red', bty='u', main='ZNZ0: Cumulative number of trades for 5 Oct 2020', axes=FALSE, 
     xlab='', ylab='', ylim=c(0, max(yax)), lwd=2)
polygon(c(tmp$time, rev(tmp$time)), c(tmp$trade_v, rep(0, length(tmp$trade_v))), col = "pink", border=NA)
axis(side=2, at=yax, labels = ylabels, cex.axis = 0.75, las=1)
mtext('Cumulative number of trades', side=2, line=3, cex=0.9)
axis(side=1, at=xax, labels = xlabels, cex.axis = 0.75)
mtext('Time', side=1, line=2, cex=0.9)

#### Ch 4: Calibrate VAR model ####
dates <- seq(as.Date('2020-10-05'), as.Date('2020-10-30'), by="days")
dates <- dates[!weekdays(dates) %in% c('Saturday','Sunday')]
dates <- format(dates, "%Y.%m.%d")
sym = 'ZNZ0'
m = 1000
lag = 5
series <- build_ts_exp(dates, sym, m)
varmod <- build_var(series$rt, series$xt, lag)
varmod <- as.double(varmod)

#### Ch 4: Plot Simulator with no impact ####
sym = 'ZNZ0'
start_t = sample((7*hr):(14*hr), 1)
side = 'S'
day = '2020.10.15'
V = c(1000, 5000, 10000, 50000, 100000, 500000)
n = 50
t = c(2*hr, 3*hr, 4*hr)
slips = matrix(nrow=length(t),ncol=length(V))
for (i in seq_along(t)) {
  row = rep(0, length(V))
  for (j in seq_along(V)) {
    info <- protocol(side, day, start_t, V[j], n, t[i], sym)
    row[j] = mean(execute_lot(info$p, info$arr_p, side))
    print('done')
  }
  slips[i,] = row
}
if (side == 'B') title = paste0(sym, ': Average slippage (no impact, buy), $n$ = ', n) else title = 
  paste0(sym, ': Average slippage (no impact, sell), $n$ = ', n)
plot(V, slips[1,], pch=19, type='o', col='red', main=title, xlab='$V$ (volume)', ylab='Average slippage', las=1, 
     xlim=c(0,max(V)+100), ylim=c(min(slips)-0.02,0))
lines(V, slips[2,], pch=19, type='o', col='blue')
lines(V, slips[3,], pch=19, type='o', col='green')
segments(0, 0, max(V), 0, col = 'darkgray')
segments(0, min(slips)-0.02, 0, 0, col = 'darkgray')
legend('topright', legend = c('$2$ hr', '$3$ hr', '$4$ hr'), col = c('red', 'blue', 'green'), lty = 1, pch = 19, cex=0.6, inset = 0.01)

#### Ch 5: Kernel ####
K = c(10, 500, 1000)
tsmax = max(K)*2 # these are the t-s values
params <- setup_kernel(dates, sym, K)
a <- calc_kernel(tsmax, params$B, K)

par(mar = c(5.1, 6, 4.1, 3))
plot((0:tsmax), a, type='l', col='dark green', lwd=2, xlab = '$t-s$', ylab = '', main = 'Kernel', las = 1)
title(ylab = "$a_{t-s}$", mgp = c(4.8, 1, 0))  
lines(c(0,tsmax), c(0,0), col='black')

par(mar = c(5.1, 6, 4.1, 3))
plot((1:100), a[1:100], type='l', col='dark green', lwd=2, xlab = '$t-s$', ylab = '', main = 'Kernel', las = 1)
title(ylab = "$a_{t-s}$", mgp = c(4.8, 1, 0))  
lines(c(0,tsmax), c(0,0), col='black')
dev.off()

interval_opt = which.min(a)
#### Ch 6: plot slip vs V ####
start_t = sample((7*hr):(14*hr),1)
day = '2020.10.12'

side = 'B'
V = c(500, 1000, 5000, 10000, 50000)
n = 10
t = 30*min
slips <- rep(0, length(V))
for (i in seq_along(V)) {
  slippages <- simulate_and_cumrev(side, day, start_t, V[i], n, t, sym, m, lag, varmod, 0, 'time')
  slips[i] <- mean(slippages)
  print('done')
}
plot(V, slips, main = paste0(sym, ': Average slippages for varying V, n = ', n, ', $T$ = ', t/min, ' mins'), xlab = '$V$ (volume)', 
     ylab = 'Predicted slippage (average)', ylim = c(0, max(slips) + 0.1), type = 'o', pch = 19, las = 1, cex.main = 0.9, col = 'red')
segments(0, 0, max(V), 0, col = 'darkgray')
segments(0, 0, 0, max(slips) + 0.05, col = 'darkgray')

side = 'S'
slips_S <- rep(0, length(V))
for (i in seq_along(V)) {
  slippages <- simulate_and_cumrev(side, day, start_t, V[i], n, t, sym, m, lag, varmod, 0, 'time')
  slips_S[i] <- mean(slippages)
}
lines(V, slips_S, type = 'o', pch = 19, col = 'blue')
legend('bottomright', legend = c('Buy', 'Sell'), col = c('red', 'blue'), lty = 1, pch = 19, inset = 0.05)

#### Ch 6: plot slip vs n ####
side = 'B'
V = 50000
n <- c(10, 100, 500, 1000, 5000)
t = 30*min
slips <- rep(0, length(n))
for (i in seq_along(n)) {
  slippages <- simulate_and_cumrev(side, day, start_t, V, n[i], t, sym, m, lag, varmod, 0, 'time')
  slips[i] <- mean(slippages)
}
plot(n, slips, main = paste0(sym, ': Average slippages for varying $n$, $V$ = ', V/1000, 'k, $T$ = ', t/min, ' mins'), xlab = '$n$ (number of orders)', 
     ylab = 'Predicted slippage (average)', ylim = c(0, max(slips) + 0.05), type = 'o', pch = 19, las = 1, cex.main = 0.9, col = 'red')
segments(0, 0, max(n), 0, col = 'darkgray')
segments(0, 0, 0, max(slips) + 0.05, col = 'darkgray')

side = 'S'
slips_S <- rep(0, length(n))
for (i in seq_along(n)) {
  slippages <- simulate_and_cumrev(side, day, start_t, V, n[i], t, sym, m, lag, varmod, 0, 'time')
  slips_S[i] <- mean(slippages)
}
lines(n, slips_S, type = 'o', pch = 19, col = 'blue')
legend('topright', legend = c('Buy', 'Sell'), col = c('red', 'blue'), lty = 1, pch = 19, inset = 0.05)

#### Ch 6: plot slip vs T ####
start_t = sample((7*hr):(14*hr),1)
day = '2020.10.12'
side = 'B'
V = 50000
n = 10
t <- c(15*min, 30*min, 60*min, 2*hr)
tvals <- c(15, 30, 60, 120)
slips <- rep(0, length(t))
for (i in seq_along(t)) {
  slippages <- simulate_and_cumrev(side, day, start_t, V, n, t[i], sym, m, lag, varmod, 0, 'time')
  slips[i] <- mean(slippages)
}

side = 'S'
slips_S <- rep(0, length(t))
for (i in seq_along(t)) {
  slippages <- simulate_and_cumrev(side, day, start_t, V, n, t[i], sym, m, lag, varmod, 0, 'time')
  slips_S[i] <- mean(slippages)
}
plot(tvals, slips_S, main = paste0(sym, ': Average slippages for varying $T$, $V$ = ', V/1000, 'k, $n$ = ', n), xlab = '$T$ (duration in mins)', 
     ylab = 'Predicted slippage (average)', xlim = c(0, max(tvals)), type = 'o', pch = 19, cex.main = 0.9, las = 1, col = 'blue', 
     ylim = c(0, max(slips_S, slips) + 0.05))
segments(0, 0, max(tvals), 0, col = 'darkgray')
segments(0, 0, 0, max(slips_S, slips) + 0.05, col = 'darkgray')

lines(tvals, slips, type = 'o', pch = 19, col = 'red')
legend('topright', legend = c('Buy', 'Sell'), col = c('red', 'blue'), lty = 1, pch = 19)

#### Ch 6: Time interval vs trade interval ####
start_t = sample((7*hr):(14*hr), 1)
start_t/hr
side = 'S'
day = '2020.10.29'

V = 5000000
# n_time_int = c(4, 24, 40, 120, 160, 240)
# t = 2*hr
n_time_int = c(3, 6, 18, 36, 60, 180)
t = 3*hr
tvals = (60*t/hr)/n_time_int
slips <- rep(0, length(tvals))
for (i in seq_along(n_time_int)) {
  slippages <- simulate_and_cumrev(side, day, start_t, V, n_time_int[i], t, sym, m, lag, varmod, 0, 'time')
  slips[i] <- mean(slippages)
}

intervals = c(78, 200, 500, 1000, 2000, 2750)
n_tr_int = rep(0, length(intervals))
slips_opt <- rep(0, length(intervals))
for (i in seq_along(intervals)) {
  n <- look_back(intervals[i], day, start_t, t, sym)
  n_tr_int[i] <- n
  slippages <- simulate_and_cumrev(side, day, start_t, V, n, t, sym, m, lag, varmod, intervals[i], 'interval')
  slips_opt[i] <- mean(slippages)
}

# plot both
trades = look_back_for_num('2020.10.20', 11*hr, 1*hr, sym)
multiplier = trades/60
taxis = tvals*multiplier
scaled_axis = seq(0, max(taxis, intervals), 250)
if (side == 'B') title = paste0(sym, ': Time \\& Trade Interval Strategies (Buy), $V$ = ', V/1000000, 'MM, $T$ = ', t/hr, ' hrs') else title = 
  paste0(sym, ': Time \\& Trade Interval Strategies (Sell), $V$ = ', V/1000000, 'MM, $T$ = ', t/hr, ' hrs')
par(mar = c(9, 7, 5, 6))
plot(intervals, slips_opt, pch=19, type='o', col='red', main=title, axes=FALSE, xlab='', ylab='', 
     xlim=c(0, max(taxis,intervals)+100), ylim=c(0, max(slips,slips_opt)+0.05), cex.main=0.98)
arrows(78, 15, 78, slips_opt[1]-0.2, length = 0.1)
lines(taxis, slips, pch=19, type='o', col='blue', xlab='', ylab='')

axis(side=2, seq(0, max(slips, slips_opt)+3, 3), las=1)
mtext('Predicted slippage (average)', side=2, line=3)
axis(side=1, at=scaled_axis, cex.axis = 0.78)
# axis(side=1, at=scaled_axis, labels=c(0, '', 500, '', 1000, '', 1500, '', 2000, '', 2500, '', 3000), cex.axis=0.83)
mtext('Trade interval (trades)', side=1, line=2, col='red', cex=0.9)
axis(side=1, line=4, at=scaled_axis, labels=floor(scaled_axis/multiplier))
mtext('Time interval (mins)', side=1, line=6, col='blue', cex=0.9)

legend('bottomright', legend = c('Times', 'Trades'), col = c('blue', 'red'), lty = 1, pch = 19, cex=0.85, inset=0.07)
segments(0, 0, max(taxis,intervals), 0, col = 'darkgray')
segments(0, 0, 0, max(slips,slips_opt)+0.5, col = 'darkgray')
