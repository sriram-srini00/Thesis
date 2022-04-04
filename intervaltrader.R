#### FUNCTIONS ####
look_back <- function(interval, day, start_t, t, sym) {
  q1 <- paste0('tqmergeT[', day,'; `', sym, '; ', toString(start_t-t), '; ', toString(start_t), ' ]')
  tmp1 <- execute(h, q1)
  num_trades = sum(!is.na(tmp1$siz))
  num_orders = ceiling(num_trades/interval)
  return (num_orders)
}

look_back_for_num <- function(day, start_t, t, sym) {
  q1 <- paste0('tqmergeT[', day,'; `', sym, '; ', toString(start_t-t), '; ', toString(start_t), ' ]')
  tmp1 <- execute(h, q1)
  return (sum(!is.na(tmp1$siz)))
}

protocol_interval <- function(side, day, start_t, V, n, sym, interval) {
  q1 <- paste0('tqmergeT[', day,'; `', sym, '; ', toString(start_t), '; ', toString(16*hr), ' ]')
  tmp1 <- execute(h, q1)
  tmp1 <- tmp1[tmp1$time >= start_t,]
  index = 1
  
  while (is.na(tmp1[index, 'bsiz'])) {
    index <- index + 1
  }
  
  if (side == 'B') {
    microtrader_info <- buy_side_interval(tmp1, index, V, n, interval)
  }
  else {
    microtrader_info <- sell_side_interval(tmp1, index, V, n, interval)
  }
  
  return (microtrader_info)
}

buy_side_interval <- function(tmp1, index, V, n, interval) {
  # initialize bid size, price, cumulative sell order flow, and midpoint benchmark
  bidsiz <- tmp1[index, 'bsiz']
  bidp <- tmp1[index, 'bid']
  midpointp <- 0.5*(tmp1[index, 'bid'] + tmp1[index, 'ask'])
  
  step = T
  update = interval_elapsed = F
  svolume = lots_exec = trade_t = n_trades = 0
  fill_times <- fill_prices <- trades <- c()
  
  # stepping through the data
  while(step) {
    if (lots_exec >= V) step = F
    
    # check to see if we need to update info before posting next order
    if (update) {
      if (!interval_elapsed) {
        while ((n_trades < interval) | (is.na(tmp1[index, 'bsiz']))) {
          index = index + 1
          if (!is.na(tmp1[index,'siz'])) n_trades = n_trades + 1
        }
        n_trades = 0
      }
      bidsiz <- tmp1[index, 'bsiz']
      bidp <- tmp1[index, 'bid']
      update = interval_elapsed = F
    }
    
    index <- index + 1
    if (!is.na(tmp1[index, 'bsiz'])) {
      # T1 trades have occurred so cross the spread to execute V/n lots at ask price
      if (n_trades == interval) {
        lots_exec <- lots_exec + ceiling(V/n)
        fill_prices[length(fill_prices) + 1] <- tmp1[index, 'ask']
        trade_t = trade_t + 1
        fill_times[length(fill_times) + 1] <- trade_t
        trades[length(trades) + 1] <- 1*ceiling(V/n)
        n_trades = 0
        update = interval_elapsed = T
      }
      
      # if bid price decreases below order price, execute trade at order price so bid price in simulated is the same
      else if (tmp1[index, 'bid'] < bidp) {
        lots_exec <- lots_exec + ceiling(V/n)
        fill_prices[length(fill_prices) + 1] <- bidp
        trade_t = trade_t + 1
        fill_times[length(fill_times) + 1] <- trade_t
        trades[length(trades) + 1] <- 1*ceiling(V/n)
        update = T
      }
      
      else {
        # bid price moves up so move quote up to new bid and repeat
        if (tmp1[index, 'bid'] > bidp) {
          currenttime = tmp1[index,'time']
          # 100 ms delay (unless we flick back to original bid) before moving quote up to new bid
          while (tmp1[index,'time'] < currenttime + 100*ms & tmp1[index,'bid'] != bidp) {
            index = index + 1
            while (is.na(tmp1[index, 'bsiz'])) {
              if (tmp1[index, 'aggr'] == 'S') {
                trade_t = trade_t + 1
                n_trades = n_trades + 1
                trades[length(trades) + 1] <- -1*tmp1[index, 'siz']
              }
              else {
                trade_t = trade_t + 1
                n_trades = n_trades + 1
                trades[length(trades) + 1] <- 1*tmp1[index, 'siz']
              }
              index <- index + 1
            }
          }
          
          # resetting bid price, size, and cumulative sell volume if we place new bid (move the quote up)
          if (tmp1[index,'bid'] > bidp) {
            bidp <- tmp1[index, 'bid']
            bidsiz <- tmp1[index, 'bsiz']
            svolume = 0
          }
        }
      }
    }
    
    else if (!is.na(tmp1[index,'siz'])){
      # if enough sells occur for our trade to execute (at bid order price)
      n_trades = n_trades + 1
      if (tmp1[index, 'aggr'] == 'S') {
        trade_t = trade_t + 1
        trades[length(trades) + 1] <- -1*tmp1[index, 'siz']
      }
      else {
        trade_t = trade_t + 1
        trades[length(trades) + 1] <- 1*tmp1[index, 'siz']
      }
      
      if (tmp1[index, 'aggr'] == 'S') {
        svolume <- svolume + tmp1[index, 'siz']
        
        if (svolume >= bidsiz) {
          lots_exec <- lots_exec + ceiling(V/n)
          fill_prices[length(fill_prices) + 1] <- bidp
          trade_t = trade_t + 1
          fill_times[length(fill_times) + 1] <- trade_t
          trades[length(trades) + 1] <- 1*ceiling(V/n)
          update = T
        }
      }
    }
  }
  microtrader_info <- list(arr_p=midpointp, p=fill_prices, t=fill_times, tr=trades)
  return (microtrader_info)
}

sell_side_interval <- function(tmp1, index, V, n, interval) {
  # initialize ask size, price, cumulative buy order flow, and midpoint benchmark
  asksiz <- tmp1[index, 'asiz']
  askp <- tmp1[index, 'ask']
  midpointp <- 0.5*(tmp1[index, 'bid'] + tmp1[index, 'ask'])
  
  step = T
  update = interval_elapsed = F
  bvolume = lots_exec = trade_t = n_trades = 0
  fill_times <- fill_prices <- trades <- c()
  
  # stepping through the data
  while(step) {
    if (lots_exec >= V) step = F
    
    # check to see if we need to update info before posting next lot
    if (update) {
      if (!interval_elapsed) {
        while ((n_trades < interval) | (is.na(tmp1[index, 'asiz']))) {
          index = index + 1
          if (!is.na(tmp1[index,'siz'])) n_trades = n_trades + 1
        }
        n_trades = 0
      }
      asksiz <- tmp1[index, 'asiz']
      askp <- tmp1[index, 'ask']
      update = interval_elapsed = F
    }
    
    index <- index + 1
    
    if (!is.na(tmp1[index, 'asiz'])) {
      # T1 trades have elapsed so cross the spread to execute V/n lots at bid price
      if (n_trades == interval) {
        lots_exec <- lots_exec + ceiling(V/n)
        fill_prices[length(fill_prices) + 1] <- tmp1[index, 'bid']
        trade_t = trade_t + 1
        fill_times[length(fill_times) + 1] <- trade_t
        trades[length(trades) + 1] <- -1*ceiling(V/n)
        n_trades = 0
        update = interval_elapsed = T
      }
      
      # if ask price increases above order price, execute at ask price
      else if (tmp1[index, 'ask'] > askp) {
        lots_exec <- lots_exec + ceiling(V/n)
        fill_prices[length(fill_prices) + 1] <- askp
        trade_t = trade_t + 1
        fill_times[length(fill_times) + 1] <- trade_t
        trades[length(trades) + 1] <- -1*ceiling(V/n)
        update = T
      }
      
      else {
        # ask price moves down
        if (tmp1[index, 'ask'] < askp) {
          currenttime = tmp1[index,'time']
          # 100 ms delay (unless we flick back to original ask)
          while (tmp1[index,'time'] < currenttime + 100*ms & tmp1[index,'ask'] != askp){
            index = index + 1
            while (is.na(tmp1[index, 'asiz'])) {
              if (tmp1[index, 'aggr'] == 'S') {
                trade_t = trade_t + 1
                n_trades = n_trades + 1
                trades[length(trades) + 1] <- -1*tmp1[index, 'siz']
              }
              else {
                trade_t = trade_t + 1
                n_trades = n_trades + 1
                trades[length(trades) + 1] <- 1*tmp1[index, 'siz']
              }
              index <- index + 1
            }
          }
          
          # resetting ask price, size, and cumulative buy volume if we place new ask
          if(tmp1[index,'ask'] < askp){
            askp <- tmp1[index, 'ask']
            asksiz <- tmp1[index, 'asiz']
            bvolume = 0
          }
        }
      }
    }
    
    else if (!is.na(tmp1[index,'siz'])){
      # if enough buys occur for our trade to execute (at ask price)
      n_trades = n_trades + 1
      if (tmp1[index, 'aggr'] == 'S') {
        trade_t = trade_t + 1
        trades[length(trades) + 1] <- -1*tmp1[index, 'siz']
      }
      else {
        trade_t = trade_t + 1
        trades[length(trades) + 1] <- 1*tmp1[index, 'siz']
      }
      
      if (tmp1[index, 'aggr'] == 'B') {
        bvolume <- bvolume + tmp1[index, 'siz']
        
        if (bvolume >= asksiz) {
          lots_exec <- lots_exec + ceiling(V/n)
          fill_prices[length(fill_prices) + 1] <- askp
          trade_t = trade_t + 1
          fill_times[length(fill_times) + 1] <- trade_t
          trades[length(trades) + 1] <- -1*ceiling(V/n)
          update = T
        }
      }
    }
  }
  microtrader_info <- list(arr_p=midpointp, p=fill_prices, t=fill_times, tr=trades)
  return (microtrader_info)
}