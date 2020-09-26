dl_summon <- function(regular_rate = 0.04,
                      feature_rate = 0.005,
                      pity_rate = 0.005,
                      ten_pull_strat_threshold = 20,
                      num_trials = 10000,
                      seed_num = 123) {
  
  roll_counter <- integer(num_trials) # Initialize a vector of zeroes to store the number of rolls we take for each trial
  set.seed(seed_num) # Setting a seed to ensure reproducible results
  
  for (i in 1:num_trials) {
    # Initialise the base rates for each attempt
    get_unit <- 0 # This flag changes to 1 once we get the feature unit (duplicates ignored)
    regular_rng <- regular_rate # This rate initially starts at the base rate, updates when the pity increases, and resets when a 5* is obtained
    feature_rng <- feature_rate # This rate initially starts at the base rate, updates when the pity increases, and resets when a 5* is obtained
    pity_counter <- 0 # This counts the how much pity we have until a 5* is obtained
    
    while(get_unit < 1) {
      # Use single pulls up to the ten pull strat threshold, or if it's the 101st pity pull which guarantees a 5*
      if(pity_counter < ten_pull_strat_threshold | pity_counter >= 100) {
        roll_counter[i] <- roll_counter[i] + 1 # We roll once, so increase the roll counter by 1
        rng <- runif(1, min = 0, max = 1) # This is the RNG of the roll
        
        # If the feature unit is obtained, we are done
        if(rng <= feature_rng) {
          get_unit <- 1
          
          # If a 5-star is obtained, reset the base rate, feature rate, and pity counter
        } else if(rng <= regular_rng | pity_counter >= 100) {
          regular_rng <- regular_rate
          feature_rng <- feature_rate
          pity_counter <- 0
          
          # Otherwise add to the pity counter and re-calculate the rates    
        } else {
          pity_counter <- pity_counter + 1 # Increase the pity counter because we didn't get a 5*
          regular_rng <- (regular_rate + (pity_counter %/% 10) * pity_rate) # Changes each time we accumulate 10 rolls without a 5*
          feature_rng <- (feature_rate + (pity_counter %/% 10) * pity_rate * feature_rate/regular_rate) # Changes each time we accumulate 10 rolls without a 5*
        }
      } else {
        # Switch to tenpulls once we've accumulated enough pity
        roll_counter[i] <- roll_counter[i] + 10 # We roll 10 times, so increase the roll counter by 10
        rng <- runif(10, min = 0, max = 1) # This is the RNG of the ten rolls
        
        # If the feature unit is obtained in any of the ten rolls, we are done
        if(max(rng <= feature_rng)) {
          get_unit <- 1
          
          # If a 5-star is obtained (ignoring duplicates and multiple 5*), reset the base rate, feature rate, and pity counter
        } else if(max(rng <= regular_rng)) {
          regular_rng <- regular_rate
          feature_rng <- feature_rate
          pity_counter <- 0
          
          # Otherwise add to the pity counter and re-calculate the rates    
        } else {
          pity_counter <- pity_counter + 10 # Increase the pity counter because we didn't get a 5*
          regular_rng <- (regular_rate + (pity_counter %/% 10) * pity_rate) # Changes each time we accumulate 10 rolls without a 5*
          feature_rng <- (feature_rate + (pity_counter %/% 10) * pity_rate * feature_rate/regular_rate) # Changes each time we accumulate 10 rolls without a 5*
        }
      }
    }
  }
  
  results <- list("Average rolls" = mean(roll_counter), "25th quantile" = quantile(roll_counter)[2], "75th quantile" = quantile(roll_counter)[4])
  return(results)
}

dl_summon(regular_rate = 0.04,feature_rate = 0.005,pity_rate = 0.005,ten_pull_strat_threshold = 0,num_trials = 50000,seed_num = 123) # 174.4531 | 60 | 240
dl_summon(regular_rate = 0.06,feature_rate = 0.005,pity_rate = 0.005,ten_pull_strat_threshold = 0,num_trials = 50000,seed_num = 123) # 189.2233 | 60 | 260
dl_summon(regular_rate = 0.04,feature_rate = 0.008,pity_rate = 0.005,ten_pull_strat_threshold = 0,num_trials = 50000,seed_num = 123) # 111.248  | 40 | 150
dl_summon(regular_rate = 0.06,feature_rate = 0.008,pity_rate = 0.005,ten_pull_strat_threshold = 0,num_trials = 50000,seed_num = 123) # 120.8914 | 40 | 170

dl_summon(regular_rate = 0.04,feature_rate = 0.005,pity_rate = 0.005,ten_pull_strat_threshold = 10,num_trials = 50000,seed_num = 123) # 171.5328 | 54 | 236
dl_summon(regular_rate = 0.06,feature_rate = 0.005,pity_rate = 0.005,ten_pull_strat_threshold = 10,num_trials = 50000,seed_num = 123) # 185.7616 | 58 | 256
dl_summon(regular_rate = 0.04,feature_rate = 0.008,pity_rate = 0.005,ten_pull_strat_threshold = 10,num_trials = 50000,seed_num = 123) # 109.1479 | 39 | 150
dl_summon(regular_rate = 0.06,feature_rate = 0.008,pity_rate = 0.005,ten_pull_strat_threshold = 10,num_trials = 50000,seed_num = 123) # 118.062  | 39 | 163

dl_summon(regular_rate = 0.04,feature_rate = 0.005,pity_rate = 0.005,ten_pull_strat_threshold = 20,num_trials = 50000,seed_num = 123) # 170.2621 | 53 | 234
dl_summon(regular_rate = 0.06,feature_rate = 0.005,pity_rate = 0.005,ten_pull_strat_threshold = 20,num_trials = 50000,seed_num = 123) # 184.4073 | 56 | 254
dl_summon(regular_rate = 0.04,feature_rate = 0.008,pity_rate = 0.005,ten_pull_strat_threshold = 20,num_trials = 50000,seed_num = 123) # 107.6609 | 37 | 148
dl_summon(regular_rate = 0.06,feature_rate = 0.008,pity_rate = 0.005,ten_pull_strat_threshold = 20,num_trials = 50000,seed_num = 123) # 116.8381 | 37 | 161

dl_summon(regular_rate = 0.04,feature_rate = 0.005,pity_rate = 0.005,ten_pull_strat_threshold = 30,num_trials = 50000,seed_num = 123) # 169.7988 | 52 | 234
dl_summon(regular_rate = 0.06,feature_rate = 0.005,pity_rate = 0.005,ten_pull_strat_threshold = 30,num_trials = 50000,seed_num = 123) # 184.163  | 55 | 255
dl_summon(regular_rate = 0.04,feature_rate = 0.008,pity_rate = 0.005,ten_pull_strat_threshold = 30,num_trials = 50000,seed_num = 123) # 107.0139 | 36 | 148
dl_summon(regular_rate = 0.06,feature_rate = 0.008,pity_rate = 0.005,ten_pull_strat_threshold = 30,num_trials = 50000,seed_num = 123) # 116.3338 | 36 | 161
