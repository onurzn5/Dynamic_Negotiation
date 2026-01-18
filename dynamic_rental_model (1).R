# ==============================================================================
# DYNAMIC RENTAL NEGOTIATION MODEL
# ==============================================================================
#
# Based on: EXPANDED_STATİC_5.R
# 
# WHAT BECOMES ENDOGENOUS IN DYNAMIC MODEL:
#   - v (vacancy rate): evolves based on moves and new supply
#   - delta_M: landlord's offer responds to market conditions
#   - Rent_burden: base rent evolves over time
#
# STATE VARIABLES (evolve over time):
#   - v_t        : vacancy rate at time t
#   - supply_t   : new rental supply entering market
#   - Rent_base_t: average rent level in the market
#
# FLOW EACH PERIOD:
#   1. Update market conditions (v, delta_M) based on t-1
#   2. Run negotiation
#   3. Stay/Move decisions
#   4. Compute outcomes → feed into t+1
#
# ==============================================================================

library(data.table)

# ==============================================================================
# SINGLE PERIOD FUNCTION (Modified from your static model)
# ==============================================================================

run_single_period <- function(dt, params, v_current, delta_M_current, t) {
  
  # Uses existing agent data (dt) but with updated market conditions
  
  N <- nrow(dt)
  delta_L <- params$delta_L
  delta_M <- delta_M_current  # Now time-varying
  n_rounds <- params$n_rounds
  
  # --------------------------------------------------------------------------
  # NEGOTIATION
  # --------------------------------------------------------------------------
  
  aggr_vec <- dt$aggr
  resist_vec <- dt$resist
  tol_agree_vec <- dt$tolerance
  
  # Endogenous Theta - NOW ALSO DEPENDS ON VACANCY
  # High vacancy → landlords less aggressive (lower theta_L)
  # vacancy_effect: when v is high, landlords soften their stance
  vacancy_effect <- (1 - v_current)^params$vacancy_sensitivity
  
  theta_L_vec <- aggr_vec * 0.50 * vacancy_effect
  theta_T_vec <- resist_vec * 0.50
  
  # Initialize positions
  offer_L <- rep(delta_M, N)
  accept_T <- rep(delta_L, N)
  
  # Tracking
  is_active <- rep(TRUE, N)
  final_price <- rep(NA_real_, N)
  rounds_done <- rep(n_rounds, N)
  agreed <- rep(FALSE, N)
  
  # Negotiation Loop
  for (r in 1:n_rounds) {
    if (!any(is_active)) break
    
    gap <- abs(offer_L - accept_T)
    just_agreed <- (gap < tol_agree_vec) & is_active
    
    if (any(just_agreed)) {
      final_price[just_agreed] <- (offer_L[just_agreed] + accept_T[just_agreed]) / 2
      rounds_done[just_agreed] <- r
      agreed[just_agreed] <- TRUE
      is_active[just_agreed] <- FALSE
    }
    
    # Position updates
    target_L <- aggr_vec * delta_M + (1 - aggr_vec) * accept_T
    offer_L <- theta_L_vec * offer_L + (1 - theta_L_vec) * target_L
    
    target_T <- resist_vec * delta_L + (1 - resist_vec) * offer_L
    accept_T <- theta_T_vec * accept_T + (1 - theta_T_vec) * target_T
  }
  
  # Store negotiation results
  dt[, `:=`(
    delta_C = final_price,
    rounds = rounds_done,
    agreed = agreed,
    final_gap = abs(offer_L - accept_T),
    delta_M_t = delta_M,  # Store the delta_M used this period
    v_t = v_current,       # Store vacancy rate this period
    t = t                  # Time period
  )]
  
  # --------------------------------------------------------------------------
  # STAY/MOVE DECISION
  # --------------------------------------------------------------------------
  
  dt[, `:=`(
    C_stay = NA_real_, 
    C_move = NA_real_, 
    P_move = NA_real_, 
    moved = NA
  )]
  
  conflict_idx <- which(dt$agreed == FALSE)
  
  if (length(conflict_idx) > 0) {
    
    kappa <- params$kappa
    phi <- params$phi
    alpha <- params$alpha
    search_months <- params$search_months
    
    sub_dt <- dt[conflict_idx]
    
    # ----- CORRECTED COST STRUCTURE -----
    # phi = probability court sides with tenant
    
    # Rent if tenant wins (stays at delta_L)
    rent_stay <- sub_dt$Rent_burden * (1 + delta_L)
    
    # Rent if tenant loses or moves (pays delta_M)
    rent_move <- sub_dt$Rent_burden * (1 + delta_M)
    
    # Search cost: decreases with vacancy (easier to find when many vacant)
    search_cost <- (search_months / 12) * rent_move * (1 - v_current)^alpha
    
    # Legal cost
    legal_cost <- kappa
    
    # Expected cost of STAY AND FIGHT:
    # Win (prob phi): rent_stay + legal
    # Lose (prob 1-phi): rent_move + search + legal
    C_stay <- phi * (rent_stay + legal_cost) + (1 - phi) * (rent_move + search_cost + legal_cost)
    
    # Cost of MOVE VOLUNTARILY (no court)
    C_move <- rent_move + search_cost
    
    # Decision with stochastic error
    cost_diff <- C_stay - C_move
    epsilon <- rnorm(length(conflict_idx), mean = 0, sd = params$error_sd)
    moved_decision <- (cost_diff + epsilon) > 0
    
    dt[conflict_idx, `:=`(
      C_stay = C_stay,
      C_move = C_move,
      moved = moved_decision
    )]
  }
  
  # Final status
  dt[, final_status := fcase(
    agreed == TRUE, "agreed",
    agreed == FALSE & moved == FALSE, "stayed_conflict",
    agreed == FALSE & moved == TRUE, "moved"
  )]
  
  dt[, final_rent := fcase(
    final_status == "agreed", Rent_burden * (1 + delta_C),
    final_status == "stayed_conflict", Rent_burden * (1 + delta_L),
    final_status == "moved", Rent_burden * (1 + delta_M)
  )]
  
  dt[, vacancy := (final_status == "moved")]
  
  return(dt)
}

# ==============================================================================
# MARKET DYNAMICS FUNCTIONS
# ==============================================================================

# Vacancy rate evolution
# v_t = v_base + (moved / total_units) - (filled / total_units)
# Simplified: filled units come from new supply matching with movers
compute_vacancy <- function(v_prev, n_moved, N, new_supply, fill_rate) {
  
  # Vacancies created by movers
  new_vacancies <- n_moved / N
  
  # Vacancies filled (some movers fill existing vacancies, new supply adds units)
  # fill_rate: proportion of vacancies filled each period
  filled <- fill_rate * v_prev
  
  # Net change
  v_new <- v_prev + new_vacancies - filled
  
  # Bound between 0 and 1
  v_new <- pmax(0.01, pmin(0.50, v_new))
  
  return(v_new)
}

# Delta_M evolution (landlord's market offer)
# When vacancy is high → landlords reduce demands
# When vacancy is low → landlords increase demands
compute_delta_M <- function(delta_M_prev, v_current, params) {
  
  v_target <- params$v_base  # "Normal" vacancy rate
  
  # If v > v_target: too many vacancies → reduce delta_M
  # If v < v_target: tight market → increase delta_M
  adjustment <- params$delta_M_sensitivity * (v_target - v_current)
  
  delta_M_new <- delta_M_prev + adjustment
  
  # Bound delta_M (can't go below delta_L or above some maximum)
  delta_M_new <- pmax(params$delta_L + 0.01, pmin(params$delta_M_max, delta_M_new))
  
  return(delta_M_new)
}

# New supply entering market
# Supply responds to rent gap (profitability signal)
compute_new_supply <- function(delta_M_current, delta_L, params) {
  
  rent_gap <- delta_M_current - delta_L
  
  # Higher rent gap → more profitable → more supply
  supply <- params$supply_base * (1 + params$supply_elasticity * rent_gap)
  
  return(max(0, supply))
}

# Rent burden evolution (base rent grows over time)
update_rent_burden <- function(dt, inflation_rate) {
  dt[, Rent_burden := Rent_burden * (1 + inflation_rate)]
  return(dt)
}

# ==============================================================================
# MAIN DYNAMIC SIMULATION
# ==============================================================================

run_dynamic_model <- function(params) {
  
  # --------------------------------------------------------------------------
  # INITIALIZATION (Period 0)
  # --------------------------------------------------------------------------
  
  set.seed(params$seed)
  N <- params$N
  T <- params$T  # Number of periods
  
  # Generate initial agent characteristics (FIXED over time)
  Y_T <- rlnorm(N, meanlog = params$Y_T_meanlog, sdlog = params$Y_T_sdlog)
  Y_L_noise <- exp(rnorm(N, mean = 0, sd = params$Y_L_noise_sd))
  Y_L <- Y_T * params$Y_L_premium * Y_L_noise
  
  Rent_burden_vec <- rnorm(N, params$rent_burden_base, sd = params$rent_sd)
  Rent_burden_vec <- pmax(0.10, pmin(0.50, Rent_burden_vec))
  Rent_burden <- Rent_burden_vec * Y_T
  
  aggr <- runif(N, 0, 1)
  resist <- runif(N, 0, 1)
  
  tolerance <- 0.05 - (aggr * 0.02) + (resist * 0.01)
  tolerance <- pmax(0.001, tolerance)
  
  # Base agent data (will be copied each period)
  agents_base <- data.table(
    id = 1:N,
    Y_T = Y_T,
    Y_L = Y_L,
    Ratio = Rent_burden_vec,
    Rent_burden = Rent_burden,
    aggr = aggr,
    resist = resist,
    tolerance = tolerance
  )
  
  # Initialize state variables
  v_current <- params$v_base
  delta_M_current <- params$delta_M_init
  
  # Storage for results
  all_periods <- vector("list", T)
  market_history <- data.table(
    t = 1:T,
    v = NA_real_,
    delta_M = NA_real_,
    agreement_rate = NA_real_,
    move_rate = NA_real_,
    avg_rent = NA_real_,
    new_supply = NA_real_
  )
  
  # --------------------------------------------------------------------------
  # DYNAMIC LOOP
  # --------------------------------------------------------------------------
  
  for (t in 1:T) {
    
    if (t %% 10 == 0) cat("Period:", t, "| v =", round(v_current, 3), 
                          "| delta_M =", round(delta_M_current, 3), "\n")
    
    # --- A. Copy agent data for this period ---
    dt <- copy(agents_base)
    
    # Update rent burden with inflation (cumulative)
    if (t > 1) {
      dt[, Rent_burden := Rent_burden * (1 + params$inflation_rate)^(t-1)]
    }
    
    # --- B. Run single period ---
    set.seed(params$seed + t)  # Different randomness each period
    dt <- run_single_period(dt, params, v_current, delta_M_current, t)
    
    # --- C. Compute market outcomes ---
    n_agreed <- sum(dt$agreed)
    n_moved <- sum(dt$moved, na.rm = TRUE)
    n_conflict <- sum(!dt$agreed)
    
    agreement_rate <- n_agreed / N
    move_rate <- ifelse(n_conflict > 0, n_moved / n_conflict, 0)
    avg_rent <- mean(dt$final_rent)
    
    # --- D. Store results ---
    all_periods[[t]] <- dt
    
    market_history[t, `:=`(
      v = v_current,
      delta_M = delta_M_current,
      agreement_rate = agreement_rate,
      move_rate = move_rate,
      avg_rent = avg_rent,
      new_supply = compute_new_supply(delta_M_current, params$delta_L, params)
    )]
    
    # --- E. Update state variables for next period ---
    new_supply <- compute_new_supply(delta_M_current, params$delta_L, params)
    
    v_next <- compute_vacancy(v_current, n_moved, N, new_supply, params$fill_rate)
    delta_M_next <- compute_delta_M(delta_M_current, v_current, params)
    
    v_current <- v_next
    delta_M_current <- delta_M_next
  }
  
  # --------------------------------------------------------------------------
  # COMBINE RESULTS
  # --------------------------------------------------------------------------
  
  # Combine all period data
  all_agents <- rbindlist(all_periods)
  
  return(list(
    agents = all_agents,
    market = market_history
  ))
}

# ==============================================================================
# PARAMETERS
# ==============================================================================

params <- list(
  # --- Population ---
  N = 300,
  seed = 1234,
  T = 50,  # Number of time periods
  
  # --- Income Distribution ---
  Y_T_meanlog = 10.7,
  Y_T_sdlog = 0.4,
  Y_L_premium = 1.5,
  Y_L_noise_sd = 0.3,
  
  # --- Rent ---
  rent_burden_base = 0.25,
  rent_sd = 0.03,
  inflation_rate = 0.02,  # 2% annual rent inflation
  
  # --- Negotiation ---
  delta_L = 0.25,
  delta_M_init = 0.35,    # Initial landlord demand
  delta_M_max = 0.60,     # Maximum landlord demand
  n_rounds = 10,
  
  # --- Stay/Move Decision ---
  phi = 0.50,             # Court sides with tenant probability

  kappa = 8000,           # Legal cost
  alpha = 1,              # Search cost elasticity
  search_months = 3,      # Search cost in months of rent
  error_sd = 5000,        # Decision noise (in TL scale)
  
  # --- Market Dynamics ---
  v_base = 0.10,          # "Normal" vacancy rate (target)
  fill_rate = 0.30,       # Proportion of vacancies filled each period
  vacancy_sensitivity = 0.5, # How much vacancy affects landlord theta
  delta_M_sensitivity = 0.10, # How fast delta_M adjusts to vacancy
  supply_base = 10,       # Base new supply (units per period)
  supply_elasticity = 0.5 # Supply response to rent gap
)

# ==============================================================================
# RUN MODEL
# ==============================================================================

results <- run_dynamic_model(params)

# Extract results
agents <- results$agents
market <- results$market

# ==============================================================================
# INSPECT RESULTS
# ==============================================================================

# Market history
print("--- Market Evolution ---")
print(market)

# Final period status
print("--- Final Period Status ---")
print(table(agents[t == params$T]$final_status))

# Status evolution over time
print("--- Agreement Rate Over Time ---")
print(agents[, .(agreement_rate = mean(agreed)), by = t])
