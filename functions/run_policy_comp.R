#' @title Build Your Own Policy Analysis 
#' @description
#' Pulls in reactive settings from Shiny users to run the model under a customized build your own policy consisting of target rates for source reduction, recycling, and post consumer recycled content. 
#' @return Returns a list of data frames and summary outputs for consumption, greenhouse gases and disposal outcomes cumulatively from implementation year.



run_policy_comp <- function(params_comp, bau_results, incineration, consum_bau, lifetimes, emission_factors, bau_rr_sect){
  
  # sr
  policy_rate_sr    <- params_comp$policy_rate_sr
  implement_year_sr <- params_comp$implement_year_sr
  target_year_sr    <- params_comp$target_year_sr
  baseline_year_sr  <- params_comp$baseline_year_sr
  target_sector_sr  <- params_comp$target_sector_sr
  
  # rr
  policy_rate_rr    <- params_comp$policy_rate_rr
  implement_year_rr <- params_comp$implement_year_rr
  target_year_rr    <- params_comp$target_year_rr
  target_sector_rr  <- params_comp$target_sector_rr
  
  # rc
  policy_rate_rc    <- params_comp$policy_rate_rc
  implement_year_rc <- params_comp$implement_year_rc
  target_year_rc    <- params_comp$target_year_rc
  target_sector_rc  <- params_comp$target_sector_rc
  baseline_rc  <- params_comp$baseline_rc
  is_scrap_consump  <- params_comp$is_scrap_consump
  
  # using the lowest implement year for summaries?
  
  implement_year_min <- min(implement_year_sr, implement_year_rr, implement_year_rc, na.rm = TRUE)
  

# Consumption -------------------------------------------------------------
# with recycled content
  
  
  consum_comp <- calc_consum_sr(consum_bau, 
                                target_year_sr, 
                                policy_rate_sr, 
                                target_sector_sr, 
                                baseline_year_sr, 
                                implement_year_sr)
  
  rc_perc_comp <- calc_rc_perc(consum=consum_comp, 
                               target_rc=policy_rate_rc, 
                               target_year_rc=target_year_rc, 
                               implement_year=implement_year_rc, 
                               target_sector=target_sector_rc, 
                               baseline_rc=baseline_rc)

scrap_input_comp <- calc_scrap_input(rc_perc_comp,
                                      is_scrap_consump)

# avoid_virgin_comp <- calc_avoid_virgin(rc_perc_comp,
#                                        is_scrap_consump)



# Waste Generation  -------------------------------------------------------
 
wastegen_comp <- calc_wastegen(lifetimes, consum_comp)


# Waste Management --------------------------------------------------------



       

eol_comp <- calc_eol_policy(target_year = target_year_rr, 
                            implement_year = implement_year_rr,  
                            target_rr = policy_rate_rr,  
                            wastegen = wastegen_comp, 
                            bau_eol = bau_results$eol_bau, 
                            incineration = incineration, 
                            r_yield = 0.7, 
                            target_sect = target_sector_rr) #using rr for targets, as implement, target year used for recycling calculations



# GHG ---------------------------------------------------------------------


ghg_comp <- calc_ghg(consum_comp,
                     emission_factors,
                     eol_comp,
                     target_sector_rc,
                     implement_year = implement_year_min)

ghg_diff_comp <- calc_ghg_diff(
  ghg_prod = ghg_comp$ghg_prod,
  ghg_prod_bau = bau_results$ghg_bau$ghg_prod,
  ghg_eol = ghg_comp$ghg_eol,
  ghg_eol_bau = bau_results$ghg_bau$ghg_eol,
  ghg_avoid_prim_prod = ghg_comp$ghg_avoid_prim_prod,
  ghg_avoid_prim_prod_bau = bau_results$ghg_bau$ghg_avoid_prim_prod,
  implement_year = implement_year_min
)

# Summary Output List ---------------------------------------------------------------



# consumption 

consum_comp_summary <- consum_comp |>
  filter(sector == 'all_sec') |>
  filter(year > implement_year_sr) 

total_consumption_comp <-  sum(consum_comp_summary$mt_plastic_policy)

#avoided primary production 

total_avoid_prod_comp <- calc_avoid_prod(consum_bau = consum_bau, 
                                         consum_scenario = consum_comp, 
                                         eol_bau = bau_results$eol_bau, 
                                         eol_scenario = eol_comp, 
                                         target_pcr= policy_rate_rc, 
                                         target_rr= policy_rate_rr, 
                                         rc_perc = rc_perc_comp, 
                                         r_yield = 0.7, 
                                         displacement_rate = 0.8,
                                         is_scrap_consum = 0.5)

# ghg summary

total_avoid_ghg_comp <- ghg_comp$ghg_avoid_prim_prod |>
  filter(year > implement_year_min) |>
  pull(mt_co2e_avoidprod) |>
  sum(na.rm = TRUE) * -1

# Avoided GHG compared to BAU
total_ghg_diff_comp <- sum(ghg_diff_comp$total_diff)

#total ghg implement year on

total_ghg_comp <- ghg_comp$ghg_total

#returning list of outputs

return(
  list(
    # values for policy comparison table
    total_consumption_comp = total_consumption_comp,
    total_avoid_prod_comp  = total_avoid_prod_comp,
    total_avoid_ghg_comp = total_avoid_ghg_comp,
    total_ghg_diff_comp = total_ghg_diff_comp, #Avoided GHG compared to BAU
    total_ghg_comp = total_ghg_comp,
    # data frames for graphing later
    consum_comp_data = consum_comp,
    eol_comp_data = eol_comp,
    ghg_comp_data = ghg_comp,
    ghg_diff_comp = ghg_diff_comp
  )
)

  
}



