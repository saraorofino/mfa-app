#' @title Calculating end of life measures under business as usual (BAU)
#' @param wastegen dataframe output of calc_wastegen for business as usual
#' @param incineration static dataframe for incineration rates
#' @param bau_rr static dataframe for recycling rates under business as usual (BAU)
#' @param r_yield hard coded assumption of a 0.7 recycling yield
#' @param target_sector the target sector to apply policy to
#' @description returns a dataframe with columns for weight of plastic landfilled, recycled and incinerated in million metric tons (Mt)

calc_eol_bau <- function(wastegen, incineration, bau_rr, r_yield = 0.7, target_sector){
  
  # total plastic collected all sectors 
  total_collected <- wastegen |> 
    filter(sector == "all_sec") |> 
    left_join(bau_rr, by = "year") |> #joining bau_rr column
    mutate(mt_plastic_collected_total = mt_plastic_wastegen * bau_rr) |> 
    select(year, mt_plastic_collected_total)
  
  # calculate recycling rate for target sector
  ## only applies to BAU 
  rr <- wastegen |> 
    filter(sector == target_sector) |> 
    left_join(total_collected, by = "year") |> 
    mutate(rr_multiplier = mt_plastic_collected_total / mt_plastic_wastegen)
  
  
  # apply rr for target sector, summarize amt collected by year
  annual_amt_collected <- wastegen |> 
    left_join(rr |> 
                dplyr::select(sector, year, rr_multiplier), by = c('sector', 'year')) |> 
    mutate(mt_plastic_collected = ifelse(!is.na(rr_multiplier), mt_plastic_wastegen * rr_multiplier, 0)) |> 
    group_by(year) |> 
    summarize(mt_plastic_collected = sum(mt_plastic_collected), .groups="drop")
  

  # annual amts by fate all sectors
  annual_eol <- wastegen |> 
    filter(sector == "all_sec") |> 
    # need to reference target sector reyclcing rate in other calculations
    left_join(rr |> 
                dplyr::select(year, target_sect_recyc_rate=rr_multiplier), 
              by = "year") |> 
    left_join(annual_amt_collected, by = "year") |> 
    left_join(incineration, by=c("year", "sector")) |> 
    mutate(mt_secondary_plastic_output = mt_plastic_collected * r_yield,
           mt_plastic_landfill = mt_plastic_wastegen-mt_secondary_plastic_output-mt_incin)
  
  return(annual_eol)
}
