# ------ Water intake ----------------------------------------------------------

#' Calculate daily milk consumed by offspring
#'
#' Calculates the average daily milk consumed by offspring associated with an
#' adult female cohort, based on annual reproductive performance, offspring
#' live weight gain from birth to weaning, and species-specific coefficients.
#'
#' @param animal_short Character. Species code. Supported values are
#' \code{CTL}, \code{BFL}, \code{SHP}, \code{GTS}, \code{PGS}, \code{CML},
#' and \code{CHK}.
#' @param cohort Character. Sex- and age-specific cohort code. Supported values
#' are \code{FA}, \code{FS}, \code{FJ}, \code{MA}, \code{MS}, and
#' \code{MJ}. Non-zero values are calculated only for \code{FA}.
#' @param parturition_rate Numeric. Average number of parturitions per adult
#' female per year (parturitions/adult female/year).
#' @param litter_size Numeric. Average number of offspring born per parturition
#' (offspring/parturition).
#' @param live_weight_at_birth Numeric. Average offspring live weight at birth
#' (kg).
#' @param live_weight_at_weaning Numeric. Average offspring live weight at
#' weaning (kg).
#'
#' @details
#' Milk consumed by offspring is estimated from offspring live weight gain
#' between birth and weaning and species-specific milk requirements per unit of
#' gain:
#' \itemize{
#' \item \code{CTL}, \code{BFL}, and \code{CML}: 5 kg milk per kg live-weight
#' gain.
#' \item \code{SHP} and \code{GTS}: 5 kg milk per kg live-weight gain,
#' multiplied by litter size.
#' \item \code{PGS}: 4 kg milk per kg live-weight gain, multiplied by litter
#' size.
#' }
#'
#' The annual amount associated with the adult female cohort is converted to an
#' average daily value by dividing by \code{365}. All cohorts other than
#' \code{FA} return \code{0}.
#'
#' @return Numeric. Average daily milk consumed by offspring associated with
#' the adult female cohort (kg milk/adult female/day).
#'
#' @export
#'
#' @seealso \code{\link{run_liwap}}
calculate_milk_offspring <- function(
  animal_short,
  cohort,
  parturition_rate,
  litter_size,
  live_weight_at_birth,
  live_weight_at_weaning
) {
  if (cohort != "FA") {
    return(0)
  }

  live_weight_gain_to_weaning <- live_weight_at_weaning - live_weight_at_birth

  if (animal_short %in% c("CTL", "BFL", "CML")) {
    return(parturition_rate * 5 * live_weight_gain_to_weaning / 365)
  }

  if (animal_short %in% c("SHP", "GTS")) {
    return(litter_size * parturition_rate * 5 * live_weight_gain_to_weaning / 365)
  }

  if (animal_short == "PGS") {
    return(litter_size * parturition_rate * 4 * live_weight_gain_to_weaning / 365)
  }

  0
}

#' Calculate daily water intake
#'
#' Calculates daily feed water, drinking water, and total water intake for a
#' single livestock cohort using species- and cohort-specific equations.
#'
#' @param animal_short Character. Species code.
#' @param cohort Character. Sex- and age-specific cohort code.
#' @param ration_intake Numeric. Average daily dry matter intake
#' (kg DM/head/day).
#' @param diet_dm Numeric. Dry matter content of the ration. Values in the
#' interval \code{0}-\code{1} are interpreted as fractions; values greater
#' than \code{1} are interpreted as percentages and divided by \code{100}.
#' @param diet_na Numeric. Dietary sodium concentration used in the cattle
#' drinking-water equation.
#' @param tav Numeric. Average ambient temperature used in the species-specific
#' drinking-water equations.
#' @param milking_fraction Numeric. Fraction of adult females lactating during
#' the assessment period.
#' @param milk_yield Numeric. Average daily milk yield for human consumption per
#' milk-producing animal (kg/head/day). Defaults to \code{0}.
#' @param milk_offspring Numeric. Average daily milk consumed by offspring
#' associated with the adult female cohort (kg milk/adult female/day).
#' Defaults to \code{0}.
#' @param open_time_frac_pigs Numeric. For adult female pigs, fraction of the
#' reproductive cycle spent open (non-pregnant and non-lactating).
#' @param lact_time_frac_pigs Numeric. For adult female pigs, fraction of the
#' reproductive cycle spent lactating.
#' @param gest_time_frac_pigs Numeric. For adult female pigs, fraction of the
#' reproductive cycle spent gestating.
#'
#' @details
#' Feed water is calculated from dry matter intake and ration dry matter
#' content as the water contained in the as-fed ration.
#'
#' Juvenile cohorts \code{FJ} and \code{MJ} are assigned zero drinking-water
#' intake for all species except chickens. Their total water intake therefore
#' corresponds to water supplied through feed.
#'
#' For the remaining cohorts, species-specific drinking-water equations are
#' applied:
#' \itemize{
#' \item \code{CTL}: separate equations for lactating and non-lactating
#' animals, combined according to \code{milking_fraction} for adult females.
#' \item \code{BFL}: equation based on dry matter intake and ambient
#' temperature.
#' \item \code{SHP} and \code{GTS}: equation based on dry matter intake, with
#' an additional temperature response above 26 degrees C.
#' \item \code{CML}: equation based on dry matter intake, with an additional
#' temperature response above 27 degrees C.
#' \item \code{PGS}: equation based on dry matter intake, with an additional
#' temperature response above 25 degrees C. For adult females, drinking water
#' is calculated as the weighted average of open, gestating, and lactating
#' reproductive states.
#' \item \code{CHK}: equation based on dry matter intake, with an additional
#' temperature response above 21 degrees C.
#' }
#'
#' Total water intake is calculated as:
#' \preformatted{
#' WaterIntake = DrinkingWater + FeedWater
#' }
#'
#' Water quantities are expressed as L/head/day under the model convention.
#'
#' @return A named list containing:
#' \describe{
#' \item{\code{DrinkingWater}}{
#' Numeric. Daily drinking-water intake (L/head/day).
#' }
#' \item{\code{FeedWater}}{
#' Numeric. Daily water supplied through feed moisture (L/head/day).
#' }
#' \item{\code{WaterIntake}}{
#' Numeric. Total daily water intake, equal to drinking water plus feed
#' water (L/head/day).
#' }
#' }
#'
#' @export
#'
#' @seealso \code{\link{run_liwap}}, \code{\link{calculate_milk_offspring}}
calculate_water_intake <- function(
  animal_short,
  cohort,
  ration_intake,
  diet_dm,
  diet_na,
  tav,
  milking_fraction,
  milk_yield = 0,
  milk_offspring = 0,
  open_time_frac_pigs,
  lact_time_frac_pigs,
  gest_time_frac_pigs
  
) {
  diet_dm_fraction <- ifelse(test = diet_dm > 1, yes = diet_dm / 100, no = diet_dm)
  water_in_feed <- ifelse(
    test = !is.na(diet_dm_fraction) & diet_dm_fraction > 0,
    yes = (ration_intake / diet_dm_fraction) - ration_intake,
    no = NA_real_
  )

  if (cohort %in% c("FJ", "MJ") && animal_short != "CHK") {
    return(list(
      DrinkingWater = 0,
      FeedWater = water_in_feed,
      WaterIntake = water_in_feed
    ))
  }

  drinking_water <- NA_real_

  if (animal_short %in% c("CTL")) {
    sodium_content <- ration_intake * diet_na

    if (cohort == "FA") {
      intake_milking <- 11.4 + (1.58 * ration_intake) + (0.90 * (milk_yield + milk_offspring)) +
        (0.05 * sodium_content) + (1.168 * tav)
      intake_dry <- ration_intake * (3.413 + 0.01592 * exp(0.17596 * tav))

      drinking_water <- (intake_milking * milking_fraction) + (intake_dry * (1 - milking_fraction))
    } else if (!cohort %in% c("FJ", "MJ")) {
      drinking_water <- ration_intake * (3.413 + 0.01592 * exp(0.17596 * tav))
    }
  } else if (animal_short %in% c("BFL")) {
    drinking_water <- -6.226 + (2.97 * ration_intake) + ifelse(tav > 0, 0.428 * tav, 0)
    
  } else if (animal_short %in% c("SHP", "GTS")) {
    drinking_water <- ration_intake * (2.945 + (0.25 * max(0, tav - 26)))
  } else if (animal_short == "CML") {
    drinking_water <- ration_intake * (1.91 + (0.1 * max(0, tav - 27)))
  } else if (animal_short == "PGS") {
    if (cohort == "FA") {
    drinking_water_open <- (2.25 * ration_intake) * (1 + 0.12 * max(0, tav - 25))
    drinking_water_lact <- ((2.52 * ration_intake) + 4.22) * (1 + 0.12 * max(0, tav - 25))
    drinking_water_gest <- (6.4 * ration_intake) * (1 + 0.12 * max(0, tav - 25))
    
    drinking_water <-  open_time_frac_pigs * drinking_water_open + gest_time_frac_pigs * drinking_water_gest + lact_time_frac_pigs * drinking_water_lact
    
    } else if (!cohort %in% c("FJ", "MJ")) {
    drinking_water <- (2.25 * ration_intake) * (1 + 0.12 * max(0, tav - 25))
    } 
  } else if (animal_short == "CHK") {
    drinking_water <- (2.213 * ration_intake) * (1 + 0.07 * max(0, tav - 21))
  }
  total_water_intake <- drinking_water + water_in_feed

  list(
    DrinkingWater = drinking_water,
    FeedWater = water_in_feed,
    WaterIntake = total_water_intake
  )
}

# ------ Water outputs ---------------------------------------------------------

#' Calculate water output components
#'
#' Calculates daily fecal, milk, egg, growth, urinary, and evaporative water
#' flows for a single livestock cohort.
#'
#' @param animal_short Character. Species code.
#' @param cohort Character. Sex- and age-specific cohort code.
#' @param total_water_intake Numeric. Total daily water intake (L/head/day).
#' @param lactating_fraction Numeric. Fraction of adult females lactating during
#' the assessment period.
#' @param milk_yield Numeric. Average daily milk yield for human consumption per
#' milk-producing animal (kg/head/day).
#' @param water_output_df A \code{data.table} containing species-specific
#' coefficients for water output calculations. Required columns include
#' \code{Animal_short}, \code{Item}, \code{V1_milk_producing}, and
#' \code{V1_all}.
#' @param milk_offspring Numeric. Average daily milk consumed by offspring
#' associated with the adult female cohort (kg milk/adult female/day).
#' @param daily_weight_gain Numeric. Average daily live-weight gain of the
#' cohort (kg/head/day).
#' @param eggs_consumption_year Numeric. Number of eggs produced annually for
#' human consumption (eggs/head/year).
#' @param eggs_repro_year Numeric. Number of eggs produced annually for
#' reproduction (eggs/head/year).
#' @param egg_weight Numeric. Average egg weight (g/egg).
#' @param is_egg_producing Logical. Indicates whether the cohort produces eggs.
#' @param growth_water_content Numeric. Water retained per unit of live-weight
#' gain (kg water/kg live-weight gain). Defaults to \code{0.49}.
#'
#' @details
#' The parameter table provides species-specific coefficients for fecal,
#' urinary, evaporative, milk, and egg water flows.
#'
#' For adult female cohorts (\code{FA}), except chickens, fecal, urinary, and
#' evaporative coefficients are calculated as weighted averages of
#' \code{V1_all} and \code{V1_milk_producing} according to
#' \code{lactating_fraction}. For all other cohorts, \code{V1_all} is used.
#'
#' Water flows are calculated sequentially:
#' \enumerate{
#' \item Fecal water is calculated as a proportion of total water intake.
#' \item Water retained in eggs is calculated from annual egg production,
#' average egg weight, and the species-specific egg water coefficient.
#' \item Water secreted in milk is calculated from milk produced for human
#' consumption and milk consumed by offspring.
#' \item Water retained in growth is calculated from daily live-weight gain
#' and \code{growth_water_content}.
#' \item Urinary and evaporative water are calculated from the water remaining
#' after fecal, egg, milk, and growth water have been allocated.
#' }
#'
#' Egg, milk, and growth water are capped at the amount of water remaining at
#' the corresponding step. Negative or non-finite intermediate values for these
#' components are replaced with \code{0}.
#'
#' Water quantities are expressed as L/head/day under the model convention.
#'
#' @return A named list containing:
#' \describe{
#' \item{\code{WaterFeces}}{
#' Numeric. Daily fecal water output (L/head/day).
#' }
#' \item{\code{WaterMilk}}{
#' Numeric. Daily water secreted in milk, including milk produced for human
#' consumption and milk consumed by offspring where applicable
#' (L/head/day).
#' }
#' \item{\code{WaterEggs}}{
#' Numeric. Daily water retained in eggs (L/head/day).
#' }
#' \item{\code{WaterGrowth}}{
#' Numeric. Daily water retained in live-weight gain (L/head/day).
#' }
#' \item{\code{WaterUrine}}{
#' Numeric. Daily urinary water output (L/head/day).
#' }
#' \item{\code{WaterEvaporation}}{
#' Numeric. Daily evaporative water output (L/head/day).
#' }
#' \item{\code{WaterOutput}}{
#' Numeric. Sum of fecal, milk, egg, urinary, and evaporative water flows
#' (L/head/day).
#' }
#' }
#'
#' @export
#'
#' @seealso \code{\link{run_liwap}}
calculate_water_outputs <- function(
    animal_short,
    cohort,
    total_water_intake,
    lactating_fraction,
    milk_yield,
    water_output_df,
    milk_offspring,
    daily_weight_gain,
    eggs_consumption_year,
    eggs_repro_year,
    egg_weight,
    is_egg_producing,
    growth_water_content = 0.49
) {
  water_output_dt <- data.table::as.data.table(water_output_df)
  
  water_output_dt <- data.table::as.data.table(water_output_df)
  
  feces_share_all_animals <- water_output_dt[
    Animal_short == animal_short & Item == "Feces",
    V1_all
  ][1]
  
  feces_share_lactating <- water_output_dt[
    Animal_short == animal_short & Item == "Feces",
    V1_milk_producing
  ][1]
  
  urine_share_all_animals <- water_output_dt[
    Animal_short == animal_short & Item == "Urine",
    V1_all
  ][1]
  
  urine_share_lactating <- water_output_dt[
    Animal_short == animal_short & Item == "Urine",
    V1_milk_producing
  ][1]
  
  evaporation_share_all_animals <- water_output_dt[
    Animal_short == animal_short & Item == "Evaporation",
    V1_all
  ][1]
  
  evaporation_share_lactating <- water_output_dt[
    Animal_short == animal_short & Item == "Evaporation",
    V1_milk_producing
  ][1]
  
  milk_water_per_kg_milk_lactating <- water_output_dt[
    Animal_short == animal_short & Item == "Milk",
    V1_milk_producing
  ][1]
  
  egg_water_per_kg_eggs <- water_output_dt[
    Animal_short == animal_short & Item == "Eggs",
    V1_all
  ][1]
  
  use_lactating_coefficients <- cohort == "FA" &&
    !animal_short %in% c("CHK")
  
  
  if (use_lactating_coefficients) {
    feces_share <- feces_share_all_animals * (1 - lactating_fraction) +
      feces_share_lactating * lactating_fraction
    
    urine_share <- urine_share_all_animals * (1 - lactating_fraction) +
      urine_share_lactating * lactating_fraction
    
    evaporation_share <- evaporation_share_all_animals * (1 - lactating_fraction) +
      evaporation_share_lactating * lactating_fraction
    
    effective_milk_water_per_kg_milk <- milk_water_per_kg_milk_lactating
  } else {
    feces_share <- feces_share_all_animals
    urine_share <- urine_share_all_animals
    evaporation_share <- evaporation_share_all_animals
    effective_milk_water_per_kg_milk <- 0
  }
  
  
  feces_water_output <- total_water_intake * feces_share
  remaining_water <- total_water_intake - feces_water_output
  
  egg_water_output <- ifelse(
    is_egg_producing == TRUE,
    (eggs_consumption_year + eggs_repro_year) / 365 *
      egg_weight / 1000 *
      egg_water_per_kg_eggs,
    0
  )
  
  if (!is.finite(egg_water_output) || egg_water_output < 0) {
    egg_water_output <- 0
  }
  
  egg_water_output <- min(egg_water_output, remaining_water)
  remaining_water <- remaining_water - egg_water_output
  
  milk_water_output <- (
    milk_yield * lactating_fraction + milk_offspring
  ) * effective_milk_water_per_kg_milk
  
  if (!is.finite(milk_water_output) || milk_water_output < 0) {
    milk_water_output <- 0
  }
  
  milk_water_output <- min(milk_water_output, remaining_water)
  remaining_water <- remaining_water - milk_water_output
  
  growth_water_output <- daily_weight_gain * growth_water_content
  
  if (!is.finite(growth_water_output) || growth_water_output < 0) {
    growth_water_output <- 0
  }
  
  growth_water_output <- min(growth_water_output, remaining_water)
  remaining_water <- remaining_water - growth_water_output
  
  if (!is.finite(remaining_water) || remaining_water < 0) {
    remaining_water <- 0
  }
  
  urine_evaporation_share <- urine_share + evaporation_share
  
  urine_water_output <- pmax(remaining_water * urine_share, 0)
  evaporated_water_output <- pmax(remaining_water * evaporation_share, 0)


  total_water_output <- feces_water_output +
    milk_water_output +
    egg_water_output +
    urine_water_output +
    evaporated_water_output

  if (!is.finite(total_water_output)) total_water_output <- 0
  
  list(
    WaterFeces = feces_water_output,
    WaterMilk = milk_water_output,
    WaterEggs = egg_water_output,
    WaterGrowth = growth_water_output,
    WaterUrine = urine_water_output,
    WaterEvaporation = evaporated_water_output,
    WaterOutput = total_water_output
  )
}

# ------ Consumptive water -----------------------------------------------------

#' Calculate consumptive water use
#'
#' Calculates daily consumptive water use by subtracting the recoverable share
#' of fecal and urinary water deposited on grazing land from total water intake.
#'
#' @param total_water_intake Numeric. Total daily water intake (L/head/day).
#' @param feces_water_output Numeric. Daily fecal water output (L/head/day).
#' @param urine_water_output Numeric. Daily urinary water output (L/head/day).
#'
#' @details
#' Consumptive water use is calculated as:
#' \preformatted{
#' ConsumptiveWater =
#' WaterIntake - (WaterFeces + WaterUrine)
#' }
#'
#' @return Numeric. Daily consumptive water use (L/head/day).
#'
#' @export
#'
#' @seealso \code{\link{run_liwap}}
calculate_consumptive_water <- function(
  total_water_intake,
  feces_water_output,
  urine_water_output
) {
  
 total_water_intake - (feces_water_output + urine_water_output) 
  
}
