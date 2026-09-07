#' Run Livestock Water Partitioning (LiWaP) Model
#'
#' Runs the livestock water balance pipeline at cohort level, calculating milk
#' consumed by offspring, water intake, water outputs, and consumptive water use.
#'
#' @section Common identifiers:
#'
#' Several input tables use the following identifiers.
#'
#' \strong{\code{Animal_short}} - Character. Species code:
#' \itemize{
#'   \item \code{CTL}: cattle
#'   \item \code{BFL}: buffalo
#'   \item \code{SHP}: sheep
#'   \item \code{GTS}: goats
#'   \item \code{PGS}: pigs
#'   \item \code{CML}: camels
#'   \item \code{CHK}: chickens
#' }
#'
#' \strong{\code{cohort_short}} - Character. Sex- and age-specific cohort code:
#' \itemize{
#'   \item \code{FA}: adult females, from age at first parturition
#'   \item \code{FS}: sub-adult females, from weaning to age at first parturition
#'   \item \code{FJ}: juvenile females, from birth to weaning
#'   \item \code{MA}: adult males, from age at first breeding
#'   \item \code{MS}: sub-adult males, from weaning to age at first breeding
#'   \item \code{MJ}: juvenile males, from birth to weaning
#' }
#'
#' @param water_input_df A \code{data.table} containing cohort-level inputs.
#'   Required columns are:
#'
#'   \describe{
#'
#'     \item{\code{Animal_short}}{
#'       Character. Species code; see \strong{Common identifiers}.
#'     }
#'
#'     \item{\code{cohort_short}}{
#'       Character. Cohort code; see \strong{Common identifiers}.
#'     }
#'
#'     \item{\code{parturition_rate}}{
#'       Numeric. Average number of parturitions per adult female per year
#'       (parturitions/adult female/year).
#'     }
#'
#'     \item{\code{litter_size}}{
#'       Numeric. Average number of offspring born per parturition
#'       (offspring/parturition).
#'     }
#'
#'     \item{\code{live_weight_at_birth}}{
#'       Numeric. Average offspring live weight at birth (kg).
#'     }
#'
#'     \item{\code{live_weight_at_weaning}}{
#'       Numeric. Average offspring live weight at weaning (kg).
#'     }
#'
#'     \item{\code{ration_intake}}{
#'       Numeric. Average daily dry matter intake
#'       (kg DM/head/day).
#'     }
#'
#'     \item{\code{ration_dm_content}}{
#'       Numeric. Dry matter content of the ration, used to calculate water
#'       supplied through feed moisture. Expressed according to the convention
#'       expected by \code{calculate_water_intake()}.
#'     }
#'
#'     \item{\code{diet_na}}{
#'       Numeric. Dietary sodium concentration used in the cattle drinking-water
#'       intake equation.
#'     }
#'
#'     \item{\code{tav}}{
#'       Numeric. Average ambient temperature used in the species-specific
#'       drinking-water equations.
#'     }
#'
#'     \item{\code{lactating_fraction}}{
#'       Numeric. Fraction of adult females that are lactating during the
#'       assessment period.
#'     }
#'
#'     \item{\code{milk_yield}}{
#'       Numeric. Average daily milk yield per milk-producing animal
#'       (kg/head/day). This is calculated as milk produced for human consumption
#'       divided by the number of milk-producing animals and the duration of the
#'       assessment period.
#'     }
#'
#'     \item{\code{open_time_frac_pigs}}{
#'       Numeric. For adult female pigs, fraction of time spent in the open
#'       (non-pregnant and non-lactating) reproductive state.
#'     }
#'
#'     \item{\code{lact_time_frac_pigs}}{
#'       Numeric. For adult female pigs, fraction of time spent lactating.
#'     }
#'
#'     \item{\code{gest_time_frac_pigs}}{
#'       Numeric. For adult female pigs, fraction of time spent gestating.
#'     }
#'
#'     \item{\code{daily_weight_gain}}{
#'       Numeric. Average daily live-weight gain of the cohort
#'       (kg/head/day).
#'     }
#'
#'     \item{\code{eggs_consumption_year}}{
#'       Numeric. Number of eggs produced annually for human consumption
#'       (eggs/head/year).
#'     }
#'
#'     \item{\code{eggs_repro_year}}{
#'       Numeric. Number of eggs produced annually for reproduction
#'       (eggs/head/year).
#'     }
#'
#'     \item{\code{egg_weight}}{
#'       Numeric. Average egg weight (g/egg).
#'     }
#'
#'     \item{\code{is_egg_producing}}{
#'       Logical. Indicates whether the cohort produces eggs.
#'     }
#'   }
#'
#' @param water_output_df A \code{data.table} containing species-specific
#'   parameters used to calculate water outputs. Required columns are:
#'
#'   \describe{
#'
#'     \item{\code{Animal_short}}{
#'       Character. Species code matching the codes in \code{water_input_df}.
#'     }
#'
#'     \item{\code{Item}}{
#'       Character. Water-output component. Supported components include
#'       \code{"Feces"}, \code{"Urine"}, \code{"Evaporation"},
#'       \code{"Milk"}, and \code{"Eggs"}.
#'     }
#'
#'     \item{\code{V1_milk_producing}}{
#'       Numeric. Species- and component-specific coefficient applied to
#'       milk-producing adult females where relevant.
#'     }
#'
#'     \item{\code{V1_all}}{
#'       Numeric. Species- and component-specific coefficient applied to the
#'       general animal population where relevant.
#'     }
#'   }
#'
#' @details
#' The livestock water balance is calculated sequentially for each cohort:
#' \enumerate{
#'
#'   \item Milk consumed by offspring is estimated for relevant adult female
#'   cohorts from reproductive parameters and offspring live weights.
#'
#'   \item Water intake is calculated as the sum of drinking water and water
#'   supplied through feed moisture. Drinking-water requirements are estimated
#'   using species- and cohort-specific equations and, where applicable,
#'   temperature, diet, milk production, offspring milk demand, and reproductive
#'   state.
#'
#'   \item Water outputs are calculated for feces, milk, eggs, live-weight gain,
#'   urine, and evaporation using the corresponding species-specific equations
#'   and parameters in \code{water_output_df}.
#'
#'   \item Consumptive water use is calculated from total water intake after
#'   accounting for the fraction of fecal and urinary.
#' }
#'
#' Water-flow variables are expressed on a daily per-head basis. Because the
#' density of water is approximately 1 kg/L, water quantities expressed as
#' kg/head/day are numerically equivalent to L/head/day under the model
#' convention.
#'
#' If \code{water_input_df} is a \code{data.table}, it is modified by reference.
#' Use \code{data.table::copy(water_input_df)} before calling \code{run_liwap()}
#' if the original object must remain unchanged.
#'
#' Non-finite values generated for calculated water variables are replaced with
#' \code{NA_real_} before the result is returned.
#'
#' @return A \code{data.table} containing all columns in
#'   \code{water_input_df}, together with the following calculated variables:
#'
#'   \describe{
#'
#'     \item{\code{MilkOffspring}}{
#'       Numeric. Daily milk consumed by offspring (kg/head/day).
#'     }
#'
#'     \item{\code{DrinkingWater}}{
#'       Numeric. Daily drinking-water intake (L/head/day).
#'     }
#'
#'     \item{\code{FeedWater}}{
#'       Numeric. Daily water intake supplied through feed moisture
#'       (L/head/day).
#'     }
#'
#'     \item{\code{WaterIntake}}{
#'       Numeric. Total daily water intake, calculated as drinking water plus
#'       water supplied through feed (L/head/day).
#'     }
#'
#'     \item{\code{WaterFeces}}{
#'       Numeric. Daily fecal water output (L/head/day).
#'     }
#'
#'     \item{\code{WaterMilk}}{
#'       Numeric. Daily water output in milk, including modeled milk flows where
#'       applicable (L/head/day).
#'     }
#'
#'     \item{\code{WaterEggs}}{
#'       Numeric. Daily water retained in egg production (L/head/day).
#'     }
#'
#'     \item{\code{WaterGrowth}}{
#'       Numeric. Daily water retained in live-weight gain (L/head/day).
#'     }
#'
#'     \item{\code{WaterUrine}}{
#'       Numeric. Daily urinary water output (L/head/day).
#'     }
#'
#'     \item{\code{WaterEvaporation}}{
#'       Numeric. Daily evaporative water output (L/head/day).
#'     }
#'
#'     \item{\code{WaterOutput}}{
#'       Numeric. Total daily water output across the modeled output components
#'       (L/head/day).
#'     }
#'
#'     \item{\code{ConsumptiveWater}}{
#'       Numeric. Daily drinking consumptive water intake (L/head/day).
#'   }
#'   }
#'
#' @examples
#' \dontrun{
#' water_input <- data.table::fread("path/to/your_cohort_input.csv")
#' water_output <- data.table::fread("path/to/your_water_output_parameters.csv")
#'
#' water_results <- run_liwap(
#'   water_input_df = water_input,
#'   water_output_df = water_output
#' )
#' }
#'
#' @export

run_liwap <- function(water_input_df, water_output_df) {

  # Milk consumed by offspring
  water_input_df[, MilkOffspring := mapply(
    calculate_milk_offspring,
    animal_short = Animal_short,
    cohort = cohort_short,
    parturition_rate = parturition_rate,
    litter_size = litter_size,
    live_weight_at_birth = live_weight_at_birth,
    live_weight_at_weaning = live_weight_at_weaning
  )]
  
  # Water intake
  intake <- rbindlist(mapply(
    calculate_water_intake,
    animal_short = water_input_df$Animal_short,
    cohort = water_input_df$cohort_short,
    ration_intake = water_input_df$ration_intake,
    diet_dm = water_input_df$ration_dm_content,
    diet_na = water_input_df$diet_na,
    tav = water_input_df$tav,
    milking_fraction = water_input_df$lactating_fraction,
    milk_yield = water_input_df$milk_yield,
    milk_offspring = water_input_df$MilkOffspring,
    open_time_frac_pigs = water_input_df$open_time_frac_pigs,
    lact_time_frac_pigs = water_input_df$lact_time_frac_pigs,
    gest_time_frac_pigs = water_input_df$gest_time_frac_pigs,
    SIMPLIFY = FALSE
  ))
  
  water_input_df[, `:=`(
    DrinkingWater = intake$DrinkingWater,
    FeedWater = intake$FeedWater,
    WaterIntake = intake$WaterIntake
  )]
  
  # Water outputs
  outputs <- rbindlist(mapply(
    calculate_water_outputs,
    animal_short = water_input_df$Animal_short,
    cohort = water_input_df$cohort_short,
    total_water_intake = water_input_df$WaterIntake,
    lactating_fraction = water_input_df$lactating_fraction,
    milk_yield = water_input_df$milk_yield,
    milk_offspring = water_input_df$MilkOffspring,
    daily_weight_gain = water_input_df$daily_weight_gain,
    eggs_consumption_year = water_input_df$eggs_consumption_year,
    eggs_repro_year = water_input_df$eggs_repro_year,
    egg_weight = water_input_df$egg_weight,
    is_egg_producing = water_input_df$is_egg_producing,
    MoreArgs = list(water_output_df = water_output_df),
    SIMPLIFY = FALSE
  ))
  
  water_input_df[, `:=`(
    WaterFeces = outputs$WaterFeces,
    WaterMilk = outputs$WaterMilk,
    WaterEggs = outputs$WaterEggs,
    WaterGrowth = outputs$WaterGrowth,
    WaterUrine = outputs$WaterUrine,
    WaterEvaporation = outputs$WaterEvaporation,
    WaterOutput = outputs$WaterOutput
  )]
  
  # Consumptive water
  water_input_df[, ConsumptiveWater := mapply(
    calculate_consumptive_water,
    total_water_intake = WaterIntake,
    feces_water_output = WaterFeces,
    urine_water_output = WaterUrine  )]
  
  # Clean bad values
  water_cols <- c(
    "MilkOffspring",
    "DrinkingWater",
    "FeedWater",
    "WaterIntake",
    "WaterFeces",
    "WaterMilk",
    "WaterEggs",
    "WaterGrowth",
    "WaterUrine",
    "WaterEvaporation",
    "WaterOutput",
    "ConsumptiveWater"
  )
  
  for (x in water_cols) {
    water_input_df[!is.finite(get(x)), (x) := NA_real_]
  }
  
  water_input_df[]
}
