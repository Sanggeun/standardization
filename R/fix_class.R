#' fix class for standardization.
#'
#' fix_age_class_tochs function will change classes of population data in standardization of community health survey result.
#'
#' @param data_set data set.
#' @param class_var class variable.
#' @param value_var value variable.
#' @param josa_year year investigated.
#' @param fix_var fixed variable
#' @return a vector including values in changed classes
#' @export
#' @importFrom as_tibble dplyr

fix_age_class_tochs <- function(data_set, class_var='age', value_var=c('pop_all','pop_male','pop_female'),
                                josa_year, fix_var = 'death') {

  age_range_1999 <- c("0 - 4세", "5 - 9세","10 - 14세", "15 - 19세", "20 - 24세", "25 - 29세", "30 - 34세", "35 - 39세",
                      "40 - 44세", "45 - 49세", "50 - 54세", "55 - 59세", "60 - 64세", "65 - 69세", "70 - 74세",
                      "75 - 79세", "80 - 84세", "85 - 89세", "90 - 94세","95+" )

  age_range_2011 <- c(age_range_1999, "95 - 99세", "100+")

  chs_class_range <-  c("1.19-29","2.19-29","1.30-39","2.30-39","1.40-49","2.40-49",
                        "1.50-59", "2.50-59", "1.60-69", "2.60-69", "1.70이상", "2.70이상")

  if(josa_year %in% 2011:2019) {

    pop <- data_set[data_set[[class_var]] %in% age_range_2011,
                    c(class_var,value_var)]

    male.20_29 <- pop[pop[[class_var]]=='20 - 24세',value_var[2]] + pop[pop[[class_var]]=='25 - 29세',value_var[2]]
    female.20_29 <- pop[pop[[class_var]]=='20 - 24세',value_var[3]] + pop[pop[[class_var]]=='25 - 29세',value_var[3]]
    male.30_39 <- pop[pop[[class_var]]=='30 - 34세',value_var[2]] + pop[pop[[class_var]]=='35 - 39세',value_var[2]]
    female.30_39 <- pop[pop[[class_var]]=='30 - 34세',value_var[3]] + pop[pop[[class_var]]=='35 - 39세',value_var[3]]
    male.40_49 <- pop[pop[[class_var]]=='40 - 44세',value_var[2]] + pop[pop[[class_var]]=='45 - 49세',value_var[2]]
    female.40_49 <- pop[pop[[class_var]]=='40 - 44세',value_var[3]] + pop[pop[[class_var]]=='45 - 49세',value_var[3]]
    male.50_59 <- pop[pop[[class_var]]=='50 - 54세',value_var[2]] + pop[pop[[class_var]]=='55 - 59세',value_var[2]]
    female.50_59 <- pop[pop[[class_var]]=='50 - 54세',value_var[3]] + pop[pop[[class_var]]=='55 - 59세',value_var[3]]
    male.60_69 <- pop[pop[[class_var]]=='60 - 64세',value_var[2]] + pop[pop[[class_var]]=='65 - 69세',value_var[2]]
    female.60_69 <- pop[pop[[class_var]]=='60 - 64세',value_var[3]] + pop[pop[[class_var]]=='65 - 69세',value_var[3]]

    male.70 <- pop[pop[[class_var]]=='70 - 74세',value_var[2]] + pop[pop[[class_var]]=='75 - 79세',value_var[2]] +
      pop[pop[[class_var]]=='80 - 84세',value_var[2]] + pop[pop[[class_var]]=='85 - 89세',value_var[2]] +
      pop[pop[[class_var]]=='90 - 94세',value_var[2]] + pop[pop[[class_var]]=='95 - 99세',value_var[2]] +
      pop[pop[[class_var]]=='100+',value_var[2]]

    female.70 <- pop[pop[[class_var]]=='70 - 74세',value_var[3]] + pop[pop[[class_var]]=='75 - 79세',value_var[3]] +
      pop[pop[[class_var]]=='80 - 84세',value_var[3]] + pop[pop[[class_var]]=='85 - 89세',value_var[3]] +
      pop[pop[[class_var]]=='90 - 94세',value_var[3]] + pop[pop[[class_var]]=='95 - 99세',value_var[3]] +
      pop[pop[[class_var]]=='100+',value_var[3]]

    result_pop <- c(male.20_29, female.20_29, male.30_39, female.30_39, male.40_49, female.40_49,
                    male.50_59, female.50_59, male.60_69, female.60_69, male.70, female.70)

  } else if (josa_year %in% 1999:2010) {
    pop <- data_set[data_set[[class_var]] %in% age_range_1999,
                    c(class_var,value_var)]

    male.20_29 <- pop[pop[[class_var]]=='20 - 24세',value_var[2]] + pop[pop[[class_var]]=='25 - 29세',value_var[2]]
    female.20_29 <- pop[pop[[class_var]]=='20 - 24세',value_var[3]] + pop[pop[[class_var]]=='25 - 29세',value_var[3]]
    male.30_39 <- pop[pop[[class_var]]=='30 - 34세',value_var[2]] + pop[pop[[class_var]]=='35 - 39세',value_var[2]]
    female.30_39 <- pop[pop[[class_var]]=='30 - 34세',value_var[3]] + pop[pop[[class_var]]=='35 - 39세',value_var[3]]
    male.40_49 <- pop[pop[[class_var]]=='40 - 44세',value_var[2]] + pop[pop[[class_var]]=='45 - 49세',value_var[2]]
    female.40_49 <- pop[pop[[class_var]]=='40 - 44세',value_var[3]] + pop[pop[[class_var]]=='45 - 49세',value_var[3]]
    male.50_59 <- pop[pop[[class_var]]=='50 - 54세',value_var[2]] + pop[pop[[class_var]]=='55 - 59세',value_var[2]]
    female.50_59 <- pop[pop[[class_var]]=='50 - 54세',value_var[3]] + pop[pop[[class_var]]=='55 - 59세',value_var[3]]
    male.60_69 <- pop[pop[[class_var]]=='60 - 64세',value_var[2]] + pop[pop[[class_var]]=='65 - 69세',value_var[2]]
    female.60_69 <- pop[pop[[class_var]]=='60 - 64세',value_var[3]] + pop[pop[[class_var]]=='65 - 69세',value_var[3]]

    male.70 <- pop[pop[[class_var]]=='70 - 74세',value_var[2]] + pop[pop[[class_var]]=='75 - 79세',value_var[2]] +
      pop[pop[[class_var]]=='80 - 84세',value_var[2]] + pop[pop[[class_var]]=='85 - 89세',value_var[2]] +
      pop[pop[[class_var]]=='90 - 94세',value_var[2]] + pop[pop[[class_var]]=='95+',3]

    female.70 <- pop[pop[[class_var]]=='70 - 74세',value_var[3]] + pop[pop[[class_var]]=='75 - 79세',value_var[3]] +
      pop[pop[[class_var]]=='80 - 84세',value_var[3]] + pop[pop[[class_var]]=='85 - 89세',value_var[3]] +
      pop[pop[[class_var]]=='90 - 94세',value_var[3]] + pop[pop[[class_var]]=='95+',value_var[3]]

    result_pop <- c(male.20_29, female.20_29, male.30_39, female.30_39, male.40_49, female.40_49,
                    male.50_59, female.50_59, male.60_69, female.60_69, male.70, female.70)

  }

  names(result_pop) <- chs_class_range
  result_pop <- t(dplyr::as_tibble(result_pop))
  return(result_pop)

}
