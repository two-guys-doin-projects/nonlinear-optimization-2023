# getting basic results - found extremum, value at extremum,
# no. of target function calls and which extremum we've hit
function_calls <- 0
funkcja_celu <- function(x) {
  sin(x/10)*exp(-(x/10+pi)^2) - cos(x/10)*exp(-(x/10-2*pi)^2) + 0.003*(x/10)^2
}

# checks which extremum the function has found.
# Indedx 0 means that the found x is near the global extremum.
determine_extremum <- function(found_extremum){
    extremums <- c(62.5, -52.5, -25, 0)
    distance_from_extremums <- abs(found_extremum - extremums)
    return(which.min(distance_from_extremums) - 1)
}

# TODO need to account for lagrange method return value(need no. of target function calls from it!)
performLagrange <- function(func, a, b){
    epsilon <- 0.01
    result <- lagrange_min(func, c(a, b), epsilon)
    if(is.numeric(result[1])){
        extremum_center <- result[1]
        function_value_at_extremum <- result[2]
        found_extremum <- determine_extremum(extremum_center)
        number_of_func_calls <- result[4]
    }
    else{
        extremum_center <- NA
        function_value_at_extremum <- NA
        found_extremum <- NA
        number_of_func_calls <- result[4]
    }
    return(c(
        extremum_center,
        function_value_at_extremum,
        number_of_func_calls,
        found_extremum))
}

performNewtonArmijo <- function(func, a, b) {
    function_calls <<- 0
    epsilon <- 0.01
    alpha <- 0.15
    rho <- 0.1
    starting_point <- (a + b)/2
    result <- newtonArmijo(func, starting_point, alpha, rho, epsilon)
    extremum <- result[1]
    function_value_at_extremum <- result[2]
    found_extremum <- determine_extremum(result[1])
    number_of_func_calls <- result[3]
    return(c(
        extremum,
        function_value_at_extremum,
        number_of_func_calls,
        found_extremum))
}

performNewtonArmijo(funkcja_celu, 10, 100)


performCalculations <- function(func, points, bds_step_size){
    intervals <- NULL
    for(point in points){
        next_interval <- BDS(func, point, bds_step_size)
        intervals <- rbind(intervals, next_interval)
    }
    interval_list <- split(
        intervals, seq(nrow(intervals))) # dataframe to list of rows conversion
    lagrange_results <- NULL
    newton_results <- NULL
    for(interval in interval_list){
        a <- interval[1]
        b <- interval[2]
        lagrange_result <- performLagrange(func, a, b)
        lagrange_results <- rbind(lagrange_results, lagrange_result)
        newton_result <- performNewtonArmijo(func, a, b)
        newton_results <- rbind(newton_results, newton_result)
    }
    return(list(
        intervals,
        lagrange_results,
        newton_results
    ))
}

calc_results <- performCalculations(funkcja_celu, points, 0.1)

lagrange_results <- calc_results[[2]]
as.numeric(lagrange_results[,2])
# processing dataframes to get mean values

processBDSResults <- function(bds_data_frame){
    interval_lengths <- abs(bds_data_frame[,2] - bds_data_frame[,1])
    mean_interval_length <- mean(interval_lengths)
    mean_no_of_func_calls <- round(
        mean(bds_data_frame[,3]), digits=0
    )
    return(c(mean_interval_length, mean_no_of_func_calls))
}

processBDSResults(calc_results[[1]])
library(imputeTS)
processResultsOfExtremum<- function(results_dataframe, extremum_index){
    results_of_extremum <- results_dataframe[
        as.numeric(results_dataframe[,4]) == as.numeric(extremum_index),]
    if(is.na(extremum_index)){
        mean_extremum_point <- NA
        mean_extremum_value <- NA
    }
    else{
        mean_extremum_point <- mean(
            na_replace(
            as.numeric(results_of_extremum[,1]),
            0)
            )
        mean_extremum_value <- mean(
            na_replace(
            as.numeric(results_of_extremum[,2]),
            0)
            )
    }
    mean_no_of_func_calls <- round(
        mean(
            na_replace(
            as.numeric(results_of_extremum[,3]),
            0)), digits=0)
    no_of_occurences <- length(results_of_extremum[,1])
    return(c(
        mean_extremum_point,
        mean_extremum_value,
        mean_no_of_func_calls,
        no_of_occurences
    ))
}

processResultsOfMethod <- function(method_results_dataframe){
    extremums <- as.numeric(c(sort(unique(method_results_dataframe[,4])), NA))
    mean_results <- NULL
    for(extremum in extremums){
        results_for_extremum <- processResultsOfExtremum(method_results_dataframe, extremum)
        mean_results <- rbind(mean_results, results_for_extremum)
    }
    return(mean_results)
}
calc_results[[2]] <- lapply(calc_results[[2]], as.numeric)
processResultsOfMethod(lagrange_results)

library(stringr)

doSimulationForStepSizes <- function(func, points, step_sizes){
    step_size_result_paths <- c("step1", "step2", "step3")
    for(step_size_index in 1:3){
        simulationResults <- performCalculations(func, points, step_sizes[step_size_index])
        bds_intervals <- simulationResults[[1]]
        lagrange_results <- simulationResults[[2]]
        newton_results <- simulationResults[[3]]
        write.csv(bds_intervals,
         str_interp(
            "./results/${step_size_result_paths[step_size_index]}/bds_intervals.csv"))
        write.csv(lagrange_results, 
            str_interp(
                "./results/${step_size_result_paths[step_size_index]}/lagrange_results.csv"))
        write.csv(newton_results, 
            str_interp(
                "./results/${step_size_result_paths[step_size_index]}/newton_results.csv"))
        mean_bds_results <- processBDSResults(bds_intervals)
        mean_lagrange_results <- processResultsOfMethod(lagrange_results)
        mean_newton_results <- processResultsOfMethod(newton_results)
        write.csv(mean_bds_results,
            str_interp(
                "./results/${step_size_result_paths[step_size_index]}/mean_bds_intervals.csv"))
        write.csv(mean_lagrange_results,
            str_interp(
                "./results/${step_size_result_paths[step_size_index]}/mean_lagrange_results.csv"))
        write.csv(mean_newton_results,
            str_interp(
                "./results/${step_size_result_paths[step_size_index]}/mean_newton_results.csv"))
    }
}
library(stringr)
doSimulationForStepSizes(funkcja_celu, points, c(0.1, 0.15, 0.3))
