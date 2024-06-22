#' Calculate seasonal factors
#'
#' This function calculates seasonal factors for each product based on demand data.
#'
#' @param demand_data A data frame containing demand data with columns: Product, Month, Demand.
#' @return A data frame with columns: Product, Month, SeasonalFactor.
#' @export
# Function to calculate seasonal factors
calculate_seasonal_factors <- function(demand_data) {
  demand_data %>%
    group_by(Product, Month) %>%
    summarise(AverageDemand = mean(Demand, na.rm = TRUE), .groups = 'drop') %>%
    group_by(Product) %>%
    mutate(OverallAverage = mean(AverageDemand, na.rm = TRUE),
           SeasonalFactor = AverageDemand / OverallAverage) %>%
    ungroup() %>%
    select(Product, Month, SeasonalFactor)
}
#' Stockin function
#'
#' This function calculates reorder quantities based on demand, inventory, and service level parameters.
#'
#' @param parameters_df A data frame containing product and store information.
#' @param inventory_data A data frame containing inventory levels for each product and store.
#' @param demand_data A data frame containing demand data with columns: Product, Month, Demand.
#' @param orderleadtime Lead time for placing orders.
#' @param delivery_leadtime Lead time for delivery of orders.
#' @param product_service_levels A data frame containing service levels for each product.
#' @param order_leadtime_filter Filter for specific order lead time.
#' @return A data frame with order information including product, store, reorder status, order quantity, and colisage.
#' @export
# Modified stockin function
stockin <- function(parameters_df, inventory_data, demand_data, orderleadtime, delivery_leadtime, product_service_levels, seasonal_factors_df, order_leadtime_filter = NULL) {
  if (is.null(parameters_df) || nrow(parameters_df) == 0) {
    stop("Parameter data frame is empty.")
  }

  # Join seasonal factors with demand data
  demand_data <- demand_data %>%
    left_join(seasonal_factors_df, by = c("Product", "Month"))

  results_list <- list()

  for (i in 1:nrow(parameters_df)) {
    product <- parameters_df$Product[i]
    store <- parameters_df$Store[i]

    reorder_needed <- rep(FALSE, orderleadtime)
    order_quantity <- rep(0, orderleadtime)
    product_demand_data <- demand_data %>% filter(Product == product)

    if (nrow(product_demand_data) == 0) {
      next
    }

    # Colisage and service level
    colisage <- product_demand_data$colisage[1]
    service_level <- product_service_levels %>% filter(Product == product) %>% pull(ServiceLevel)

    # Initial inventory for the product in the specific store
    current_inventory <- inventory_data %>% filter(Product == product, Store == store) %>% pull(Inventory)
    if (length(current_inventory) != 1) {
      stop(paste("Invalid inventory for Product:", product, "Store:", store))
    }

    for (lt in 1:orderleadtime) {
      month <- (lt %% 12) + 1
      seasonal_factor <- product_demand_data %>% filter(Month == month) %>% pull(SeasonalFactor)

      if (length(seasonal_factor) == 0) {
        seasonal_factor <- 1
      }

      # Total demand
      demand <- sum(product_demand_data$Demand, na.rm = TRUE)
      seasonal_demand <- demand * seasonal_factor[1]  # Ensure scalar value

      historical_demand_length <- max(1, nrow(product_demand_data))
      average_historical_demand <- demand / historical_demand_length

      # Calculation of order quantity
      if (lt > 1) {
        order_quantity[lt] <- seasonal_demand * ceiling((lt - delivery_leadtime) / historical_demand_length) +
          service_level * mean(order_quantity[max(1, lt-2):lt])
      } else {
        order_quantity[lt] <- seasonal_demand
      }

      # Ensure order quantity is non-negative
      order_quantity[lt] <- max(0, order_quantity[lt])

      # Update inventory and check if reorder is needed
      if (current_inventory - seasonal_demand < 0) {
        reorder_needed[lt] <- TRUE
        current_inventory <- current_inventory + order_quantity[lt]
      } else {
        current_inventory <- current_inventory - seasonal_demand
      }

      # Calculation of colisages needed
      colisages_needed <- if (reorder_needed[lt]) ceiling(order_quantity[lt] / colisage) else 0

      if (is.null(order_leadtime_filter) || lt == order_leadtime_filter) {
        product_data <- data.frame(
          Product = product,
          Store = store,
          ReorderNeeded = reorder_needed[lt],
          OrderQuantity = ifelse(reorder_needed[lt], order_quantity[lt], 0),
          Colisage = ifelse(reorder_needed[lt], colisages_needed, 0)
        )
        results_list[[length(results_list) + 1]] <- product_data
      }
    }
  }

  final_results <- do.call(rbind, results_list)
  return(final_results)
}


#' Calculate service level
#'
#' This function calculates the service level for a given product based on demand data.
#'
#' @param demand_data A data frame containing demand data with columns: Product, Demand.
#' @param product The product for which service level is to be calculated.
#' @return The calculated service level.
#' @export
# Updated calculate_service_level function with demand_data as an argument
calculate_service_level <- function(demand_data, product) {
  product_demand <- demand_data$Demand[demand_data$Product == product]

  demand_mean <- mean(product_demand)
  demand_sd <- sd(product_demand)

  desired_service_level <- 0.95

  service_level <- qnorm(desired_service_level, demand_mean, demand_sd)

  return(service_level)
}
