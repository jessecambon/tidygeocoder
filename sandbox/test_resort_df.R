library(tibble)
library(dplyr)

addresses_in_order <- tibble(
  order = c(1, 2, 3, 4),
  city = c('Las Vegas', 'Chicago', 'Detroit', 'Pittsburgh'),
  state = c('NV', 'IL', 'MI', 'PA')
)

addresses_out_of_order <- addresses_in_order %>%
  arrange(desc(state))

addresses_resort <- addresses_out_of_order %>%
  left_join(
    addresses_in_order %>% mutate(.id = 1:nrow(addresses_in_order)) %>%
      select(city, state, .id),
    by = c('city', 'state')
  ) %>%
  arrange(.id)
