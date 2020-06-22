library(tidyr)

### Test the renaming behaviour of bind_cols when column names match

j <- mtcars['cyl']
names(j) <- 'cyl1'

x_tidy <- bind_cols(mtcars['cyl'],mtcars['cyl'],j,mtcars['cyl'])

x <- cbind(mtcars['cyl'],mtcars['cyl'],j,mtcars['cyl'])

x_tib <- as_tibble(x,.name_repair='unique')
