
set.seed(16)

# Sample NACCIDs. These will be used to make sure a few people have 0 for ALL CDR variables
sample_naccids <- sample(100, 5)

# Create basic table
nacc_cdr_data_simulated <- tibble(NACCID = 1:100,
                                  VISITDATE = rep(lubridate::as_date("2010-01-1"),100) + sample(0:5000, 100),
                                  MEMORY = sample(c(0,0.5,1,2,3), size = 100, replace = T, prob = c(40,30,20,20,10)),
                                  ORIENT = sample(c(0,0.5,1,2,3), size = 100, replace = T, prob = c(35,35,20,20,10)),
                                  JUDGMENT = sample(c(0,0.5,1,2,3), size = 100, replace = T, prob = c(60,30,10,10,10)),
                                  COMMUN = sample(c(0,0.5,1,2,3), size = 100, replace = T, prob = c(60,30,10,10,10)),
                                  HOMEHOBB = sample(c(0,0.5,1,2,3), size = 100, replace = T, prob = c(60,30,10,10,10)),
                                  PERSCARE = sample(c(0,0.5,1,2,3), size = 100, replace = T, prob = c(60,30,10,10,10)),
                                  COMPORT = sample(c(0,0.5,1,2,3), size = 100, replace = T, prob = c(60,30,10,10,10)),
                                  CDRLANG = sample(c(0,0.5,1,2,3), size = 100, replace = T, prob = c(60,30,10,10,10))) %>%
  mutate( across(MEMORY:CDRLANG, ~if_else(NACCID %in% sample_naccids, 0, .x) )
  )

# usethis::use_data(nacc_cdr_data_simulated, overwrite = T)

rm(sample_naccids, nacc_cdr_data_simulated)
