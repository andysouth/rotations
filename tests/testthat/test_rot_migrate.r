library(rotations)

context("migration")

# TODO decide what tests to do
# test that insecticides don't get muddled
# test that genders don't get muddled
# test that tot freqs for sites are same before as after
# maybe test that sum of whole array before and after is the same

RAF2 <- set_start_freqs(n=3, max_generations=1, freqs=c(0.1,0.2,0.3))
# set freqs in refugia to 0 to see effects of migration
RAF2[,,"refugia",] <- 0
bef <- RAF2[,,,1]
aft <- rot_migrate(bef, migration = 0.1, coverage = 0.5)

sum(bef) == sum(aft)

sum(bef[,'m',]) == sum(aft[,'m',])
# insecticide1
sum(bef[1,,]) == sum(aft[1,,])

# expect that if an insecticide has freq 0 in both before 

test_that("migration works for m & f ...", {
  
  RAF2 <- set_start_freqs(n=3, max_generations=1, freqs=c(0.1,0.2,0.3))
  
  # set freqs in refugia to 0 to see effects of migration
  RAF2[,,"refugia",] <- 0
  
  RAF1gen <- rot_migrate(RAF2[,,,1], migration = 0.1, coverage = 0.5)
  
  rot_migrate(RAF2[,,,1], migration = 0.1, coverage = 0.5, verbose = TRUE)
  
  #try setting m to 0 just to test
  RAF2[,"m",,] <- 0  
  
  #expect ...
  #expect_equal( a_fitnic['RR1','RR2','A','B'], 1 )
  
  
})