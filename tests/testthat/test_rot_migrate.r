library(rotations)

context("migration")

# TODO decide what tests to do
# test that insecticides don't get muddled
# test that genders don't get muddled
# test that tot freqs for sites are same before as after
# maybe test that sum of whole array before and after is the same

RAF2 <- set_start_freqs(max_gen=1, freqs=c(0.1,0.2,0.3))
# set freqs in refugia to 0 to see effects of migration
RAF2[,,"refugia",] <- 0
# set m' to 0 to check that mf not getting mixed up
RAF2[,"m",,] <- 0

bef <- RAF2[,,,1]
aft <- rot_migrate(bef, migration = 0.1, coverage = 0.5)

#best way to compare by eye
#raf_get(bef,asdf=TRUE)
#raf_get(aft,asdf=TRUE)


test_that("sum of frequencies is same before & after migration ...", {
  
  expect_equal( sum(bef), sum(aft) )
  
})

test_that("sum of frequencies for each insecticide is same before & after migration ...", {
  
  for(i in 1:dim(bef)['insecticide'])
  {
    expect_equal( sum(raf_get(bef,insecticide=i)), sum(raf_get(aft,insecticide=i)) )    
  }
})

test_that("sum of frequencies by sex is same before & after migration ...", {
  
  for(sex in c('m','f'))
  {
    expect_equal( sum(raf_get(bef,sex=sex)), sum(raf_get(aft,sex=sex)) )    
  }
})
