############ TEST: tdf #########
# Transform a data frame

########### Last Updated by Joanne 12/6 ############


context("tdf")

test_that("tdf transpose a data frame and return a data frame", {
  expect_is(tdf(data.frame(x=1:5, y = 6:10)),"data.frame")
  df <- as.data.frame(rbind(1:5, 6:10))
  rownames(df) <- c("x", "y")
  expect_equal(tdf(data.frame(x = 1:5, y = 6:10)),
               df)
})
