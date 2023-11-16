library(stringr)

test_that("rvec_to_matlabcell behaves the same with default args", {
  # called with only the x arg specified
  expect_equal(rvec_to_matlabcell(letters[1:3]), "{'a'; 'b'; 'c';};")
})

test_that("rvec_to_matlabcell does not quote numeric inputs", {
  numeric_vector <- 1:3
  # this should undo the matlab cell array syntax and recover the "numbers" within
  # without stripping off single quotes applied by rvec_to_matlabcell
  recovered_vector <- as.numeric(
    str_split_1(
      str_remove_all(
        rvec_to_matlabcell(1:3), 
        "\\{|\\}|;"),
      " ")
  )
  expect_equal(numeric_vector, recovered_vector)
})

test_that("rvec_to_matlabcell always closes the line with semicolon", {
  expect_true(endsWith(rvec_to_matlabcell(letters[1:3]), ";"))
  expect_true(endsWith(rvec_to_matlabcell(letters[1:3], sep = ","), ";"))
})

test_that("rvec_to_matlabcell errors when list contains non-vectors", {
  expect_error(rvec_to_matlabcell(list(mean)))
})
