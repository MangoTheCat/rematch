
context("re_match_all")

test_that("corner cases", {

  res <- re_match_all("", c("foo", "bar"))
  expect_identical(
    res,
    tibble::data_frame(.match = c(list(c("", "", "")), list(c("", "", ""))))
  )

  res <- re_match_all("", c("", "bar"))
  expect_identical(
    res,
    tibble::data_frame(
      .match = c(list(""), list(c("", "", ""))))
  )

  expect_error(re_match_all("", character()))

  expect_error(re_match_all("foo", character()))

  res <- re_match_all("foo", "not")
  expect_identical(res, tibble::data_frame(.match = list(NA_character_)))
})


test_that("capture groups", {

  pattern <- "([0-9]+)"

  res <- re_match_all(pattern, c("123xxxx456", "", "xxx", "1", "123"))
  expect_identical(
    res,
    tibble::data_frame(
      .match = c(
          list(c("123", "456")),
            list(NA_character_),
            list(NA_character_),
            list("1"),
            list("123")),
      V1 = c(
          list(c("123", "456")),
            list(""),
            list(""),
            list("1"),
            list("123")))
  )

})


test_that("scalar text with capture groups", {

  res <- re_match_all("\\b(\\w+)\\b", "foo bar")
  expect_identical(res,
    tibble::data_frame(.match = list(c("foo", "bar")), V1 = list(c("foo", "bar"))))

  res <- re_match_all("\\b(?<word>\\w+)\\b", "foo bar")
  expect_identical(res,
    tibble::data_frame(.match = list(c("foo", "bar")), word = list(c("foo", "bar"))))
})
