context("Prefix notation support and conversion")

test_that("The . function works", {
    expect_null(.())
    expect_equal(.(NULL), quote(NULL()))
    expect_error(eval(.(NULL)))

    expect_equal(.(c, 1, 2, 3), quote(c(1,2,3)))
    expect_equal(eval(.(c, 1, 2, 3)), c(1,2,3))

    #Should also work where the resulting expression is not sensible R
    expect_equal(.(1, 2, 3), quote(1(2,3)))
    expect_error(eval(.(1, 2, 3)))
})

test_that("Infix-to-prefix conversion works", {

})

test_that("Prefix-to-infix conversion works", {

})
