context("List manipulation functions")

test_that("List utility functions work correctly", {
    expect_identical(member, `%in%`)
    expect_identical(reverse, rev)

    expect_null(cons())
    expect_equal(cons(1,2), c(1,2))
    expect_equal(cons(12, nil), 12)
    expect_equal(cons(nil, 12), 12)
    expect_equal(cons(nil, "a"), "a")
    expect_equal(cons("a", nil), "a")

    expect_true(is.empty(nil))
    expect_true(is.empty(list()))
    expect_true(is.empty(numeric(0)))
    expect_true(is.empty(character(0)))
    expect_false(is.empty(0))
    expect_false(is.empty("a"))
    expect_false(is.empty(123))
    expect_false(is.empty(list(1,2)))

    expect_equal(nth(1:10, 1), 1)
    expect_equal(nth(1:10, 5), 5)
    expect_equal(nth(as.list(1:10), 1), 1)
    expect_equal(nth(as.list(1:10), 5), 5)
    expect_equal(nth(as.pairlist(as.list(1:10)), 1), 1)
    expect_equal(nth(as.pairlist(as.list(1:10)), 5), 5)

    expect_error(nth(1:10, -1))
    expect_error(nth(1:10, 11))
    expect_error(nth(as.list(1:10), -1))
    expect_error(nth(as.list(1:10), 11))
    expect_error(nth(as.pairlist(as.list(1:10)), -1))
    expect_error(nth(as.pairlist(as.list(1:10)), 11))

    expect_equal(make.list(4, 1), list(1,1,1,1))
    expect_equal(make.list(4, "a"), list("a", "a", "a", "a"))
    expect_equal(make.list(4, list(1,2)), list(list(1,2),list(1,2),
                                               list(1,2),list(1,2)))

    suppressWarnings(expect_error(make.list("a", 4)))
    expect_error(make.list(-1, 4))
    expect_error(make.list(nil, 4))
})

test_that("List utility functions work correctly", {
    expect_identical(first, car)

    expect_null(car(nil))
    expect_null(car(list()))
    expect_equal(car(1:10), 1)
    expect_equal(car(as.list(1:10)), 1)
    expect_equal(car(as.pairlist(as.list(1:10))), 1)
    expect_equal(car(list(list(1, 2), list(3, 4), list(5, 6))), list(1, 2))

    expect_null(cdr(nil))
    expect_null(cdr(list()))
    expect_equal(cdr(1:10), 2:10)
    expect_equal(cdr(as.list(1:10)), as.list(2:10))
    expect_equal(cdr(as.pairlist(as.list(1:10))), as.pairlist(as.list(2:10)))
    expect_equal(cdr(list(list(1, 2), list(3, 4), list(5, 6))),
                 list(list(3, 4), list(5, 6)))

    expect_null(last(nil))
    expect_null(last(list()))
    expect_equal(last(1:10), 10)
    expect_equal(last(as.list(1:10)), 10)
    expect_equal(last(as.pairlist(as.list(1:10))), 10)
    expect_equal(last(list(list(1, 2), list(3, 4), list(5, 6))), list(5, 6))

    #We're not testing the c____r functions: they're exactly compositions of
    #car and cdr, so provided car, cdr and compose behave as expected,
    #correctness is guaranteed
})
