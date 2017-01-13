context("Prefix flow-control operators")

test_that('"or" and "and" work correctly', {
    expect_true(or(1==2, TRUE, print("foo")))
    expect_length(capture.output(f <- (or(1==2, TRUE, print("foo")))), 0)
    expect_equal(f, TRUE)

    expect_equal(or(1==2, FALSE, capture.output(cat("foo")), 1), "foo")
    expect_length(capture.output(f <- (or(1==2, FALSE, print("foo")))), 1)
    expect_equal(f, "foo")

    expect_false(and(1==2, TRUE, print("foo")))
    expect_length(capture.output(f <- and(1==2, TRUE, print("foo"))), 0)
    expect_equal(f, FALSE)

    expect_equal(and(1==1, TRUE, 1, capture.output(cat("foo"))), "foo")
    expect_length(capture.output(f <- and(1==1, TRUE, print("foo"))), 1)
    expect_equal(f, "foo")
})

test_that("when and unless work correctly", {

})

test_that("case works correctly", {
    schemeR({
        .(case, .(`+`, 1, 1),
            .(3, .(print, "foo")),
            .(2, .(print, "bar")))
     })
})

test_that("cond works correctly", {
    schemeR({
        .(cond,
            .(.(`==`, .(`+`, 1, 4), 4), .(print, "foo")),
            .(.(`==`, .(`+`, 1, 3), 5), .(print, "bar")),
            .(TRUE, .(print, "baz")))
    })
})

test_that("do works correctly", {
    schemeR({
        .(let, .(.(x, .(c, 1, 3, 5, 7, 9))),
            .(do, .(.(x, .(c, 1, 3, 5, 7, 9), .(cdr, x)),
                    .(s, 0, .(`+`, s, .(car, x))),
                    .(foo, 4)),
                .(.(is.nil, x), s),
        TRUE)) == 25
    })
})
