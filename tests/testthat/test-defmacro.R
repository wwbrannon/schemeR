context("Macros")

test_that("gensym works correctly", {
    sm <- gensym(str="testfoo", len=15)
    expect_equal(substr(as.character(sm), 1, 7), "testfoo")
    expect_equal(nchar(as.character(sm)), 15)
    expect_is(sm, "name")

    sm <- gensym(str="testfoo")
    expect_equal(substr(as.character(sm), 1, 7), "testfoo")
    expect_equal(nchar(as.character(sm)), 10) #the default
    expect_is(sm, "name")

    for(i in 2:100)
        expect_true(nchar(as.character(gensym(len=i))) == i)
})
