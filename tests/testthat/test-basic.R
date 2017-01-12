context("Basic aliases")

test_that("nil and is.nil work correctly", {
    expect_true(is.null(nil))

    expect_true(is.nil(nil))
    expect_true(is.nil(NULL))
    expect_true(is.nil(pairlist()))
    expect_false(is.nil(1))
    expect_false(is.nil("foo"))
    expect_false(is.nil(list()))
})

test_that("is.procedure works correctly", {
    expect_true(is.procedure(body))
    expect_true(is.procedure(sum))
    expect_true(is.procedure(c))
    expect_true(is.procedure(`+`))
    expect_true(is.procedure(`{`))

    expect_false(is.procedure(1))
    expect_false(is.procedure("foo"))
    expect_false(is.procedure(NULL))
})

test_that("is.boolean works correctly", {
    expect_true(is.boolean(TRUE))
    expect_true(is.boolean(FALSE))
    expect_true(is.boolean(NA))

    expect_false(is.boolean(1))
    expect_false(is.boolean("foo"))
    expect_false(is.boolean(NULL))
})

test_that("begin and progn work correctly", {
    expect_identical(begin, progn)

    e1 <- begin(nil, 1 + 4)
    e2 <- expression({nil; 1 + 4})[[1]]
    expect_equivalent(e1, e2)

    e1 <- begin(1 + 4, sin(x))
    e2 <- expression({1 + 4; sin(x)})[[1]]
    expect_equivalent(e1, e2)

    #an expr with a side effect - tests lazy eval
    e1 <- begin(1 + 4, print("foo"))
    e2 <- expression({1 + 4; print("foo")})[[1]]
    expect_equivalent(e1, e2)
})

test_that("fromPkg works correctly", {
    expect_equivalent(c, fromPkg(base, c))
    expect_equivalent(utils::zip, fromPkg(utils, zip))
    expect_equivalent(stats::glm, fromPkg(stats, glm))

    expect_error(fromPkg())

    #if you have installed packages named this, you deserve the failing test
    expect_error(fromPkg(foobar, bazquux))
})

test_that("display works correctly", {
    expect_error(display())

    f <- capture.output(display(1))
    expect_equal(f, "[1] 1")

    f <- capture.output(display(NULL))
    expect_equal(f, "NULL")

    f <- capture.output(display("foo"))
    expect_equal(f, '[1] "foo"')

})

test_that("Non-binary comparison works correctly", {
    expect_false(eq())
    expect_false(ge())
    expect_false(le())
    expect_false(gt())
    expect_false(lt())

    expect_false(eq(nil))
    expect_false(ge(nil))
    expect_false(le(nil))
    expect_false(gt(nil))
    expect_false(lt(nil))

    expect_true(eq(1))
    expect_true(ge(1))
    expect_true(le(1))
    expect_true(gt(1))
    expect_true(lt(1))

    expect_true(eq(0))
    expect_true(ge(0))
    expect_true(le(0))
    expect_true(gt(0))
    expect_true(lt(0))

    expect_true(eq(-1))
    expect_true(ge(-1))
    expect_true(le(-1))
    expect_true(gt(-1))
    expect_true(lt(-1))

    expect_true(eq("foo"))
    expect_true(ge("foo"))
    expect_true(le("foo"))
    expect_true(gt("foo"))
    expect_true(lt("foo"))

    expect_false(eq(nil, nil))
    expect_false(ge(nil, nil))
    expect_false(le(nil, nil))
    expect_false(gt(nil, nil))
    expect_false(lt(nil, nil))

    expect_true(eq(1,1))
    expect_true(eq(1,1,1))
    expect_true(eq(1,1,1,1,1,1,1))
    expect_false(eq(1,1,2,1,1,1,1))
    expect_false(eq(1,1,nil,1,1,1,1))
    expect_true(eq("a", "a", "a"))
    expect_false(eq("a", "b", "a"))
    expect_false(eq("a", nil, "a"))

    expect_true(ge(4,3))
    expect_true(ge(4,3,2,1))
    expect_true(ge(4,4,2,1))
    expect_false(ge(1,2,3,4))
    expect_false(ge(1,2,3,nil))
    expect_true(ge("d","c"))
    expect_true(ge("d","c","b","a"))
    expect_true(ge("d","d","b","a"))
    expect_false(ge("a","b","c","d"))
    expect_false(ge("d",nil,"b","a"))

    expect_true(le(3,4))
    expect_true(le(1,2,3,4))
    expect_true(le(1,2,4,4))
    expect_false(le(4,3,2,1))
    expect_false(le(nil,1,2,3))
    expect_true(le("c","d"))
    expect_true(le("a","b","c","d"))
    expect_true(le("a","b","d","d"))
    expect_false(le("d","c","b","a"))
    expect_false(le("a","b",nil,"d"))

    expect_true(gt(4,3))
    expect_true(gt(4,3,2,1))
    expect_false(gt(1,2,3,4))
    expect_false(gt(1,2,3,nil))
    expect_true(gt("d","c"))
    expect_true(gt("d","c","b","a"))
    expect_false(gt("a","b","c","d"))
    expect_false(gt("d",nil,"b","a"))

    expect_true(lt(3,4))
    expect_true(lt(1,2,3,4))
    expect_false(lt(4,3,2,1))
    expect_false(lt(nil,1,2,3))
    expect_true(lt("c","d"))
    expect_true(lt("a","b","c","d"))
    expect_false(lt("d","c","b","a"))
    expect_false(lt("a","b",nil,"d"))
})
