context('literal parsers')

test_that('literal parser works', {
    expect_that(lit('')(''), has_match(1L))
    expect_that(lit('')('a'), has_match(1L))
    expect_that(lit('foo')('foo'), has_match(4L))
    expect_that(lit('foo')('foobar'), has_match(4L))
})

test_that('literals can be printed', {
    expect_that(lit('foo'), prints_as('"foo"'))
})
