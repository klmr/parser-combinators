context('negative parsers')

a = chr('a')
b = chr('b')
aa = lit('aa')

test_that('negative parsers work', {
    expect_that(not(a)(''), has_match(1L))
    expect_that(not(a)('a'), has_no_match())
    expect_that(not(a)('b'), has_match(1L))

    expect_that(not(aa)(''), has_match(1L))
    expect_that(not(aa)('a'), has_match(1L))
    expect_that(not(aa)('aa'), has_no_match())
    expect_that(not(aa)('ab'), has_match(1L))

    # `empty` always matches; hence, `not(empty)` never matches.
    expect_that(not(empty)(''), has_no_match())
    expect_that(not(empty)('a'), has_no_match())
})

test_that('negative parsers can be printed', {
    expect_that(not(a), prints_as('!"a"'))
})
