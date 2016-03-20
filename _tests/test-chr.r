context('character parsers')

test_that('character parsers work', {
    a = chr('a')
    expect_that(a('a'), has_match(2L))
    expect_that(a('b'), has_no_match())
})

test_that('characters can be printed', {
    a = chr('a')
    q = chr('"')

    expect_that(a, prints_as('"a"'))
    expect_that(q, prints_as('"\\""'))
})
