context('anchor parsers')

test_that('anchor parsers work', {
    expect_that(start(''), has_match(1L))
    expect_that(start('a'), has_match(1L))
    expect_that(start('ab', 2L), has_no_match())

    expect_that(end(''), has_match(1L))
    expect_that(end('a'), has_no_match())
    expect_that(end('a', 2L), has_match(2L))
})

test_that('anchors can be printed', {
    expect_that(start, prints_as('^'))
    expect_that(end, prints_as('$'))
})
