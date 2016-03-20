context('empty parser')

test_that('empty parser works', {
    expect_that(empty(''), has_match(1L))
    expect_that(empty('a'), has_match(1L))
    expect_that(empty('ab'), has_match(1L))
})

test_that('empty parser can be printed', {
    expect_that(empty, prints_as('ε'))
})
