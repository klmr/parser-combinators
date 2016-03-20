context('Kleene star parser')

a = chr('a')
aa = lit('aa')

test_that('Kleene star parser works', {
    expect_that(many(a)(''), has_match(1L))
    expect_that(many(a)('a'), has_match(1L, 2L))
    expect_that(many(a)('aa'), has_match(1L, 2L, 3L))
    expect_that(many(a)('b'), has_match(1L))
    expect_that(many(a)('ab'), has_match(1L, 2L))
    expect_that(many(a)('aba'), has_match(1L, 2L))

    expect_that(many(aa)(''), has_match(1L))
    expect_that(many(aa)('a'), has_match(1L))
    expect_that(many(aa)('aa'), has_match(1L, 3L))
    expect_that(many(aa)('aaa'), has_match(1L, 3L))
})

test_that('Kleene star can be printed', {
    expect_that(many(a), prints_as('"a"*'))
    expect_that(many(aa), prints_as('"aa"*'))
})
