context('alternative parsers')

a = chr('a')
aa = lit('aa')
a2 = or(a, aa)
ab = or(a, chr('b'))
vowel = any_of('aeiou')

test_that('parsers can be alternated', {
    expect_that(a2('a'), has_match(2L))
    expect_that(a2('aa'), has_match(2L, 3L))
    expect_that(or(empty, a, aa)('aa'), has_match(1L, 2L, 3L))
    expect_that(a2('ab'), has_match(2L))

    expect_that(a2(''), has_no_match())
    expect_that(a2('b'), has_no_match())
    expect_that(a2('ba'), has_no_match())
    
    expect_that(ab(''), has_no_match())
    expect_that(ab('a'), has_match(2L))
    expect_that(ab('b'), has_match(2L))
    expect_that(ab('c'), has_no_match())

    expect_that(or(a, vowel)(''), has_no_match())
    expect_that(or(a, vowel)('a'), has_match(2L))
    expect_that(or(a, vowel)('b'), has_no_match())
    expect_that(or(a, vowel)('e'), has_match(2L))
})

test_that('alternative parsers can be printed', {
    expect_that(a2, prints_as('("a"|"aa")'))
    expect_that(ab, prints_as('("a"|"b")'))
})

test_that('nested alternations are flattened', {
    expect_that(or(ab, a2), prints_as('("a"|"b"|"a"|"aa")'))
    expect_that(as.character(or(lit('a'), lit('b'), lit('c'))),
                equals(as.character(or(or(lit('a'), lit('b')), lit('c')))))
})
