context('sequential parsers')

a = chr('a')
aa = lit('aa')
aaa = seq(a, aa)
aab = seq(aa, chr('b'))
vowel = any_of('aeiou')

test_that('parsers can be combined sequentially', {
    expect_that(aaa('aaa'), has_match(4L))
    expect_that(aaa('aaaa'), has_match(4L))
    expect_that(aaa('aa'), has_no_match())
    expect_that(aab('aaa'), has_no_match())
    expect_that(aab('aab'), has_match(4L))

    expect_that(seq(aab, vowel)('aabe'), has_match(5L))
    expect_that(seq(aab, vowel)('aae'), has_no_match())
    expect_that(seq(aab, vowel)('aabec'), has_match(5L))

    expect_that(seq(start, aa)('aa'), has_match(3L))
    expect_that(seq(aa, end)('aa'), has_match(3L))
    expect_that(seq(start, aa, end)('aa'), has_match(3L))
    expect_that(seq(aa, start)('aa'), has_no_match())
})

test_that('sequentially combined parsers can be printed', {
    expect_that(aaa, prints_as('("a""aa")'))
    expect_that(seq(aa, chr('b')), prints_as('("aa""b")'))
    expect_that(seq(start, vowel, end), prints_as('(^["aeiou"]$)'))
})

test_that('nested sequences are flattened', {
    expect_that(as.character(seq(a, a, a)),
                equals(as.character(seq(seq(a, a), a))))
})
