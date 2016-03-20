context('parser combinator operators')

a = chr('a')
aa = lit('aa')
vowel = any_of('aeiou')

test_that('parsers can be combined via operators', {
    expect_that(as.character(start & vowel),
                equals(as.character(seq(start, vowel))))
    expect_that(as.character(a & aa & vowel),
                equals(as.character(seq(a, aa, vowel))))

    expect_that(as.character(a | aa),
                equals(as.character(or(a, aa))))
    expect_that(as.character(a | aa | vowel),
                equals(as.character(or(a, aa, vowel))))
})
