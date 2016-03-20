context('character class parsers')

vowel = any_of('aeiou')
not_vowel = any_not_of('aeiou')

test_that('character classes work', {
    expect_that(vowel('a'), has_match(2L))
    expect_that(vowel('e'), has_match(2L))
    expect_that(vowel('i'), has_match(2L))
    expect_that(vowel('o'), has_match(2L))
    expect_that(vowel('u'), has_match(2L))
    expect_that(vowel('b'), has_no_match())

    expect_that(not_vowel('a'), has_no_match())
    expect_that(not_vowel('e'), has_no_match())
    expect_that(not_vowel('i'), has_no_match())
    expect_that(not_vowel('o'), has_no_match())
    expect_that(not_vowel('u'), has_no_match())
    expect_that(not_vowel('b'), has_match(2L))
})

test_that('character classes can be printed', {
    expect_that(vowel, prints_as('["aeiou"]'))
    expect_that(not_vowel, prints_as('[^"aeiou"]'))
})
