is_empty = function () {
    function (x)
        expectation(length(x) == 0L, 'isn’t empty', 'is empty')
}

has_match = function (...) {
    function (actual) {
        equals(sort(unlist(list(...))))(sort(actual))
    }
}

has_no_match = function () {
    is_empty()
}

prints_as = function (display) {
    function (actual) {
        expect_that(actual, is_a('parser'))
        equals(display)(as.character(actual))
    }
}
