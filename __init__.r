decorate = modules::import('decorator/decorate', TRUE)

# Prototype of a parser:
# parser = function (input: character(1), positions: integer(n))

parser = function (display, extra_class = NULL)
    decorator %@% function (f) {
        formals(f)$positions = 1L
        class(f) = c(extra_class, 'parser')
        attr(f, 'display') = display
        f
    }

# TODO: Use this to implement custom parsing actions.
`[.parser` = function (parser, action) {
    attr(parser, 'action') = substitute(action)
    parser
}

escape_char = function (char)
    gsub('\n', '\\\\n',
    gsub('"', '\\\\"',
    gsub('\\', '\\\\', char, fixed = TRUE)))

as.character.parser = function (x, ...)
    attr(x, 'display')

print.parser = function (x, ...) {
    cat(as.character(x))
    cat('\n')
    invisible(x)
}

xapply = function (X, FUN, ...)
    unique(unlist(lapply(X, FUN, ...)))

chr = function (char) {
    parser(paste0('"', escape_char(char), '"')) %@%
    function (input, positions) {
        chr_ = function (pos)
            if (substr(input, pos, pos) == char)
                pos + 1L

        xapply(positions, chr_)
    }
}

lit = function (str) {
    parser(paste0('"', escape_char(str), '"')) %@%
    function (input, positions) {
        lit_ = function (pos)
            if (substr(input, pos, pos + nchar(str) - 1) == str)
                pos + nchar(str)

        xapply(positions, lit_)
    }
}

any_of = function (chars) {
    chars = unlist(strsplit(chars, ''))

    parser(paste0('["', escape_char(paste(chars, collapse = '')), '"]')) %@%
    function (input, positions) {
        any_of_ = function (pos)
            if (substr(input, pos, pos) %in% chars)
                pos + 1L

        xapply(positions, any_of_)
    }
}

any_not_of = function (chars) {
    chars = unlist(strsplit(chars, ''))

    parser(paste0('[^"', escape_char(paste(chars, collapse = '')), '"]')) %@%
    function (input, positions) {
        any_not_of_ = function (pos)
            if (! substr(input, pos, pos) %in% chars)
                pos + 1L

        xapply(positions, any_not_of_)
    }
}

start = parser('^') %@% function (input, positions)
    positions[positions == 1L]

end = parser('$') %@% function (input, positions)
    positions[positions == nchar(input) + 1L]

empty = parser('ε') %@% function (input, positions)
    positions

flatten_parser = function (parser, class) {
    if (inherits(parser, class))
        environment(parser)$parsers
    else
        parser
}

seq = function (...) {
    parsers = unlist(lapply(list(...), flatten_parser, class = 'seq'))
    display = paste0('(', paste(sapply(parsers, as.character), collapse = ''), ')')

    parser(display, 'seq') %@%
    function (input, positions) {
        seq_ = function (pos) {
            for (parser in parsers)
                pos = parser(input, pos)
            pos
        }
        xapply(positions, seq_)
    }
}

or = function (...) {
    parsers = unlist(lapply(list(...), flatten_parser, class = 'or'))
    display = paste0('(', paste(sapply(parsers, as.character), collapse = '|'), ')')

    parser(display, 'or') %@%
    function (input, positions) {
        or_ = function (pos)
            xapply(parsers, function (p) p(input, pos))

        xapply(positions, or_)
    }
}

# > seq(or(empty, a, aa), or(empty, a, aa))('aa', 1)
# [1] 1 2 3
# > seq(many(a), many(a))('aa', 1)
# [1] 1 2 3

many = function (p) {
    parser(paste0(as.character(p), '*')) %@%
    function (input, positions) {
        result = p(input, positions)
        if (identical(result, positions))
            positions
        else
            c(positions, Recall(input, result))
    }
}

# TODO: Add infix syntax for `many`.

# ((a | aa) + !a)('aab', 1L)        => {2, 3} => {3}
# ((a | aa) + !a)('aaa', 1L)        => {2, 3} => {}
# ((a | aa) + !b)('aab', 1L)        => {2, 3} => {2}
# ((a | aa) + !c)('aab', 1L)        => {2, 3} => {2, 3}
# ((a | aa) + (!a) + b)('aab', 1L)  => {2, 3} => {3}    => {4}

not = function (p) {
    parser(paste0('!', as.character(p))) %@%
    function (input, positions) {
        is_empty = function (x) length(x) == 0
        match_pos = lapply(positions, p, input = input)
        positions[xapply(match_pos, is_empty)]
    }
}

`&.parser` = function (p1, p2)
    seq(p1, p2)

`|.parser` = function (p1, p2)
    or(p1, p2)

`!.parser` = function (x)
    not(x)

if (is.null(modules::module_name()))
    modules::import('./_tests')
