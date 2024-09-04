module [parse]

Token a : [Pipe, Dot, Star, ParenOpen, ParenClose, Lit a]

lex : List U8, List (Token U8) -> List (Token U8)
lex = \text, acc ->
    when text is
        [] -> acc
        ['.', .. as rest] -> lex rest (List.append acc Dot)
        ['|', .. as rest] -> lex rest (List.append acc Pipe)
        ['*', .. as rest] -> lex rest (List.append acc Star)
        ['(', .. as rest] -> lex rest (List.append acc ParenOpen)
        [')', .. as rest] -> lex rest (List.append acc ParenClose)
        [c, .. as rest] -> lex rest (List.append acc (Lit c))

parse = \text ->
    tokens = lex (Str.toUtf8 text) []
    p tokens [[]]

p = \tokens, stack ->
    when (tokens, stack) is
        ([], [top]) -> Ok top
        ([], [_, _, ..]) -> Err UnclosedParens
        # ([Star, .. as rest], [.. as restStack, [.., lastPattern] as top]) ->
        #     topItered = setLast top (Iter lastPattern)
        #     newStack = setLast stack topItered
        #     p rest newStack
            
        ([Lit l, .. as rest], [.., top]) ->
            newTop = List.append top (Char l)
            newStack = setLast stack newTop
            p rest newStack

        ([ParenOpen, .. as rest], _) ->
            newStack = List.append stack []
            p rest newStack

        ([ParenClose, .. as rest], [.. as restStack, nextTop, top]) ->
            newStack =
                newTop = List.append nextTop (Group top)
                poppedStack = List.dropLast stack 1
                setLast poppedStack newTop
            p rest newStack
        ([ParenClose, .. as rest], [.. , _ ]) ->
            Err UnexpectedClosingParen

        _ -> crash "Only parens"

setLast = \l, a ->
    List.set l (List.len l - 1) a

expect
    inp = "(a|bc).*" |> Str.toUtf8
    tested = lex inp [] 
    expected = [ParenOpen, Lit 'a', Pipe, Lit 'b', Lit 'c', ParenClose, Dot, Star]
    tested == expected

expect
    inp = "(())"
    tested = parse inp
    expected = Ok [Group [Group []]]
    tested == expected

expect
    inp = "a(b(c)d)e"
    tested = parse inp
    expected = Ok [Char 'a', Group [Char 'b', Group [Char 'c'], Char 'd'], Char 'e']
    tested == expected

expect
    inp = "()a(b)"
    tested = parse inp
    expected = Ok [Group [], Char 'a', Group [Char 'b']]
    tested == expected

expect
    inp = "ab(cd"
    tested = parse inp
    expected = Err UnclosedParens
    tested == expected

expect
    inp = "a(b)c)"
    tested = parse inp
    expected = Err UnexpectedClosingParen 
    tested == expected
