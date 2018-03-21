import ast
from dolang.codegen import to_source
from dolang.symbolic import stringify, list_variables, list_symbols, ListSymbols


def test_parse_string():
    from dolang.symbolic import parse_string
    e = parse_string('sin(a(1)+b+f(1)+f(+4)+a(t+1))')
    assert isinstance(e, ast.Expr)
    s = to_source(e)
    assert (s == "sin(a(1) + b + f(1) + f(+(4)) + a(t + 1))")


def test_list_symbols_debug():
    from dolang.symbolic import parse_string
    e = parse_string('sin(a(1)+b+f(1)+f(+4)+a(1)+a+a(t+1))+cos(0)')
    l = ListSymbols(known_functions=['sin', 'f'])
    l.visit(e)
    # note that cos is recognized as variable
    assert (l.variables == [(('a', 1), 4), (('a', 1), 22), (('cos', 0), 37)])
    assert (l.constants == [('b', 9), ('a', 27)])
    assert (l.functions == [('sin', 0), ('f', 11), ('f', 16)])
    assert (l.problems == [['a', 0, 29, 'incorrect subscript']])


def test_list_symbols():
    from dolang.symbolic import parse_string
    e = parse_string('sin(a(1)+b+f(1)+f(+4)+a(1))+cos(0)')
    ll = list_symbols(e)
    # cos is recognized as a usual function
    assert (ll.variables == [('a', 1), ('f', 1), ('f', 4)])
    assert (ll.parameters == ['b'])

    e = parse_string('sin(a(1)+b+f(1)+f(+4)+a(1))+cos(0)')
    ll = list_symbols(e, funs=['f'])
    # now we add a custom function
    assert (ll.variables == [('a', 1)])
    assert (ll.parameters == ['b'])


def test_list_variables():
    from dolang.symbolic import parse_string
    e = parse_string('sin(a(1)+b+f(1)+f(+4)+sin(a)+k*cos(a(0)))')
    list_variables(e)
    assert (list_variables(e) == [('a', 1), ('f', 1), ('f', 4), ('a', 0)])
    assert (list_variables(e, funs=['f']) == [('a', 1), ('a', 0)])


def test_sanitize():

    from dolang.symbolic import sanitize, parse_string
    from dolang.codegen import to_source

    s = 'sin(a(1)+b+a+f(-1)+f(+4)+a(1))'
    expected = "sin(a(1) + b + a(0) + f(-(1)) + f(4) + a(1))"
    e = parse_string('sin(a(1)+b+a+f(-1)+f(+4)+a(1))')
    enes = sanitize(e, variables=['a', 'f'])

    assert (to_source(enes) == expected)

    # it also works with the string directly
    assert (sanitize(s, variables=['a', 'f']) == expected)

    # we also deal with = signs, and convert to python exponents
    assert (sanitize("a(1) = a^3 + b") == "a(1) == (a) ** (3) + b")


def test_stringify():

    from dolang.symbolic import parse_string
    e = parse_string('sin(a(1) + b + a(0) + f(-(1)) + f(4) + a(1))')
    to_source(e)
    enes = stringify(e, variables=['a', 'f'])
    print(to_source(enes))
    assert (
        to_source(enes) == "sin(a__1_ + b_ + a__0_ + f_m1_ + f__4_ + a__1_)")


def test_time_shift():

    from dolang.symbolic import parse_string
    e = parse_string('sin(a(1) + b + a(0) + f(-(1)) + f(4) + a(1))')
    to_source(e)
    enes = stringify(e, variables=['a', 'f'])
    print(to_source(enes))
    assert (
        to_source(enes) == "sin(a__1_ + b_ + a__0_ + f_m1_ + f__4_ + a__1_)")
