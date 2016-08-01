
def test_parse_string():
    from dolang.parser import parse_string
    e = parse_string('sin(a(1)+b+f(1)+f(+4)+a(t+1))')
    s = to_source(e)
    assert( s == "sin(a(1) + b + f(1) + f(+(4)) + a(t + 1))" )

def test_list_symbols_debug():
    e = parse_string('sin(a(1)+b+f(1)+f(+4)+a(t+1))')
    l = ListSymbols(known_functions=['f'], known_variables=['a'])
    l.visit(e)
    assert(l.variables==[(('a', 1), 4)])
    assert(l.constants==[('b', 9), ('t', 24)])
    assert(l.functions==[('f', 11), ('f', 16)])
    assert(l.problems==[['sin', 0, 0, 'unknown_function'], ['a', 0, 22, 'incorrect subscript']])

def test_list_variables():
    e = parse_string('sin(a(1)+b+f(1)+f(+4)+a(t+1))')
    list_variables(e,vars=['a','f'])
    assert(list_variables(e) == [('a', 1), ('f', 1), ('f', 4)])
    assert(list_variables(e, funs=['f']) == [('a', 1)])

def test_normalize():
    e = parse_string('sin(a(1)+b+a+f(-1)+f(+4)+a(1))')
    to_source(e)
    enes = normalize(e, variables=['a','f'])
    print(to_source(enes))
    assert( to_source(enes) == "sin(a__1_ + b + a__ + f__m1_ + f__4_ + a__1_)" )


ast.dump(enes)
ast.dump(e)

to_source(enes)



from dolang.codegen import to_source
# to_source(e)
e = parse_string("a(1) + b")

e2 = parse_string("a(1) + b")

to_source(e2)
to_source(e)
print(ast.dump(e))
to_source(e)
to_source(time_shift(e, -2, vars=['a']))
to_source(steady_state(e, vars='a'))
