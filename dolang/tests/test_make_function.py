from dolang.codegen import to_source
from dolang.function_compiler import make_method


eqs = ['a + b*c']
arguments = [[('a',1)],[('a',0),('b',0)]]
parameters = [('c',0)]

fun = make_method(eqs, arguments, parameters)


print(to_source(fun))
