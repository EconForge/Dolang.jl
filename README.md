**What is dolang ?**

It is the current codename for a complete rewrite of the dolo language. It aims at:

- being a first stone for the rewrite of dolo in julia
- improving model validity checks and providing not so cryptic error messages (for dolo 0.5)
- allow for easy language extensions
- being reusable in conjunction with other libraries (like pandas, or bttt)

**What does it consist in ?**

- a standardized way of describing equations with different types of variables as well as methods to operate on these equations
- `compilation` routines to turn these equations into vectorized/optimized functions
- rules to define arbitrary objects with a yaml file and a collection of useful objects (distributions, markov chains, approximation spaces, priors, ...)


## Equation description (codename dolex)

- unicode fomulas:  use crazy hebrew letters !

- latex output (and input ?)

- time subscripts (default to be determined):
    - implicit time: `a(1)`, `a(-1)` for `a_{t}` and `a_{t-1}`
    - explicit time: `a(t+1)`, `a(t-1)` for `a_{t}` and `a_{t-1}`    
    - continuous time:  `a(t)`, `a(t+dt)` for `a_{t}` and `a_{t+dt}`

- other subscripts:
    - subscripted variables `a[i,j](t)` for `a_{i,j,t}``
    - superscripts  `a[~i,j](t)` for `a^i_{j,t}``

- expressions defined on an event tree (see bttt)
    - `v[s-1] + v[s] + E[ s[1] | s[1], S(s) ]`

- common constructs:
    - expectations: `E[...]` or  `E_t[ ... ]`
    - differential operators: ??

- basic symbolic operations on expressions:
    - count/check incidence of symbols
    - substitutions
    - differentiation

How does it work (for now)?

    - expressions are parsed by python/julia parser
    - pattern matching is used to substitute symbols by a unique string identifier:

        - a[x,y] -> a__x__y
        - a(t,t+dt,t-dt) -> a__t__tpdt__tmdt
        - a(1, -1) -> a____p1__m1

    - symbolic operations can then be done by manipulating the expression trees directly or by using a symbolic engine

## Objects in Yaml  (codename dolaml)

For instance

```
FunctionalSpace:
    domain:
        SmolyakGrid:
            a: [0,0]
            b: [1,1]
            m: 4
    interp_type: polynomial
```

gets translated to Julia/Python object:

```
FunctionalSpace(
     domain=SmolyakGrid(
          a=[0,0],
          b=[1,1],
          l=4
     ),
     interp_type='polynomial'
)
```

## Compilation

Example:

```
eqs = [
    'a*x + y(1)',
    'x(1) - b*y',
]
from collections import OrderedDict
symbols = OrderedDict(
    states=['x', 'y'],
    parameters=['a', 'b']
)
api = [
    ('states',0,'s'),        # represents all variables v(0) where v is a state
    ('states',1,'s'),        # represents all variables v(1) where v is a state
    ('parameters',0,'p')
]

f = compile_function(eqs, symbols, api)

# returns a function f(s_0, s_1, p)
```
