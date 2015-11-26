Features:

- unicode fomulas:  use crazy hebrew letters !
- optional latex writing (for very lazy ones)
- several subscripting schemes:
    - time subscript: `a(1)`, `a(-1)` for `a_{t}` and `a_{t-1}`
    - continuous time:  `a(t)`, `a(t+dt)` for `a_{t}` and `a_{t+dt}`
- use () or [] as you prefer:
    - a[t] or a(t)
- index symbols
    - a[t, i], a[t+dt, i]


- each symbol is associated to a unique valid identifier
        - a[x,y] -> a__x__y
        - a[t,t+dt,t-dt] -> a__t__tpdt__tmdt
        - a[, 1, -1] -> a____p1__m1
