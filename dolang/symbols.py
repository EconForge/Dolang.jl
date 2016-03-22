def write_time_subscript(s):

    neg = 'm'
    if isinstance(s, int):
        if s>=0:
            return str(s)
        elif s<0:
            return neg + str(-s)
    else:
        raise Exception("Don't know what to do with that")

class Symbol:

    def __init__(self, name=None, subscripts=None, date=None):
        if date is None:
            date = 0
        if subscripts is None:
            subscripts = []
        assert(isinstance(date,int))
        for s in subscripts:
            assert(isinstance(s,str))
            assert('__' not in s)
        assert(name.isidentifier())
        self.subscripts = tuple( subscripts )
        self.date = date
        self.name = name

    def __str__(self):
        # sep = 'ⵂ'
        # close_lhs = 'ⵂ'
        # close_rhs = 'ⵂ'
        sep = '__'
        close_lhs = '__'
        close_rhs = ''
        # close_lhs = '᚜'
        # close_rhs = '᚛'

        if self.date:
            argss = str.join(sep, [write_time_subscript(self.date)] + list(self.subscripts))
        else:
            argss =  str.join(sep, list(self.subscripts))
        s = self.name + close_lhs + argss + close_rhs
        return s
