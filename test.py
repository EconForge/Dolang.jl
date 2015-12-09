# exec(open('lib.py').read())
from sympy import Symbol

results = {
    'p1' : +1,
    'p10': +10,
    'm1': -1,
    'm10': -10
}


import ast
import yaml
txt = yaml.load(open("tests.yaml"))
summary = []
for ex in txt['expressions']:
    try:
        ast.parse(ex)
        result = "OK"
    except:
        result = "Failed"
    summary.append([ex, result])


for l in summary:
    print("{} : {}".format(*l))
