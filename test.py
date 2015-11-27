exec(open('lib.py').read())

results = {
    'p1' : +1,
    'p10': +10,
    'm1': -1,
    'm10': -10
}
#
# def test_parse_subcript():

    #
    # for k in results:
    #     res = parse_time_subscript(k)
    #     expected = results[k]
    # #     print(res)
    # #     print(expected)
    # #     assert(res == expected)
    #
    #
    # for k in results:
    #     res = write_time_subscript(results[k])
    #     print(res)
    #     print(k)
    #     assert(res == k)

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
