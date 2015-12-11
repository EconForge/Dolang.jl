import yaml
import ast

ll = yaml.load(open('example.yaml'))

print(ll)


for eqss in ll['equations']:
    eq = eqss.replace(' = ', '==')
    try:
        ast.parse(eq)
        success = True
    except Exception as e:
        success = False
        print(e)
    print(eqss +  ' : ' + str(success))
