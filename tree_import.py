import yaml

def import_tree_model(filename, key):

    with open(filename) as f:
        d = yaml.load(f)

    lines = []
    for k in d[key]:
        if isinstance(k,str):
            lines.append(k)
        else:
            cond = next(iter(k.keys()))
            val = k[cond][0]
            s = cond + ' : ' + val
            lines.append(s)

    return lines
