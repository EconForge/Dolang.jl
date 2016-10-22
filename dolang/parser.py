import typing
import ast


def parse_string(s: str)->ast.Expr:
    return ast.parse(s).body[0]
