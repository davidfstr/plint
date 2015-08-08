#!/usr/bin/env python3

import ast
from collections import OrderedDict
import json
import sys


def main(args):
    (filename, ) = args
    
    with open(filename, 'rb') as f:
        source_bytes = f.read()
    
    source_ast = compile(
        source_bytes, filename, 'exec', ast.PyCF_ONLY_AST, dont_inherit=True)
    
    print(json.dumps(format_ast_as_json(source_ast)))


def format_ast_as_json(an_ast):
    if isinstance(an_ast, list):
        return [format_ast_as_json(x) for x in an_ast]
    if isinstance(an_ast, str):
        return an_ast
    if an_ast is None:
        return an_ast
    
    if not isinstance(an_ast, ast.AST):
        raise ValueError('Unrecognized value type in ast: %s' % repr(an_ast))
    
    fields = OrderedDict([
        (field_name, format_ast_as_json(getattr(an_ast, field_name)))
        for field_name in an_ast._fields
    ])
    
    if isinstance(an_ast, ast.stmt) or isinstance(an_ast, ast.expr):
        attributes = OrderedDict([
            ('lineno', an_ast.lineno),
            ('col_offset', an_ast.lineno),
        ])
        return [type(an_ast).__name__, fields, attributes]
    else:
        return [type(an_ast).__name__, fields]


if __name__ == '__main__':
    main(sys.argv[1:])
