import ast
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
    
    return [type(an_ast).__name__, {
        field_name: format_ast_as_json(getattr(an_ast, field_name))
        for field_name in an_ast._fields
    }]


if __name__ == '__main__':
    main(sys.argv[1:])