import ast
import sys


def main(args):
    (filename, ) = args
    
    with open(filename, 'rb') as f:
        source_bytes = f.read()
    
    source_ast = compile(
        source_bytes, filename, 'exec', ast.PyCF_ONLY_AST, dont_inherit=True)
    
    print(ast.dump(source_ast))


if __name__ == '__main__':
    main(sys.argv[1:])