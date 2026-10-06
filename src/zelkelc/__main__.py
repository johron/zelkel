from src.zelkelc.lexer.tokenizer import lex
from src.zelkelc.ast.parser import Parser
from src.zelkelc.validator.validator import Validator
from src.zelkelc.codegen.codegen import Codegen

import rich

source = """
class String {
    value: str
}

fn main() -> i64 {
    val a: i64 = 1
    val test: i64 = 74 + 123 * (2+3 * 4)
    return 0
}
"""

if __name__ == "__main__":
    tokens = lex(source)
    print(tokens)
    parser = Parser(tokens, 0)
    ast = parser.parse()
    rich.print(ast)
    
    Validator(ast)
    
    codegen = Codegen(ast)
    ir = codegen.generate()
    print(ir)