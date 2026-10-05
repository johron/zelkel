from src.zelkelc.lexer.tokenizer import lex
from src.zelkelc.ast.parser import Parser
from src.zelkelc.validator.validator import Validator
from src.zelkelc.codegen.codegen import Codegen

import rich

source = """
fn main() -> i64 {
    val test: i64 = 10 + (5 * 5)
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