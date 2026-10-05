from src.zelkelc.lexer.tokenizer import lex
from src.zelkelc.ast.parser import Parser
from src.zelkelc.validator.validator import Validator
from src.zelkelc.codegen.codegen import Codegen

import rich

source = """
struct MyStruct {
    val test: i64
}

//test
class MyClass {
    val immutable_value: i64
    var mutable_value: i64
    
    fn function(test: i64, test2: i64) -> i64 {
        return 12 * "sd"
    }
}

fn main() -> i64 {
    val test: i64 = 16
    var test2: i64 = test
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