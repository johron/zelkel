from src.zelkelc.lexer.tokenizer import lex
from src.zelkelc.ast.parser import Parser

source = """
//test
class MyClass {
    val immutable_value: String = "sdf"
    var mutable_value: i64 = 128
    
    fn function(test: i64, test2: String) -> void {
        return
    }
}

static fn main() -> i64 {
    return 0 + 1 + 2
}
"""

if __name__ == "__main__":
    tokens = lex(source)
    print(tokens)
    parser = Parser(tokens, 0)
    ast = parser.parse()
    print(ast)