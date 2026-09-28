import src.zelkelc.lexer.token as token
import src.zelkelc.ast.nodes as nodes

class Parser:
    tokens: list[token.Token]
    cursor: int
    
    def __init__(self, tokens: list[token.Token], cursor: int):
        self.tokens = tokens
        self.cursor = cursor
    
    def expect(self, tok: token.Token): 
        if not self.tokens[self.cursor] == tok:
            print(f"Expected {tok}, but found {self.tokens[self.cursor]}")
            exit(1)
        self.cursor += 1
            
    def expect_type(self, tok_type: token.Token) -> token.Token:
        if not type(self.tokens[self.cursor]) == tok_type:
            print(f"Expected {tok_type}, but found {type(self.tokens[self.cursor])}")
            exit(1)
        self.cursor += 1
        return self.tokens[self.cursor - 1]
                

    def parse(self):
        ast: list[nodes.Node] = []
        
        while self.cursor < len(self.tokens):
            t = self.tokens[self.cursor]
            
            match t:
                case token.Identifier():
                    match t.value:
                        case "class":
                            ast.append(self.parse_class_declaration())
                        case "static":
                            print("TODO: static keyword not implemented")
                            exit(1)
        
        return ast

    def parse_class_declaration(self) -> nodes.ClassDeclaration:
        self.expect(token.Identifier("class"))
        name = self.expect_type(token.Identifier)
        self.expect(token.LBrace())
        
        self.expect(token.RBrace())
        
        return nodes.ClassDeclaration(
            name,
            members = [],
            methods = [],
        )
        