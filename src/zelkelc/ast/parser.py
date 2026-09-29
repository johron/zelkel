import src.zelkelc.lexer.token as token
import src.zelkelc.ast.nodes as nodes
import src.zelkelc.ast.values as values

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
    
    def parse_type(self) -> values.Value:
        pass    

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
        
        members, methods = self.parse_class_declaration_body()
        
        self.expect(token.RBrace())
        
        return nodes.ClassDeclaration(
            name,
            members,
            methods,
        )
    
    def parse_class_declaration_body(self) -> tuple[list[nodes.ValueDeclaration], list[nodes.FunctionDeclaration]]:
        members: list[nodes.ValueDeclaration] = []
        methods: list[nodes.FunctionDeclaration] = []
        
        while self.cursor < len(self.tokens):
            t = self.tokens[self.cursor]
            
            match t:
                case token.Identifier():
                    match t.value:
                        case "fn":
                            methods.append(self.parse_function_declaration())
                        case "val":
                            members.append(self.parse_value_declaration(False))
                        case "var":
                            members.append(self.parse_value_declaration(True))
                            
        return members, methods
    
    def parse_function_declaration(self) -> nodes.FunctionDeclaration:
        self.expect(token.Identifier("fn"))
        name = self.expect_type(token.Identifier)
        self.expect(token.LParen())
        
        # parse declaration arguments
        
        self.expect(token.RParen())
        
        self.expect(token.Arrow())
        
        typ = self.parse_type()
        
        self.expect(token.LBrace())
        
        # parse function declaration body
        
        self.expect(token.RBrace())
        
        return nodes.FunctionDeclaration(
            name,
            typ,
            args = [],
            body = []
        )
        
    def parse_function_declaration_body(self) -> list[nodes.Node]:
        pass
    
    def parse_value_declaration(self, mutable: bool) -> nodes.ValueDeclaration:
        if mutable:
            self.expect(token.Identifier("var"))
        else:
            self.expect(token.Identifier("val"))
        
        name = self.expect_type(token.Identifier)
        
        self.expect(token.Colon())
        typ = self.parse_type()
        
        self.expect(token.Equals)
        
        # TODO: parse value declaration expression
        expr = None
        
        return nodes.ValueDeclaration(
            name,
            mutable,
            typ,
            expr,
        )