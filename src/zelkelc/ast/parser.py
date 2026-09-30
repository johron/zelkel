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
    
    def current(self, tok: token.Token): 
        if not self.tokens[self.cursor] == tok:
            return False
        return True

    def current_type(self, tok_type: token.Token) -> bool:
        if not type(self.tokens[self.cursor]) == tok_type:
            return False
        return True

    def parse_type(self, is_return_type: bool) -> values.Value:
        identifier = self.expect_type(token.Identifier)
        match identifier.value:
            case "String":
                return values.String
            case "i64":
                return values.Integer
            case "void" if is_return_type == True:
                return values.Void
            case _:
                print("Invalid type found in parse_type")
                exit(1)

    def parse(self):
        ast: list[nodes.FunctionDeclaration | nodes.ClassDeclaration] = []
        
        while self.cursor < len(self.tokens):
            t = self.tokens[self.cursor]
            
            match t:
                case token.Identifier():
                    match t.value:
                        case "static":
                            self.expect(token.Identifier("static"))
                            if self.current(token.Identifier("fn")):
                                ast.append(self.parse_function_declaration(True))
                            else:
                                print(f"Keyword {self.tokens[self.cursor]} does not exist or may not be static")
                                exit(1)
                        case "class":
                            ast.append(self.parse_class_declaration())
                        case _:
                            print(f"Invalid keyword {t.value}")
                            exit(1)
                case _:
                    print(f"Invalid token found while parsing global {t}")
                    exit(1)
                            
        
        return ast
    
    def parse_class_declaration(self) -> nodes.ClassDeclaration:
        self.expect(token.Identifier("class"))
        name = self.expect_type(token.Identifier).value
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
        
        while self.cursor < len(self.tokens) and not self.tokens[self.cursor] == token.RBrace():
            t = self.tokens[self.cursor]
            
            match t:
                case token.Identifier():
                    match t.value:
                        case "fn":
                            methods.append(self.parse_function_declaration(False))
                        case "val":
                            members.append(self.parse_value_declaration(False))
                        case "var":
                            members.append(self.parse_value_declaration(True))
                        case _:
                            print(f"Invalid keyword {t.value}")
                            exit(1)
                case _:
                    print(f"Invalid token found while parsing class declaration: {t}")
                    exit(1)
                            
        return members, methods
    
    def parse_function_declaration(self, static: bool) -> nodes.FunctionDeclaration:
        self.expect(token.Identifier("fn"))
        name = self.expect_type(token.Identifier).value
        self.expect(token.LParen())
        
        args: dict[str, values.Value] = {}
        while self.cursor < len(self.tokens) and not self.current(token.RParen()):
            name = self.expect_type(token.Identifier).value
            if name in args:
                print(f"Cannot define argument {name} twice")
                exit(1)
            
            self.expect(token.Colon())
            typ = self.parse_type(False)
            args[name] = typ
            
            if not self.current(token.Comma()):
                break
            else:
                self.expect(token.Comma())
        
        self.expect(token.RParen())
        
        self.expect(token.Arrow())
        typ = self.parse_type(True)
        
        self.expect(token.LBrace())
        body = self.parse_function_declaration_body()
        self.expect(token.RBrace())
        
        return nodes.FunctionDeclaration(
            name = name,
            typ = typ,
            static = static,
            args = args,
            body = body,
        )
        
    def parse_function_declaration_body(self) -> list[nodes.Node]:
        body: list[nodes.Node] = []
        hasReturn: bool = False # TODO: should be replaced with real return tree checking, this is not a good system
        
        while self.cursor < len(self.tokens) and not self.tokens[self.cursor] == token.RBrace():
            t = self.tokens[self.cursor]
            
            match t:
                case token.Identifier():
                    match t.value:
                        case "fn":
                            body.append(self.parse_function_declaration())
                        case "val":
                            body.append(self.parse_value_declaration(False))
                        case "var":
                            body.append(self.parse_value_declaration(True))
                        case "return":
                            body.append(self.parse_return())
                            hasReturn = True
                        case _:
                            print(f"Invalid keyword {t.value}")
                            exit(1)
                case _:
                    #TODO: expression statement
                    print(f"implement expression statement: {t}")
                    exit(1)
        
        if hasReturn == False:
            print("Function must have return")
            exit(1)
        
        return body
    
    def parse_return(self) -> nodes.ReturnStatement:
        self.expect(token.Identifier("return"))
        # if there is an expression then check for add it?
        
        expr = self.parse_expression()
        
        return nodes.ReturnStatement(
            expr
        )
    
    def parse_value_declaration(self, mutable: bool) -> nodes.ValueDeclaration:
        if mutable:
            self.expect(token.Identifier("var"))
        else:
            self.expect(token.Identifier("val"))
        
        name = self.expect_type(token.Identifier).value
        
        self.expect(token.Colon())
        typ = self.parse_type(False)
        
        self.expect(token.Equals())
        
        # TODO: parse value declaration expression
        expr = None
        self.cursor += 1
        
        return nodes.ValueDeclaration(
            name,
            mutable,
            typ,
            expr,
        )
    
    def parse_expression(self) -> nodes.Expression:
        if self.current_type(token.Operator) == True:
            self.parse_unary_expression()
        
        left = self.parse_primary_expression()
        if self.current_type(token.Operator) == True:
            self.cursor -= 1
            return self.parse_binary_expression()
        
        return left
    
    def parse_binary_expression(self) -> nodes.BinaryExpression:
        left = self.parse_primary_expression()
        op = self.expect_type(token.Operator).value
        right = self.parse_primary_expression()
        return nodes.BinaryExpression(
            left,
            op,
            right,
        )
    
    def parse_unary_expression(self) -> nodes.UnaryExpression:
        print("TODO: Implement parse_unary_expression")
        exit(1)
    
    def parse_primary_expression(self) -> nodes.PrimaryExpression:
        t = self.tokens[self.cursor]
        
        match t:
            case token.Identifier():
                self.cursor += 1
                return nodes.PrimaryExpression(
                    value = values.Variable(t.value)
                )
            case token.Integer():
                self.cursor += 1
                return nodes.PrimaryExpression(
                    value = values.Integer(t.value)
                )
            case token.String():
                self.cursor += 1
                return nodes.PrimaryExpression(
                    value = values.String(t.value)
                )
            case _:
                return None
            