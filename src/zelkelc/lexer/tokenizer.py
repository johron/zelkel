import src.zelkelc.lexer.token as token

def lex(source: str) -> list[token.Token]:
    tokens: list[token.Token] = []
    
    i = 0
    while i < len(source):
        c = source[i]
        if c == '/' and source[i + 1] == '/':
            i += 2
            for _, comment_c in enumerate(source[i:]):
                i += 1                
                if comment_c == '\n':
                    break
        elif c.isnumeric():
            i += 1
            
            integer = int(c)
            for _, integer_c in enumerate(source[i:]):
                if not integer_c.isnumeric():
                    break
                i += 1
                integer += int(integer_c)
            tokens.append(token.Integer(integer))
        elif c.isalpha() or c == '_':
            i += 1
            
            identifier = c
            for _, identifier_c in enumerate(source[i:]):
                if not (identifier_c.isalnum() or identifier_c == '_'):
                    break
                i += 1
                identifier += identifier_c
            tokens.append(token.Identifier(identifier))
        elif c == '"':
            i += 1
            
            string = ""
            for _, string_c in enumerate(source[i:]):
                if string_c == '"':
                    break
                i += 1
                string += string_c
            if i > len(source) or source[i] != '"':
                print("No '\"' to close string")
                break
            i += 1
            tokens.append(token.String(string))
        elif c == '-' and source[i + 1] == '>':
            tokens.append(token.Arrow())
            i += 2
        elif c == '<' and source[i + 1] == '=':
            tokens.append(token.Operator("<="))
            i += 2
        elif c == '>' and source[i + 1] == '=':
            tokens.append(token.Operator(">="))
            i += 2
        elif c == '|' and source[i + 1] == '|':
            tokens.append(token.Operator("||"))
            i += 2
        elif c ==  '&' and source[i + 1] == '&':
            tokens.append(token.Operator("&&"))
            i += 2
        elif c == '=' and source[i + 1] == '=':
            tokens.append(token.Operator("=="))
            i += 2
        elif c == '!' and source[i + 1] == '=':
            tokens.append(token.Operator("!="))
            i += 2
        else:
            match c:
                case ' ' | '\n':
                    i += 1
                case '(':
                    tokens.append(token.LParen())
                    i += 1
                case ')':
                    tokens.append(token.RParen())
                    i += 1
                case '{':
                    tokens.append(token.LBrace())
                    i += 1
                case '}':
                    tokens.append(token.RBrace())
                    i += 1
                case '.':
                    tokens.append(token.Period())
                    i += 1
                case ',':
                    tokens.append(token.Comma())
                    i += 1
                case ':':
                    tokens.append(token.Colon())
                    i += 1
                case '=':
                    tokens.append(token.Equals())
                    i += 1
                case '+':
                    tokens.append(token.Operator('+'))
                    i += 1
                case '-':
                    tokens.append(token.Operator('-'))
                    i += 1
                case '*':
                    tokens.append(token.Operator('*'))
                    i += 1
                case '/':
                    tokens.append(token.Operator('/'))
                    i += 1
                case '%':
                    tokens.append(token.Operator('%'))
                case _:
                    print(f"Unknown character found during lexing: '{c}' at {i}")
                    break
    
    return tokens
