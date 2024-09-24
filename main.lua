local function error(file, line, msg)
    print(string.format("%s:%s: %s", file, line, msg))
    os.exit(1)
end

local function is_in_table(v, t)
    for _, j in ipairs(t) do
        if j == v then
            return true
        end
    end

    return false
end

local function lex(input, file)
    local toks = {}
    local line = 1

    local function is_alpha(c)
        return c:match("[a-zA-Z_]")
    end

    local function is_digit(c)
        return c:match("%d")
    end

    local chars = {}
    for i = 1, #input do
        table.insert(chars, input:sub(i, i))
    end

    local i = 1
    while i <= #chars do
        local c = chars[i]
        print("<" .. c .. ">")

        if is_alpha(c) then
            local v = ""
            while i <= #chars and (is_alpha(chars[i]) or is_digit(chars[i])) do
                v = v .. chars[i]
                i = i + 1
            end
            table.insert(toks, {type = "identifier", value = v})
        elseif is_digit(c) then
            local v = ""
            while i <= #chars and is_digit(chars[i]) do
                v = v .. chars[i]
                i = i + 1
            end
            table.insert(toks, {type = "integer", value = tonumber(v)})
        elseif is_in_table(c, {"+", "-", "*", "/", "="}) then
            table.insert(toks, {type = "operator", value = c})
            i = i + 1
        elseif is_in_table(c, {";", ":"}) then
            table.insert(toks, {type = "punctuation", value = c})
            i = i + 1
        elseif is_in_table(c, {"(", ")", "{", "}"}) then
            table.insert(toks, {type = "parenthesis", value = c})
            i = i + 1
        elseif c == "\n" then
            table.insert(toks, {type = "newline", value = "\n"})
            line = line + 1
            i = i + 1
        elseif c == " " then -- disregard
            i = i + 1
        else
            error(file, line, string.format("Unexpected character found by lexer: '%s'", c))
        end
    end

    return toks
end

local function parse(toks, file)
    local line = 1
    local ast = {}
    local i = 1

    local function current()
        return toks[i]
    end

    local function expect(type, value)
        local t = current()
        if t.type == "newline" then
            i = i + 1
            line = line + 1
            return expect(type, value)
        end

        if value ~= nil and t.value ~= value then
            error(file, line, string.format("Expected '%s' with value '%s', but found '%s' with value '%s'", type, value, t.type, t.value))
        elseif t.type ~= type then
            error(file, line, string.format("Expected '%s', but found '%s' with value '%s'", type, t.type, t.value))
        end

        i = i + 1
        return t
    end

    local function parse_expression()
        -- check for variable reference, function call with non void return value, just a value no operators,
        -- check for mathematical expressions that can include all this aswell

        local function parse_primary()
            local t = current()
            if t.type == "identifier" then
                i = i + 1
                return {type = "variable", name = t.value}
            elseif t.type == "integer" then
                i = i + 1
                return {type = "integer", value = t.value}
            elseif t.type == "parenthesis" and t.value == "(" then
                i = i + 1
                local expr = parse_expression()
                expect("parenthesis", ")")
                return expr
            else
                error(file, line, string.format("Unexpected token in primary expression: '%s'", t.value))
            end
        end

        local function parse_unary()
            local t = current()
            if t.type == "operator" and (t.value == "-" or t.value == "+") then
                i = i + 1
                local expr = parse_unary()
                return {type = "unary_expression", operator = t.value, operand = expr}
            else
                return parse_primary()
            end
        end

        local function parse_term()
            local expr = parse_unary()
            while i <= #toks and current().type == "operator" and (current().value == "*" or current().value == "/") do
                local op = current().value
                i = i + 1
                local right = parse_unary()
                expr = {type = "binary_expression", operator = op, left = expr, right = right}
            end
            return expr
        end

        local expr = parse_term()
        while i <= #toks and current().type == "operator" and (current().value == "+" or current().value == "-") do
            local op = current().value
            i = i + 1
            local right = parse_term()
            expr = {type = "binary_expression", operator = op, left = expr, right = right}
        end
        return expr
    end

    local function parse_function_declaration_args()
        local args = {}

        local start = i
        while i <= #toks and not toks[i].type == "parenthesis" do
            if i ~= start then
                expect("punctuation", ",")
            elseif toks[i] and toks[i] ~= "identifier" then
                return {}
            end

            local name = expect("identifier")
            expect("punctuation", ":")
            local type = expect("identifier")

            table.insert(args, {name, type})
        end

        return args
    end

    local function parse_function_call_args()
        error("TODO: implement parse_function_call_args")
    end

    local function parse_function_call()
        local name = expect("identifier").value
        expect("parenthesis", "(")

        local args = parse_function_call_args()

        expect("parenthesis", ")")
        expect("punctuation", ";")

        return {
            type = "function_call",
            name = name,
            args = args
        }
    end

    local function parse_variable_assignment(mutable)
        local str = ""
        if mutable == true then
            expect("identifier", "let")
            str = "mutable"
        else
            expect("identifier", "const")
            str = "immutable"
        end

        local name = expect("identifier").value
        expect("operator", "=")

        local expr = parse_expression()
        expect("punctuation", ";")

        return {
            type = str .. "_variable_assignment",
            name = name,
            value = expr
        }
    end

    local function parse_body()
        local body = {}
        while i <= #toks and current().type ~= "parenthesis" and current().value ~= "}" do
            table.insert(body, parse_statement())
            i = i + 1
        end

        return body
    end

    local function parse_function_declaration()
        expect("identifier", "fn")
        local name = expect("identifier").value
        expect("parenthesis", "(")

        local args = parse_function_declaration_args()

        expect("parenthesis", ")")
        expect("punctuation", ":")
        local type = expect("identifier").value
        expect("parenthesis", "{")

        if not is_in_table(type, {"void", "int"}) then
            error(file, line, string.format("Unrecognized return type: '%s'", type))
        end

        local body = parse_body()
        expect("parenthesis", "}")
        expect("punctuation", ";")

        return {
            type = "function_declaration",
            name = name,
            args = args,
            returns = type,
            body = body
        }
    end

    function parse_statement()
        local type = current().type
        local value = current().value

        if type == "identifier" then
            if value == "fn" then
                return parse_function_declaration()
            elseif value == "let" then
                return parse_variable_assignment(true)
            elseif value == "const" then
                return parse_variable_assignment(false)
            -- elseif is variable reference
            -- elseif is function call
            else
                error(file, line, string.format("Unexpected token found while parsing statement: '%s'", value))
            end
        elseif type == "newline" then
            line = line + 1
            i = i + 1
            return parse_statement()
        else
            error(file, line, string.format("Unexpected token found while parsing statement: '%s'", value))
        end
    end

    while i <= #toks do
        local node = parse_statement()
        table.insert(ast, node)
        i = i + 1
    end

    return ast
end

local file_name = "test.zk"
local file = io.open(file_name, "rb")
if not file then print("No 'test.zk' file") os.exit(1)  end
local content = file:read("a")
file:close()

local toks = lex(content, file_name)
for _, tok in ipairs(toks) do
    print(string.format("{type = '%s', value = '%s'}", tok.type, tok.value))
end

local ast = parse(toks, file_name)

local inspect = require("inspect")
print(inspect(ast))