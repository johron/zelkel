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
        if is_alpha(c) then
            local v = ""
            while i <= #chars and (is_alpha(chars[i]) or is_digit(chars[i])) do
                v = v .. chars[i]
                i = i + 1
            end
            table.insert(toks, {type = "identifier", value = v})
        elseif is_digit(c) then
            local found_dot = false
            local type = "integer"
            local v = ""
            while i <= #chars and (is_digit(chars[i]) or (chars[i] == "." and found_dot == false)) do
                if chars[i] == "." then
                    found_dot = true
                    type = "float"
                end
                v = v .. chars[i]
                i = i + 1
            end

            table.insert(toks, {type = type, value = tonumber(v)})
        elseif c == "\"" then
            local v = ""
            i = i + 1
            while i <= #chars do
                if chars[i] == "\"" then
                    i = i + 1
                    break
                end
                v = v .. chars[i]
                i = i + 1
            end
            table.insert(toks, {type = "string", value = v})
        elseif is_in_table(c, {"+", "-", "*", "/", "="}) then
            local cmt = ""
            if c == "/" and chars[i + 1] == "/" then
                while i <= #chars and chars[i] ~= "\n" do
                    i = i + 1
                end
                if chars[i] == "\n" then
                    line = line + 1
                end
            else
                table.insert(toks, {type = "operator", value = c})
            end
            i = i + 1
        elseif is_in_table(c, {";", ":", ","}) then
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

    local scope_stack = {}

    local function current_scope()
        return scope_stack[#scope_stack] or scope_stack[#scope_stack - 1] or {mut_vars = {}, imut_vars = {}, functions = {}}
    end

    local function enter_scope()
        local parent_scope = current_scope()
        local new_scope = {
            mut_vars = {table.unpack(parent_scope.mut_vars)},
            imut_vars = {table.unpack(parent_scope.imut_vars)},
            functions = {table.unpack(parent_scope.functions)}
        }
        table.insert(scope_stack, new_scope)
    end

    local function exit_scope()
        if #scope_stack == 0 then
            error(file, line, string.format("Scope out of bounds"))
        end
        table.remove(scope_stack)
    end

    local function has_variable_in_scope(name)
        for j = #scope_stack, 1, -1 do
            local scope = scope_stack[j]
            if is_in_table(name, scope.mut_vars) then
                return true, true
            elseif is_in_table(name, scope.imut_vars) then
                return true, false
            end
        end
        return false, false
    end

    local function add_variable_to_scope(name, mutable)
        local scope = current_scope()
        if mutable then
            table.insert(scope.mut_vars, name)
        else
            table.insert(scope.imut_vars, name)
        end
    end

    local function has_function_in_scope(name)
        for j = #scope_stack, 1, -1 do
            local scope = scope_stack[j]
            if is_in_table(name, scope.functions) then
                return true
            end
        end
        return false
    end

    local function add_function_to_scope(name)
        local scope = current_scope()
        table.insert(scope.functions, name)
    end

    local function current()
        if toks[i] then
            return toks[i]
        else
            error(file, line, "Tokens out of bounds")
        end
    end

    local function expect(type, value)
        local t = current()
        if t.type == "newline" then
            line = line + 1
            i = i + 1
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

    local function parse_function_call_args()
        if toks[i].type == "parenthesis" and toks[i].value == ")" then
            return {}
        end

        local args = {}
        local start = i
        while i <= #toks and current().type ~= "parenthesis" and current().value ~= ")" do
            if i ~= start then
                expect("punctuation", ",")
            end
            table.insert(args, parse_expression())
        end

        return args
    end

    local function parse_function_call()
        local name = expect("identifier").value
        expect("parenthesis", "(")

        local args = parse_function_call_args()

        expect("parenthesis", ")")

        return {
            type = "function_call",
            name = name,
            args = args
        }
    end

    local function parse_function_declaration_args()
        local args = {}
        local start = i
        while i <= #toks and toks[i].type ~= "parenthesis" and toks[i].value ~= ")" do
            if i ~= start then
                expect("punctuation", ",")
            elseif toks[i + 1].type == "parenthesis" and toks[i + 1].value == ")" then
                return {}
            end

            local name = expect("identifier").value
            expect("punctuation", ":")
            local type = expect("identifier").value

            add_variable_to_scope(name, false)
            table.insert(args, {name = name, type = type})
        end

        return args
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
        local has_var, is_mutable = has_variable_in_scope(name)
        if has_var then
            error(file, line, string.format("Variable already defined in current scope: '%s'", name))
        end

        expect("punctuation", ":")
        local value_type = expect("identifier").value
        expect("operator", "=")

        local expr = parse_expression()
        expect("punctuation", ";")

        add_variable_to_scope(name, mutable)
        return {
            type = str .. "_variable_assignment",
            name = name,
            value_type = value_type,
            value = expr
        }
    end

    local function parse_variable_reassignment()
        local name = expect("identifier").value
        local has_var, is_mutable = has_variable_in_scope(name)
        if not has_var or not is_mutable then
            error(file, line, string.format("Variable is not mutable: '%s'", name))
        end

        expect("operator", "=")

        local expr = parse_expression()
        expect("punctuation", ";")
        return {
            type = "mutable_variable_reassignment",
            name = name,
            value = expr
        }
    end

    local function parse_body()
        local body = {}
        while i <= #toks and (current().type ~= "parenthesis" or current().value ~= "}") do
            if current().type == "newline" and current().value == "\n" then
                line = line + 1
                i = i + 1
            end
            if current().type == "parenthesis" or current().value == "}" then
                return body
            end

            local stmt = parse_statement()
            if stmt ~= nil then
                table.insert(body, stmt)
            end
        end
        return body
    end

    local function parse_function_declaration()
        expect("identifier", "fn")
        local name = expect("identifier").value
        if has_function_in_scope(name) then
            error(file, line, string.format("Function already declared in current scope: '%s'", name))
        end

        add_function_to_scope(name)
        enter_scope()

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
        exit_scope()

        return {
            type = "function_declaration",
            name = name,
            args = args,
            return_type = type,
            body = body
        }
    end

    local function parse_function_return()
        expect("identifier", "return")

        local expr = {}
        if current().type ~= "punctuation" and current().value ~= ";" then
            expr = parse_expression()
        end

        expect("punctuation", ";")

        return {
            type = "function_return",
            value = expr
        }
    end

    function parse_expression()
        local function parse_primary()
            local t = current()
            if t.type == "identifier" then
                if toks[i + 1].type == "parenthesis" and toks[i + 1].value == "(" then
                    if not has_function_in_scope(t.value) then
                        error(file, line, string.format("Function not defined in current scope: '%s'", t.value))
                    end
                    return parse_function_call()
                else
                    local has_var, is_mutable = has_variable_in_scope(t.value)
                    if not has_var then
                        error(file, line, string.format("Variable not defined in current scope: '%s'", t.value))
                    end
                    i = i + 1
                    return {type = "variable", name = t.value}
                end
            elseif t.type == "integer" then
                i = i + 1
                return {type = "integer", value = t.value}
            elseif t.type == "float" then
                i = i + 1
                return {type = "float", value = t.value}
            elseif t.type == "string" then
                i = i + 1
                return {type = "string", value = t.value}
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
            elseif value == "return" then
                return parse_function_return()
            elseif has_variable_in_scope(value) == true then
                return parse_variable_reassignment()
            else
                local expr = parse_expression()
                expect("punctuation", ";")
                return expr
            end
        elseif type == "newline" and value == "\n" then
            line = line + 1
            i = i + 1
            return nil
        else
            error(file, line, string.format("Unexpected token found while parsing statement: '%s'", value))
        end
    end

    local function add_standard_functions()
        add_function_to_scope("echo")
        add_function_to_scope("puts")
    end

    enter_scope()
    add_standard_functions()

    while i <= #toks do
        local node = parse_statement()
        if node then
            table.insert(ast, node)
        end
    end

    exit_scope()

    return ast
end

local file_name = "test.zk"
local file = io.open(file_name, "rb")
if not file then print("No 'test.zk' file") os.exit(1)  end
local content = file:read("a")
file:close()

local toks = lex(content, file_name)
local ast = parse(toks, file_name)

local inspect = require("inspect")
print(inspect(ast))