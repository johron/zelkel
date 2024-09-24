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
        if t.type ~= type then
            error(file, line, string.format("Expected '%s', but found '%s'", type, t.type))
        end
        if value ~= nil and t.value ~= value then
            error(file, line, string.format("Expected '%s' with value '%s', but found '%s' with value '%s'", type, value, t.type, t.value))
        end

        i = i + 1
        return t
    end

    local function parse_expression()
        print("TODO: implement parse_expression")
        return {}
    end

    local function parse_function_declaration_args()
        print("TODO: implement parse_function_declaration_args")
        return {}
    end

    local function parse_function_call_args()
        print("TODO: implement parse_function_call_args")
        return {}
    end

    local function parse_function_declaration()
        local name = expect("identifier").value
        expect("parenthesis", "(")

        local args = {}
        while i <= #toks and toks[i].value ~= ")" do
            table.insert(args, toks[i])
            i = i + 1
        end

        expect("parenthesis", ")")
        expect("punctuation", ":")
        local type = expect("identifier").value
        expect("parenthesis", "{")

        if not is_in_table(type, {"void", "int"}) then
            error(file, line, string.format("Unrecognized return type: '%s'", type))
        end

        return {
            type = "function_declaration",
            name = name,
            args = parse_function_declaration_args(),
            returns = type,
        }
    end

    local function parse_function_call()
        local name = expect("identifier").value
        expect("parenthesis", "(")

        local args = {}
        while i <= #toks and toks[i].value ~= ")" do
            table.insert(args, toks[i])
            i = i + 1
        end

        expect("parenthesis", ")")
        expect("punctuation", ";")

        return {
            type = "function_call",
            name = name,
            args = parse_function_call_args()
        }
    end

    local function parse_constant_variable_assignment()
        print("TODO: implement parse_constant_variable_assignment")
        return {}
    end

    local function parse_mutable_variable_assignment()
        print("TODO: implement parse_mutable_variable_assignment")
        return {}
    end

    local function parse_statement()
        print("TODO: implement parse_statement")
        return {}
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
local function print_table(t, indent)
    indent = indent or 0
    local indent_str = string.rep("  ", indent)
    for k, v in pairs(t) do
        if type(v) == "table" then
            print(indent_str .. "{")
            print_table(v, indent + 1)
            print(indent_str .. "}")
        else
            print(indent_str .. tostring(v))
        end
    end
end
print_table(ast)