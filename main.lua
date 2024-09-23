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
    local ir = {}
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

    local function emit(instr)
        table.insert(ir, instr)
    end

    while i <= #toks do
        local t = toks[i].type
        local v = toks[i].value

        if t == "identifier" then
            i = i + 1
            if v == "fn" then
                local name = expect("identifier").value
                expect("parenthesis", "(")

                local args = {}
                while i <= #toks and toks[i].value ~= ")" do
                    table.insert(args, toks[i])
                    i = i + 1
                end

                print("TODO: implement args to functions")

                expect("parenthesis", ")")
                expect("punctuation", ":")
                local type = expect("identifier").value
                expect("parenthesis", "{")

                if type == "int" then
                    emit(string.format("define i32 @%s() {", name))
                elseif type == "void" then
                    emit(string.format("define void @%s() {", name))
                else
                    error(file, line, string.format("Unrecognized return type: '%s'", type))
                end
            elseif v == "echo" then
                expect("parenthesis", "(")
                print("TODO: parse expression inside here")
                local int = expect("integer").value
                expect("parenthesis", ")")
                expect("punctuation", ";")

                emit("call i32 (i8*, ...) @printf(i8* getelementptr ([4 x i8], [4 x i8]* @.int_fmt, i32 0, i32 0), i32 " .. int .. ")")
            else
                print("TODO: implement adding variables here and other stuff")
                error(file, line, string.format("Unexpected identifier found during parsing: '%s'", v))
            end
        elseif t == "parenthesis" then
            i = i + 1
            if v == "}" then
                expect("punctuation", ";")
                emit("}")
            else
                error(file, line, string.format("Unexpected punctuation found during parsing: '%s'", v))
            end
        elseif t == "newline" then
            line = line + 1
            i = i + 1
        else
            error(file, line, string.format("Unexpected token found during parsing: '%s'", v))
        end
    end

    return ir
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
local ir = parse(toks, file_name)
print(table.concat(ir, "\n"))