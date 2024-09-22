local file_name = "test.zk"
local file = io.open(file_name, "rb")
if not file then print("No 'test.zk' file") os.exit(1)  end
local content = file:read("a")
file:close()

print(content)
print("div\n")

local function is_alpha(str)
    return str:match("^[a-zA-Z_]+$") ~= nil
end

local function has_value(arr, val)
    for _, v in ipairs(arr) do
        if v == val then
            return true
        end
    end

    return false
end

local function printf(...)
    print(string.format(...))
end

local scope_stack = {}

local function enter_scope(line)
    table.insert(scope_stack, {mut_vars = {}, imut_vars = {}, line = line})
end

local function exit_scope(line)
    if #scope_stack == 0 then
        printf("%s:%s: Unexpected `end` statement", file_name, line)
        os.exit(1)
    end
    table.remove(scope_stack)
end

local function current_scope()
    return scope_stack[#scope_stack] or {mut_vars = {}, imut_vars = {}}
end

local function has_variable_in_scope(name)
    for i = #scope_stack, 1, -1 do
        local scope = scope_stack[i]
        if has_value(scope.mut_vars, name) or has_value(scope.imut_vars, name) then
            return true
        end
    end
    return false
end

local function add_variable_to_scope(name, mutable)
    local scope = current_scope()
    if mutable then
        table.insert(scope.mut_vars, name)
    else
        table.insert(scope.imut_vars, name)
    end
end

local chars = {}
for i = 1, #content do
    table.insert(chars, content:sub(i, i))
end

local toks = {}
local i = 1
while i <= #chars do
    local c = chars[i]
    if tonumber(c) then
        local num = c
        local j = i + 1
        while chars[j] and tonumber(chars[j]) do
            num = num .. chars[j]
            j = j + 1
        end
        table.insert(toks, "INT(" .. num .. ")")
        i = j - 1
    elseif c == "\"" then
        local str = c
        local j = i + 1
        while chars[j] or chars[j] == "\"" do
            str = str .. chars[j]
            if chars[j] == "\"" then
                break
            end
            j = j + 1
        end
        table.insert(toks, "STR(" .. str .. ")")
        i = j
    elseif is_alpha(c) then
        local str = c
        local j = i + 1
        while chars[j] and is_alpha(chars[j]) do
            str = str .. chars[j]
            j = j + 1
        end
        table.insert(toks, "ID(" .. str .. ")")
        i = j - 1
    elseif c == "@" or c == ":" or c == "~" then
        table.insert(toks, "OP(" .. c .. ")")
    elseif c == "+" or c == "-" or c == "*" or c == "/" then
        table.insert(toks, "AROP(" .. c .. ")")
    elseif c == "=" or c == "!" or c == ">" or c == "<" then
        table.insert(toks, "RAOP(" .. c .. ")")
    elseif c == "\n" then
        table.insert(toks, "NL")
    elseif c == " " then -- disregard
    else
        print("Unrecognized character: '" .. c .. "'")
        os.exit(1)
    end

    i = i + 1
end

print(table.concat(toks, "\n"))

print("div\n")

local line = 1

local function tok(t)
    if t == nil then
        return nil
    end
    local patterns = {
        int = "^INT%(([0-9]+)%)$",
        str = "^STR%((\".-\")%)$",
        op = "^OP%(([%@%:])%)$",
        arop = "^AROP%(([%+%-%*%/])%)$",
        raop = "^RAOP%(([%=%!%>%<])%)$",
        id = "^ID%(([a-zA-Z_][a-zA-Z0-9_]*)%)$"
    }

    for pattern in pairs(patterns) do
        local value = string.match(t, patterns[pattern])
        if value then
            return pattern, value
        end
    end

    return nil
end

local r_ends = 0
local ends = 0

local procs = {}
local vars = {}

local current_reg = 0
local literal_count = 0
local string_literals = {}
local llvm_ir = {}
local stack = {}

local function next_reg()
    current_reg = current_reg + 1
    return "%r" .. current_reg
end

local function emit(instr)
    table.insert(llvm_ir, instr)
end

local function next_literal_name()
    literal_count = literal_count + 1
    return "@.str" .. literal_count
end

local function store_string_literal(value, length)
    local literal_name = next_literal_name()
    table.insert(string_literals, literal_name .. " = constant [" .. length .. " x i8] c\"" .. value .. "\\00\"")
    return literal_name
end

local function push(value)
    local reg = next_reg()
    emit(reg .. " = alloca i32")
    emit("store i32 " .. value .. ", i32* " .. reg)
    table.insert(stack, {reg = reg, type = "i32"})
end

local function push_string(value, length)
    local literal_name = store_string_literal(value, length)
    local reg = next_reg()
    emit(reg .. " = getelementptr [" .. length .. " x i8], [" .. length .. " x i8]* " .. literal_name .. ", i32 0, i32 0")
    table.insert(stack, {reg = reg, type = "i8*"})
end

local function pop()
    return table.remove(stack)
end

emit("declare i32 @printf(i8*, ...)")
table.insert(string_literals, "@.int_fmt = constant [4 x i8] c\"%d\\0A\\00\"")

i = 1
while i <= #toks do
    local t = toks[i]

    local p, v = tok(t)
    if p == "int" then
        emit("; " .. t)
        push(v)
    elseif p == "str" then
        if v == nil then
            printf("%s:%s: String value is nil", file_name, line)
            os.exit(1)
        end

        if v:sub(1, 1) ~= "\"" then
            printf("%s:%s: String does not lead with '\"'", file_name, line)
            os.exit(1)
        end

        if v:sub(-1) ~= "\"" then
            printf("%s:%s: String must end with '\"'", file_name, line)
            os.exit(1)
        end

        emit("; " .. t)
        local str = v:match("^\"(.*)\"$"):gsub("\\n", "\\0A")
        local length = #str + 1
        for _ in string.gmatch(str, "\\0A") do
            print("newline")
            length = length - 2
        end
        print(str)
        push_string(str, length)
    elseif p == "id" then
        emit("; " .. t)
        if v == "echo" then
            local loaded_value = next_reg()
            emit(loaded_value .. " = load i32, i32* " .. pop().reg)
            emit("call i32 (i8*, ...) @printf(i8* getelementptr ([4 x i8], [4 x i8]* @.int_fmt, i32 0, i32 0), i32 " .. loaded_value .. ")")
        elseif v == "puts" then
            local value = pop()
            local loaded_value = next_reg()
            emit(loaded_value .. " = load i32, i32* " .. value.reg)
            emit("call i32 (i8*, ...) @printf(i8* " .. value.reg .. ")")
        elseif v == "proc" then
            local pat, name = tok(toks[i + 1])
            if pat ~= "id" or name == nil then
                printf("%s:%s: The token after the procedure definition is not valid", file_name, line)
                os.exit(1)
            end

            if has_value(vars, name) or has_value(procs, name) then
                printf("%s:%s: '%s' is already defined", file_name, line, name)
                os.exit(1)
            end

            local pat, val = tok(toks[i + 2])
            if pat ~= "id" or val ~= "in" then
                printf("%s:%s: Missing `in` to open procedure", file_name, line)
                os.exit(1)
            end

            emit("define i32 @" .. name .. "() {")
            enter_scope(line)

            table.insert(procs, name)
            i = i + 2
            r_ends = r_ends + 1
        elseif v == "if" then
            local condition = {}
            local has_in = false
            local j = i + 1
            while j <= #toks do
                local pat, val = tok(toks[j])
                if pat == "id" and val == "in" then
                    has_in = true
                    break
                end
                table.insert(condition, toks[j])
                j = j + 1
            end

            if has_in == false then
                printf("%s:%s: Missing `in` to open if statement", file_name, line)
                os.exit(1)
            end

            --code(parse(condition))
            --code(f("if (pop() == 1) {"))
            enter_scope(line)

            i = j
            r_ends = r_ends + 1
        elseif v == "else" then
            if ends <= 0 then
                printf("%s:%s: Unexpected `else` statement", file_name, line)
                os.exit(1)
            end

            local pat, val = tok(toks[i + 1])
            if pat ~= "id" or val ~= "in" then
                printf("%s:%s: Missing `in` to open else", file_name, line)
                os.exit(1)
            end

            exit_scope(line)
            --code("} else {")
            enter_scope(line)

            i = i + 1
        elseif v == "while" then
            local condition = {}
            local has_in = false
            local j = i + 1
            while j <= #toks do
                local pat, val = tok(toks[j])
                if pat == "id" and val == "in" then
                    has_in = true
                    break
                end
                table.insert(condition, toks[j])
                j = j + 1
            end

            if has_in == false then
                printf("%s:%s: Missing `in` to open while", file_name, line)
                os.exit(1)
            end

            --local ident = "while_i" .. i

            --local parsed, _, including_names_table = parse(condition)
            --local def_including_names = ""
            --if table.concat(including_names_table, " ") ~= "" then
            --    def_including_names = "int " .. table.concat(including_names_table, ", int ")
            --end
            --local including_names = table.concat(including_names_table, ", ")

            --before_str = before_str .. f("int %s(%s) {%sreturn pop();}\n", ident, def_including_names, parsed:gsub("\n", ""):gsub("    ", ""))
            --code(f("while (%s(%s) == 1) {", ident, including_names))
            --enter_scope(line)

            i = j
            r_ends = r_ends + 1
        elseif v == "end" then
            if r_ends == 0 then
                printf("%s:%s: Unexpected `end` statement", file_name, line)
                os.exit(1)
            end

            exit_scope(line)
            if i == #toks then
                emit("ret i32 0")
            end
            emit("}")
            ends = ends + 1
        elseif v == "mut" then
            local pat, name = tok(toks[i + 1])
            if pat ~= "id" or name == nil then
                printf("%s:%s: The token after the variable definition is not valid", file_name, line)
                os.exit(1)
            end

            if has_value(procs, name) then
                printf("%s:%s: Procedure with same name already defined", file_name, line)
                os.exit(1)
            end

            local expr = {}
            local has_end = false
            local j = i + 2
            while j <= #toks do
                local pat, val = tok(toks[j])
                if pat == "id" and val == "end" then
                    has_end = true
                    break
                end
                table.insert(expr, toks[j])
                j = j + 1
            end

            if not has_end then
                printf("%s:%s: Missing `end` to close variable definition", file_name, line)
                os.exit(1)
            end

            --code(parse(expr))
--
            --if has_variable_in_scope(name) then
            --    code(f("%s = pop();", name))
            --else
            --    code(f("int %s = pop();", name))
            --    add_variable_to_scope(name, true)
            --end

            table.insert(vars, name)
            i = j
        elseif has_variable_in_scope(v) then
            --code(f("push(%s);", v))
            --table.insert(including, v)
        elseif has_value(procs, v) then
            --code(f("call %s", v))
        else
            printf("%s:%s: Identifier `%s` not recognized", file_name, line, v)
            os.exit(1)
        end
    elseif p == "raop" then
        emit("; " .. t)
        --if v == "=" or v == "!" then
        --    code(f("push(pop() %s= pop());", v))
        --elseif v == "<" then
        --    code(f("push(pop() > pop());", v)) -- this is not an error, this is on purpose
        --elseif v == ">" then
        --    code(f("push(pop() < pop());", v)) -- this is not an error, this is on purpose
        --else
        --    printf("%s:%s: Rational operator `%s` not recognized", file_name, line, v)
        --    os.exit(1)
        --end
    elseif p == "arop" then
        local b = pop()
        local a = pop()

        local loaded_a = next_reg()
        local loaded_b = next_reg()

        emit("; " .. t)
        emit(loaded_a .. " = load i32, i32* " .. a.reg)
        emit(loaded_b .. " = load i32, i32* " .. b.reg)

        local result = next_reg()
        if v == "+" then
            emit(result .. " = add i32 " .. loaded_a .. ", " .. loaded_b)
        elseif v == "-" then
            emit(result .. " = sub i32 " .. loaded_a .. ", " .. loaded_b)
        elseif v == "*" then
            emit(result .. " = mul i32 " .. loaded_a .. ", " .. loaded_b)
        elseif v == "/" then
            emit(result .. " = div i32 " .. loaded_a .. ", " .. loaded_b)
        else
            printf("%s:%s: Arithmetic operator `%s` not recognized", file_name, line, v)
            os.exit(1)
        end

        push(result)
    elseif p == "op" then
        emit("; " .. t)
        --if v == "@" then
        --    code(f("__a__ = pop();"))
        --    code(f("__b__ = pop();"))
        --    code(f("push(__a__);"))
        --    code(f("push(__b__);"))
        --elseif v == ":" then
        --    code(f("__a__ = pop();"))
        --    code(f("push(__a__);"))
        --    code(f("push(__a__);"))
        --else
        --    printf("%s:%s: Stack operator `%s` not recognized", file_name, line, v)
        --    os.exit(1)
        --end
    elseif t == "NL" then
        line = line + 1
    else
        printf("%s:%s: Token `%s` not recognized", file_name, line, t)
        os.exit(1)
    end

    i = i + 1
end

if r_ends ~= ends then
    if r_ends > ends then
        local last_scope = scope_stack[#scope_stack]
        printf("%s:%s: Missing `end` to close `in` body", file_name, last_scope.line)
    else
        printf("Too many `end`s compared to `in`s")
    end
    os.exit(1)
end

local out = io.open("out/test.ll", "w")
if out == nil then
    print("file is nil")
    os.exit(1)
end

local ir = table.concat(string_literals, "\n") .. table.concat(llvm_ir, "\n")

out:write(ir)
out:close()

os.execute("llvm-as out/test.ll -o out/test.bc")
os.execute("llc out/test.bc -o out/test.s")
os.execute("clang out/test.s -o out/test")