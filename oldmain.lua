local file_name = "test.zk"
local file = io.open(file_name, "rb")
if not file then print("No 'test.zk' file") os.exit(1)  end
local content = file:read("a")
file:close()

local indent = ""

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
    --indent = indent .. "    "
    printf("enter '%s'", indent)
end

local function exit_scope(line)
    if #scope_stack == 0 then
        printf("%s:%s: Unexpected `end` statement", file_name, line)
        os.exit(1)
    end
    table.remove(scope_stack)
    --indent = indent:sub(1, -5)
    printf("exit  '%s'", indent)
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

local function f(...)
    return string.format(...)
end

local code_str = [[
BITS 64
section .text
echo:
    mov r9, -3689348814741910323
    sub rsp, 40
    mov BYTE [rsp+31], 10
    lea rcx, [rsp+30]
.L2:
    mov rax, rdi
    lea r8, [rsp+32]
    mul r9
    mov rax, rdi
    sub r8, rcx
    shr rdx, 3
    lea rsi, [rdx+rdx*4]
    add rsi, rsi
    sub rax, rsi
    add eax, 48
    mov BYTE [rcx], al
    mov rax, rdi
    mov rdi, rdx
    mov rdx, rcx
    sub rcx, 1
    cmp rax, 9
    ja .L2
    lea rax, [rsp+32]
    mov edi, 1
    sub rdx, rax
    xor eax, eax
    lea rsi, [rsp+32+rdx]
    mov rdx, r8
    mov rax, 1
    syscall
    add rsp, 40
    ret
global main
]]

local r_ends = 0
local ends = 0

local procs = {}
local vars = {}

local function parse(arr)
    local before_str = ""
    local ret = ""
    local including = {}
    local function code(toadd)
        if ret ~= "" then
            ret = ret .. "\n"
        end
        ret = ret .. indent .. toadd
    end

    i = 1
    while i <= #arr do
        local t = arr[i]

        local p, v = tok(t)
        if p == "int" then
            code(f("mov rax, %s", v))
        elseif p == "str" then
            print("here")
            print(v)
        elseif p == "id" then
            if v == "echo" then
                code("pop rdi")
                code("call echo")
            elseif v == "proc" then
                local pat, name = tok(arr[i + 1])
                if pat ~= "id" or name == nil then
                    printf("%s:%s: The token after the procedure definition is not valid", file_name, line)
                    os.exit(1)
                end

                if has_value(vars, name) or has_value(procs, name) then
                    printf("%s:%s: '%s' is already defined", file_name, line, name)
                    os.exit(1)
                end

                local pat, val = tok(arr[i + 2])
                if pat ~= "id" or val ~= "in" then
                    printf("%s:%s: Missing `in` to open procedure", file_name, line)
                    os.exit(1)
                end

                code(f("%s:", name))
                enter_scope(line)

                table.insert(procs, name)
                i = i + 2
                r_ends = r_ends + 1
            elseif v == "if" then
                local condition = {}
                local has_in = false
                local j = i + 1
                while j <= #arr do
                    local pat, val = tok(arr[j])
                    if pat == "id" and val == "in" then
                        has_in = true
                        break
                    end
                    table.insert(condition, arr[j])
                    j = j + 1
                end

                if has_in == false then
                    printf("%s:%s: Missing `in` to open if statement", file_name, line)
                    os.exit(1)
                end

                code(parse(condition))
                code(f("if (pop() == 1) {"))
                enter_scope(line)

                i = j
                r_ends = r_ends + 1
            elseif v == "else" then
                if ends <= 0 then
                    printf("%s:%s: Unexpected `else` statement", file_name, line)
                    os.exit(1)
                end

                local pat, val = tok(arr[i + 1])
                if pat ~= "id" or val ~= "in" then
                    printf("%s:%s: Missing `in` to open else", file_name, line)
                    os.exit(1)
                end

                exit_scope(line)
                code("} else {")
                enter_scope(line)

                i = i + 1
            elseif v == "while" then
                local condition = {}
                local has_in = false
                local j = i + 1
                while j <= #arr do
                    local pat, val = tok(arr[j])
                    if pat == "id" and val == "in" then
                        has_in = true
                        break
                    end
                    table.insert(condition, arr[j])
                    j = j + 1
                end

                if has_in == false then
                    printf("%s:%s: Missing `in` to open while", file_name, line)
                    os.exit(1)
                end

                local ident = "while_i" .. i

                local parsed, _, including_names_table = parse(condition)
                local def_including_names = ""
                if table.concat(including_names_table, " ") ~= "" then
                    def_including_names = "int " .. table.concat(including_names_table, ", int ")
                end
                local including_names = table.concat(including_names_table, ", ")

                before_str = before_str .. f("int %s(%s) {%sreturn pop();}\n", ident, def_including_names, parsed:gsub("\n", ""):gsub("    ", ""))
                code(f("while (%s(%s) == 1) {", ident, including_names))
                enter_scope(line)

                i = j
                r_ends = r_ends + 1
            elseif v == "end" then
                if r_ends == 0 then
                    printf("%s:%s: Unexpected `end` statement", file_name, line)
                    os.exit(1)
                end

                exit_scope(line)
                ends = ends + 1
            elseif v == "mut" then
                local pat, name = tok(arr[i + 1])
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
                while j <= #arr do
                    local pat, val = tok(arr[j])
                    if pat == "id" and val == "end" then
                        has_end = true
                        break
                    end
                    table.insert(expr, arr[j])
                    j = j + 1
                end

                if not has_end then
                    printf("%s:%s: Missing `end` to close variable definition", file_name, line)
                    os.exit(1)
                end

                code(parse(expr))

                if has_variable_in_scope(name) then
                    code(f("%s = pop();", name))
                else
                    code(f("int %s = pop();", name))
                    add_variable_to_scope(name, true)
                end

                table.insert(vars, name)
                i = j
            elseif has_variable_in_scope(v) then
                code(f("push(%s);", v))
                table.insert(including, v)
            elseif has_value(procs, v) then
                code(f("call %s", v))
            else
                printf("%s:%s: Identifier `%s` not recognized", file_name, line, v)
                os.exit(1)
            end
        elseif p == "raop" then
            if v == "=" or v == "!" then
                code(f("push(pop() %s= pop());", v))
            elseif v == "<" then
                code(f("push(pop() > pop());", v)) -- this is not an error, this is on purpose
            elseif v == ">" then
                code(f("push(pop() < pop());", v)) -- this is not an error, this is on purpose
            else
                printf("%s:%s: Rational operator `%s` not recognized", file_name, line, v)
                os.exit(1)
            end
        elseif p == "arop" then
            if v == "+" then
                code(f("push(pop() + pop());"))
            elseif v == "-" then
                code(f("__a__ = pop();"))
                code(f("push(pop() - __a__);"))
            elseif v == "*" then
                code(f("push(pop() * pop());"))
            elseif v == "/" then
                code(f("__a__ = pop();"))
                code(f("push(pop() / __a__);"))
            else
                printf("%s:%s: Arithmetic operator `%s` not recognized", file_name, line, v)
                os.exit(1)
            end
        elseif p == "op" then
            if v == "@" then
                code(f("__a__ = pop();"))
                code(f("__b__ = pop();"))
                code(f("push(__a__);"))
                code(f("push(__b__);"))
            elseif v == ":" then
                code(f("__a__ = pop();"))
                code(f("push(__a__);"))
                code(f("push(__a__);"))
            else
                printf("%s:%s: Stack operator `%s` not recognized", file_name, line, v)
                os.exit(1)
            end
        elseif t == "NL" then
            line = line + 1
        else
            printf("%s:%s: Token `%s` not recognized", file_name, line, t)
            os.exit(1)
        end

        i = i + 1
    end

    return ret, before_str, including
end

local ret, before_str, _ = parse(toks)
code_str = code_str .. before_str .. ret

if r_ends ~= ends then
    if r_ends > ends then
        local last_scope = scope_stack[#scope_stack]
        printf("%s:%s: Missing `end` to close `in` body", file_name, last_scope.line)
    else
        printf("Too many `end`s compared to `in`s")
    end
    os.exit(1)
end

code_str = code_str .. [[

mov rax, 60
xor rdi, rdi
syscall
]]

local out = io.open("test.asm", "w")
if out == nil then
    print("file is nil")
    os.exit(1)
end
out:write(code_str)
out:close()