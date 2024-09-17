local function print_table(table)
    for i in pairs(table) do
        print(table[i])
    end
end

local function is_alpha(str)
    return str:match("^[a-zA-Z_]+$") ~= nil
end

local function has_value(arr, val)
    for index, value in ipairs(arr) do
        if value == val then
            return true
        end
    end

    return false
end

local function printf(...)
    print(string.format(...))
end

local file_name = "test.stabel"
local file = io.open(file_name, "rb")
if not file then print("No 'test.stabel' file") os.exit(1)  end
local content = file:read("a")
file:close()

print(content)
print("div\n")


--[[
 - TYPE(val?, ...)
 -
 - INT(val) > integer
 - ID(val) > identifier
 - OP(+|-|*|/)
 - ]]


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

print_table(toks)

print("div\n")

local line = 1

local function tok(t)
    if t == nil then
        return nil
    end
    local patterns = {
        int = "^INT%(([0-9]+)%)$",
        op = "^OP%(([%@%:%~])%)$",
        arop = "^AROP%(([%+%-%*%/])%)$",
        raop = "^RAOP%(([%=%!%>%<])%)$",
        id = "^ID%(([a-zA-Z_]+)%)$"
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
#include <stdio.h>
#include <stdlib.h>
#define MAX_SIZE 255
int stack[MAX_SIZE];
int top = -1;
int __a__;
int __b__;
void push(int item) {
if (top == MAX_SIZE - 1) {
printf("Stack Overflow\n");
exit(1);
}
stack[++top] = item;
}
int pop() {
if (top == -1) {
printf("Stack Underflow\n");
exit(1);
}
return stack[top--];
}
]]

local r_ends = 0
local ends = 0

local names = {}

local function parse(arr)
    local before_str = ""
    local ret = ""
    local including = {}
    local function code(toadd)
        if ret ~= "" and ret:sub(-1) ~= "\n" then
            ret = ret .. "\n"
        end
        ret = ret .. toadd
    end

    i = 1
    while i <= #arr do
        local t = arr[i]

        local p, v = tok(t)
        if p == "int" then
            code(f("push(%s);", v))
        elseif p == "id" then
            if v == "echo" then
                code("printf(\"%d\\n\", pop());")
            elseif v == "proc" then
                local pat, name = tok(arr[i + 1])
                if pat ~= "id" or name == nil then
                    printf("%s:%s: The token after the procedure definition is not valid", file_name, line)
                    os.exit(1)
                end

                if has_value(names, name) then
                    printf("%s:%s: '%s' is already defined", file_name, line, name)
                    os.exit(1)
                end

                local pat, val = tok(arr[i + 2])
                if pat ~= "id" or val ~= "in" then
                    printf("%s:%s: Missing `in` to open procedure body", file_name, line)
                    os.exit(1)
                end

                code(f("void %s() {", name))

                table.insert(names, name)
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
                    printf("%s:%s: Missing `in` to open if statement body", file_name, line)
                    os.exit(1)
                end

                local rand = ""
                for i = 1, 30 do
                    rand = rand .. string.char(math.random(97, 122))
                end

                local parsed, _, including_names = parse(condition)
                local def_including_names = ""
                if table.concat(including_names, " ") ~= "" then
                    def_including_names = "int " .. table.concat(including_names, ", int ")
                end
                including_names = table.concat(including_names, ", ")

                before_str = before_str .. f("int %s(%s) {%sreturn pop();}\n", rand, def_including_names, parsed:gsub("\n", ""))
                code(f("if (%s(%s) == 1) {", rand, including_names))

                i = j
                r_ends = r_ends + 1
            elseif v == "else" then
                if ends <= 0 then
                    printf("%s:%s: Unexpected `else` statement", file_name, line)
                    os.exit(1)
                end

                code("} else {")
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

                local rand = ""
                for i = 1, 30 do
                    if math.random() > math.random() then
                        rand = rand .. string.char(math.random(97, 122))
                    else
                        rand = rand .. string.upper(string.char(math.random(97, 122)))
                    end
                end

                local parsed, _, including_names = parse(condition)
                local def_including_names = ""
                if table.concat(including_names, " ") ~= "" then
                    def_including_names = "int " .. table.concat(including_names, ", int ")
                end
                including_names = table.concat(including_names, ", ")

                before_str = before_str .. f("int %s(%s) {%sreturn pop();}\n", rand, def_including_names, parsed:gsub("\n", ""))
                code(f("while (%s(%s) == 1) {", rand, including_names))

                i = j
                r_ends = r_ends + 1
            elseif v == "end" then
                if r_ends == 0 then
                    printf("%s:%s: Unexpected `end` statement", file_name, line)
                    os.exit(1)
                end

                code("}")
                ends = ends + 1
            elseif v == "let" then
                local pat, name = tok(arr[i + 1])
                if pat ~= "id" or name == nil then
                    printf("%s:%s: The token after the variable definition is not valid", file_name, line)
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

                if has_end == false then
                    printf("%s:%s: Missing `end` to close variable definition", file_name, line)
                    os.exit(1)
                end

                code(parse(expr))

                if has_value(names, name) then
                    code(f("%s = pop();", name))
                else
                    code(f("int %s = pop();", name))
                end

                table.insert(names, name)
                i = j
            elseif has_value(names, v) then
                code(f("push(%s);", v))
                table.insert(including, v)
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
                printf("%s:%s: Token `%s` not recognized rational operator", file_name, line, v)
                os.exit(1)
            end
        elseif p == "arop" then
            if v == "+" then -- they are turned around since because
                code(f("push(pop() + pop());"))
            elseif v == "-" then
                code(f("__a__ = pop();"))
                code(f("push(pop() - __a__);"))
            elseif v == "*" then
                code(f("push(pop() * pop());"))
            elseif v == "/" then
                code(f("__a__ = pop();"))
                code(f("push(pop() / __a__);"))
            end
        elseif t == "NL" then
            line = line + 1
        else
            printf("%s:%s: Token `%s` not recognized", file_name, line, t)
            os.exit(1)
        end

        --if t ~= "NL" then
        --    code(f("// %s", t))
        --end

        i = i + 1
    end

    return ret, before_str, including
end


local ret, before_str, _ = parse(toks)
code_str = code_str .. before_str .. ret

if r_ends ~= ends then
    print(r_ends, ends)
    if r_ends > ends then
        print("Missing `end` to close `in` body")
    else
        print("Too many `end`s compared to `in`s") -- finn en måte å få linje nummer til `in` som ikke ble lukket, kanskje bytt dette systemet til å sjekke etter end i parser
    end
    os.exit(1)
end

local out = io.open("test.c", "w")
if out == nil then
    print("file is nil")
    os.exit(1)
end
out:write(code_str)
out:close()

os.execute("gcc test.c -o test.out")