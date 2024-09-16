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
    elseif c == "+" or c == "-" or c == "*" or c == "/" or c == "@" or c == ":" or c == "=" or c == "!" or c == ">" or c == "<" or c == "~" then
        table.insert(toks, "OP(" .. c .. ")")
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
        int = "^INT%((%-?%d+)%)$",
        op = "^OP%(([%+%-%*%@%:%=%!%>%</])%)$",
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

local function code(toadd)
    code_str = code_str .. toadd
end

local r_ends = 0
local ends = 0

i = 1
while i <= #toks do
    local t = toks[i]

    local p, v = tok(t)
    if p == "int" then
        code(f("push(%s);\n", v))
    elseif p == "id" then
        if v == "echo" then
            code("printf(\"%d\\n\", pop());\n")
        elseif v == "proc" then
            local pat, name = tok(toks[i + 1])
            if pat ~= "id" or name == nil then
                printf("%s:%s: The token after the procedure definition is not valid", file_name, line)
                os.exit(1)
            end

            local pat, val = tok(toks[i + 2])
            if pat ~= "id" or val ~= "in" then
                printf("%s:%s: Missing `in` to open procedure body", file_name, line)
                os.exit(1)
            end

            code(f("void %s() {\n", name))

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
                table.insert(condition, val)
                j = j + 1
            end

            if has_in == false then
                printf("%s:%s: Missing `in` to open if statement body", file_name, line)
                os.exit(1)
            end

            print(table.concat(condition, " "))
            -- TODO: make the condition into something that c can understand, ignore for now

            i = j
            r_ends = r_ends + 1
        elseif v == "else" then
            if ends <= 0 then
                printf("%s:%s: Unexpected `else` statement", file_name, line)
                os.exit(1)
            end

            code("} else {\n")
        elseif v == "end" then
            if r_ends == 0 then
                printf("%s:%s: Unexpected `end` statement", file_name, line)
                os.exit(1)
            end

            code("}\n")
            ends = ends - 1
        elseif v == "var" or v == "let" then
            local expr = {}
            local has_end = false
            local j = i + 1
            while j <= #toks do
                local pat, val = tok(toks[j])
                if pat == "id" and val == "end" then
                    has_end = true
                    break
                end
                table.insert(expr, val)
                j = j + 1
            end

            if has_end == false then
                printf("%s:%s: Missing `end` to close variable definition", file_name, line)
                os.exit(1)
            end

            print(table.concat(expr, " ")) --TODO: implement this, ignore for now

            if v == "var" then
                -- mutable
            elseif v == "let" then
                -- immutable
            end

            i = j
            r_ends = r_ends + 1
        else
            printf("%s:%s: Identifier `%s` not recognized", file_name, line, v)  line = line + 1
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

if r_ends ~= ends then
    if r_ends > ends then
        print("Missing `end` to close an `in` body")
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

--os.execute("gcc test.c -o test.out")