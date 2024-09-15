local function print_table(table)
    for i in pairs(table) do
        print(table[i])
    end
end

local function is_alpha(str)
    return str:match("^[a-zA-Z_]+$") ~= nil
end

local function check_pattern(str, pat)
    local value = string.match(str, pat)

    if value then
        return value
    else
        return nil
    end
end

local function has_value(arr, val)
    for index, value in ipairs(arr) do
        if value == val then
            return true
        end
    end

    return false
end

local file = io.open("test.stabel", "rb")
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

local p = {
    int = "^INT%((%-?%d+)%)$",
    op = "^OP%(([%+%-%*%@%:%=%!%>%<%~/])%)$",
    id = "^ID%(([a-zA-Z_]+)%)$"
}

local code = [[
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

local defs = {}
local procs = {}
local line = 1

i = 1
while i <= #toks do
    local t = toks[i]
    code = code .. "// " .. t .. "\n"
    if check_pattern(t, p.int) then
        local value = check_pattern(t, p.int)
        code = code .. "push(" .. value .. ");\n"
    elseif check_pattern(t, p.id) then
        local value = check_pattern(t, p.id)
        if value == "echo" then
            code = code .. "printf(\"%d\\n\", pop());\n"
        elseif value == "peek" then
            code = code .. "__a__ = pop();\n"
            code = code .. "push(__a__);\n"
            code = code .. "printf(\"%d\\n\", __a__);\n"
        elseif value == "proc" then
            local name = check_pattern(toks[i - 1], p.id)
            if toks[i - 1] and name then
                if has_value(defs, name) then
                    print("Line " .. line .. ": Name is in use by a defintion: " .. name)
                    os.exit(1)
                end
                if has_value(procs, name) then
                    print("Line " .. line .. ": Name is in use by another procedure: " .. name)
                    os.exit(1)
                end

                code = code .. "void " .. name .. "()"
                table.insert(procs, name)
            else
                print("Line " .. line .. ": The word `proc` must have an identifier before it")
                os.exit(1)
            end
        elseif value == "drop" then
            code = code .. "__a__ = pop();\n"
        elseif value == "end" then
            code = code .. "}\n"
        elseif value == "in" then
            local id = check_pattern(toks[i - 1], p.id)
            if toks[i - 1] and not (id == "proc" or id == "else" or id == "if" or id == "while") then
                print("Line " .. line .. ": The word `in` cannot initiate a body here")
                os.exit(1)
            end
            code = code .. "{\n"
        elseif value == "if" then
            local op = check_pattern(toks[i - 1], p.op)
            if toks[i - 1] and (toks[i + 1] and check_pattern(toks[i+1], p.id) == "in") then
                code = code .. "if (pop() == 1)"
            else
                print("Line " .. line .. ": The word `if` shouldn't be here")
                os.exit(1)
            end
        elseif value == "while" then
            local op = check_pattern(toks[i - 1], p.op)
            if toks[i - 1] and (toks[i + 1] and check_pattern(toks[i+1], p.id) == "in") then
                code = code .. "while (pop() == 1)"
            else
                print("Line " .. line .. ": The word `while` shouldn't be here")
                os.exit(1)
            end
        elseif value == "else" then
            if toks[i + 1] and check_pattern(toks[i + 1], p.id) == "in" then
                code = code .. "} else"
            end
        elseif value == "def" then
            local name = check_pattern(toks[i - 1], p.id)
            if toks[i - 1] and name then
                if has_value(procs, name) then
                    print("Line " .. line .. ": Name is in use by a procedure: " .. name)
                    os.exit(1)
                end
                if has_value(defs, name) then
                    code = code .. name .. " = pop();\n"
                else
                    code = code .. "int " .. name .. " = pop();\n"
                    table.insert(defs, name)
                end
            else
                print("Line " .. line .. ": word `def` must have an identifier before it")
                os.exit(1)
            end
        elseif has_value(defs, value) then
            if toks[i + 1] and check_pattern(toks[i + 1], p.id) ~= "def" then
                code = code .. "push(" .. value .. ");\n"
            end
        elseif has_value(procs, value) then
            if toks[i + 1] and check_pattern(toks[i + 1], p.id) ~= "proc" then
                code = code .. value .. "();\n"
                code = code .. "push(pop());\n"
            end
        else
            if toks[i + 1] and check_pattern(toks[i + 1], p.id) ~= "def" then
            elseif toks[i + 1] and check_pattern(toks[i + 1], p.id) ~= "proc" then
            else
                print("Line " .. line .. ": Unrecognized identifier: " .. value)
                os.exit(1)
            end
        end
    elseif check_pattern(t, p.op) then
        local s = check_pattern(t, p.op)
        if s == "+" then
            code = code .. "__a__ = pop();\n"
            code = code .. "push(pop() + __a__);\n"
        elseif s == "-" then
            code = code .. "__a__ = pop();\n"
            code = code .. "push(pop() - __a__);\n"
        elseif s == "*" then
            code = code .. "__a__ = pop();\n"
            code = code .. "push(pop() * __a__);\n"
        elseif s == "/" then
            code = code .. "__a__ = pop();\n"
            code = code .. "push(pop() / __a__);\n"
        elseif s == "@" then
            code = code .. "__a__ = pop();\n"
            code = code .. "__b__ = pop();\n"
            code = code .. "push(__a__);\n"
            code = code .. "push(__b__);\n"
        elseif s == "~" then
            code = code .. "__a__ = pop();\n"
            code = code .. "__b__ = pop();\n"
            code = code .. "push(__a__);\n"
            code = code .. "push(__b__);\n"
            code = code .. "push(__b__);\n"
        elseif s == ":" then
            code = code .. "__a__ = pop();\n"
            code = code .. "push(__a__);\n"
            code = code .. "push(__a__);\n"
        elseif s == "=" or s == ">" or s == "<" or s == "!" then
            code = code .. "__b__ = pop();\n"
            code = code .. "__a__ = pop();\n"
            code = code .. "push(__a__ " .. s .. "= __b__);\n"
        else
            print("Line " .. line .. ": Unrecognized token " .. t)
            os.exit(1)
        end
    elseif t == "NL" then
        line = line + 1
    else
        print("Line " .. line .. ": Unrecognized token " .. t)
        os.exit(1)
    end

    if i < #toks then
        code = code .. "\n"
    end

    i = i + 1
end

local out = io.open("test.c", "w")
if out == nil then
    print("file is nil")
    os.exit(1)
end
out:write(code)
out:close()

os.execute("gcc test.c -o test.out")