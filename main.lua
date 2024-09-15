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

local function tok(t)
    local patterns = {
        int = "^INT%((%-?%d+)%)$",
        op = "^OP%(([%+%-%*%@%:%=%!%>%<%~/])%)$",
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

local function f(str, ...)
    return string.format(str, ...)
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

local defs = {}
local procs = {}
local line = 1

i = 1
while i <= #toks do
    local t = toks[i]

    local p, v = tok(t)
    if p == "int" then
        code(f("push(%s);\n"))
    elseif p == "id" then
        
    end

    i = i + 1
end

--local out = io.open("test.c", "w")
--if out == nil then
--    print("file is nil")
--    os.exit(1)
--end
--out:write(code)
--out:close()
--
--os.execute("gcc test.c -o test.out")