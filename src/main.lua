local stack = {}

function stack:push(value)
    table.insert(stack, value)
end

function stack:pop()
    return table.remove(stack)
end

local function push(number)
    stack:push(number)
end

local function add()
    local a = stack:pop()
    local b = stack:pop()

    table.insert(stack, b + a)
end

local function sub()
    local a = stack:pop()
    local b = stack:pop()

    table.insert(stack, b - a)
end

local function mul()
    local a = stack:pop()
    local b = stack:pop()

    table.insert(stack, b * a)
end

local function div()
    local a = stack:pop()
    local b = stack:pop()

    table.insert(stack, b / a)
end

local function rot()
    local a = stack:pop()
    local b = stack:pop()

    table.insert(stack, a)
    table.insert(stack, b)
end

local function dup()
    local a = stack:pop()

    table.insert(stack, a)
    table.insert(stack, a)
end

local function echo()
    print(stack:pop())
end

local function peek()
    local a = stack:pop()
    stack:push(a)
    print(a)
end

--[[local program = {
    push(5),
    push(5),
    add(),
    push(3),
    mul(),
    peek(),
    push(2),
    rot(),
    div(),
    push(5),
    push(2),
    sub(),
    rot(),
    sub(),
    echo()
}]]

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
    elseif c == "+" or c == "-" or c == "*" or c == "/" or c == "@" or c == ":" then
        table.insert(toks, "OP(" .. c .. ")")
    elseif c == " " or c == "\n" then -- disregard
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
    op = "^OP%(([%+%-%*%@%:/])%)$",
    id = "^ID%(([a-zA-Z_]+)%)$"
}

local subc = arg[1]
local sim = false
if subc == "s" then
    sim = true
end

local code = [[
#include <stdio.h>
#include <stdlib.h>

#define MAX_SIZE 255

int stack[MAX_SIZE];
int top = -1;
int _;
int __;

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

void main() {
]]

i = 1
while i <= #toks do
    local t = toks[i]
    if check_pattern(t, p.int) then
        local value = check_pattern(t, p.int)
        if sim then
            push(value)
        else
            code = code .. "// " .. t .. "\n"
            code = code .. "push(" .. value .. ");\n"
        end
    elseif check_pattern(t, p.id) then
        local value = check_pattern(t, p.id)
        if value == "echo" then
            if sim then
                echo()
            else
                code = code .. "// " .. t .. "\n"
                code = code .. "printf(\"%d\\n\", pop());\n"
            end
        elseif value == "peek" then
            if sim then
                peek()
            else
                code = code .. "// " .. t .. "\n"
                code = code .. "_ = pop();\n"
                code = code .. "push(_);\n"
                code = code .. "printf(\"%d\\n\", _);\n"
            end
        end
    elseif check_pattern(t, p.op) then
        local s = check_pattern(t, p.op)
        if s == "+" then
            if sim then
                add()
            else
                code = code .. "// " .. t .. "\n"
                code = code .. "_ = pop();\n"
                code = code .. "push(pop() + _);\n"
            end
        elseif s == "-" then
            if sim then
                sub()
            else
                code = code .. "// " .. t .. "\n"
                code = code .. "_ = pop();\n"
                code = code .. "push(pop() - _);\n"
            end
        elseif s == "*" then
            if sim then
                mul()
            else
                code = code .. "// " .. t .. "\n"
                code = code .. "_ = pop();\n"
                code = code .. "push(pop() * _);\n"
            end
        elseif s == "/" then
            if sim then
                div()
            else
                code = code .. "// " .. t .. "\n"
                code = code .. "_ = pop();\n"
                code = code .. "push(pop() / _);\n"
            end
        elseif s == "@" then
            if sim then
                rot()
            else
                code = code .. "// " .. t .. "\n"
                code = code .. "_ = pop();\n"
                code = code .. "__ = pop();\n"
                code = code .. "push(_);\n"
                code = code .. "push(__);\n"
            end
        elseif s == ":" then
            if sim then
                dup()
            else
                code = code .. "// " .. t .. "\n"
                code = code .. "_ = pop();\n"
                code = code .. "push(_);\n"
                code = code .. "push(_);\n"
            end
        else
            print("Unrecognized token " .. t)
            os.exit(1)
        end
    end

    if i < #toks then
        code = code .. "\n"
    end

    i = i + 1
end

code = code .. "}"
if not sim then
    local out = io.open("test.c", "w")
    out:write(code)
    out:close()
end

os.execute("gcc test.c -o test.out")