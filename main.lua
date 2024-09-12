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

    table.insert(stack, a + b)
end

local function sub()
    local a = stack:pop()
    local b = stack:pop()

    table.insert(stack, a - b)
end

local function mul()
    local a = stack:pop()
    local b = stack:pop()

    table.insert(stack, a * b)
end

local function div()
    local a = stack:pop()
    local b = stack:pop()

    table.insert(stack, a / b)
end

local function rot()
    local a = stack:pop()
    local b = stack:pop()

    table.insert(stack, a)
    table.insert(stack, b)
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

local file = io.open("test.sl", "rb")
if not file then print("No 'test.sl' file") os.exit(1)  end
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
    elseif c == "+" or c == "-" or c == "*" or c == "/" then
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
    op = "^OP%(([%+%-%*/])%)$",
    id = "^ID%(([a-zA-Z_]+)%)$"
}

i = 1
while i <= #toks do
    local t = toks[i]
    if check_pattern(t, p.int) then
        local value = check_pattern(t, p.int)
        push(value)
    elseif check_pattern(t, p.id) then
        local value = check_pattern(t, p.id)
        if value == "echo" then
            echo()
        elseif value == "peek" then
            peek()
        end
    elseif check_pattern(t, p.op) then
        local s = check_pattern(t, p.op)
        if s == "+" then
            add()
        elseif s == "-" then
            sub()
        elseif s == "*" then
            mul()
        elseif s == "/" then
            div()
        end
    end

    i = i + 1
end
