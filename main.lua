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

local function dump()
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
    dump()
}]]

local function print_table(table)
    for i in pairs(table) do
        print(table[i])
    end
end

local function is_alpha(str)
    return str:match("^[%a]+$") ~= nil
end

local file = io.open("test.sl", "rb")
if not file then print("no 'test.sl' file") os.exit(1)  end
local content = file:read("a")
file:close()

print(content)

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
while i - 1 < #chars do
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