local inspect = require("inspect") -- debugging

local function is_in_table(v, t)
    for _, j in ipairs(t) do
        if j == v then
            return true
        end
    end

    return false
end

local function contains_string(t, str)
    for _, v in ipairs(t) do
        if v == str then
            return true
        end
    end
    return false
end

function lex(input, file, line)
    local toks = {}
    local line = line or 1

    local function error(msg)
        print(string.format("%s:%s: %s", file, line, msg))
        os.exit(1)
    end

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
        if c == "f" and chars[i + 1] == "\"" then
            local v = ""
            i = i + 2
            while i <= #chars do
                if chars[i] == "\"" then
                    i = i + 1
                    break
                end
                v = v .. chars[i]
                i = i + 1
            end
            table.insert(toks, {type = "fstring", value = v})
    elseif is_alpha(c) then
            local v = ""
            while i <= #chars and (is_alpha(chars[i]) or is_digit(chars[i])) do
                v = v .. chars[i]
                i = i + 1
            end
            table.insert(toks, {type = "identifier", value = v})
        elseif is_digit(c) then
            local found_dot = false
            local type = "integer"
            local v = ""
            while i <= #chars and (is_digit(chars[i]) or (chars[i] == "." and found_dot == false)) do
                if chars[i] == "." then
                    found_dot = true
                    type = "float"
                end
                v = v .. chars[i]
                i = i + 1
            end

            table.insert(toks, {type = type, value = tonumber(v)})
        elseif c == "\"" then
            local v = ""
            i = i + 1
            while i <= #chars do
                if chars[i] == "\"" then
                    i = i + 1
                    break
                end
                v = v .. chars[i]
                i = i + 1
            end
            table.insert(toks, {type = "string", value = v})
        elseif is_in_table(c, {"+", "-", "*", "/", "=", "%"}) then
            if c == "/" and chars[i + 1] == "/" then
                while i <= #chars and chars[i] ~= "\n" do
                    i = i + 1
                end
                if chars[i] == "\n" then
                    line = line + 1
                end
            elseif c == "=" and chars[i + 1] == "=" then
                table.insert(toks, {type = "operator", value = c .. chars[i]})
                i = i + 1
            else
                table.insert(toks, {type = "operator", value = c})
            end
            i = i + 1
        elseif is_in_table(c, {">", "<", "!"}) then
            if chars[i + 1] == "=" then
                table.insert(toks, {type = "operator", value = c .. chars[i + 1]})
                i = i + 1
            else
                table.insert(toks, {type = "operator", value = c})
            end
            i = i + 1
        elseif is_in_table(c, {";", ":", ","}) then
            table.insert(toks, {type = "punctuation", value = c})
            i = i + 1
        elseif is_in_table(c, {"(", ")", "{", "}"}) then
            table.insert(toks, {type = "parenthesis", value = c})
            i = i + 1
        elseif c == "\n" then
            line = line + 1
            i = i + 1
        elseif is_in_table(c, {" ", "\r"}) then -- disregard
            i = i + 1
        elseif c == "." and #chars - i >= 3 and chars[i + 1] == "." and chars[i + 2] == "." then
            table.insert(toks, {type = "ellipsis", value = "..."})
            i = i + 3
        else
            error(string.format("Unexpected character found by lexer: '%s'", c))
        end

        toks[#toks].line = line
        toks[#toks].file = file
    end

    return toks
end

function preprocess(toks)
    local newtoks = {}
    local required = {}
    local i = 1

    local function current()
        if i <= #toks then
            return toks[i]
        else
            error("Attempt to access token out of bounds")
        end
    end

    local function error(msg)
        print(string.format("%s:%s: %s", current().file, current().line, msg))
        os.exit(1)
    end

    local function expect(type, value)
        local t = current()

        if value ~= nil and t.value ~= value then
            error(string.format("Expected '%s', but found '%s'", value, t.value))
        elseif t.type ~= type then
            error(string.format("Expected '%s', but found '%s'", type, t.value))
        end

        if i <= #toks then
            i = i + 1
        end

        return t
    end

    local function preprocess_require()
        expect("identifier", "require")

        local to_require = current().value
        if not to_require:match("%.%w+$") then
            to_require = to_require .. ".zk"
        end
        if to_require == current().file then
            error("Cannot require itself")
        end
        i = i + 1

        expect("punctuation", ";")

        if not is_in_table(to_require, required) then
            table.insert(required, to_require)
            local sub_file = io.open(to_require, "rb")
            if not sub_file then
                error(string.format("Cannot find required file: '%s'", to_require))
            end

            local sub_content = sub_file:read("a")
            sub_file:close()

            local sub_toks = lex(sub_content, to_require)
            local new_sub_toks = preprocess(sub_toks)

            for _, tok in ipairs(new_sub_toks) do
                table.insert(newtoks, tok)
            end
        end
    end

    local function preprocess_assignment_and_operator() -- +=, -=, *=, ...
        local name = expect("identifier").value
        local operator = expect("operator").value
        expect("operator", "=")
        table.insert(newtoks, {type = "identifier", value = name})
        table.insert(newtoks, {type = "operator", value = "="})
        table.insert(newtoks, {type = "identifier", value = name})
        table.insert(newtoks, {type = "operator", value = operator})
    end

    local function preprocess_var_op_op() -- ++, --, i++; i--;
        local name = expect("identifier").value
        local op1 = expect("operator").value
        local op2 = expect("operator").value

        if op1 ~= op2 then
            error(string.format("Unexpected tokens found: %s, %s, %s", name, op1, op2))
        end

        table.insert(newtoks, {type = "identifier", value = name})
        table.insert(newtoks, {type = "operator", value = "="})
        table.insert(newtoks, {type = "identifier", value = name})
        table.insert(newtoks, {type = "operator", value = op1})
        table.insert(newtoks, {type = "integer", value = "1"})
    end

    while i <= #toks do
        local curr = current()
        local type = curr.type
        local value = curr.value

        if type == "identifier" and value == "require" then
            preprocess_require()
        elseif type == "identifier" and toks[i + 1] and toks[i + 1].value and contains_string({"+", "-", "/", "*", "%"}, toks[i + 1].value) and toks[i + 2].value == "=" then
            preprocess_assignment_and_operator()
        elseif type == "identifier" and contains_string({"+", "-"}, toks[i + 1].value) and contains_string({"+", "-"}, toks[i + 2].value) then
            preprocess_var_op_op()
        else
            table.insert(newtoks, toks[i])
            i = i + 1
        end
    end

    return newtoks
end

function parse(toks, scope_stack, expression)
    local scope_stack = scope_stack or {}
    local ast = {}
    local i = 1

    local function enter_scope()
        local parent_scope = scope_stack[#scope_stack] or {}
        local new_scope = {
            variables = {table.unpack(parent_scope.variables or {})},
            functions = {table.unpack(parent_scope.functions or {})},
            currentfunc = parent_scope.currentfunc or ""
        }
        table.insert(scope_stack, new_scope)
    end

    local function exit_scope()
        table.remove(scope_stack)
    end

    local function current_scope()
        return scope_stack[#scope_stack]
    end

    local function is_global_scope()
        return #scope_stack == 1
    end

    local function set_currentfunc(func)
        local scope = current_scope()
        scope.currentfunc = func
    end

    local function get_currentfunc()
        return current_scope().currentfunc
    end

    local function has_variable_in_scope(name)
        for j = #scope_stack, 1, -1 do
            local scope = scope_stack[j]
            for _, var in ipairs(scope.variables) do
                if var.name == name then
                    return true
                end
            end
        end
        return false
    end

    local function add_variable_to_scope(var)
        local scope = current_scope()
        if has_variable_in_scope(var.name) then
            error(string.format("Variable '%s' is already defined", var.name))
        end
        table.insert(scope.variables, var)
    end

    local function variable_in_scope_get_value(name, value)
        for j = #scope_stack, 1, -1 do
            local scope = scope_stack[j]
            for _, var in ipairs(scope.variables) do
                if var.name == name then
                    return var[value]
                end
            end
        end
        return nil
    end

    local function has_function_in_scope(name)
        for j = #scope_stack, 1, -1 do
            local scope = scope_stack[j]
            for _, func in ipairs(scope.functions) do
                if func.name == name then
                    return true
                end
            end
        end
        return false
    end

    local function add_function_to_scope(func)
        local scope = current_scope()
        table.insert(scope.functions, func)
    end

    local function function_in_scope_get_value(name, value)
        for j = #scope_stack, 1, -1 do
            local scope = scope_stack[j]
            for _, func in ipairs(scope.functions) do
                if func.name == name then
                    return func[value]
                end
            end
        end
        return nil
    end

    function current()
        if i <= #toks then
            return toks[i]
        else
            print(string.format("%s:%s: Attempt to access token out of bounds", toks[i - 1].file, toks[i - 1].line))
            os.exit(1)
        end
    end

    function error(msg)
        print(string.format("%s:%s: %s", current().file, current().line, msg))
        os.exit(1)
    end

    local function expect(type, value)
        local t = current()

        if value ~= nil and t.value ~= value then
            error(string.format("Expected '%s', but found '%s'", value, t.value))
        elseif t.type ~= type then
            error(string.format("Expected '%s', but found '%s'", type, t.value))
        end

        if i <= #toks then
            i = i + 1
        end

        return t
    end

    local function parse_function_call_args()
        if toks[i].type == "parenthesis" and toks[i].value == ")" then
            return {}
        end

        local args = {}
        local start = i
        while i <= #toks and current().value ~= ")" do
            if i ~= start then
                expect("punctuation", ",")
            end

            if current().type == "ellipsis" then
                expect("ellipsis", "...")
                table.insert(args, {type = "ellipsis", name = "...", file = current().file, line = current().line})
            else
                table.insert(args, parse_expression())
            end
        end

        return args
    end

    local function parse_function_call()
        local name = expect("identifier")
        expect("parenthesis", "(")

        local args = parse_function_call_args()
        local decl_args = function_in_scope_get_value(name.value, "args")

        if decl_args ~= nil and #args < #decl_args -1 then -- TODO: remove the -1 if i get problems
            error(string.format("Argument count mismatch for function '%s'", name.value))
        end

        if decl_args ~= nil then
            for j = 1, #args do
                if args[j].value_type ~= decl_args[j].type then
                    if decl_args[j].type == "ellipsis" then break end -- stop checking types
                    error(string.format("Type mismatch for argument '%s' in function '%s': expected '%s', got '%s'", decl_args[j].name, name.value, decl_args[j].type, args[j].value_type))
                end
            end
        end

        expect("parenthesis", ")")

        local value_type = function_in_scope_get_value(name.value, "value_type")

        return {
            type = "function_call",
            name = name.value,
            args = args,
            value_type = value_type,
            line = name.line,
            file = name.file
        }
    end

    local function parse_variable_assignment(mutable)
        local str = ""
        if mutable == true then
            expect("identifier", "let")
            str = "mutable"
        else
            expect("identifier", "const")
            str = "immutable"
        end

        local name = expect("identifier")
        local name_val = name.value
        if has_variable_in_scope(name_val) then
            error(string.format("Variable already defined in current scope: '%s'", name_val))
        end

        local value_type = nil
        if toks[i].value == ":" then
            expect("punctuation", ":")
            value_type = expect("identifier").value
            if value_type == "void" then
                error("Cannot set variable value type as void")
            end
        end

        if current().value == "=" then -- but this will need a null type? 
            expect("operator", "=")

            local expr = parse_expression()

            value_type = value_type or expr.value_type

            if value_type ~= expr.value_type then
                error(string.format("Type mismatch between casted type '%s' and expression type '%s'", value_type, expr.value_type))
            end

            expect("punctuation", ";")

            add_variable_to_scope({name = name_val, mutable = mutable, value_type = value_type})
            return {
                type = str .. "_variable_assignment",
                name = name_val,
                global = is_global_scope(),
                value_type = value_type,
                expression = expr,
                line = name.line,
                file = name.file
            }
        else
            expect("punctuation", ";")
            add_variable_to_scope({name = name_val, mutable = mutable, value_type = value_type})

            return {
                type = str .. "_variable_assignment",
                name = name_val,
                global = is_global_scope(),
                value_type = value_type,
                line = name.line,
                file = name.file,
                null = true
            }
        end
    end

    local function parse_variable_reassignment(no_semi)
        local name = expect("identifier")

        if not variable_in_scope_get_value(name.value, "mutable") then
            error(string.format("Cannot reasign immutable variable: '%s'", name.value))
        end

        expect("operator", "=")
        local expr = parse_expression()
        if no_semi ~= true then
            expect("punctuation", ";")
        end

        return {
            type = "mutable_variable_reassignment",
            name = name.value,
            value_type = expr.value_type,
            expression = expr,
            line = name.line,
            isarg = variable_in_scope_get_value(name.value, "isarg"),
            file = name.file
        }
    end

    local function parse_body()
        local body = {}
        while i <= #toks and not (current().type == "parenthesis" and current().value == "}") do
            local stmt = parse_statement()
            table.insert(body, stmt)
        end

        return body
    end

    local function parse_function_declaration_args()
        local args = {}
        local start = i
        local found_ellipsis = false

        while i <= #toks and toks[i].value ~= ")" do
            if found_ellipsis == true then
                error("Cannot have more arguments after an ellipsis")
            end

            if i ~= start then
                expect("punctuation", ",")
            elseif current().value == ")" and toks[i + 1].type == "parenthesis" and toks[i + 1].value == ")" then
                return {}
            end

            local curr = current()
            if curr.type == "ellipsis" then
                expect("ellipsis", "...")
                found_ellipsis = true
                table.insert(args, {type = "ellipsis", mutable = false})
            elseif curr.type == "parenthesis" and curr.value == "(" then
                expect("parenthesis", "(")
                expect("identifier", "let")
                expect("parenthesis", ")")

                local name = expect("identifier").value
                expect("punctuation", ":")
                local type = expect("identifier").value
                table.insert(args, {name = name, mutable = true, type = type})
            else
                local name = expect("identifier").value
                expect("punctuation", ":")
                local type = expect("identifier").value
                table.insert(args, {name = name, mutable = false, type = type})
            end
        end

        local short = {}
        for _, arg in ipairs(args) do
            table.insert(short, {name = arg.name, type = arg.type})
        end

        return {long = args, short = short}
    end

    local function parse_function_declaration()
        if not is_global_scope() then
            error("Function declaration only in global scope")
        end

        local fn_id = expect("identifier", "fn")
        local line = fn_id.line
        local file = fn_id.file

        local name = expect("identifier").value
        if has_function_in_scope(name) then
            error(string.format("Function already declared in current scope: '%s'", name))
        end

        expect("parenthesis", "(")

        local args_res = parse_function_declaration_args()
        local args = args_res.long
        local short = args_res.short

        expect("parenthesis", ")")
        expect("punctuation", ":")
        local value_type = expect("identifier").value
        add_function_to_scope({name = name, value_type = value_type, args = short})
        enter_scope()
        set_currentfunc(name)

        if args ~= nil then
            for _, arg in ipairs(args) do
                add_variable_to_scope({name = arg.name, mutable = arg.mutable, value_type = arg.type, isarg = true})
            end
        end

        expect("parenthesis", "{")

        if not is_in_table(value_type, {"void", "int", "float", "string"}) then
            error(string.format("Unrecognized return type: '%s'", value_type))
        end

        local body = parse_body()
        if #body ~= 0 then
            if body[#body].type ~= "function_return" then
                if body[#body].type == "if_statement" and body[#body].else_body and body[#body].else_body[#body[#body].else_body].type == "function_return" then
                    table.insert(body, {type = "revision", value = "unreachable", line = current().line}) -- TODO: fix the weird thing with values taking function declaration's value_type
                elseif value_type == "void" then
                    if name == "main" then
                        value_type = "int"
                        table.insert(body, {type = "revision", value = "ret i32 0", line = current().line})
                    else
                        table.insert(body, {type = "revision", value = "ret void", line = current().line})
                    end
                else
                    error(string.format("Missing return statement for function '%s'", name))
                end
            end
        else
            value_type = "void"
            table.insert(body, {type = "revision", value = "ret void"})
        end

        expect("parenthesis", "}")
        exit_scope()

        return {
            type = "function_declaration",
            name =  name,
            args = args,
            value_type = value_type,
            body = body,
            line = line,
            file = file
        }
    end

    local function parse_function_return()
        local ret = expect("identifier", "return")

        local expr = {}
        if current().type ~= "punctuation" and current().value ~= ";" then
            expr = parse_expression()
        end

        local func_value_type = function_in_scope_get_value(get_currentfunc(), "value_type")
        if expr.value_type ~= nil and func_value_type ~= expr.value_type then
            error(string.format("Expected type '%s' for function return, but got '%s' instead", func_value_type, expr.value_type))
        end

        local value_type = expr.value_type
        if value_type == nil then
            if func_value_type == "void" then
                value_type = "void"
            else
                error(string.format("Expected type '%s' for function return, but got 'void' instead", func_value_type))
            end
        end

        expect("punctuation", ";")

        return {
            type = "function_return",
            expression = expr,
            value_type = value_type,
            line = ret.line,
            file = ret.file
        }
    end

    local function parse_if_statement()
        local if_id = expect("identifier", "if")
        expect("parenthesis", "(")

        local cond = parse_expression()

        expect("parenthesis", ")")
        expect("parenthesis", "{")
        enter_scope()

        local body = parse_body()
        expect("parenthesis", "}")
        exit_scope()

        local else_body = nil
        local elseif_statements = {}

        while i <= #toks and current().type == "identifier" and current().value == "else" and toks[i + 1].type == "identifier" and toks[i + 1].value == "if" do
            expect("identifier", "else")
            expect("identifier", "if")
            expect("parenthesis", "(")
            local elseif_cond = parse_expression()
            expect("parenthesis", ")")
            expect("parenthesis", "{")
            enter_scope()
            local elseif_body = parse_body()
            expect("parenthesis", "}")
            exit_scope()
            table.insert(elseif_statements, {condition = elseif_cond, body = elseif_body, line = elseif_cond.line})
        end

        if i <= #toks and current().type == "identifier" and current().value == "else" then
            expect("identifier", "else")
            expect("parenthesis", "{")
            enter_scope()
            else_body = parse_body()
            expect("parenthesis", "}")
            exit_scope()
        end

        return {
            type = "if_statement",
            condition = cond,
            body = body,
            elseif_statements = elseif_statements,
            else_body = else_body,
            line = if_id.line,
            file = if_id.file
        }
    end

    function parse_while_statement()
        local while_id = expect("identifier", "while")
        expect("parenthesis", "(")
        local cond = parse_expression()

        expect("parenthesis", ")")
        expect("parenthesis", "{")
        local body = parse_body()

        enter_scope()
        expect("parenthesis", "}")
        exit_scope()

        return {
            type = "while_statement",
            condition = cond,
            body = body,
            line = while_id.line,
            file = while_id.file
        }
    end

    function parse_for_statement()
        local for_id = expect("identifier", "for")
        expect("parenthesis", "(")
        local var = parse_variable_assignment(true)
        local cond = parse_expression()
        expect("punctuation", ";")
        local iter = parse_statement(true)

        expect("parenthesis", ")")
        expect("parenthesis", "{")
        local body = parse_body()
        table.insert(body, iter)

        enter_scope()
        expect("parenthesis", "}")
        exit_scope()

        return {
            type = "for_statement",
            var = var,
            condition = cond,
            body = body,
            line = for_id.line,
            file = for_id.file
        }
    end

    local function parse_fstring(t)
        i = i + 1
        local value = t.value:gsub("\\n", "\\0A"):gsub("%%", "%%%%")

        local parts = {}
        local j = 1
        while j <= #value do
            if value:sub(j, j) == "{" then
                local h = j + 1
                while h <= #value and value:sub(h, h) ~= "}" do
                    h = h + 1
                end
                if h > #value then
                    error("Unmatched '{' in fstring")
                end
                table.insert(parts, {type = "expression", value = value:sub(j + 1, h - 1)})
                j = h + 1
            else
                local h = j
                while h <= #value and value:sub(h, h) ~= "{" do
                    h = h + 1
                end
                table.insert(parts, {type = "text", value = value:sub(j, h - 1)})
                j = h
            end
        end

        local exprs = {}
        for _, part in ipairs(parts) do
            if part.type == "expression" then
                local expr_toks = lex(part.value, t.file, t.line)
                local expr_prepro = preprocess(expr_toks)
                local expr = parse(expr_prepro, scope_stack, true)
                local formatter = ""
                local value_type = expr.value_type

                if value_type == "int" then
                    formatter = "%%i"
                elseif value_type == "float" then
                    formatter = "%%i"
                elseif value_type == "string" then
                    formatter = "%%s"
                else
                    error("Unrecognized type for fstring")
                end

                table.insert(exprs, expr)
                value = value:gsub("{" .. part.value:gsub("%%", "") .. "}", formatter)
            end
        end

        local length = #value + 1
        for _ in string.gmatch(value, "\\0A") do
            length = length - 2
        end

        return {type = "fstring", value = value, expressions = exprs, length = length, value_type = "string", line = t.line, file = t.file}
    end

    function parse_expression()
        local function parse_primary()
            local t = current()
            if t.type == "identifier" then
                if toks[i + 1].type == "parenthesis" and toks[i + 1].value == "(" then
                    if not has_function_in_scope(t.value) then
                        error(string.format("Function not defined in current scope: '%s'", t.value))
                    end
                    return parse_function_call()
                elseif t.value == "true" then
                    i = i + 1
                    return {type = "bool", value = 1, value_type = "bool", line = t.line, file = t.file}
                elseif t.value == "false" then
                    i = i + 1
                    return {type = "integer", value = 0, value_type = "bool", line = t.line, file = t.file}
                else
                    if not has_variable_in_scope(t.value) then
                        error(string.format("Variable not defined in current scope: '%s'", t.value))
                    end
                    i = i + 1

                    local value_type = variable_in_scope_get_value(t.value, "value_type")
                    local isarg = variable_in_scope_get_value(t.value, "isarg")
                    local mutable = variable_in_scope_get_value(t.value, "mutable")

                    return {type = "variable", name = t.value, value_type = value_type, isarg = isarg, line = t.line, file = t.file, mutable = mutable}
                end
            elseif t.type == "integer" then
                i = i + 1
                return {type = "integer", value = t.value, value_type = "int", line = t.line, file = t.file}
            elseif t.type == "float" then
                i = i + 1
                return {type = "float", value = t.value, value_type = "float", line = t.line, file = t.file}
            elseif t.type == "string" then
                i = i + 1
                local value = t.value:gsub("\\n", "\\0A"):gsub("%%", "%%%%")
                local length = #value + 1
                for _ in string.gmatch(value, "\\0A") do
                    length = length - 2
                end
                return {type = "string", value = value, length = length, value_type = "string", line = t.line, file = t.file}
            elseif t.type == "fstring" then
                return parse_fstring(t)
            elseif t.type == "parenthesis" and t.value == "(" then
                i = i + 1
                local expr = parse_expression()
                expect("parenthesis", ")")
                return expr
            elseif t.type == "parenthesis" and t.value == "}" then
                i = i + 1
                error(string.format("Unexpected token in primary expression: '%s', could be missing ';' on line above", t.value))
            else
                error(string.format("Unexpected token in primary expression: '%s'", t.value))
            end
        end

        local function parse_unary()
            local t = current()
            if t.type == "operator" and (t.value == "-" or t.value == "+") then
                i = i + 1
                local expr = parse_unary()
                if expr.value_type ~= "int" and expr.value_type ~= "float" then
                    error(string.format("Unary operator '%s' cannot be applied to type '%s'", t.value, expr.value_type))
                end
                return {type = "unary_expression", operator = t.value, operand = expr, value_type = expr.value_type, line = t.line, file = t.file}
            else
                return parse_primary()
            end
        end

        local function parse_term()
            local expr = parse_unary()
            while i <= #toks and current().type == "operator" and (current().value == "*" or current().value == "/" or current().value == "%") and current().type ~= "punctuation" and current().value ~= ";" do
                local op = expect("operator").value
                local right = parse_unary()
                if expr.value_type ~= right.value_type then
                    error(string.format("Type mismatch in binary expression: '%s' and '%s'", expr.value_type, right.value_type))
                end
                if expr.value_type == "string" then
                    error(string.format("Operator '%s' cannot be applied to type 'string'", op))
                end
                expr = {type = "binary_expression", operator = op, left = expr, right = right, value_type = expr.value_type, line = expr.line, file = expr.file}
            end
            return expr
        end

        local function parse_comparison()
            local expr = parse_term()
            while i <= #toks and current().type == "operator" and (current().value == "==" or current().value == "!=" or current().value == ">=" or current().value == "<=" or current().value == ">" or current().value == "<") and current().type ~= "punctuation" and current().value ~= ";" do
                local op = expect("operator").value
                local right = parse_term()
                if expr.value_type ~= right.value_type then
                    error(string.format("Type mismatch in comparison expression: '%s' and '%s'", expr.value_type, right.value_type))
                end
                expr = {type = "comparison_expression", operator = op, left = expr, right = right, line = expr.line, file = expr.file}
            end
            return expr
        end

        local expr = parse_comparison()
        while i <= #toks and current().type == "operator" and is_in_table(current().value, {"+", "-"}) and current().type ~= "punctuation" and current().value ~= ";" do
            local op = current().value
            i = i + 1
            local right = parse_comparison()
            if expr.value_type ~= right.value_type then
                error(string.format("Type mismatch in binary expression: '%s' and '%s'", expr.value_type, right.value_type))
            end
            if expr.value_type == "string" and op ~= "+" then
                error(string.format("Operator '%s' cannot be applied to type 'string'", op))
            end
            expr = {type = "binary_expression", operator = op, left = expr, right = right, value_type = expr.value_type, line = expr.line, file = expr.file}
        end
        return expr
    end

    local function parse_expression_as_statement()
        local expr = parse_expression()
        expect("punctuation", ";")
        return expr
    end

    function parse_statement(no_semi)
        local curr = current()
        local type = curr.type
        local value = curr.value

        if type == "identifier" then
            if value == "fn" then
                return parse_function_declaration()
            elseif value == "if" then
                return parse_if_statement()
            elseif value == "while" then
                return parse_while_statement()
            elseif value == "for" then
                return parse_for_statement()
            elseif value == "let" then
                return parse_variable_assignment(true)
            elseif value == "const" then
                return parse_variable_assignment(false)
            elseif value == "return" then
                return parse_function_return()
            elseif has_variable_in_scope(value) then
                return parse_variable_reassignment(no_semi)
            elseif has_function_in_scope(value) then
                return parse_expression_as_statement()
            else
                error(string.format("Attempt to reference undefined: '%s'", value))
            end
        elseif type == "revision" and value == "unreachable" then
            i = i + 1
            return {type = "revision", value = "unreachable", line = current().line}
        else
            error(string.format("Cannot parse '%s' as statement", value))
        end
    end

    local function add_standard_functions()
        add_function_to_scope({name = "print", value_type = "void"})
        add_function_to_scope({name = "vprintf", value_type = "void"})
        add_function_to_scope({name = "sprintf", value_type = "void"})
        add_function_to_scope({name = "exit", value_type = "void"})
    end

    enter_scope()
    add_standard_functions()

    while i <= #toks do
        local node
        if expression == true then
            node = parse_expression()
        else
            node = parse_statement()
        end
        table.insert(ast, node)
    end

    exit_scope()

    return ast
end

function generate_llvm(ast)
    local llvm = {}
    local top_code = {}
    local var_counter = 0
    local label_counter = 0
    local indent = 0
    local line = 1
    local file = ""
    local has_variadic = false

    local function error(msg)
        print(string.format("%s:%s: %s", file, line, msg))
        os.exit(1)
    end

    local function new_var(old)
        if not old or old == false then
            var_counter = var_counter + 1
        end
        return "%.tmp_" .. var_counter
    end

    local function new_label()
        label_counter = label_counter + 1
        return "lbl_" .. label_counter
    end

    local function emit_top(str)
        for _, code in ipairs(top_code) do
            if code == str then
                return
            end
        end
        table.insert(top_code, str)
    end

    local function convert_type(type)
        if type == "int" then
            return "i32"
        elseif type == "float" then
            return "double"
        elseif type == "string" then
            return "i8*"
        elseif type == "void" then
            return "void"
        elseif type == "bool" then
            return "i1"
        else
            error(string.format("Unsupported value type: '%s'", type))
        end
    end

    local function emit(str)
        if str == "}" or str:sub(-1) == ":" then
            indent = 0
        end

        local indent_str = string.rep("    ", indent)
        table.insert(llvm, indent_str .. str)

        if str:sub(-1) == ":" then
            indent = 1
        end
    end

    local function generate_assignment(assignment)
        line = assignment.line
        file = assignment.file

        local expr
        if not assignment.null then
            expr = generate_expression(assignment.expression)
        end

        local value_type = assignment.value_type
        local type = convert_type(value_type)
        local isarg = assignment.isarg

        
        if assignment.type == "immutable_variable_assignment" or assignment.type == "mutable_variable_assignment" then
            if assignment.global == true then
                error("Global variables not implemented")
            end
            if assignment.isarg then
                emit("%a." .. assignment.name .. " = alloca " .. type)
            else
                emit("%" .. assignment.name .. " = alloca " .. type)
            end
            if expr then
                if isarg then
                    emit("store " .. type .. " " .. expr .. ", " .. type .. "* %a." .. assignment.name)
                else
                    emit("store " .. type .. " " .. expr .. ", " .. type .. "* %" .. assignment.name)
                end
            end
        elseif assignment.type == "mutable_variable_reassignment" then
            if isarg then
                emit("store " .. type .. " " .. expr .. ", " .. type .. "* %a." .. assignment.name)
            else
                emit("store " .. type .. " " .. expr .. ", " .. type .. "* %" .. assignment.name)
            end
        else
            error(string.format("Unsupported assignment type: '%s'", assignment.type))
        end
    end

    local function generate_body(body)
        local i = 1
        while i <= #body do
            generate_statement(body[i])
            i = i + 1
        end
    end

    local function generate_function_declaration(func)
        line = func.line
        file = func.file

        local name = func.name
        local args = func.args
        local body = func.body
        local value_type = func.value_type

        local args_str = ""
        local i = 1
        has_variadic = false


        while i <= #args do
            if i ~= 1 then
                args_str = args_str .. ", "
            end

            local arg = args[i]
            if arg.type == "ellipsis" then
                args_str = args_str .. "..."
                has_variadic = true
            else
                local type = convert_type(arg.type)
                args_str = args_str .. type .. " %" .. arg.name
            end

            i = i + 1
        end

        local type = convert_type(value_type)

        emit("")
        emit(string.format("define %s @%s(%s) {", type, name, args_str))

        i = 1
        while i <= #args do
            if i == 1 then
                emit("args:")
            end
            local arg = args[i]
            if arg.type ~= "ellipsis" then
                local type = convert_type(arg.type)
                emit("%a." .. arg.name .. " = alloca " .. type)
                emit("store " .. type .. " %" .. arg.name .. ", " .. type .. "* %a." .. arg.name)
            end

            i = i + 1
        end

        if #args > 0 then
            emit("br label %entry")
        end
        emit("entry:")


        if has_variadic then
            emit_top("declare void @llvm.va_start(i8*)")
            emit_top("declare void @llvm.va_end(i8*)")
            emit("%.va_list = alloca i8, i32 128")
            emit("call void @llvm.va_start(i8* %.va_list)")
        end

        generate_body(body)
        emit("}")
    end

    local function generate_function_return(statement)
        line = statement.line
        file = statement.file

        local expression = statement.expression
        local value_type = statement.value_type

        local type = convert_type(value_type)

        if has_variadic then
            emit("call void @llvm.va_end(i8* %.va_list)")
        end

        if type == "void" then
            emit(string.format("ret void"))
        else
            local expr = generate_expression(expression)
            emit(string.format("ret %s %s", type, expr))
        end
    end

    local function generate_comparison_check(left, right, operator)
        line = left.line
        file = left.file

        local left_var = generate_expression(left)
        local right_var = generate_expression(right)
        local result_var = new_var()

        if left.value_type == "int" and right.value_type == "int" then
            if operator == "==" then
                emit(string.format("%s = icmp eq i32 %s, %s", result_var, left_var, right_var))
            elseif operator == "!=" then
                emit(string.format("%s = icmp ne i32 %s, %s", result_var, left_var, right_var))
            elseif operator == ">" then
                emit(string.format("%s = icmp sgt i32 %s, %s", result_var, left_var, right_var))
            elseif operator == "<" then
                emit(string.format("%s = icmp slt i32 %s, %s", result_var, left_var, right_var))
            elseif operator == ">=" then
                emit(string.format("%s = icmp sge i32 %s, %s", result_var, left_var, right_var))
            elseif operator == "<=" then
                emit(string.format("%s = icmp sle i32 %s, %s", result_var, left_var, right_var))
            else
                error(string.format("Unsupported comparison operator: '%s'", operator))
            end
        elseif left.value_type == "float" and right.value_type == "float" then
            if operator == "==" then
                emit(string.format("%s = fcmp oeq double %s, %s", result_var, left_var, right_var))
            elseif operator == "!=" then
                emit(string.format("%s = fcmp one double %s, %s", result_var, left_var, right_var))
            elseif operator == ">" then
                emit(string.format("%s = fcmp ogt double %s, %s", result_var, left_var, right_var))
            elseif operator == "<" then
                emit(string.format("%s = fcmp olt double %s, %s", result_var, left_var, right_var))
            elseif operator == ">=" then
                emit(string.format("%s = fcmp oge double %s, %s", result_var, left_var, right_var))
            elseif operator == "<=" then
                emit(string.format("%s = fcmp ole double %s, %s", result_var, left_var, right_var))
            else
                error(string.format("Unsupported comparison operator: '%s'", operator))
            end
        else
            error(string.format("Unsupported value types for comparison: '%s' and '%s'", left.value_type, right.value_type))
        end

        return result_var
    end

    local function generate_condition_check(condition)
        line = condition.line
        file = condition.file

        if condition.type == "comparison_expression" then
            return generate_comparison_check(condition.left, condition.right, condition.operator)
        elseif condition.type == "variable" then
            local var = generate_expression(condition)
            local result_var = new_var()
            emit(string.format("%s = icmp ne %s %s, 0", result_var, convert_type(condition.value_type), var))
            return result_var
        elseif condition.type == "integer" then
            local result_var = new_var()
            emit(string.format("%s = icmp ne i32 %d, 0", result_var, condition.value))
            return result_var
        elseif condition.type == "float" then
            local result_var = new_var()
            emit(string.format("%s = fcmp one double %f, 0.0", result_var, condition.value))
            return result_var
        elseif condition.type == "bool" then
            local result_var = new_var()
            emit(string.format("%s = icmp ne i32 %d, 0", result_var, condition.value))
            return result_var
        else
            error(string.format("Unsupported condition type: '%s'", condition.type))
        end
    end

    local function generate_if_statement(statement)
        line = statement.line
        file = statement.file

        local condition = statement.condition
        local body = statement.body
        local else_body = statement.else_body
        local else_if_stmts = statement.elseif_statements

        local cond_var = generate_condition_check(condition)
        local then_label = new_label()
        local else_label = new_label()
        local end_label = new_label()

        emit(string.format("br i1 %s, label %%%s, label %%%s", cond_var, then_label, else_label))
        emit(then_label .. ":")
        generate_body(body)
        emit(string.format("br label %%%s", end_label))

        emit(else_label .. ":")
        if #else_if_stmts > 0 then
            for _, elseif_stmt in ipairs(else_if_stmts) do
            local elseif_cond_var = generate_condition_check(elseif_stmt.condition)
            local elseif_then_label = new_label()
            local elseif_else_label = new_label()

            emit(string.format("br i1 %s, label %%%s, label %%%s", elseif_cond_var, elseif_then_label, elseif_else_label))
            emit(elseif_then_label .. ":")
            generate_body(elseif_stmt.body)
            emit(string.format("br label %%%s", end_label))

            emit(elseif_else_label .. ":")
            else_label = elseif_else_label
            end
        end

        if else_body then
            generate_body(else_body)
        end

        emit(string.format("br label %%%s", end_label))
        emit(end_label .. ":")
    end

    local function generate_while_statement(statement)
        line = statement.line
        file = statement.file

        local cond_label = new_label()
        local body_label = new_label()
        local end_label = new_label()

        emit(string.format("br label %%%s", cond_label))

        emit(cond_label .. ":")
        local cond_var = generate_condition_check(statement.condition)
        emit(string.format("br i1 %s, label %%%s, label %%%s", cond_var, body_label, end_label))

        emit(body_label .. ":")
        generate_body(statement.body)
        emit(string.format("br label %%%s", cond_label))

        emit(end_label .. ":")
    end

    local function generate_for_statement(statement)
        line = statement.line
        file = statement.file

        local cond_label = new_label()
        local body_label = new_label()
        local end_label = new_label()

        generate_statement(statement.var)
        emit(string.format("br label %%%s", cond_label))

        emit(cond_label .. ":")
        local cond_var = generate_condition_check(statement.condition)
        emit(string.format("br i1 %s, label %%%s, label %%%s", cond_var, body_label, end_label))

        emit(body_label .. ":")
        generate_body(statement.body)
        emit(string.format("br label %%%s", cond_label))

        emit(end_label .. ":")
    end

    local function generate_function_call(expression)
        line = expression.line
        file = expression.file

        local args = expression.args
        local name = expression.name
        local type = convert_type(expression.value_type)

        local args_str = ""

        local i = 1
        while i <= #args do
            if i ~= 1 then
                args_str = args_str .. ", "
            end

            local arg = args[i]
            if arg.type == "ellipsis" then
                args_str = args_str .. "i8* %.va_list"
                i = i + 1
            else
                local expr = generate_expression(arg)
                local type = convert_type(arg.value_type)
                args_str = args_str .. type .. " " .. expr
                i = i + 1
            end
        end

        if name == "print" then
            name = "printf"
            emit_top("declare void @printf(i8*, ...)")
        elseif name == "exit" then
            emit_top("declare void @exit(i32)")
        elseif name == "vprintf" then
            emit_top("declare void @vprintf(i8*, i8*)")
        elseif name == "sprintf" then
            emit_top("declare i32 @sprintf(i8*, i8*, ...)")
        end

        if type == "void" then
            emit(string.format("call %s @%s(%s)", type, name, args_str))
        else
            local result_var = new_var()
            emit(string.format("%s = call %s @%s(%s)", result_var, type, name, args_str))
            return result_var
        end
    end

    function generate_expression(expression)
        line = expression.line
        file = expression.file

        if expression.type == "binary_expression" then
            local left = expression.left
            local right = expression.right
            local operator = expression.operator
            local left_var = generate_expression(left)
            local right_var = generate_expression(right)
            local result_var = new_var()

            if operator == "+" then
                if expression.value_type == "int" then
                    emit(string.format("%s = add i32 %s, %s", result_var, left_var, right_var))
                elseif expression.value_type == "float" then
                    emit(string.format("%s = fadd double %s, %s", result_var, left_var, right_var))
                elseif expression.value_type == "string" then
                    error("String concatenation not supported yet")
                else
                    error(string.format("Unexpected value type for '+': '%s'", expression.value_type))
                end
            elseif operator == "-" then
                if expression.value_type == "int" then
                    emit(string.format("%s = sub i32 %s, %s", result_var, left_var, right_var))
                elseif expression.value_type == "float" then
                    emit(string.format("%s = fsub double %s, %s", result_var, left_var, right_var))
                else
                    error(string.format("Unexpected value type for '-': '%s'", expression.value_type))
                end
            elseif operator == "*" then
                if expression.value_type == "int" then
                    emit(string.format("%s = mul i32 %s, %s", result_var, left_var, right_var))
                elseif expression.value_type == "float" then
                    emit(string.format("%s = fmul double %s, %s", result_var, left_var, right_var))
                else
                    error(string.format("Unexpected value type for '*': '%s'", expression.value_type))
                end
            elseif operator == "/" then
                if expression.value_type == "int" then
                    emit(string.format("%s = sdiv i32 %s, %s", result_var, left_var, right_var))
                elseif expression.value_type == "float" then
                    emit(string.format("%s = fdiv double %s, %s", result_var, left_var, right_var))
                else
                    error(string.format("Unexpected type found: '%s'", expression.value_type))
                end
            elseif operator == "%" then
                if expression.value_type == "int" then
                    emit(string.format("%s = urem i32 %s, %s", result_var, left_var, right_var))
                elseif expression.value_type == "float" then
                    emit(string.format("%s = frem double %s, %s", result_var, left_var, right_var))
                else
                    error(string.format("Unexpected value type for '%%': '%s'", expression.value_type))
                end
            else
                error(string.format("Unexpected binary operator: '%s'", operator))
            end

            return result_var
        elseif expression.type == "comparison_expression" then
            local left = expression.left
            local right = expression.right
            local operator = expression.operator
            local left_var = generate_expression(left)
            local right_var = generate_expression(right)
            local result_var = new_var()

            if operator == "==" then
                if expression.value_type == "int" then
                    emit(string.format("%s = icmp eq i32 %s, %s", result_var, left_var, right_var))
                elseif expression.value_type == "float" then
                    emit(string.format("%s = fcmp oeq double %s, %s", result_var, left_var, right_var))
                else
                    error(string.format("Unexpected value type for '==': '%s'", expression.value_type))
                end
            elseif operator == "!=" then
                if expression.value_type == "int" then
                    emit(string.format("%s = icmp ne i32 %s, %s", result_var, left_var, right_var))
                elseif expression.value_type == "float" then
                    emit(string.format("%s = fcmp one double %s, %s", result_var, left_var, right_var))
                else
                    error(string.format("Unexpected value type for '!=': '%s'", expression.value_type))
                end
            elseif operator == ">" then
                if expression.value_type == "int" then
                    emit(string.format("%s = icmp sgt i32 %s, %s", result_var, left_var, right_var))
                elseif expression.value_type == "float" then
                    emit(string.format("%s = fcmp ogt double %s, %s", result_var, left_var, right_var))
                else
                    error(string.format("Unexpected value type for '>': '%s'", expression.value_type))
                end
            elseif operator == "<" then
                if expression.value_type == "int" then
                    emit(string.format("%s = icmp slt i32 %s, %s", result_var, left_var, right_var))
                elseif expression.value_type == "float" then
                    emit(string.format("%s = fcmp olt double %s, %s", result_var, left_var, right_var))
                else
                    error(string.format("Unexpected value type for '<': '%s'", expression.value_type))
                end
            elseif operator == ">=" then
                if expression.value_type == "int" then
                    emit(string.format("%s = icmp sge i32 %s, %s", result_var, left_var, right_var))
                elseif expression.value_type == "float" then
                    emit(string.format("%s = fcmp oge double %s, %s", result_var, left_var, right_var))
                else
                    error(string.format("Unexpected value type for '>=': '%s'", expression.value_type))
                end
            elseif operator == "<=" then
                if expression.value_type == "int" then
                    emit(string.format("%s = icmp sle i32 %s, %s", result_var, left_var, right_var))
                elseif expression.value_type == "float" then
                    emit(string.format("%s = fcmp ole double %s, %s", result_var, left_var, right_var))
                else
                    error(string.format("Unexpected value type for '<=': '%s'", expression.value_type))
                end
            else
                error(string.format("Unexpected comparison operator: '%s'", operator))
            end

            return result_var
        elseif expression.type == "integer" then
            return tostring(expression.value)
        elseif expression.type == "float" then
            return tostring(expression.value)
        elseif expression.type == "string" then
            local str = expression.value
            local str_name = "@.str_" .. #top_code
            local str_len = expression.length
            emit_top(string.format('%s = private unnamed_addr constant [%d x i8] c"%s\\00"', str_name, str_len, str))
            return str_name
        elseif expression.type == "fstring" then
            local str = expression.value
            local str_name = "@.str_" .. #top_code
            local str_len = expression.length
            local var = new_var()

            emit_top(string.format('%s = private unnamed_addr constant [%d x i8] c"%s\\00"', str_name, str_len, str))
            emit_top("declare i32 @sprintf(i8*, i8*, ...)")

            emit(string.format("%s = alloca i8*", var))
            local tab = {}
            for _, comma_value in ipairs(expression.comma_values) do
                if _ == 1 then
                    table.insert(tab, "")
                end
                if comma_value:match("%%[a-zA-Z_]") then
                    local temp_var = new_var()
                    emit(string.format("%s = load %s, %s* %s", temp_var, comma_value:match("^(%S+)"), comma_value:match("^(%S+)"), comma_value:match("%s(%S+)$")))
                    table.insert(tab, comma_value:match("^(%S+)") .. " " .. temp_var)
                else
                    table.insert(tab, comma_value)
                end
            end

            emit(string.format("call void @sprintf(i8* %s, i8* %s%s)", var, str_name, table.concat(tab, ", ")))

            return var
        elseif expression.type == "variable" then
            local value_type = convert_type(expression.value_type)
            local isarg = expression.isarg

            if isarg then
                local var = new_var()
                emit(var .. " = load " .. value_type .. ", " .. value_type .. "* %a." .. expression.name)
                return var
            else
                local var = new_var()
                emit(var .. " = load " .. value_type .. ", " .. value_type .. "* %" .. expression.name)
                return var
            end
        elseif expression.type == "function_call" then
            return generate_function_call(expression)
        elseif expression.type == "unary_expression" then
            local operand_var = generate_expression(expression.operand)
            local result_var = new_var()

            if expression.operator == "-" then
                if expression.value_type == "int" then
                    emit(string.format("%s = sub i32 0, %s", result_var, operand_var))
                elseif expression.value_type == "float" then
                    emit(string.format("%s = fsub double 0.0, %s", result_var, operand_var))
                else
                    error(string.format("Unsupported value type for unary expression: '%s'", expression.value_type))
                end
            elseif expression.operator == "+" then
                return operand_var
            else
                error(string.format("Unsupported unary operator: '%s'", expression.operator))
            end

            return result_var
        else
            error(string.format("Unsupported expression type: '%s'", expression.type))
        end
    end

    local function generate_revision(statement)
        emit(statement.value)
    end

    function generate_statement(statement)
        local type = statement.type
        if type == "function_declaration" then
            generate_function_declaration(statement)
        elseif type == "immutable_variable_assignment" or type == "mutable_variable_assignment" or type == "mutable_variable_reassignment" then
            generate_assignment(statement)
        elseif type == "function_return" then
            generate_function_return(statement)
        elseif type == "function_call" then
            generate_expression(statement)
        elseif type == "if_statement" then
            generate_if_statement(statement)
        elseif type == "while_statement" then
            generate_while_statement(statement)
        elseif type == "for_statement" then
            generate_for_statement(statement)
        elseif type == "revision" then
            generate_revision(statement)
        elseif type == "ignore" then
        else
            error(string.format("Unrecognized statement type found: '%s'", type))
        end
    end

    for _, node in ipairs(ast) do
        generate_statement(node)
    end

    return table.concat(top_code, "\n") .. "\n" .. table.concat(llvm, "\n")
end

local function compile()
    local file_name = arg[1]
    if not file_name then
        file_name = "test.zk"
    end

    local trimmed_name = file_name:match("([^/]+)%.%w+$")

    local file = io.open(file_name, "rb")
    if not file then
        print("No file")
        os.exit(1)
    end

    local content = file:read("a")
    file:close()

    local toks = lex(content, file_name)
    local preprocessed = preprocess(toks)
    --print(inspect(preprocessed))
    local ast = parse(preprocessed)
    --print(inspect(ast))
    local llvm = generate_llvm(ast)

    local out = io.open("out/" .. trimmed_name .. ".ll", "w")
    if out == nil then
        print("file is nil")
        os.exit(1)
    end

    out:write(llvm)
    out:close()

    if is_in_table("-opt", arg) then
        os.execute(string.format("opt -Oz -S ./out/%s.ll -o ./out/%s.ll", trimmed_name, trimmed_name))
    end
    os.execute(string.format("clang ./out/%s.ll -o ./out/%s.out", trimmed_name, trimmed_name))

    if is_in_table("-run", arg) then
        os.execute(string.format("./out/%s.out", trimmed_name))
    end
end

compile()