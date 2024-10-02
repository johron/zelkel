local inspect = require("inspect") -- debugging

local function is_in_table(v, t)
    for _, j in ipairs(t) do
        if j == v then
            return true
        end
    end

    return false
end

local function lex(input, file)
    local toks = {}
    local line = 1

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
        if is_alpha(c) then
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
        elseif is_in_table(c, {"+", "-", "*", "/", "="}) then
            local cmt = ""
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
            table.insert(toks, {type = "newline", value = "\n"})
            line = line + 1
            i = i + 1
        elseif c == " " then -- disregard
            i = i + 1
        else
            error(string.format("Unexpected character found by lexer: '%s'", c))
        end
    end

    return toks
end

local function parse(toks, file)
    local line = 1
    local ast = {}
    local i = 1

    local function luaerror(msg)
        error(msg)
    end

    local function error(msg)
        print(string.format("%s:%s: %s", file, line, msg))
        os.exit(1)
    end

    local scope_stack = {}

    local function current_scope()
        return scope_stack[#scope_stack] or scope_stack[#scope_stack - 1] or {variables = {}, functions = {}}
    end

    local function enter_scope()
        local parent_scope = current_scope()
        local new_scope = {
            variables = {table.unpack(parent_scope.variables)}, -- remove table.unpack ? TODO: 
            functions = {table.unpack(parent_scope.functions)}
        }
        table.insert(scope_stack, new_scope)
    end

    local function exit_scope()
        if #scope_stack == 0 then
            error(string.format("Scope out of bounds"))
        end
        table.remove(scope_stack)
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
        table.insert(scope.variables, var)
    end

    local function variable_in_scope_set_value(name, value, v)
        for j = #scope_stack, 1, -1 do
            local scope = scope_stack[j]
            for _, var in ipairs(scope.variables) do
                if var.name == name then
                    var[value] = v
                    return
                end
            end
        end
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

    local function add_function_to_scope(name, value_type)
        local scope = current_scope()
        table.insert(scope.functions, {name = name, value_type = value_type})
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

    local function current()
        if toks[i] then
            return toks[i]
        else
            error("Attempt to access token out of bounds")
        end
    end

    local function expect(type, value)
        local t = current()
        if t == nil then
            error("current() is nil")
        end

        if t.type == "newline" then
            line = line + 1
            i = i + 1
            return expect(type, value)
        end

        if value ~= nil and t.value ~= value then
            error(string.format("Expected '%s', but found '%s'", value, t.value))
        elseif t.type ~= type then
            error(string.format("Expected '%s', but found '%s'", type, t.value))
        end

        i = i + 1
        return t
    end

    local function parse_function_call_args()
        if toks[i].type == "parenthesis" and toks[i].value == ")" then
            return {}
        end

        local args = {}
        local start = i
        while i <= #toks and current().type ~= "parenthesis" and current().value ~= ")" do
            if i ~= start then
                expect("punctuation", ",")
            end
            table.insert(args, parse_expression())
        end

        return args
    end

    local function parse_function_call()
        local name = expect("identifier").value
        expect("parenthesis", "(")

        local args = parse_function_call_args()

        expect("parenthesis", ")")

        local value_type = function_in_scope_get_value(name, "value_type")

        return {
            type = "function_call",
            name = name,
            args = args,
            value_type = value_type
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

        local name = expect("identifier").value
        if has_variable_in_scope(name) then
            error(string.format("Variable already defined in current scope: '%s'", name))
        end

        expect("punctuation", ":")
        local value_type = expect("identifier").value
        if value_type == "void" then
            error("Cannot set variable value type as void")
        end

        expect("operator", "=")

        local expr = parse_expression()
        expect("punctuation", ";")

        add_variable_to_scope({name = name, mutable = mutable, value_type = expr.value_type, counter = 0})
        return {
            type = str .. "_variable_assignment",
            name = name,
            counter = 0,
            value_type = value_type,
            expression = expr
        }
    end

    local function parse_variable_reassignment()
        local name = expect("identifier").value

        if not variable_in_scope_get_value(name, "mutable") then
            error(string.format("Cannot reasign immutable variable: '%s'", name))
        end

        expect("operator", "=")
        local expr = parse_expression()
        expect("punctuation", ";")

        variable_in_scope_set_value(name, "counter", variable_in_scope_get_value(name, "counter") + 1)

        return {
            counter = variable_in_scope_get_value(name, "counter"),
            type = "mutable_variable_reassignment",
            name = name,
            value_type = expr.value_type,
            expression = expr
        }
    end

    local function parse_body()
        local body = {}
        while i <= #toks and not (current().type == "parenthesis" and current().value == "}") do
            if current().type == "parenthesis" or current().value == "}" then
                return body
            end

            local stmt = parse_statement()
            while stmt == nil and toks[i].value ~= "}" do
                stmt = parse_statement()
            end
            table.insert(body, stmt)
        end

        return body
    end

    local function parse_function_declaration_args()
        local args = {}
        local start = i
        while i <= #toks and toks[i].type ~= "parenthesis" and toks[i].value ~= ")" do
            if i ~= start then
                expect("punctuation", ",")
            elseif toks[i + 1].type == "parenthesis" and toks[i + 1].value == ")" then
                return {}
            end

            local name = expect("identifier").value
            expect("punctuation", ":")
            local type = expect("identifier").value

            table.insert(args, {name = name, mutable = false, type = type})
        end

        return args
    end

    local function parse_function_declaration()
        expect("identifier", "fn")
        local name = expect("identifier").value
        if has_function_in_scope(name) then
            error(string.format("Function already declared in current scope: '%s'", name))
        end

        expect("parenthesis", "(")

        local args = parse_function_declaration_args()

        expect("parenthesis", ")")
        expect("punctuation", ":")
        local type = expect("identifier").value
        add_function_to_scope(name, type)
        enter_scope()

        if args ~= nil then
            for _, arg in ipairs(args) do
                add_variable_to_scope({name = arg.name, mutable = arg.mutable, value_type = arg.type, counter = 0, isarg = true})
            end
        end

        expect("parenthesis", "{")

        if not is_in_table(type, {"void", "int", "float"}) then
            error(string.format("Unrecognized return type: '%s'", type))
        end

        local body = parse_body()
        expect("parenthesis", "}")
        exit_scope()

        return {
            type = "function_declaration",
            name = name,
            args = args,
            value_type = type,
            body = body
        }
    end

    local function parse_function_return()
        expect("identifier", "return")

        local expr = {}
        if current().type ~= "punctuation" and current().value ~= ";" then
            expr = parse_expression()
        end

        expect("punctuation", ";")

        return {
            type = "function_return",
            expression = expr,
            value_type = expr.value_type or "void"
        }
    end

    local function parse_if_statement()
        expect("identifier", "if")
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
            table.insert(elseif_statements, {condition = elseif_cond, body = elseif_body})
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
            else_body = else_body
        }
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
                else
                    if not has_variable_in_scope(t.value) then
                        error(string.format("Variable not defined in current scope: '%s'", t.value))
                    end
                    i = i + 1
                    local value_type = variable_in_scope_get_value(t.value, "value_type")
                    local counter = variable_in_scope_get_value(t.value, "counter")
                    local isarg = variable_in_scope_get_value(t.value, "isarg")
                    return {type = "variable", name = t.value, value_type = value_type, counter = counter, isarg = isarg}
                end
            elseif t.type == "integer" then
                i = i + 1
                return {type = "integer", value = t.value, value_type = "int"}
            elseif t.type == "float" then
                i = i + 1
                return {type = "float", value = t.value, value_type = "float"}
            elseif t.type == "string" then
                i = i + 1
                return {type = "string", value = t.value, value_type = "string"}
            elseif t.type == "parenthesis" and t.value == "(" then
                i = i + 1
                local expr = parse_expression()
                expect("parenthesis", ")")
                return expr
            elseif t.type == "newline" and t.value == "\n" then
                i = i + 1
                line = line + 1
                return parse_primary()
            elseif t.type == "parenthesis" and t.value == "}" then
                i = i + 1
                error(string.format("Unexpected token in primary expression: '%s', could be missing ';' on line above", t.value))
            else
                print(inspect(t))
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
                return {type = "unary_expression", operator = t.value, operand = expr, value_type = expr.value_type}
            else
                return parse_primary()
            end
        end

        local function parse_term()
            local expr = parse_unary()
            while i <= #toks and current().type == "operator" and (current().value == "*" or current().value == "/") and current().type ~= "punctuation" and current().value ~= ";" do
                local op = current().value
                i = i + 1
                local right = parse_unary()
                if expr.value_type ~= right.value_type then
                    error(string.format("Type mismatch in binary expression: '%s' and '%s'", expr.value_type, right.value_type))
                end
                if expr.value_type == "string" then
                    error(string.format("Operator '%s' cannot be applied to type 'string'", op))
                end
                expr = {type = "binary_expression", operator = op, left = expr, right = right, value_type = expr.value_type}
            end
            return expr
        end

        local function parse_comparison()
            local expr = parse_term()
            while i <= #toks and current().type == "operator" and (current().value == "==" or current().value == "!=" or current().value == ">=" or current().value == "<=" or current().value == ">" or current().value == "<") and current().type ~= "punctuation" and current().value ~= ";" do
                local op = current().value
                i = i + 1
                local right = parse_term()
                if expr.value_type ~= right.value_type then
                    error(string.format("Type mismatch in comparison expression: '%s' and '%s'", expr.value_type, right.value_type))
                end
                expr = {type = "comparison_expression", operator = op, left = expr, right = right, value_type = "bool"}
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
            expr = {type = "binary_expression", operator = op, left = expr, right = right, value_type = expr.value_type}
        end
        return expr
    end

    local function parse_expression_as_statement()
        local expr = parse_expression()
        expect("punctuation", ";")
        return expr
    end

    function parse_statement()
        local curr = current()

        local type = curr.type
        local value = curr.value

        if type == "identifier" then
            if value == "fn" then
                return parse_function_declaration()
            elseif value == "if" then
                return parse_if_statement()
            elseif value == "let" then
                return parse_variable_assignment(true)
            elseif value == "const" then
                return parse_variable_assignment(false)
            elseif value == "return" then
                return parse_function_return()
            elseif has_variable_in_scope(value) then
                return parse_variable_reassignment()
            elseif has_function_in_scope(value) then
                return parse_expression_as_statement()
            else
                error(string.format("Unrecognized token found while parsing: '%s'", value))
            end
        elseif type == "newline" and value == "\n" then
            line = line + 1
            i = i + 1
            return nil
        else
            error(string.format("Unexpected token found while parsing statement: '%s'", value))
        end
    end

    local function add_standard_functions()
        add_function_to_scope("print", "void")
        add_function_to_scope("string", "string")
    end

    enter_scope()
    add_standard_functions()

    while i <= #toks do
        local node = parse_statement()
        table.insert(ast, node)
    end

    exit_scope()

    return ast
end

local function generate_llvm(ast, file)
    local llvm = {}
    local top_code = {}
    local var_counter = 0
    local indent = 0

    local function error(msg)
        print(string.format("%s: %s", file, msg))
        os.exit(1)
    end

    local function new_var()
        var_counter = var_counter + 1
        return "%t" .. var_counter
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
        else
            error(string.format("Unsupported value type: '%s'", type))
        end
    end

    local function emit(str)
        if str == "}" then
            indent = indent - 1
        end

        local indent_str = string.rep("    ", indent)
        table.insert(llvm, indent_str .. str)

        if str:sub(-1) == ":" then
            indent = indent + 1
        end
    end

    local function generate_assignment(assignment)
        local expr = generate_expression(assignment.expression)
        local value_type = assignment.value_type
        if assignment.type == "immutable_variable_assignment" or assignment.type == "mutable_variable_assignment" then
            local type = convert_type(value_type)
            emit("%" .. assignment.name .. "_" .. assignment.counter .. " = alloca " .. type)
            emit("store " .. type .. " " .. expr .. ", " .. type .. "* %" .. assignment.name .. "_" .. assignment.counter)
        elseif assignment.type == "mutable_variable_reassignment" then
            local type = convert_type(value_type)
            emit("%" .. assignment.name .. "_" .. assignment.counter .. " = alloca " .. type)
            emit("store " .. convert_type(value_type) .. " " .. expr .. ", " .. convert_type(value_type) .. "* %" .. assignment.name .. "_" .. assignment.counter)
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
        local name = func.name
        local args = func.args
        local body = func.body
        local value_type = func.value_type

        local args_str = ""
        local i = 1
        while i <= #args do
            if i ~= 1 then
                args_str = args_str .. ", "
            end

            local arg = args[i]
            local type = convert_type(arg.type)
            args_str = args_str .. type .. " %" .. arg.name .. "_0"

            i = i + 1
        end

        local type = convert_type(value_type)

        emit("")
        if type == "void" then
            emit(string.format("define i32 @%s(%s) {", name, args_str))
        else
            emit(string.format("define %s @%s(%s) {", type, name, args_str))
        end
        emit("entry:")
        generate_body(body)
        emit("}")
    end

    local function generate_function_return(statement)
        local expression = statement.expression
        local value_type = statement.value_type

        local type = convert_type(value_type)

        if type == "void" then
            emit(string.format("ret i32 0"))
        else
            local expr = generate_expression(expression)
            emit(string.format("ret %s %s", type, expr))
        end
    end

    function generate_expression(expression)
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
            else
                error(string.format("Unexpected binary operator: '%s'", operator))
            end

            return result_var
        elseif expression.type == "integer" then
            return tostring(expression.value)
        elseif expression.type == "float" then
            return tostring(expression.value)
        elseif expression.type == "string" then
            local str = expression.value:gsub("\\n", "\\0A")
            local str_var = new_var()
            local str_name = "@.str_" .. #top_code -- TODO: fix length
            local str_len = #str + 1
            for _ in string.gmatch(str, "\\0A") do
                str_len = str_len - 2
            end
            table.insert(top_code, string.format('%s = private unnamed_addr constant [%d x i8] c"%s\\00"', str_name, str_len, str))
            emit(string.format("%s = getelementptr [%d x i8], [%d x i8]* %s, i32 0, i32 0", str_var, str_len, str_len, str_name))
            return str_var
        elseif expression.type == "variable" then
            local value_type = convert_type(expression.value_type)
            local var = new_var()
            if not expression.isarg or expression.isarg ~= true then
                emit(var .. " = load " .. value_type .. ", " .. value_type .. "* " .. "%" .. expression.name .. "_" .. expression.counter)
                return var
            end
            return "%" .. expression.name .. "_" .. expression.counter
        elseif expression.type == "function_call" then
            local args = expression.args
            local name = expression.name
            local value_type = expression.value_type

            local args_str = ""
            local type = convert_type(value_type)

            local i = 1
            while i <= #args do
                if i ~= 1 then
                    args_str = args_str .. ", "
                end

                local arg = args[i]
                local expr = generate_expression(arg)
                local type = convert_type(arg.value_type)

                args_str = args_str .. type .. " " .. expr
                i = i + 1
            end

            if name == "string" then
                args_str = ""
                for i, arg in ipairs(args) do
                    if arg.value_type == "int" then
                        local buffer = new_var()
                        emit(string.format("%s = alloca [12 x i8]", buffer))
                        emit(string.format("call i32 (i8*, i8*, ...) @sprintf(i8* %s, i8* getelementptr ([3 x i8], [3 x i8]* @.int_fmt, i32 0, i32 0), i32 %s)", buffer, generate_expression(arg)))

                        emit_top("declare i32 @sprintf(i8*, ...)")
                        emit_top("@.int_fmt = constant [3 x i8] c\"%d\\00\"")

                        return buffer
                    elseif arg.value_type == "float" then
                        local buffer = new_var()
                        emit(string.format("%s = alloca [32 x i8]", buffer))
                        emit(string.format("call i32 (i8*, i8*, ...) @sprintf(i8* %s, i8* getelementptr ([3 x i8], [3 x i8]* @.float_fmt, i32 0, i32 0), double %s)", buffer, generate_expression(arg)))

                        emit_top("declare i32 @sprintf(i8*, ...)")
                        emit_top("@.float_fmt = constant [3 x i8] c\"%f\\00\"")

                        return buffer
                    else
                        error("Unrecognized type for string function: " .. arg.value_type)
                    end
                end
            else
                if name == "print" then
                    name = "printf"
                    emit_top("declare i32 @printf(i8*, ...)")
                end
                if type == "void" then
                    emit(string.format("call %s @%s(%s)", type, name, args_str))
                else
                    local result_var = new_var()
                    emit(string.format("%s = call %s @%s(%s)", result_var, type, name, args_str))
                    return result_var
                end
            end
        elseif expression.type == "unary_expression" then
            local operand_var = generate_expression(expression.operand)
            local result_var = new_var()

            if expression.operator == "-" then
                emit(string.format("%s = sub i32 0, %s", result_var, operand_var))
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
        elseif type == "ignore" or type == "newline" then
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
    local file = io.open(file_name, "rb")
    if not file then print("No file") os.exit(1)  end
    local content = file:read("a")
    file:close()

    local toks = lex(content, file_name)
    local ast = parse(toks, file_name)

    print(inspect(ast))

    local llvm = generate_llvm(ast, file_name)

    local out = io.open("out/" .. file_name .. ".ll", "w")
    if out == nil then
        print("file is nil")
        os.exit(1)
    end

    out:write(llvm)
    out:close()

    os.execute(string.format("clang ./out/%s.ll -o ./out/%s.out", file_name, file_name))
end

compile()