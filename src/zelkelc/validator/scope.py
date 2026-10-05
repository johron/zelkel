from __future__ import annotations

import src.zelkelc.ast.values as values
import src.zelkelc.ast.nodes as nodes
from dataclasses import dataclass
from typing import Optional


@dataclass
class SemanticError:
    message: str


@dataclass
class StructSignature:
    name: str
    real_name: str
    members: dict[str, MemberSignature]

@dataclass
class ClassSignature:
    name: str
    real_name: str
    members: dict[str, MemberSignature]
    methods: dict[str, FunctionSignature]

@dataclass 
class FunctionSignature:
    name: str
    real_name: str
    static: bool
    typ: values.Value
    args: dict[str, values.Value]
    
@dataclass
class ValueSignature:
    name: str
    real_name: str
    mutable: bool
    typ: type[values.Integer64] | type[values.String] | type[values.Void]
    
@dataclass
class MemberSignature:
    name: str
    real_name: str
    mutable: bool
    typ: type[values.Integer64] | type[values.String] | type[values.Void]
    member_idx: int


@dataclass
class Scope:
    parent: Optional[Scope]
    classes: dict[str, ClassSignature]
    structs: dict[str, StructSignature]
    functions: dict[str, FunctionSignature]
    variables: dict[str, ValueSignature]

    @staticmethod
    def root() -> Scope:
        return Scope(
            parent=None,
            classes = {},
            structs = {},
            functions = {},
            variables = {},
        )

    def child(self) -> Scope:
        return Scope(
            parent = self,
            classes = self.classes,
            structs = self.structs,
            functions = self.functions,
            variables = {},
        )

    def resolve_variable(self, name: str) -> Optional[ValueSignature]:
        current: Optional[Scope] = self
        while current is not None:
            if name in current.variables:
                return current.variables[name]
            current = current.parent
        return None


class ScopeChecker:
    global_scope: Scope
    errors: list[SemanticError]

    def __init__(self):
        self.global_scope = Scope.root()
        self.errors = []

    def check(self, tree: list[nodes.Node]) -> tuple[Scope, list[SemanticError]]:
        self._collect_globals(tree)

        for top_level in tree:
            match top_level:
                case nodes.FunctionDeclaration():
                    self._check_function(top_level, self.global_scope)
                case nodes.ClassDeclaration():
                    for method in top_level.methods:
                        self._check_function(method, self.global_scope)
                case nodes.StructDeclaration():
                    continue
                case _:
                    self._error(f"Unsupported top-level node: {type(top_level)}")

        return self.global_scope, self.errors

    def _collect_globals(self, tree: list[nodes.Node]) -> None:
        for top_level in tree:
            match top_level:
                case nodes.StructDeclaration(name=name, real_name=real_name, members=members):
                    if name in self.global_scope.structs:
                        self._error(f"Struct '{name}' is already declared")
                        continue

                    member_table: dict[str, MemberSignature] = {}
                    for member in members:
                        if member.name in member_table:
                            self._error(f"Duplicate member '{member.name}' in struct '{name}'")
                            continue
                        member_table[member.name] = MemberSignature(
                            name = member.name,
                            real_name = f"{real_name}.m_{member.name}",
                            mutable = member.mutable,
                            typ = member.typ,
                            member_idx = member.real_idx,
                        )

                    self.global_scope.structs[name] = StructSignature(
                        name=name,
                        real_name=real_name,
                        members=member_table,
                    )

                case nodes.ClassDeclaration(name=name, real_name=real_name, members=members, methods=methods):
                    if name in self.global_scope.classes:
                        self._error(f"Class '{name}' is already declared")
                        continue

                    member_table: dict[str, MemberSignature] = {}
                    for idx, member in enumerate(members):
                        if member.name in member_table:
                            self._error(f"Duplicate member '{member.name}' in class '{name}'")
                            continue
                        member_table[member.name] = MemberSignature(
                            name = member.name,
                            real_name = f"{real_name}.m_{member.name}",
                            mutable = member.mutable,
                            typ = member.typ,
                            member_idx = idx,
                        )

                    method_table: dict[str, FunctionSignature] = {}
                    for method in methods:
                        if method.name in method_table:
                            self._error(f"Duplicate method '{method.name}' in class '{name}'")
                            continue
                        method_table[method.name] = FunctionSignature(
                            name = method.name,
                            real_name = method.real_name,
                            static = False,
                            typ = method.typ,
                            args = method.args,
                        )

                    self.global_scope.classes[name] = ClassSignature(
                        name = name,
                        real_name = real_name,
                        members = member_table,
                        methods = method_table,
                    )

                case nodes.FunctionDeclaration(name=name, real_name=real_name, typ=typ, args=args):
                    if name in self.global_scope.functions:
                        self._error(f"Function '{name}' is already declared")
                        continue

                    self.global_scope.functions[name] = FunctionSignature(
                        name = name,
                        real_name = real_name,
                        static = True,
                        typ = typ,
                        args = args,
                    )

                case _:
                    self._error(f"Unsupported top-level node while collecting symbols: {type(top_level)}")

    def _check_function(self, fn: nodes.FunctionDeclaration, enclosing_scope: Scope) -> None:
        function_scope = enclosing_scope.child()

        for arg_name, arg_type in fn.args.items():
            if arg_name in function_scope.variables:
                self._error(f"Duplicate argument '{arg_name}' in function '{fn.name}'")
                continue
            function_scope.variables[arg_name] = ValueSignature(
                name = arg_name,
                real_name = arg_name,
                mutable = False,
                typ = arg_type,
            )

        has_return = False

        for child in fn.body:
            match child:
                case nodes.ValueDeclaration():
                    if child.name in function_scope.variables:
                        self._error(f"Variable '{child.name}' is already defined in function '{fn.name}'")
                        continue

                    expression_type = self._infer_expression_type(child.expr, function_scope)
                    if expression_type is not None and not self._same_type(expression_type, child.typ):
                        self._error(
                            f"Type mismatch for '{child.name}' in function '{fn.name}': "
                            f"expected {self._type_name(child.typ)}, got {self._type_name(expression_type)}"
                        )

                    function_scope.variables[child.name] = ValueSignature(
                        name = child.name,
                        real_name = child.real_name,
                        mutable = child.mutable,
                        typ = child.typ,
                    )

                case nodes.ReturnStatement():
                    has_return = True
                    if child.expr is None:
                        if fn.typ is not values.Void:
                            self._error(f"Function '{fn.name}' must return {self._type_name(fn.typ)}")
                        continue

                    return_type = self._infer_expression_type(child.expr, function_scope)
                    if return_type is None:
                        continue

                    if fn.typ is values.Void:
                        self._error(f"Function '{fn.name}' returns a value, but return type is void")
                        continue

                    if not self._same_type(return_type, fn.typ):
                        self._error(
                            f"Return type mismatch in function '{fn.name}': "
                            f"expected {self._type_name(fn.typ)}, got {self._type_name(return_type)}"
                        )

                case nodes.FunctionDeclaration():
                    if child.name in function_scope.functions:
                        self._error(f"Nested function '{child.name}' is already defined in '{fn.name}'")
                    else:
                        function_scope.functions[child.name] = FunctionSignature(
                            name = child.name,
                            real_name = child.real_name,
                            static = True,
                            typ = child.typ,
                            args = child.args,
                        )
                    self._check_function(child, function_scope)

                case _:
                    self._error(f"Unsupported node in function '{fn.name}': {type(child)}")

        if not has_return and fn.typ is not values.Void:
            self._error(f"Function '{fn.name}' has no return statement")

    def _infer_expression_type(self, expr: nodes.Expression, active_scope: Scope) -> Optional[type[values.Integer64] | type[values.String] | type[values.Void]]:
        match expr:
            case nodes.PrimaryExpression(value=values.Integer64()):
                return values.Integer64
            case nodes.PrimaryExpression(value=values.String()):
                return values.String
            case nodes.PrimaryExpression(value=values.Variable(name=name)):
                signature = active_scope.resolve_variable(name)
                if signature is None:
                    self._error(f"Unknown variable '{name}'")
                    return None
                return signature.typ
            case nodes.UnaryExpression(expr=inner_expr, sign=sign):
                inner_type = self._infer_expression_type(inner_expr, active_scope)
                if inner_type is None:
                    return None
                if sign in ("+", "-") and inner_type is not values.Integer64:
                    self._error(
                        f"Unary operator '{sign}' expects i64, got {self._type_name(inner_type)}"
                    )
                    return None
                return inner_type
            case nodes.BinaryExpression(left=left, right=right, op=op):
                left_type = self._infer_expression_type(left, active_scope)
                right_type = self._infer_expression_type(right, active_scope)
                if left_type is None or right_type is None:
                    return None
                if not self._same_type(left_type, right_type):
                    self._error(
                        f"Binary operator '{op}' type mismatch: "
                        f"{self._type_name(left_type)} vs {self._type_name(right_type)}"
                    )
                    return None
                if left_type is not values.Integer64:
                    self._error(
                        f"Operator '{op}' currently only supports i64, got {self._type_name(left_type)}"
                    )
                    return None
                return values.Integer64
            case _:
                self._error(f"Unsupported expression type: {type(expr)}")
                return None

    def _same_type(self, left: object, right: object) -> bool:
        return self._type_token(left) == self._type_token(right)

    def _type_token(self, typ: object) -> str:
        if typ is values.Integer64 or isinstance(typ, values.Integer64):
            return "i64"
        if typ is values.String or isinstance(typ, values.String):
            return "str"
        if typ is values.Void or isinstance(typ, values.Void):
            return "void"
        return str(typ)

    def _type_name(self, typ: object) -> str:
        return self._type_token(typ)

    def _error(self, message: str) -> None:
        self.errors.append(SemanticError(message))