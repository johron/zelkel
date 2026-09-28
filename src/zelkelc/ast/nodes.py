from __future__ import annotations

from dataclasses import dataclass
import src.zelkelc.ast.values as values

@dataclass
class ClassDeclaration:
    name: str
    members: list[ValueDeclaration]
    methods: list[FunctionDeclaration]

@dataclass
class ValueDeclaration:
    name: str
    typ: values.Value
    expr: Expression
    
@dataclass
class FunctionDeclaration:
    name: str
    args: dict[str, values.Value]
    typ: values.Value
    body: list[Node]

Node = ClassDeclaration | ValueDeclaration | FunctionDeclaration

@dataclass
class BinaryExpression:
    left: Expression
    right: Expression
    op: str

@dataclass
class UnaryExpression:
    expr: Expression
    sign: str

@dataclass
class PrimaryExpression:
    value: values.Value

Expression = BinaryExpression | UnaryExpression | PrimaryExpression