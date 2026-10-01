from __future__ import annotations

import src.zelkelc.ast.nodes as nodes
import src.zelkelc.ast.values as values
from dataclasses import dataclass

@dataclass
class ClassInfo:
    name: str
    members: list[ValueInfo]
    methods: list[FunctionInfo]

@dataclass 
class FunctionInfo:
    name: str
    static: bool
    typ: values.Value
    args: dict[str, values.Value]
    
@dataclass
class ValueInfo:
    name: str
    mutable: bool
    typ: values.Value

@dataclass
class Scope:
    classes: nodes.ClassDeclaration
    functions: nodes.FunctionDeclaration    
    variables: nodes.ValueDeclaration
    child: Scope | None