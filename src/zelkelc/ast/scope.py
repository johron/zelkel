from __future__ import annotations

import src.zelkelc.ast.values as values
from dataclasses import dataclass

@dataclass
class ClassSignature:
    name: str
    members: list[ValueSignature]
    methods: list[FunctionSignature]

@dataclass 
class FunctionSignature:
    name: str
    static: bool
    typ: values.Value
    args: dict[str, values.Value]
    
@dataclass
class ValueSignature:
    name: str
    mutable: bool
    typ: values.Value

@dataclass
class Scope:
    classes: ClassSignature
    functions: FunctionSignature
    variables: ValueSignature