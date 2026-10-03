from __future__ import annotations

import src.zelkelc.ast.values as values
from dataclasses import dataclass

@dataclass
class ClassSignature:
    name: str
    real_name: str
    members: list[MemberSignature]
    methods: list[FunctionSignature]

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
    typ: values.Value
    
@dataclass
class MemberSignature:
    name: str
    real_name: str
    mutable: bool
    typ: values.Value
    member_idx: int

@dataclass
class Scope:
    classes: ClassSignature
    functions: FunctionSignature
    variables: ValueSignature