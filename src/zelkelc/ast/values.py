from dataclasses import dataclass

@dataclass
class Integer:
    value: int

@dataclass
class String:
    value: str

@dataclass
class Void:
    pass

@dataclass
class Variable:
    name: str

Value = Integer | String | Void | Variable