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

Value = Integer | String | Void