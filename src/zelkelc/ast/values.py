from dataclasses import dataclass

@dataclass
class Integer:
    value: int

@dataclass
class String:
    value: str

Value = Integer | String