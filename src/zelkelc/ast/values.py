from dataclasses import dataclass

@dataclass
class Integer64:
    value: int
    
    def to_llvm() -> str:
        return "i64"

@dataclass
class String:
    value: str
    
    def to_llvm() -> str:
        print("Cannot convert String to llvm ir")
        exit(1)

@dataclass
class Void:
    pass

    def to_llvm() -> str:
        return "void"

@dataclass
class Variable:
    name: str
    
    def to_llvm() -> str:
        print("Cannot convert Variable to llvm ir")
        exit(1)

Value = Integer64 | String | Void | Variable