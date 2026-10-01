from dataclasses import dataclass

import src.zelkelc.ast.nodes as nodes
import src.zelkelc.ast.values as values

@dataclass
class IRTable:
    structs: str
    struct_functions: str
    global_functions: str

class Codegen:
    ast: list[nodes.FunctionDeclaration | nodes.ClassDeclaration]
    ir: IRTable
    
    def __init__(self, ast: list[nodes.FunctionDeclaration | nodes.ClassDeclaration]):
        self.ast = ast
        self.ir = IRTable("", "", "")
        
    def generate(self) -> str:
        for node in self.ast:
            match node:
                case nodes.FunctionDeclaration():
                    self.ir.global_functions += self.gen_function_declaration(node)
                case nodes.ClassDeclaration():
                    self.ir.structs += self.gen_class_declaration(node)
        
        return self.ir.structs + "\n" + self.ir.struct_functions + "\n" + self.ir.global_functions

    def gen_class_declaration(self, node: nodes.ClassDeclaration) -> str:
        members_str = ""
        for i, m in enumerate(node.members):
            members_str += f"{", " if i != 0 else ""}{m.typ.to_llvm()}"
        
        return f"\n%{node.name} = type " + "{ " + members_str + " }"
    
    def gen_function_declaration(self, node: nodes.FunctionDeclaration) -> str:
        args_str = ""
        for i, (name, typ) in enumerate(node.args.items()):
            args_str += f"{", " if i != 0 else ""}{typ.to_llvm()} %{name}"
        
        ir = f"define {str(node.typ.to_llvm())} @{node.name}({args_str}) " + "{"
        ir += "\nentry:"
        
        for child_node in node.body:
            match child_node:
                case nodes.ValueDeclaration():
                    ir += self.gen_value_declaration(child_node)
                case _:
                    continue
        
        return ir + "\n}"
    
    def gen_value_declaration(self, node: nodes.ValueDeclaration) -> str:
        ir = "\n"
        
        if node.mutable == True: # TODO: both are hardcoded to just put int: 16
            ir += f"%{node.name} = alloca {node.typ.to_llvm()}, align 8" # TODO: this would only work for i64s, just testing
            ir += f"\nstore {node.typ.to_llvm()} 16, {node.typ.to_llvm()}* %{node.name}, align 8"
        else:
            ir += f"%{node.name} = add {node.typ.to_llvm()} 16, 0"
            
        return ir