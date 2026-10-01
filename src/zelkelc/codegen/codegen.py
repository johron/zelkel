from dataclasses import dataclass

import src.zelkelc.ast.nodes as nodes
import src.zelkelc.ast.values as values

@dataclass
class IRTable:
    structs: str
    struct_functions: str
    functions: str

class Codegen:
    ast: list[nodes.FunctionDeclaration | nodes.ClassDeclaration]
    ir: IRTable
    
    def __init__(self, ast: list[nodes.FunctionDeclaration | nodes.ClassDeclaration]):
        self.ast = ast
        self.ir = IRTable("", "", "")
        
    def generate(self):        
        for node in self.ast:
            match node:
                case nodes.FunctionDeclaration():
                    print("TODO: functiondeclaration codegen unimplemented")
                    continue
                case nodes.ClassDeclaration():
                    self.gen_class_declaration(node)
        
        return self.ir.structs + "\n" + self.ir.struct_functions + "\n" + self.ir.functions

    def gen_class_declaration(self, node: nodes.ClassDeclaration):
        member_types: list[str] = []
        for m in node.members:
            member_types.append(str(m.typ))
        member_str = ", ".join(member_types)
        
        self.ir.structs += f"\n%{node.name} = type { {member_str} }"