import src.zelkelc.ast.nodes as nodes
import src.zelkelc.ast.values as values

class Codegen:
    ast: list[nodes.FunctionDeclaration | nodes.ClassDeclaration]
    
    def __init__(self, ast: list[nodes.FunctionDeclaration | nodes.ClassDeclaration]):
        self.ast = ast
        
    def generate(self) -> str:
        ir = ""
        
        return ir

    def gen_class_declaration(self) -> str:
        