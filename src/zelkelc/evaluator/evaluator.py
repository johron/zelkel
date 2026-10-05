from __future__ import annotations

import src.zelkelc.ast.nodes as nodes
import src.zelkelc.evaluator.scope as scope


class SemanticCheckFailed(Exception):
	pass


def validate_ast(tree: list[nodes.Node]) -> scope.Scope:
	checker = scope.ScopeChecker()
	global_scope, errors = checker.check(tree)

	if len(errors) > 0:
		pretty_errors = "\n".join(f"- {err.message}" for err in errors)
		raise SemanticCheckFailed(f"AST semantic validation failed:\n{pretty_errors}")

	return global_scope


class Evaluator:
	scope: scope.Scope

	def __init__(self, tree: list[nodes.Node]):
		self.scope = validate_ast(tree)
