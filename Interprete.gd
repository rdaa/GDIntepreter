extends Node

var global_symbol_table = SymbolTable.new()

func _ready():
	global_symbol_table.set("NULL", Number.new(0))
	global_symbol_table.set("TRUE", Number.new(1))
	global_symbol_table.set("FALSE", Number.new(0))


#################################
# CONSTANTS
#################################

const DIGITS = '0123456789'
const LETTERS = 'abcdefghijklmnñopqrstuvwxyzABCDEFGHIJKLMNÑOPQRSTUVWXYZ'
const LETTERS_DIGITS = LETTERS + DIGITS

########################################
# ERRORS
########################################

class Error:
	var error_name
	var details
	var pos_start
	var pos_end
	
	func _init(_pos_start, _pos_end, _error_name, _details):
		self.pos_start = _pos_start
		self.pos_end = _pos_end
		self.error_name = _error_name
		self.details = _details
		
	func as_string():
		var result = str(self.error_name) + ":" + str(self.details)
		result += " File " + str(self.pos_start.fn) + ", line " + str(self.pos_end.ln + 1)
		return result

class IllegalCharError extends Error:
	func _init(_pos_start, _pos_end, _error_name, _details).(_pos_start, _pos_end, _error_name,_details):
		._init(_pos_start, _pos_end, 'Illegal Character',_details)

#Error usado cuando escribimos ! pero no !=
class ExpectedCharError extends Error:
	func _init(_pos_start, _pos_end, _error_name, _details).(_pos_start, _pos_end, _error_name,_details):
		._init(_pos_start, _pos_end, 'Expected Character',_details)

class InvalidSyntaxError extends Error:
	func _init(_pos_start, _pos_end, _error_name, _details).(_pos_start, _pos_end, _error_name,_details):
		._init(_pos_start, _pos_end, 'Illegal Syntax',_details)


class RTError extends Error:
	var context
	func _init(_pos_start, _pos_end, _error_name, _details,_context).(_pos_start, _pos_end, _error_name,_details):
		._init(_pos_start, _pos_end, 'Runtime Error',_details)
		self.context = _context 
		
	func as_string():
		var result
		result = self.generate_traceback()
		result += str(self.error_name) + ":" + str(self.details)
		result += " File " + str(self.pos_start.fn) + ", line " + str(self.pos_end.ln + 1)
		return result
		
	func generate_traceback():
		var result = ''
		var pos = self.pos_start
		var ctx = self.context
		
		while ctx:
			result += " File " + str(pos.fn) + ", line " + str(pos.ln + 1) + ", in " + str(ctx.display_name) + "\n"
			pos = ctx.parent_entry_pos
			ctx = ctx.parent
			
		return "Traceback (most recent call last): \n" + result
		
########################################
# POSITION
########################################

class Position:
	var idx
	var ln
	var col
	var fn
	var ftxt
	
	func _init(_idx, _ln, _col, _fn, _ftxt):
		self.idx = _idx
		self.ln = _ln
		self.col = _col
		self.fn = _fn
		self.ftxt = _ftxt
	
	func advance(current_char = null):
		self.idx += 1
		self.col += 1
		
		if current_char == '\n':
			self.col = 0
			self.ln += 1
		return (self)
	
	func copy():
		return Position.new(self.idx, self.ln, self.col, self.fn, self.ftxt)

########################################
# TOKENS
########################################

const TT_INT         = 'TT_INT'
const TT_FLOAT       = 'FLOAT'
const TT_IDENTIFIER  = 'IDENTIFIER'
const TT_KEYWORD     = 'KEYWORD'
const TT_PLUS        = 'PLUS'
const TT_MINUS       = 'MINUS'
const TT_MUL         = 'MUL'
const TT_DIV         = 'DIV'
const TT_POW         = 'POW'
const TT_EQ          = 'EQ'
const TT_LPAREN      = 'LPAREN'
const TT_RPAREN      = 'RPAREN'
const TT_EE             = 'EE'
const TT_NE             = 'NE'
const TT_LT             = 'LT'
const TT_GT             = 'GT'
const TT_LTE            = 'LTE'
const TT_GTE            = 'GTE'
const TT_COMMA          = 'COMMA'
const TT_ARROW          = 'ARROW'
const TT_EOF         = 'EOF'

const KEYWORDS = [
	'VAR',
	'AND',
	'OR',
	'NOT',
	'IF',
	'THEN',
	'ELIF',
	'THEN',
	'ELSE',
	'FOR',
	'TO',
	'STEP',
	'WHILE',
	'FUNC'
]

class Token:
	var type
	var value
	var pos_start
	var pos_end
	
	func _init(_type,_value = null, _pos_start = null, _pos_end = null):
		type = _type
		value = _value
		
		if _pos_start:
			self.pos_start = _pos_start.copy()
			self.pos_end = _pos_start.copy()
			self.pos_end.advance()
		
		if _pos_end:
			self.pos_end = _pos_end.copy()
			
	func matches(_type, _value):
		return self.type == _type and self.value == _value

	func _to_string():
		if self.value: return str(self.type) + ":" + str(self.value)
		return str(self.type)
		

########################################
# LEXER
########################################

class Lexer:
	var text
	var pos
	var current_char
	var fn
	
	func _init(_fn, _text):
		self.fn = _fn
		self.text = _text
		self.pos = Position.new(-1, 0, -1, _fn, _text)
		self.current_char = null
		self.advance()
		
	func advance():
		self.pos.advance(self.current_char)
		self.current_char = self.text[self.pos.idx] if self.pos.idx < len(self.text) else null
		
		
	func make_tokens():
		var tokens = []
		
		while self.current_char != null:
			if current_char in ' \t':
				self.advance()
			elif self.current_char in DIGITS:
				tokens.append(self.make_number())
			elif self.current_char in LETTERS:
				tokens.append(self.make_identifier())
			elif self.current_char == '+':
				tokens.append(Token.new(TT_PLUS,null, self.pos))
				self.advance()
			elif self.current_char == '-':
				#Puede ser un simbolo menos o el inicio de la flecha
				tokens.append(self.make_minus_or_arrow())
			elif self.current_char == '*':
				tokens.append(Token.new(TT_MUL,null, self.pos))
				self.advance()
			elif self.current_char == '/':
				tokens.append(Token.new(TT_DIV,null, self.pos))
				self.advance()
			elif self.current_char == '^':
				tokens.append(Token.new(TT_POW, null, self.pos))
				self.advance()
			elif self.current_char == '(':
				tokens.append(Token.new(TT_LPAREN,null, self.pos))
				self.advance()
			elif self.current_char == ')':
				tokens.append(Token.new(TT_RPAREN,null, self.pos))
				self.advance()
			elif self.current_char == "!":
				var tok
				var error
				[tok,error] = self.make_not_equals()
				if error: return [[], error]
				tokens.append(tok)
			elif self.current_char == '=':
				tokens.append(self.make_equals())
			elif self.current_char == '<':
				tokens.append(self.make_less_than())
			elif self.current_char == '>':
				tokens.append(self.make_greater_than())
			elif self.current_char == ',':
				tokens.append(Token.new(TT_COMMA,null, self.pos))
				self.advance()
			
			else:
				var pos_start = self.pos.copy()
				var chara = self.current_char
				self.advance()
				return [[], IllegalCharError.new(pos_start,self.pos,"","'" + chara + "'")]
				
		tokens.append(Token.new(TT_EOF, null, self.pos))
		return [tokens, null]
		
	func make_number():
		var num_str = ''
		var dot_count = 0
		var pos_start = self.pos.copy()
		
		while self.current_char != null and self.current_char in DIGITS + '.':
			if self.current_char == '.':
				if dot_count == 1: break
				dot_count += 1
				num_str += '.'
			else:
				num_str += self.current_char
			self.advance()
			
		if dot_count == 0:
			return Token.new(TT_INT, int(num_str), pos_start, self.pos)
		else:
			return Token.new(TT_FLOAT, float(num_str), pos_start, self.pos)
			
	func make_identifier():
		var id_str = ''
		var pos_start = self.pos.copy()
		var tok_type
		
		while self.current_char != null and self.current_char in LETTERS_DIGITS + '_':
			id_str += self.current_char
			self.advance()
			
		if id_str in KEYWORDS:
			tok_type = TT_KEYWORD 
		else:
			tok_type = TT_IDENTIFIER
		return Token.new(tok_type, id_str, pos_start, self.pos)
		
	func make_minus_or_arrow():
		var tok_type =  TT_MINUS
		var pos_start = self.pos.copy()
		self.advance()
		
		if self.current_char == '>':
			self.advance()
			tok_type = TT_ARROW
		
		return Token.new(tok_type, null, pos_start, self.pos)
	
	func make_not_equals():
		var pos_start = self.pos.copy()
		self.advance()
		
		if self.current_char == '=':
			self.advance()
			return [Token.new(TT_NE, null, pos_start, self.pos), null]
		
		self.advance()
		return [null, ExpectedCharError.new(pos_start,self.pos, "", "'=' (after '!')")]
		
	func make_equals():
		var tok_type = TT_EQ
		var pos_start = self.pos.copy()
		self.advance()
		
		if self.current_char == '=':
			self.advance()
			tok_type = TT_EE
		
		return Token.new(tok_type, null, pos_start, self.pos)
#		return [Token.new(tok_type, null, pos_start, self.pos), null]
		
	func make_less_than():
		var tok_type = TT_LT
		var pos_start = self.pos.copy()
		self.advance()
		
		if self.current_char == '=':
			self.advance()
			tok_type = TT_LTE
		
		return Token.new(tok_type, null, pos_start, self.pos)
#		return [Token.new(tok_type, null, pos_start, self.pos), null]
	
	func make_greater_than():
		var tok_type = TT_GT
		var pos_start = self.pos.copy()
		self.advance()
		
		if self.current_char == '=':
			self.advance()
			tok_type = TT_GTE
		
		return Token.new(tok_type, null, pos_start, self.pos)
#		return [Token.new(tok_type, null, pos_start, self.pos), null]

########################################
# NODES
########################################

class NumberNode:
	var tok
	var pos_start
	var pos_end
	var type = "NumberNode"
	
	func _init(_tok):
		self.tok = _tok
		pos_start = self.tok.pos_start
		pos_end = self.tok.pos_end
		
	func _to_string():
		return str(self.tok)


class VarAccessNode:
	var var_name_tok
	var pos_start
	var pos_end
	var type = "VarAccessNode"
	
	func _init(_var_name_tok):
		self.var_name_tok = _var_name_tok
		self.pos_start = self.var_name_tok.pos_start
		self.pos_end = self.var_name_tok.pos_end


class VarAssignNode:
	var var_name_tok
	var value_node
	var pos_start
	var pos_end
	var type = "VarAssignNode"
	
	func _init(_var_name_tok, _value_node):
		self.var_name_tok = _var_name_tok
		self.value_node = _value_node
		
		self.pos_start = self.var_name_tok.pos_start
		self.pos_end = self.var_name_tok.pos_end
		


class BinOpNode:
	var left_node
	var right_node
	var op_tok
	var pos_start
	var pos_end
	var type = "BinOpNode"
	
	func _init(_left_node, _op_tok, _right_node):
		self.left_node = _left_node
		self.op_tok = _op_tok
		self.right_node = _right_node
		pos_start = self.left_node.pos_start
		pos_end = self.right_node.pos_end
		
	func _to_string():
		return "(" + str(self.left_node) + ", " + str(self.op_tok) + ", " + str(self.right_node) + ")"
		
class UnaryOpNode:
	var op_tok
	var node
	var pos_start
	var pos_end
	var type = "UnaryOpNode"
	
	func _init(_op_tok, _node):
		self.op_tok = _op_tok
		self.node = _node
		pos_start = self.op_tok.pos_start
		pos_end = self.node.pos_end
		
	func _to_string():
		return "(" + str(self.op_tok) + ", " + str(self.node) + ")"
		
class IfNode:
	var cases
	var else_case
	var pos_start
	var pos_end
	var type = "IfNode"
	
	func _init(_cases, _else_case):
		self.cases = _cases
		self.else_case = _else_case
		self.pos_start = self.cases[0][0].pos_start
		if self.else_case:
			self.pos_end = self.else_case.pos_end
		else:
			self.pos_end =  self.cases[len(self.cases)-1][0].pos_end
		
class ForNode:
	var var_name_tok
	var start_value_node
	var end_value_node
	var step_value_node
	var body_node
	var pos_start
	var pos_end
	var type = "ForNode"
	func _init(_var_name_tok, _start_value_node, _end_value_node, _step_value_node, _body_node):
		self.var_name_tok = _var_name_tok
		self.start_value_node = _start_value_node
		self.end_value_node = _end_value_node
		self.step_value_node = _step_value_node
		self.body_node = _body_node
		
		self.pos_start = self.var_name_tok.pos_start
		self.pos_end = self.body_node.pos_end
	
class WhileNode:
	var condition_node
	var body_node
	var pos_start
	var pos_end
	var type = "WhileNode"
	
	func _init(_condition_node, _body_node):
		self.condition_node = _condition_node
		self.body_node = _body_node
		
		self.pos_start = self.condition_node.pos_start
		self.pos_end = self.body_node.pos_end
		
		
class FuncDefNode:
	var var_name_tok
	var arg_name_toks
	var body_node
	var pos_start
	var pos_end
	var type = "FuncDefNode"
	
	func _init(_var_name_tok, _arg_name_toks, _body_node):
		self.var_name_tok = _var_name_tok
		self.arg_name_toks = _arg_name_toks
		self.body_node = _body_node
		
		if self.var_name_tok:
			self.pos_start = self.var_name_tok.pos_start
		elif len(self.arg_name_toks) > 0:
			self.pos_start = self.arg_name_toks[0].pos_start
		else:
			self.pos_start = self.body_node.pos_start
		
		self.pos_end = self.body_node.pos_end

class CallNode:
	var node_to_call
	var arg_nodes
	var pos_start
	var pos_end
	var type = "CallNode"
	
	func _init(_node_to_call, _arg_nodes):
		self.node_to_call = _node_to_call
		self.arg_nodes = _arg_nodes
		
		self.pos_start = self.node_to_call.pos_start
		
		if len(self.arg_nodes) > 0:
			self.pos_end = self.arg_nodes[len(self.arg_nodes) - 1].pos_end
		else:
			self.pos_end = self.node_to_call.pos_end
		
		
	
	
	
	
########################################
# PARSE RESULT
########################################

class ParseResult:
	var error
	var node
	var advance_count
	
	func _init():
		self.error = null
		self.node = null
		self.advance_count = 0

	func register(res):
		self.advance_count += res.advance_count
		if res.error: self.error = res.error
		return res.node
	
	func register_advancement():
		self.advance_count += 1

		
	func success(_node):
		self.node = _node
		return self
	
	func failure(_error):
		if not self.error or self.advance_count == 0:
			self.error = _error
		return self

########################################
# PARSER
########################################

class Parser:
	var tokens
	var tok_idx
	var current_tok
	
	func _init(_tokens):
		self.tokens = _tokens
		self.tok_idx = -1
		self.advance()
		
	func advance():
		self.tok_idx += 1
		if self.tok_idx < len(self.tokens):
			self.current_tok = self.tokens[self.tok_idx]
			
		return self.current_tok
		
	#################################
	
	func parse():
		var res = self.expr()
		if not res.error and self.current_tok.type != TT_EOF:
			return res.failure(InvalidSyntaxError.new(self.current_tok.pos_start,
					self.current_tok.pos_end, "", "Expected '+', '-', '*', '/' or '^'"))
		
		return res
	
	func llamar():
		var res = ParseResult.new()
		var atom = res.register(self.atom())
		var arg_nodes = []
		
		if res.error: return res
		
		if self.current_tok.type == TT_LPAREN:
			res.register_advancement()
			self.advance()
			arg_nodes = []
			
			if self.current_tok.type == TT_RPAREN:
				res.register_advancement()
				self.advance()
			else:
				arg_nodes.append(res.register(self.expr()))
				if res.error: 
					return res.failure(InvalidSyntaxError.new(self.current_tok.pos_start, 
						self.current_tok.pos_end,"",
						 "Expected ')', 'VAR', 'IF', 'FOR', 'WHILE', 'FUNC', int, float, '+', '-', '(', 'NOT'"
					))
				
				while self.current_tok.type == TT_COMMA:
					res.register_advancement()
					self.advance()
					
					arg_nodes.append(res.register(self.expr()))
					if res.error: return res
				
				res.register_advancement()
				self.advance()
			return res.success(CallNode.new(atom,arg_nodes))
		return res.success(atom)
				
		
	
	func atom():
		var res = ParseResult.new()
		var tok = self.current_tok
		
		if tok.type in [TT_INT, TT_FLOAT]:
			res.register_advancement()
			self.advance()
			return res.success(NumberNode.new(tok))
			
		elif tok.type == TT_IDENTIFIER:
			res.register_advancement()
			self.advance()
			return res.success(VarAccessNode.new(tok))
			
		elif tok.type == TT_LPAREN:
			res.register_advancement()
			self.advance()
			var expr = res.register(self.expr())
			if res.error:
				return res
			if self.current_tok.type == TT_RPAREN:
				res.register_advancement()
				self.advance()
				return res.success(expr)
			else:
				return res.failure(InvalidSyntaxError.new(tok.pos_start, 
			tok.pos_end,"", "Expected )"))
		
		elif tok.matches(TT_KEYWORD, 'IF'):
			var result_if_expr = res.register(self.if_expr())
			if res.error: 
				return res
			return res.success(result_if_expr)
		
		elif tok.matches(TT_KEYWORD, 'FOR'):
			var result_for_expr = res.register(self.for_expr())
			if res.error: 
				return res
			return res.success(result_for_expr)
			
		elif tok.matches(TT_KEYWORD, 'WHILE'):
			var result_while_expr = res.register(self.while_expr())
			if res.error: 
				return res
			return res.success(result_while_expr)
		
		elif tok.matches(TT_KEYWORD, 'FUNC'):
			var result_func_def = res.register(self.func_def())
			if res.error: 
				return res
			return res.success(result_func_def)
		
		return res.failure(InvalidSyntaxError.new(tok.pos_start, 
			tok.pos_end,"", "int, float, '+', '-' or '('"))

	func power():
		return self.bin_op("llamar", [TT_POW], "factor")
	
	func factor():
		var res = ParseResult.new()
		var tok = self.current_tok
		
		if tok.type in [TT_PLUS, TT_MINUS]:
			res.register_advancement()
			self.advance()
			var factor = res.register(self.factor())
			if res.error: 
				return res
			return res.success(UnaryOpNode.new(tok, factor))
			
		return self.power()
	
	
	func term():
		return self.bin_op("factor", [TT_MUL, TT_DIV])
		
	func arith_expr():
		return self.bin_op("term", [TT_PLUS, TT_MINUS])
		
	
	func comp_expr():
		var res = ParseResult.new()
		var node
		if self.current_tok.matches(TT_KEYWORD, 'NOT'):
			var op_tok = self.current_tok
			res.register_advancement()
			self.advance()
			
			node = res.register(self.comp_expr())
			if res.error: return res
			return res.success(UnaryOpNode.new(op_tok, node))
		
		node = res.register(self.bin_op("arith_expr", [TT_EE, TT_NE, TT_LT, TT_GT, TT_LTE, TT_GTE] ))
		
		if res.error:
			return res.failure(InvalidSyntaxError.new(self.current_tok.pos_start, 
				self.current_tok.pos_end,"", "int, float, '+', '-', '(', 'NOT'"
				))
		
		return res.success(node)
	
	func if_expr():
		var res = ParseResult.new()
		var cases = []
		var else_case = null
		
		if not self.current_tok.matches(TT_KEYWORD, 'IF'):
			return res.failure(InvalidSyntaxError.new(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"", "Expected 'IF'"
			))
		
		res.register_advancement()
		self.advance()
		
		var condition = res.register(self.expr())
		if res.error: return res
		
		if not self.current_tok.matches(TT_KEYWORD, 'THEN'):
			return res.failure(InvalidSyntaxError.new(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"", "Expected 'THEN'"
			))
		
		res.register_advancement()
		self.advance()
		
		var expr = res.register(self.expr())
		if res.error: return res
		cases.append([condition, expr])
		
		while self.current_tok.matches(TT_KEYWORD, 'ELIF'):
			res.register_advancement()
			self.advance()
			
			condition = res.register(self.expr())
			if res.error: return res
			
			if not self.current_tok.matches(TT_KEYWORD, 'THEN'):
				return res.failure(InvalidSyntaxError.new(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"", "Expected 'THEN'"
				))
			
			res.register_advancement()
			self.advance()
			
			expr = res.register(self.expr())
			if res.error: return res
			cases.append([condition, expr])
		
		if self.current_tok.matches(TT_KEYWORD, 'ELSE'):
			res.register_advancement()
			self.advance()
			
			expr = res.register(self.expr())
			if res.error: return res
			else_case = expr
		return res.success(IfNode.new(cases, else_case))
			
	func for_expr():
		var res = ParseResult.new()
		var var_name
		var start_value
		var end_value
		var step_value
		var body
		
		if not self.current_tok.matches(TT_KEYWORD,'FOR'):
			return res.failure(InvalidSyntaxError.new(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"", "Expected 'FOR'"
			))
		
		res.register_advancement()
		self.advance()
		
		if self.current_tok.type != TT_IDENTIFIER:
			return res.failure(InvalidSyntaxError.new(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"", "Expected identifier"
			))
		
		var_name = self.current_tok
		res.register_advancement()
		self.advance()
		
		if self.current_tok.type != TT_EQ:
			return res.failure(InvalidSyntaxError.new(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"", "Expected '='"
			))
		
		res.register_advancement()
		self.advance()
		
		start_value = res.register(self.expr())
		if res.error: return res
		
		if not self.current_tok.matches(TT_KEYWORD, 'TO'):
			return res.failure(InvalidSyntaxError.new(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"", "Expected 'TO'"
			))
		
		res.register_advancement()
		self.advance()
		
		end_value = res.register(self.expr())
		if res.error: return res
		
		if self.current_tok.matches(TT_KEYWORD, 'STEP'):
			res.register_advancement()
			self.advance()
			
			step_value = res.register(self.expr())
			if res.error: return res
		else:
			step_value = null
		
		if not self.current_tok.matches(TT_KEYWORD, 'THEN'):
			return res.failure(InvalidSyntaxError.new(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"", "Expected 'THEN'"
			))
		
		res.register_advancement()
		self.advance()
		
		body = res.register(self.expr())
		if res.error: return res
		
		return res.success(ForNode.new(var_name, start_value, end_value, step_value, body))
	
	
	func while_expr():
		var res = ParseResult.new()
		var condition
		var body
		
		if not self.current_tok.matches(TT_KEYWORD, 'WHILE'):
			return res.failure(InvalidSyntaxError.new(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"", "Expected 'WHILE'"
			))
		
		res.register_advancement()
		self.advance()
		
		condition = res.register(self.expr())
		if res.error: return res
		
		if not self.current_tok.matches(TT_KEYWORD, 'THEN'):
			return res.failure(InvalidSyntaxError.new(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"", "Expected 'THEN'"
			))
		
		res.register_advancement()
		self.advance()
		
		body = res.register(self.expr())
		if res.error: return res
		
		return res.success(WhileNode.new(condition,body))
			
		
	func expr():
		var res = ParseResult.new()
		if self.current_tok.matches(TT_KEYWORD, 'VAR'):
			res.register_advancement()
			self.advance()
			
			if self.current_tok.type != TT_IDENTIFIER:
				return res.failure(InvalidSyntaxError.new(
					self.current_tok.pos_start, self.current_tok.pos_end, 
					"", "Expected identifier"
				))
			
			var var_name = self.current_tok
			res.register_advancement()
			self.advance()
			
			if self.current_tok.type != TT_EQ:
				return res.failure(InvalidSyntaxError.new(
					self.current_tok.pos_start, self.current_tok.pos_end, 
					"", "Expected ="
				))
			
			res.register_advancement()
			self.advance()
			var expr = res.register(self.expr())
			if res.error:
				return res
			return res.success(VarAssignNode.new(var_name, expr))
			
		var node = res.register(self.bin_op("comp_expr", [[TT_KEYWORD, "AND"], [TT_KEYWORD, "OR"]]))
		if res.error:
			return res.failure(InvalidSyntaxError.new(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"", "Expected 'VAR', 'IF','FOR','WHILE','FUNC', int, float, identifier, '+', '-' or '('"
				))
		
		return res.success(node)
		
	func func_def():
		var res = ParseResult.new()
		var var_name_tok
		var arg_name_toks
		var node_to_return
		
		if not self.current_tok.matches(TT_KEYWORD, 'FUNC'):
			return res.failure(InvalidSyntaxError.new(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"", "Expected 'FUNC'"
			))
		
		res.register_advancement()
		self.advance()
		
		#si la funcion tiene un nombre...
		if self.current_tok.type == TT_IDENTIFIER:
			var_name_tok = self.current_tok
			res.register_advancement()
			self.advance()
			if self.current_tok.type != TT_LPAREN:
				return res.failure(InvalidSyntaxError.new(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"", "Expected '('"
				))
		else:
			var_name_tok = null
			if self.current_tok.type != TT_LPAREN:
				return res.failure(InvalidSyntaxError.new(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"", "Expected identifier or '('"
				))
		
		res.register_advancement()
		self.advance()
		arg_name_toks = []
		
		if self.current_tok.type == TT_IDENTIFIER:
			arg_name_toks.append(self.current_tok)
			res.register_advancement()
			self.advance()
			
			while self.current_tok.type == TT_COMMA:
				res.register_advancement()
				self.advance()
				
				if self.current_tok.type != TT_IDENTIFIER:
					return res.failure(InvalidSyntaxError.new(
						self.current_tok.pos_start, self.current_tok.pos_end,
						"", "Expected identifier"
					))
				
				arg_name_toks.append(self.current_tok)
				res.register_advancement()
				self.advance()
			
			if self.current_tok.type != TT_RPAREN:
				return res.failure(InvalidSyntaxError.new(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"", "Se esperaba ',' o ')'"
				))
		#si no nos encontramos un identificador (variable) entre los parentesis
		else:
			if self.current_tok.type != TT_RPAREN:
				return res.failure(InvalidSyntaxError.new(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"", "Se esperaba identificador o ')'"
				))
		
		res.register_advancement()
		self.advance()
		
		if self.current_tok.type != TT_ARROW:
			return res.failure(InvalidSyntaxError.new(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"", "Expected '->'"
			))
		
		res.register_advancement()
		self.advance()
		node_to_return = res.register(self.expr())
		if res.error: return res
		
		return res.success(FuncDefNode.new(
			var_name_tok,
			arg_name_toks,
			node_to_return
		))
		
		
	func bin_op(function_a, ops, function_b = null):
		if function_b == null:
			function_b = function_a
			
		var res = ParseResult.new()
		var left = res.register(self.call(function_a))
		if res.error: 
			return res
		
		#Arreglar, la información tiene que venir de la misma manera, no en dos formatos
		var siguiente_token
		print(ops[0], ops[0] is Array)
		print(self.current_tok)
		if self.current_tok is Array:
#			siguiente_token = [self.current_tok, self.current_tok.value]
			siguiente_token = [self.current_tok[0], self.current_tok[1]]
			
		else:
			siguiente_token = self.current_tok.type
		
		while siguiente_token in ops:
			print(siguiente_token, ops)
			print(siguiente_token in ops)
			var op_tok = self.current_tok
			
			res.register_advancement()
			self.advance()
			var right = res.register(self.call(function_b))
			if res.error: return res
			left = BinOpNode.new(left, op_tok,right)
			#Arreglar, la información tiene que venir de la misma manera, no en dos formatos
			siguiente_token = false
			if ops[0] is Array:
				siguiente_token = [self.current_tok.type, self.current_tok.value]
			else:
				siguiente_token = self.current_tok.type
		
		return res.success(left)
		

########################################
# RUNTIME RESULT
########################################

class RTResult:
	var value
	var error
	
	func _init():
		self.value = null
		self.error = null
		
	func register(res):
		if res.error:
			self.error = res.error
		return res.value
	
	func success(_value):
		self.value = _value
		return self
		
	func failure(_error):
		self.error = _error
		return self

########################################
# VALUES
########################################

class Value:
	var pos_start
	var pos_end
	var context
	
	func _init():
		self.set_pos()
		self.set_context()
		
	func set_pos(_pos_start = null, _pos_end = null):
		self.pos_start = _pos_start
		self.pos_end = _pos_end
		return self
		
	func set_context(_context = null):
		self.context = _context
		return self
	
	func added_to(other):
		return [null, self.illegal_operation(other)]
	
	func subbed_by(other):
		return [null, self.illegal_operation(other)]
	
	func multed_by(other):
		return [null, self.illegal_operation(other)]
	
	func dived_by(other):
		return [null, self.illegal_operation(other)]
	
	func powed_by(other):
		return [null, self.illegal_operation(other)]
			
	func get_comparison_eq(other):
		return [null, self.illegal_operation(other)]
	
	func get_comparison_ne(other):
		return [null, self.illegal_operation(other)]
	
	func get_comparison_lt(other):
		return [null, self.illegal_operation(other)]
			
	func get_comparison_gt(other):
		return [null, self.illegal_operation(other)]
	
	func get_comparison_lte(other):
		return [null, self.illegal_operation(other)]
	
	func get_comparison_gte(other):
		return [null, self.illegal_operation(other)]
	
	func anded_by(other):
		return [null, self.illegal_operation(other)]
			
	func ored_by(other):
		return [null, self.illegal_operation(other)]
	
	func notted():
		return [null, self.illegal_operation()]
	
	func execute(args):
		return [null, self.illegal_operation()]
	func copy():
		assert('No copy method defined')
	
	func is_true():
		return false
		
	func illegal_operation(other = null):
		if not other: other = self
		return RTError.new(
			self.pos_start, other.pos_end,
			'', 'Illegal operation',
			self.context
		)
		
		
class Number extends Value:
	var value
#	var pos_start
#	var pos_end
#	var context
	
	func _init(_value):
		self.value = _value
		self.set_pos()
		self.set_context()
		
	func set_pos(_pos_start = null, _pos_end = null):
		self.pos_start = _pos_start
		self.pos_end = _pos_end
		return self
		
	func set_context(_context = null):
		self.context = _context
		return self
	
	func added_to(other):
		if other is Number:
			return [Number.new(self.value + other.value).set_context(self.context), null]
		else:
			return [null, Value.illegal_operation(self.pos_start, other.pos_end)]
	
	func subbed_by(other):
		if other is Number:
			return [Number.new(self.value - other.value).set_context(self.context), null]
		else:
			return [null, Value.illegal_operation(self.pos_start, other.pos_end)]
	
	
	
	func multed_by(other):
		if other is Number:
			return [Number.new(self.value * other.value).set_context(self.context), null]
		else:
			return [null, Value.illegal_operation(self.pos_start, other.pos_end)]
	
	func dived_by(other):
		if other is Number:
			if other.value == 0:
				return [null, RTError.new(
					other.pos_start, other.pos_end,"", "Division by zero",
					self.context
				)]
			return [Number.new(self.value / other.value).set_context(self.context), null]
		else:
			return [null, Value.illegal_operation(self.pos_start, other.pos_end)]
	
	func powed_by(other):
		if other is Number:
			return [Number.new(pow(self.value, other.value)).set_context(self.context), null]
		else:
			return [null, Value.illegal_operation(self.pos_start, other.pos_end)]
			
	func get_comparison_eq(other):
		if other is Number:
			return [Number.new(int(self.value == other.value)).set_context(self.context), null]
		else:
			return [null, Value.illegal_operation(self.pos_start, other.pos_end)]
			
	func get_comparison_ne(other):
		if other is Number:
			return [Number.new(int(self.value != other.value)).set_context(self.context), null]
		else:
			return [null, Value.illegal_operation(self.pos_start, other.pos_end)]
			
	func get_comparison_lt(other):
		if other is Number:
			return [Number.new(int(self.value < other.value)).set_context(self.context), null]
		else:
			return [null, Value.illegal_operation(self.pos_start, other.pos_end)]
			
	func get_comparison_gt(other):
		if other is Number:
			return [Number.new(int(self.value > other.value)).set_context(self.context), null]
		else:
			return [null, Value.illegal_operation(self.pos_start, other.pos_end)]
		
	func get_comparison_lte(other):
		if other is Number:
			return [Number.new(int(self.value <= other.value)).set_context(self.context), null]
		else:
			return [null, Value.illegal_operation(self.pos_start, other.pos_end)]
		
	func get_comparison_gte(other):
		if other is Number:
			return [Number.new(int(self.value >= other.value)).set_context(self.context), null]
		else:
			return [null, Value.illegal_operation(self.pos_start, other.pos_end)]
		
	func anded_by(other):
		if other is Number:
			return [Number.new(int(self.value and other.value)).set_context(self.context), null]
		else:
			return [null, Value.illegal_operation(self.pos_start, other.pos_end)]
			
	func ored_by(other):
		if other is Number:
			return [Number.new(int(self.value or other.value)).set_context(self.context), null]
		else:
			return [null, Value.illegal_operation(self.pos_start, other.pos_end)]
		
	func notted():
		var resultado
		if self.value == 0:
			resultado = 1
		else:
			resultado = 0

		return [Number.new(resultado).set_context(self.context), null]
		
	func copy():
		var copy = Number.new(self.value)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		return copy
		
	func is_true():
		return self.value != 0
		
	func as_string():
		return str(self.value)

class Function extends Value:
	var name
	var body_node
	var arg_names
	
	func _init(_name, _body_node, _arg_names):
		._init()
		if not _name:
			self.name = "<anonymous>"
		else:
			self.name = _name
		
		self.body_node = _body_node
		self.arg_names = _arg_names

	func execute(args):
		var res = RTResult.new()
		var interpreter = Interpreter.new()
		var new_context = Context.new(self.name, self.context, self.pos_start)
		new_context.symbol_table = SymbolTable.new(new_context.parent.symbol_table)
		
		if len(args) > len(self.arg_names):
			return res.failure(RTError.new(
				self.pos_start, self.pos_end, "",
				str(len(args)) +" - "+str(len(self.arg_names))+" too many args passed into "+str(self.name),
				self.context
			))
		
		if len(args) < len(self.arg_names):
			return res.failure(RTError.new(
				self.pos_start, self.pos_end, "",
				str(len(self.arg_names)) +" - "+str(len(args))+" too few args passed into "+str(self.name),
				self.context
			))

		for i in range(len(args)):
			var arg_name = self.arg_names[i]
			var arg_value = args[i]
			arg_value.set_context(new_context)
			new_context.symbol_table.set(arg_name, arg_value)

		var value = res.register(interpreter.visit(self.body_node, new_context))
		if res.error: return res
		return res.success(value)
		
	func copy():
		var copy = Function.new(self.name, self.body_node, self.arg_names)
		copy.set_context(self.context)
		copy.set_pos(self.pos_start, self.pos_end)
		return copy
	
	func as_string():
		return "<function " + str(self.name) + ">"
		

########################################
# CONTEXT
########################################

class Context:
	var display_name
	var parent
	var parent_entry_pos
	var symbol_table
	
	func _init(_display_name, _parent = null, _parent_entry_pos = null):
		self.display_name = _display_name
		self.parent = _parent
		self.parent_entry_pos = _parent_entry_pos
		self.symbol_table = null

########################################
# SYMBOL TABLE
########################################

class SymbolTable:
	var symbols = {}
	var parent
	func _init(_parent = null):
		self.parent = _parent
	
	#Buscamos la variable en la tabla, si no la encontramos la buscamos
	#en el padre (La tabla no tiene padre)
	func get(_name):
		var value = self.symbols.get(_name, null)
		if value == null and self.parent:
			return self.parent.get(_name)
		return value
	
	func set(_name, _value):
		self.symbols[_name] = _value

	func remove(_name):
		self.symbols.erase(_name)
########################################
# INTERPRETER
########################################

class Interpreter:
	
	func visit(node, _context):
		var method_name = "visit_" + node.type
		var method
		if self.has_method(method_name): 
			method = self.call(method_name, node, _context)
		else:
			method = self.call("no_visit_method", node, _context)
		return method
		

	func no_visit_method(node, _context):
		printerr("No visit_" + node.type + "method defined")
		assert(false)

	###############################
	
	func visit_NumberNode(node, _context):
		return RTResult.new().success(
			Number.new(node.tok.value).set_context(_context).set_pos(node.pos_start, node.pos_end)
		)
	
	func visit_VarAccessNode(node, context):
		var res = RTResult.new()
		var var_name = node.var_name_tok.value
		var value = context.symbol_table.get(var_name)
		
		if not value:
			return res.failure(RTError.new(
				node.pos_start, node.pos_end,
				"", "'" + str(var_name) + "' is not defined", context
			))
		
		value = value.copy().set_pos(node.pos_start, node.pos_end)
		return res.success(value)
		
	func visit_VarAssignNode(node, context):
		var res = RTResult.new()
		var var_name = node.var_name_tok.value
		var value = res.register(self.visit(node.value_node, context))
		if res.error: return res
		
		context.symbol_table.set(var_name, value)
		return res.success(value)
	
	func visit_BinOpNode(node, _context):
		var result_error_list
		
		var res = RTResult.new()
		
		var left = res.register(self.visit(node.left_node, _context))
		if res.error: return res
		var right = res.register(self.visit(node.right_node, _context))
		if res.error: return res
		
		
		if node.op_tok.type == TT_PLUS:
			result_error_list = left.added_to(right)
		elif node.op_tok.type == TT_MINUS:
			result_error_list = left.subbed_by(right)
		elif node.op_tok.type == TT_MUL:
			result_error_list = left.multed_by(right)
		elif node.op_tok.type == TT_DIV:
			result_error_list = left.dived_by(right)
		elif node.op_tok.type == TT_POW:
			result_error_list = left.powed_by(right)
		elif node.op_tok.type == TT_EE:
			result_error_list = left.get_comparison_eq(right)
		elif node.op_tok.type == TT_NE:
			result_error_list = left.get_comparison_ne(right)
		elif node.op_tok.type == TT_LT:
			result_error_list = left.get_comparison_lt(right)
		elif node.op_tok.type == TT_GT:
			result_error_list = left.get_comparison_gt(right)
		elif node.op_tok.type == TT_LTE:
			result_error_list = left.get_comparison_lte(right)
		elif node.op_tok.type == TT_GTE:
			result_error_list = left.get_comparison_gte(right)
		elif node.op_tok.matches(TT_KEYWORD, 'AND'):
			result_error_list = left.anded_by(right)
		elif node.op_tok.matches(TT_KEYWORD, 'OR'):
			result_error_list = left.ored_by(right)
			
		elif result_error_list[1]:
			return res.failure(result_error_list[1])
		return res.success(result_error_list[0].set_pos(node.pos_start, node.pos_end))
		
	func visit_UnaryOpNode(node, _context):
		var res = RTResult.new()
		var number = res.register(self.visit(node.node, _context))
		var number_error_list
		if res.error: return res
		
		var error = null
		
		if node.op_tok.type == TT_MINUS:
			number_error_list = number.multed_by(Number.new(-1))
		elif node.op_tok.matches(TT_KEYWORD, 'NOT'):
			number_error_list = number.notted()
		number = number_error_list[0]
		error = number_error_list[1]
		
		if error:
			return res.failure(error)
		else:
			return res.success(number.set_pos(node.pos_start, node.pos_end))
		
	
	func visit_IfNode(node, _context):
		var res = RTResult.new()
		var condition
		var expr
		
		for condition_expr_list in node.cases:
			condition = condition_expr_list[0]
			expr = condition_expr_list[1]
			var condition_value = res.register(self.visit(condition, _context))
			if res.error: return res
			
			if condition_value.is_true():
				var expr_value = res.register(self.visit(expr, _context))
				if res.error: return res
				return res.success(expr_value)
		
		if node.else_case:
			var else_value = res.register(self.visit(node.else_case, _context))
			if res.error: return res
			return res.success(else_value)
			
		return res.success(null)
		
		
	func visit_ForNode(node, _context):
		var res = RTResult.new()
		var start_value
		var end_value
		var step_value
		var i
		var condicion#eliminar si convierto a lambda
		
		start_value = res.register(self.visit(node.start_value_node, _context))
		if res.error: return res
		
		end_value = res.register(self.visit(node.end_value_node, _context))
		if res.error: return res
		
		if node.step_value_node:
			step_value = res.register(self.visit(node.step_value_node, _context))
			if res.error: return res
		else:
			step_value = Number.new(1)
		
		i = start_value.value
		
		#usar lambda condicion() en un futuro
		if step_value.value >= 0:
			condicion = i < end_value.value
		else:
			condicion = i > end_value.value
		while condicion:
			#Actualizamos el valor del iterador en la tabla de simbolos
			_context.symbol_table.set(node.var_name_tok.value, Number.new(i))
			i += step_value.value
			
			#Ejecutamos el cuerpo
			res.register(self.visit(node.body_node, _context))
			if res.error: return res
			
			#usar lambda condicion() en un futuro
			if step_value.value >= 0:
				condicion = i < end_value.value
			else:
				condicion = i > end_value.value
		
		#el return es un null, ya que los resultados se han realizado en el cuerpo
		return res.success(null)
		
		
	func visit_WhileNode(node, _context):
		var res = RTResult.new()
		var condition
		
		while true:
			condition = res.register(self.visit(node.condition_node, _context))
			if res.error: return res
			
			if not condition.is_true(): break
			
			res.register(self.visit(node.body_node, _context))
			if res.error: return res
		
		#el return es un null, ya que los resultados se han realizado en el cuerpo
		return res.success(null)
		
	func visit_FuncDefNode(node, context):
		var res = RTResult.new()
		var func_name = null
		if node.var_name_tok:
			func_name = node.var_name_tok.value
		var body_name = node.body_node
		var arg_names = []
		for arg_name in node.arg_name_toks:
			arg_names.append(arg_name)
		
		var func_value = Function.new(func_name, body_name, arg_names).set_context(context).set_pos(node.pos_start, node.pos_end)
		
		if node.var_name_tok:
			context.symbol_table.set(func_name, func_value)
		
		return res.success(func_value)
	
	func visit_CallNode(node, context):
		var res = RTResult.new()
		var args = []
		
		var value_to_call = res.register(self.visit(node.node_to_call, context))
		if res.error: return res
		value_to_call = value_to_call.copy().set_pos(node.pos_start, node.pos_end)
		
		for arg_node in node.arg_nodes:
			args.append(res.register(self.visit(arg_node, context)))
			if res.error: return res
		
		var return_value = res.register(value_to_call.execute(args))
		if res.error: return res
		return res.success(return_value)

########################################
# RUN
########################################

#ingresar un nombre de archivo y el texto a interpretar
func run(fn, text):
	
	#Generate tokens
	var lexer = Lexer.new(fn, text)
	var tokens_error_list = lexer.make_tokens()
	var tokens = tokens_error_list[0]
	var error = tokens_error_list[1]
	if error:
		return [null, error]
	
	#Generate AST
	var parser = Parser.new(tokens)
	var ast = parser.parse()
	if ast.error: return [null, ast.error]
	
	#Run program
	var interpreter = Interpreter.new()
	var context = Context.new('<program>')
	context.symbol_table = global_symbol_table
	var result = interpreter.visit(ast.node, context)
	

	return [result.value, result.error]
