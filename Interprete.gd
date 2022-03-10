extends Node

var global_symbol_table = SymbolTable.new()

func _ready():
	global_symbol_table.set("null", Number.new(0))


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
const TT_EOF         = 'EOF'

const KEYWORDS = [
	'VAR'
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
				tokens.append(Token.new(TT_MINUS,null, self.pos))
				self.advance()
			elif self.current_char == '*':
				tokens.append(Token.new(TT_MUL,null, self.pos))
				self.advance()
			elif self.current_char == '/':
				tokens.append(Token.new(TT_DIV,null, self.pos))
				self.advance()
			elif self.current_char == '(':
				tokens.append(Token.new(TT_LPAREN,null, self.pos))
				self.advance()
			elif self.current_char == ')':
				tokens.append(Token.new(TT_RPAREN,null, self.pos))
				self.advance()
			elif self.current_char == '^':
				tokens.append(Token.new(TT_POW, null, self.pos))
				self.advance()
			elif self.current_char == '=':
				tokens.append(Token.new(TT_EQ, null, self.pos))
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
		
		
		return res.failure(InvalidSyntaxError.new(tok.pos_start, 
			tok.pos_end,"", "int, float, '+', '-' or '('"))

	func power():
		return self.bin_op("atom", [TT_POW])
	
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
			
		var node = res.register(self.bin_op("term", [TT_PLUS, TT_MINUS]))
		if res.error:
			return res.failure(InvalidSyntaxError.new(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"", "Expected 'VAR', int, float, identifier, '+', '-' or '('"
				))
		
		return res.success(node)
		
	func bin_op(function, ops):
		var res = ParseResult.new()
		var left = res.register(self.call(function))
		if res.error: 
			return res
		
		while self.current_tok.type in ops:
			var op_tok = self.current_tok
			
			res.register_advancement()
			self.advance()
			var right = res.register(self.call(function))
			if res.error: return res
			left = BinOpNode.new(left, op_tok,right)
		
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

class Number:
	var value
	var pos_start
	var pos_end
	var context
	
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
	
	func subbed_by(other):
		if other is Number:
			return [Number.new(self.value - other.value).set_context(self.context), null]
	
	func multed_by(other):
		if other is Number:
			return [Number.new(self.value * other.value).set_context(self.context), null]
	
	func dived_by(other):
		if other is Number:
			if other.value == 0:
				return [null, RTError.new(
					other.pos_start, other.pos_end,"", "Division by zero",
					self.context
				)]
			return [Number.new(self.value / other.value).set_context(self.context), null]
	
	func powed_by(other):
		if other is Number:
			return [Number.new(pow(self.value, other.value)).set_context(self.context), null]

	func copy():
		var copy = Number.new(self.value)
		copy.set_pos(self.pos_start, self.pos_end)
		copy.set_context(self.context)
		return copy
		
	func as_string():
		return str(self.value)

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
	func _init():
		parent = null
	
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
		if node.op_tok.type == TT_MINUS:
			result_error_list = left.subbed_by(right)
		if node.op_tok.type == TT_MUL:
			result_error_list = left.multed_by(right)
		if node.op_tok.type == TT_DIV:
			result_error_list = left.dived_by(right)
		if node.op_tok.type == TT_POW:
			result_error_list = left.powed_by(right)
		
		if result_error_list[1]:
			return res.failure(result_error_list[1])
		else:
			return res.success(result_error_list[0].set_pos(node.pos_start, node.pos_end))
		
	func visit_UnaryOpNode(node, _context):
		var res = RTResult.new()
		var number = res.register(self.visit(node.node, _context))
		if res.error: return res
		
		var error = null
		
		if node.op_tok.type == TT_MINUS:
			[number, error] = number.multed_by(Number.new(-1))
		
		if error:
			return res.failure(error)
		else:
			return res.success(number.set_pos(node.pos_start, node.pos_end))
		
		
		
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
