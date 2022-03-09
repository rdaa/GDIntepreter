extends Control


func _ready():
	pass


func _on_BotonEjecutar_pressed():
	var text = $TextoCodigo.text
	var result_error_list = $Interprete.run('consola', text)
	if result_error_list[1]: 
		print(result_error_list[1].as_string())
#		salida_codigo(result_error_list[1].as_string())
	else: 
		print(result_error_list[0].value)
#		for e in result_error_list[0]:
#			salida_codigo(e)
		salida_codigo(result_error_list[0].value)

func salida_codigo(codigo):
	$TextoSalida.text += str(codigo) + '\n'
	
