#Una función que verifica que haya tantos registros como se espera en el archivo:
function check_linenumber(archivo)

    #Usamos wc para contar el número de líneas. Guardamos la salida en un string:
    count_lines = read(`wc -l $archivo`, String)

    #Capturamos el resultado y operamos en él.
    number_lines_string = match(r"^(\d+) ", count_lines).captures[1]
    number_lines = parse(Int64, number_lines_string)
    number_registries = number_lines - 1 #Tenemos tantos registros como líneas menos uno (por el header)

    #Cargamos la última línea del archivo:
    final_line = read(`tail -n 1 $archivo`, String)

    #Capturamos el último número de caso del registro:
    expected_number_registries_string = match(r"^(\d+),", final_line).captures[1]
    expected_number_registries = parse(Int64, expected_number_registries_string)

    if number_registries == expected_number_registries

        return nothing

    else

        error("Mismatch in the number of registries in $archivo")
    end
end
