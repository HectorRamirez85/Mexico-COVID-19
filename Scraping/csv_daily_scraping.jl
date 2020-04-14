#Script para completar la tabla acumulativa a partir del csv de casos positivos diarios.

#Paqueterías para leer CSVs y usar dataframes.
using CSV
using DataFrames

nombres_estados = ["AGUASCALIENTES", "BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "CAMPECHE", "CHIAPAS", "CHIHUAHUA", "CIUDAD DE MÉXICO", "COAHUILA", "COLIMA", "DURANGO", "GUANAJUATO", "GUERRERO", "HIDALGO", "JALISCO", "MICHOACÁN", "MORELOS", "MÉXICO", "NAYARIT", "NUEVO LEÓN", "OAXACA", "PUEBLA", "QUERÉTARO", "QUINTANA ROO", "SAN LUIS POTOSÍ", "SINALOA", "SONORA", "TABASCO", "TAMAULIPAS", "TLAXCALA", "VERACRUZ", "YUCATÁN", "ZACATECAS"]

#Cálculo de los números de tipos de casos (sospechosos, importados, locales, positivos y fallecidos) por estado:
function tipo_casos(sospechosos_diarios, confirmados_diarios, fallecidos_diarios, estado)

    #Obtiene los datos de casos sospechosos del estado:
    casos_sospechosos_nomissing = filter(row -> ismissing(row[:Estado]) == false, sospechosos_diarios)
    casos_sospechosos_entidad = filter(row -> row[:Estado] == estado, casos_sospechosos_nomissing)
    #Obtiene los datos del estado:
    casos_entidad = filter(row -> row[:Estado] == estado, confirmados_diarios)

    #El número total de casos sospechosos en el estado:
    sospechosos = length(casos_sospechosos_entidad.Situación)
    #El número total de casos confirmados en el estado:
    positivos = length(casos_entidad.Situación)

    #El número total de fallecidos en el estado se extrae del otro documento:
    fallecidos = filter(row -> row[:Estado] == estado, fallecidos_diarios).Fallecidos[1]

    #El número de recuperados ya no se publica en los documentos oficiales. Se reporta como missing.
    #Ya no se distinguen casos importados de locales. Se reportan por retrocompatibilidad como missing.
    return [sospechosos, missing, missing, positivos, missing, fallecidos]
end

#Actualiza la tabla cumulativa del día a partir de la fecha y los datos del CTD.
#La fecha se debe de poner como string e.g. "20200320"
function fila_actualización(fecha, positivos_reportados, sospechosos_reportados, negativos_reportados)

    #Carga los datos del día:
    sospechosos_diarios = CSV.read("Daily data/$(fecha[1:6])/sospechosos_$(fecha).csv", header = 1, copycols = true)
    confirmados_diarios = CSV.read("Daily data/$(fecha[1:6])/positivos_$(fecha).csv", header = 1, copycols = true)
    fallecidos_diarios = CSV.read("Daily data/$(fecha[1:6])/fallecidos_$(fecha).csv", header = 1)

    #Calcula los casos de cada estado
    casos_estados = map(estado -> tipo_casos(sospechosos_diarios, confirmados_diarios, fallecidos_diarios, estado), nombres_estados)

    #Calcula los casos en el país
    acumulado_país = sum(casos_estados)

    #Calcula el número total de muestras analizadas.
    total_tests = acumulado_país[4] + negativos_reportados
    #Genera un vector con el número de casos positivos importados, locales, total de casos; positivos reportados, sospechosos reportados, negativos reportados, número total de pruebas, número de casos recuperados y número de casos fallecidos (para mantener el orden de la tabla original):
    reporte_país = [acumulado_país[1:4]..., positivos_reportados, sospechosos_reportados, negativos_reportados, missing, total_tests, acumulado_país[5:6]...]

    #Genera la fila de datos del día correspondiente:
    fila = string.(vcat(casos_estados..., reporte_país))

    #Agrega la fecha a la primer columna:
    pushfirst!(fila, fecha[1:4]*"-"*fecha[5:6]*"-"*fecha[7:8])

    #Agrega comas para el csv y lo junta todo en un string
    fila_csv = prod(fila[1:(end - 1)].*",")*fila[end]

    #Elimina los missing (para que quede el espacio vacío)
    fila_csv = replace(fila_csv, r"missing" => "")

    #Agrega el resultado al csv acumulativo:
    open("Mexico_COVID19.csv", "a") do io

        write(io, fila_csv*"\n")
    end

    return "Done"
end
