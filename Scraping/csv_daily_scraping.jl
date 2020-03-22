#Script para completar la tabla acumulativa a partir del csv de casos positivos diarios.

#Paqueterías para leer CSVs y usar dataframes.
using CSV
using DataFrames

nombres_estados = ["AGUASCALIENTES", "BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "CAMPECHE", "CHIAPAS", "CHIHUAHUA", "CIUDAD DE MÉXICO", "COAHUILA", "COLIMA", "DURANGO", "GUANAJUATO", "GUERRERO", "HIDALGO", "JALISCO", "MICHOACÁN", "MORELOS", "MÉXICO", "NAYARIT", "NUEVO LEÓN", "OAXACA", "PUEBLA", "QUERÉTARO", "QUINTANA ROO", "SAN LUIS POTOSÍ", "SINALOA", "SONORA", "TABASCO", "TAMAULIPAS", "TLAXCALA", "VERACRUZ", "YUCATÁN", "ZACATECAS"]

#Define las palabras claves asociadas a casos locales (no importados):
keywords_locales = ["Contacto"]

#Cálculo de los números de tipos de casos (importados, locales, positivos, recuperados y fallecidos) por estado:
function tipo_casos(datos_diarios, estado)

    #Obtiene los datos del estado:
    casos_entidad = filter(row -> row[:Estado] == estado, datos_diarios)

    #El número total de casos confirmados en el estado:
    positivos = length(casos_entidad.Situación)

    #El conjunto de casos importados al estado:
    importados = filter(row -> row[:País_fuente] ∉ keywords_locales, casos_entidad).Número_caso |> length

    #El número de casos locales:
    locales = positivos - importados

    #El conjunto de casos recuperados en el estado:
    recuperados = filter(row -> row[:Situación] == "recuperado", casos_entidad).Número_caso |> length

    #El conjunto de casos fallecidos en el estado:
    fallecidos = filter(row -> row[:Situación] == "fallecido", casos_entidad).Número_caso |> length

    return [importados, locales, positivos, recuperados, fallecidos]
end

#Actualiza la tabla cumulativa del día a partir de la fecha y los datos del CTD.
#La fecha se debe de poner como string e.g. "20200320"
function fila_actualización(fecha, positivos_reportados, sospechosos_reportados, negativos_reportados, número_IRAG)

    #Carga los datos del día:
    datos_diarios = CSV.read("Daily cases/positivos_$(fecha).csv", header = 1, copycols = true)

    #Calcula los casos de cada estado
    casos_estados = map(estado -> tipo_casos(datos_diarios, estado), nombres_estados)

    #Calcula los casos en el país
    acumulado_país = sum(casos_estados)

    #Calcula el número total de muestras analizadas.
    total_tests = acumulado_país[3] + sospechosos_reportados + negativos_reportados + número_IRAG
    #Genera un vector con el número de casos positivos importados, locales, total de casos; positivos reportados, sospechosos reportados, negativos reportados, número de tests IRAG, número total de pruebas, número de casos recuperados y número de casos fallecidos (para mantener el orden de la tabla original):
    reporte_país = [acumulado_país[1:3]..., positivos_reportados, sospechosos_reportados, negativos_reportados, número_IRAG, total_tests, acumulado_país[4:5]...]

    #Genera la fila de datos del día correspondiente:
    fila = string.(vcat(casos_estados..., reporte_país))

    #Agrega la fecha a la primer columna:
    pushfirst!(fila, fecha[1:4]*"-"*fecha[5:6]*"-"*fecha[7:8])

    #Agrega comas para el csv y lo junta todo en un string
    fila_csv = prod(fila.*",")

    #Agrega el resultado al csv acumulativo:
    open("Mexico_COVID19.csv", "a") do io

        write(io, fila_csv)
    end

    return "Done"
end
