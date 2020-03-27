#A partir de PDFs con tablas de la SSA, devuelve un CSV.

#Paquetería para leer pdfs.
using PDFIO

#Devuelve el texto del pdf como un string
function texto_pdf(archivo)

  #Abre el documento.
  documento = pdDocOpen(archivo)
  #Genera objetos que representan las páginas.
  pages = pdDocGetPageRange(documento, 1:pdDocGetPageCount(documento))
  #Creamos un buffer para compilar los datos.
  datos = IOBuffer()

  #Escribimos el texto al buffer.
  #Entre cada página forzamos una línea nueva.
  for page in pages

    pdPageExtractText(datos, page)
    write(datos, " \n ")
  end

  #Cerramos el documento.
  pdDocClose(documento)

  #Devolvemos el texto en un string.
  return String(take!(datos))
end

#Separa el string por líneas y elimina aquellas que no corresponden a casos.
function eliminar_nocasos(string)

  #Separa por línea
  rows = split(string, "\n")
  #Revisa que el inicio de la línea sea de la forma {espacios}Número y descarta la línea si no.
  filter!(row -> occursin(r"^ +\d", row), rows)
  #Transforma los substrings en strings:
  rows = String.(rows)

  return rows
end

#Elimina espacios adicionales y separa las columnas por comas.
function limpiar_fila(string)

  #Elimina el espacio inicial.
  a = replace(string, r"^ +" => "")
  #Elimina el espacio final y lo sustituye por un retorno de línea.
  b = replace(a, r" +$" => "\n")
  #Elimina espacios intermedios si son más de dos(para evitar problemas con los campos en si) y los reemplaza por una coma.
  c = replace(b, r" {2,}" => ",")
  #Corrige la ortografía de Querétaro:
  d = replace(c, r"QUERETARO" => "QUERÉTARO")
  #Cambia las fechas a formato ISO:
  row = replace(d, r"(\d{1,2})/(\d{1,2})/(\d{4})" => s"\3-\2-\1")

  return row
end

#Función principal que toma un nombre de archivo y escribe el archivo .csv correspondiente:
function scraping(archivo, sospechosos = false)

  #Remueve la extensión para nombrar el .csv apropiadamente
  nombre = replace(archivo, r".pdf" => "")

  #Obtenemos los casos en un array:
  casos = limpiar_fila.(eliminar_nocasos(texto_pdf(archivo)))

  #Escribe el archivo
  open(nombre*".csv", "w") do io

    if sospechosos
      write(io, "Número_caso,Estado,Localidad,Sexo,Edad,Fecha_síntomas,Situación,País_fuente,Fecha_regreso\n")
    else
      write(io, "Número_caso,Estado,Sexo,Edad,Fecha_síntomas,Situación,País_fuente,Fecha_regreso\n")
    end

    for caso in casos

      write(io, caso)
    end
  end

  return "Done"
end
