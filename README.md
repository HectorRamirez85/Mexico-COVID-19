# Base de datos de casos de COVID-19 reportados en México / Database of reported cases of COVID-19 in Mexico.

Última actualización/Last update: 2020-04-14 T 20:10:00-05:00

**Nota/Note:**

2020-04-14: Existe una discrepancia de una defunción (y por ende un caso positivo) en el estado de Duarango entre el día de ayer y el día de hoy en los Comunicados Técnicos Diarios (y las tablas de casos sospechosos y positivos).

2020-04-14: A inconsistency of one death (and thus one positive case) in Durango between yesterday and today in the Daily Technical Communiqués (and in the respective suspect and positive cases tables).


**Nota/Note:**

2020-04-12: La Secretaría de Salud ha publicado un archivo con datos abiertos sobre pacientes con COVID-19 en formato ```.csv```, disponible [aquí](https://www.gob.mx/salud/documentos/datos-abiertos-152127).
A partir de hoy archivaré las versiones de los datos allí contenidos en esta [carpeta](https://github.com/carranco-sga/Mexico-COVID-19/tree/master/Official%20Open%20Data%20Archive).
*Muchas gracias al Dr. Ricardo Cortés Alcalá por ayudar a hacer estos datos públicos.*

2020-04-12: The Health Secretariat has published a file with open data about COVID-19 patients in ```.csv``` format, available [here](https://www.gob.mx/salud/documentos/datos-abiertos-152127).
From now on I'll be archiving the data there published in this [folder](https://github.com/carranco-sga/Mexico-COVID-19/tree/master/Official%20Open%20Data%20Archive).
*Special thanks to Dr. Ricardo Cortés Alcalá for his help in making this data public.*

**Nota/Note:**

2020-04-11: Gracias a [@rodrigolece](https://github.com/rodrigolece), se modifican ligeramente las tablas de casos sospechosos y positivos en ```.csv``` para homogeneizar los formatos. Esencialmente se hacen los siguientes cambios:

- La columna ```Fecha_regreso``` se renombra a ```Fecha_llegada```.
- Correcciones de ortografía: "POTOSI => POTOSÍ" en las regiones sanitaria de las tablas que las reportan.
- Cambio de fechas "NA" a strings vacíos.
- Cambio de "MASCULINO"/"FEMENINO" a "M"/"F" en las tablas de abril, que corresponde con la forma en las que se reportaban esos datos en las tablas de marzo.

Adicionalmente, mayrop ([1](https://twitter.com/@mayrop), [2](https://www.covid19in.mx/)) [aquí](https://www.covid19in.mx/docs/datos/tablas-casos/normalizacion/fecha/) y Juan Claudio Toledo Roy identifican a los números que aparecen en los campos de fecha son correspondientes a una representación de fechas en Excel en el que el día 1900-01-01 se toma como día 1. ([1](https://support.office.com/en-us/article/datevalue-function-df8b07d4-7761-4a93-bc33-b7471bbff252))
Sin embargo, este formato considera que el año 1900 es bisiesto, por lo que para tranformar adecuadamente las fechas a partir del 1900-02-28, es requerido restar dos días ([2](https://docs.microsoft.com/en-us/office/troubleshoot/excel/wrongly-assumes-1900-is-leap-year)) para obtener el número correcto de días transcurridos del 1900-01-01.
*Muchas gracias a mayrop y a Juan Claudio Toledo Roy por sus comentarios sobre las fechas y a rodrigolece por sus mejoras al código de scraping.*

2020-04-11: Thanks to the work of [@rodrigolece](https://github.com/rodrigolece), the format of the positive and suspect cases ```.csv``` tables are modified slightly.
Essencially, the following changes were made:

- The column ```Fecha_regreso``` was renamed to ```Fecha_llegada```.
- Orthography corrections: "POTOSI => POTOSÍ" in the sanitary region of the cases in the tables that report them.
- Changes of the "NA" dates to empty strings.
- Changed "MASCULINO"/"FEMENINO" to "M"/"F" in the April tables, that is the way that the data was reported in the March tables.

Additionally, mayrop ([1](https://twitter.com/@mayrop), [2](https://www.covid19in.mx/)) [here](https://www.covid19in.mx/docs/datos/tablas-casos/normalizacion/fecha/) and Juan Claudio Toledo Roy identified the numbers appearing in the date fields as a way Excel represents dates, in which the day 1900-01-01 was taken as day 1. [1](https://support.office.com/en-us/article/datevalue-function-df8b07d4-7761-4a93-bc33-b7471bbff252)
However, this format considers the year 1900 as a leap year, so to transform correctly the dates from 1900-02-28,
substracting two to the Excel representation gives the number of days between the day represented and 1900-01-01. [2](https://docs.microsoft.com/en-us/office/troubleshoot/excel/wrongly-assumes-1900-is-leap-year)
*Special thanks to mayrop and to Juan Claudio Toledo Roy for their comments on the dates and to rodrigolece for his  improvement to the scraping code.*

**Nota/Note:**

2020-04-08: Desde hoy aparentemente ya no se va a reportar la procedencia de los casos.
Ya no se realizará distinción entre casos locales/importados

2020-04-08: From today onwards, it seems that the country of procedence of the cases is no longer reported.
No distinction will be longer imposed in local/imported cases.

**Nota *importante*/*Important* Note:**

2020-04-07: Las tablas de casos del día de ayer (2020-04-06) tienen un formato diferente a las de días pasados.
Se cambió la ortografía (se omitieron acentos), se ocupó "Distrito Federal" en lugar de "Ciudad de México", los países de los casos con antecedentes de viaje tienen sus nombres incompletos o confusos, se eliminó la fecha de importación, entre otras cosas.
Elí Parra ([1](https://twitter.com/elzr/status/1247428446751526913?s=20), [2](https://covidatos.mx/)); Luis G. Canales ([1](https://github.com/LGCO/MEXICO_COVID19_DATA)); Gerardo Mathus ([1](https://covid19.nextia.mx/)); Katia Guzmán Martínez ([1](https://github.com/guzmart/covid19_mex)), entre otros, han señalado la existencia de inconsistencias adicionales en los datos.
Desconozco la razón por la cual se realizaron estos cambios.
Personalmente me he limitado a cambiar la ortografía de las entidades de las tablas para constuir los archivos [```positivos_20200406.csv```](https://github.com/carranco-sga/Mexico-COVID-19/blob/01a9a953e23c1fa135be3c78e793500604b58eb1/Daily%20data/202004/positivos_20200406.csv) y [```sospechosos_20200406.csv```](https://github.com/carranco-sga/Mexico-COVID-19/blob/01a9a953e23c1fa135be3c78e793500604b58eb1/Daily%20data/202004/sospechosos_20200406.csv) a partir de las tablas originales archivadas [aquí](https://github.com/carranco-sga/Mexico-COVID-19/blob/01a9a953e23c1fa135be3c78e793500604b58eb1/Documents/Tablas%20casos/positivos_20200406.pdf) y [aquí](https://github.com/carranco-sga/Mexico-COVID-19/blob/01a9a953e23c1fa135be3c78e793500604b58eb1/Documents/Tablas%20casos/sospechosos_20200406.pdf).
Para el análisis de separar casos como locales o importados por estado, mostrado [aqui](https://github.com/carranco-sga/Mexico-COVID-19/blob/01a9a953e23c1fa135be3c78e793500604b58eb1/Mexico_COVID19.csv) y [aquí](https://github.com/carranco-sga/Mexico-COVID-19/blob/01a9a953e23c1fa135be3c78e793500604b58eb1/Mexico_COVID19.ods), las palabras clave en la columna ```País_fuente``` de la lista de casos positivos usadas para clasificar un caso positivo como local se muestran [aquí](https://github.com/carranco-sga/Mexico-COVID-19/blob/01a9a953e23c1fa135be3c78e793500604b58eb1/Scraping/csv_daily_scraping.jl#L10).
*Gracias a Luis G. Canales y a Luis López Martinelli por sus observaciones.*

2020-04-09, addenda: Respecto a los datos del día 2020-04-06 en los que existen varias inconsistencias, [wfms](https://www.wfms.org/mx-covid19/) también realiza algunos comentarios al respecto.
Adicionalmente, identifica los números aleatorios que aparecen en los campos de fechas como errores de "Power Query" de Excel. *Gracias a wfms por sus comentarios.*

2020-04-11, addenda: Adicionalmente, se me olvidó comentar que hice las siguientes modificaciones a los países de procedencia:
Reeplacé "Trieste" por "Italia" y "Estados" por "Estados Unidos".

2020-04-07: The cases tables from yesterday (2020-04-06) have a different format from those from other days.
The orthography was changed (accents were ommited), the term "Distrito Federal" (*Federal District*) was used instead of "Ciudad de México" (*Mexico City*), the countries of the cases with history of travel have their names incomplete or used in an inconsistent manner, the date of importation of the case was deleted, among other things.
Elí Parra ([1](https://twitter.com/elzr/status/1247428446751526913?s=20), [2](https://covidatos.mx/)); Luis G. Canales ([1](https://github.com/LGCO/MEXICO_COVID19_DATA)); Gerardo Mathus ([1](https://covid19.nextia.mx/)); Katia Guzmán Martínez ([1](https://github.com/guzmart/covid19_mex)), among others, have commented on the existence of additional inconsistencies in the data.
I don't know the reason behind these changes.
Personally, I have limited myself to change the ortography of the entities in the tables to build the files [```positivos_20200406.csv```](https://github.com/carranco-sga/Mexico-COVID-19/blob/01a9a953e23c1fa135be3c78e793500604b58eb1/Daily%20data/202004/positivos_20200406.csv) and [```sospechosos_20200406.csv```](https://github.com/carranco-sga/Mexico-COVID-19/blob/01a9a953e23c1fa135be3c78e793500604b58eb1/Daily%20data/202004/sopechosos_20200406.csv) from the original tables archived [here](https://github.com/carranco-sga/Mexico-COVID-19/blob/01a9a953e23c1fa135be3c78e793500604b58eb1/Documents/Tablas%20casos/positivos_20200406.pdf) and [here](https://github.com/carranco-sga/Mexico-COVID-19/blob/01a9a953e23c1fa135be3c78e793500604b58eb1/Documents/Tablas%20casos/sospechosos_20200406.pdf).
To do the analysis of classifying the positive cases per state as local or imported, shown [here](https://github.com/carranco-sga/Mexico-COVID-19/blob/01a9a953e23c1fa135be3c78e793500604b58eb1/Mexico_COVID19.csv) and [here](https://github.com/carranco-sga/Mexico-COVID-19/blob/01a9a953e23c1fa135be3c78e793500604b58eb1/Mexico_COVID19.ods)
, the keywords in the column ```País_fuente```of the list of positive cases used to classified a case as local are shown
[here](https://github.com/carranco-sga/Mexico-COVID-19/blob/01a9a953e23c1fa135be3c78e793500604b58eb1/Scraping/csv_daily_scraping.jl#L10).
*Special thanks to Luis G. Canales and to Luis López Martinelli for their observations.*

2020-04-09, addenda: Regarding the data of the day 2020-04-06 in which several inconsistencies are found, [wfms](https://www.wfms.org/mx-covid19/) also comments about them.
Additionally, they identify the random numbers that appear on the data fields as errors of "Power Query" of Excel.
*Thanks wfms for their comments.*

2020-04-11, addenda: Additionally, I forgot to comment that I made the following modifications to the countries of procedence:
I replaced "Trieste" by "Italia" and "Estados" by "Estados Unidos".

## Notas/Notes (archivo/archive):

**Nota/Note:**
Los casos recuperados ya no se listan en el CTD desde el 2020-03-23.
**La situación de cada paciente (recuperado/fallecido) no será actualizada a partir del 2020-03-23.**

Recovered cases are no longer  provided in the CTD for 2020-03-23 onwards.
***No* particular patient status (recovered/deceased) will be updated after 2020-03-23.**

**Nota/Note:**
2020-03-29: El número de pruebas realizadas en el país se corrigió retroactivamente para no incluir casos sospechosos, puesto que no se conoce con certeza si en realidad les fueron realizadas pruebas de laboratorio. Ahora sólo consiste de la suma del número de casos positivos con el número de casos negativos y las pruebas de SISVEFLU/IRAG para SARS-CoV-2. *Gracias a Luis López Martinelli por su observación*.

2020-03-29: The number of total tests done in the country was retroactively changed as to not include suspect cases, because it is not known certainly if they were lab-tested. The number currently is the sum of the number of positive cases with the number of negative cases and the random tests of samples coming from SISVEFLU/IRAG for SARS-CoV-2. *Special thanks to Luis López Martinelli  for his observation*.

**Nota/Note:**
2020-04-03: El comunicado técnico diario ya no reporta el número de pruebas de SISVEFLU/IRAG para SARS-CoV-2. Se deja de reportar este dato. El número de pruebas totales consiste en el número de casos positivos más el número de casos negativos.

2020-04-03: The daily technical communiqué no longer reports the number of SISVEFLU/IRAG tests for SARS-CoV-2. This datum will no longer be reported. The number of total tests done consists on the number of total positive cases plus the number of negative cases.

**Nota/Note:**
2020-04-04: Algunas de las fechas del inicio de síntomas en las tablas oficiales de **casos sospechosos** son sustituidas por números de forma intermitente desde finales de marzo. Esto es reflejado en los archivos ```.csv``` respectivos. Se desconoce el significado de los mismos. *Gracias a Erik Salgado Estrada por su observación.*

2020-04-04: Some of the dates of the onset of symptoms in the official tables of **supect cases** are substituted by numbers intermitently since late March. This is reflected in the respective ```.csv``` files. It's unknown if they mean anything specific. *Special thanks to Erik Salgado Estrada for his observation*.

## Más información / Extra info:

Información del sistema hospitalario en México [aquí](http://www.dgis.salud.gob.mx/contenidos/publicaciones/p_bie_gobmx.html).

Information of the mexican hospitalary system [here](http://www.dgis.salud.gob.mx/contenidos/publicaciones/p_bie_gobmx.html).


Dataset de latinoamérica [aquí](https://github.com/DataScienceResearchPeru/covid-19_latinoamerica).

Latin american dataset [here](https://github.com/DataScienceResearchPeru/covid-19_latinoamerica).


Visualización de los datos reportados por Joel Ricci-López (@jRicciL) [aqui](https://joelricci.shinyapps.io/covid_mx_jrl/).

Visualization of the reported data by Joel Ricci-López (@jRicciL) [here](https://joelricci.shinyapps.io/covid_mx_jrl/).


Mapa oficial de la DGE (Dirección General de Epidemiología) [aquí](http://ncov.sinave.gob.mx/mapa.aspx).

Official map of the DGE (General Directorate of Epidemiology by its acronym in Spanish) [here](http://ncov.sinave.gob.mx/mapa.aspx).


Un tablero bastante completo sobre el panorama en el país está disponible [aquí](http://covidatos.mx/)

A really complete dashboard with a national overview is available [here](http://covidatos.mx/)


Una visualización de datos internacional por país por Jason Campos [aquí](https://covid19.jsoncampos.com/).

A visualization of international data by country made by Jason Campos [here](https://covid19.jsoncampos.com/).


Una visualización de datos nacionales con desglose de casos por edad y sexo realizada por Erik Salgado [aquí](https://www.mappenterprise.net/covid19mapa)

A visualization of national data with some graphs of the cases by age and gender made by Erik Salgado [here](https://www.mappenterprise.net/covid19mapa)

Una visualización de datos interactiva por fecha y con representación de carga de enfermedad realizada por Francisco Leyva [aquí](https://covid19mexico.net/).

An interactive visualization of the data by date and with a graphical representation of the number of cases made by Francisco Leyva [here](https://covid19mexico.net/).

Una animación del número de casos por entidad a forma de carrera, realizada por Hugo Mosh Cardoza [aquí](https://observablehq.com/@hugomosh/progreso-covid-19-mexico)

An animation of the number of cases by state represented as a race, made by Hugo Mosh Cardoza [here](https://observablehq.com/@hugomosh/progreso-covid-19-mexico)

## Español
Información de casos de COVID-19 dada a conocer por el Gobierno Federal de México desglosada por entidad.

La información es extraída de los Comunicados Técnicos Diarios (CTD) publicados por la Secretaría de Salud Federal.

Los datos por día y estado están contenidos en ```Mexico_COVID19.ods```. Un archivo ```.csv``` con datos idénticos se proporciona por conveniencia.
Las columnas están nombradas de acuerdo al siguiente formato: ```EDO_{key}```. ```EDO``` es el [código ISO de tres letras del estado](https://www.iso.org/obp/ui/#iso:code:3166:MX). ```key``` puede tener los siguientes valores:

- ```S```: representa el número total de casos sospechosos en el estado.
- ```I```: representa el número de casos confirmados(°) importados del extranjero al estado.
- ```L```: representa el número de casos confirmados locales dados en el estado.
- *ninguno*: representa el número total de casos confirmados en el estado.
- ```R```: representa el número total de casos confirmados recuperados(°°) en el estado. No se publica información del número por estado desde el 23 de Marzo.
- ```D```: representa el número total de casos confirmados fallecidos en el estado.

Adicionalmente,  se tienen las siguientes columnas:

- ```Fecha```: fecha dada en formato ISO.
- ```Susp```: representa el número total de casos sospechosos en el país. (Difiere en uno de la cuenta oficial el 23 de marzo debido a que el paciente 576 no tiene datos de entidad ni localidad registrados.)
- ```Pos_I```: es el número total de casos confirmados importados del extranjero al país. (Suma de las columnas ```EDO_I```.)
- ```Pos_L```: es el número total de casos confirmados locales dados en el país. (Suma de las columnas ```EDO_L```.)
- ```Pos```: es el número total de casos confirmados en el país, independientemente de su origen. (Suma de las columnas ```EDO_I``` y ```EDO_L```.) (Difiere en una unidad del valor oficial dado entre el 11 y el 13 de marzo debido a que en las listas oficiales el caso de Sinaloa recuperado se dejó de contabilizar, para volverse a listar a partir del 14 de marzo, igual que con los demás casos recuperados.)
- ```Pos_rep```: es el número total de casos confirmados en el país, independientemente de su origen, reportados oficialmente. ( *Confirmados* en el CTD.)
- ```Susp_rep```: es el número total de casos sospechosos en el país, reportados oficialmente. ( *Sospechosos* en el CTD.)
- ```Neg_rep```: es el número total de casos descartados (por laboratorio) en el país, reportados oficialmente. ( *Negativos* en el CTD.)
- ```IRAG_Test```: **No reportado desde el 2020-04-03** es el número total de muestras de pacientes al azar(°°°) cuya muestra fue analizada en busca de SARS-CoV-2. (Reportado en el CTD a partir del 25 de febrero, anunciado el 23 de febrero.)
- ```Tested_total```: es el número total de pruebas realizadas en el país, según datos oficiales. (Suma de las columnas ```Pos``` y ```Neg_rep```.)
- ```Recovered```: es el número total de casos confirmados recuperados en el país. (Suma de las columnas ```EDO_R```.)
No hay números exactos reportados a partir del 23 de marzo.
- ```Deceased```: es el número total de casos confirmados fallecidos en el país. (Suma de las columnas ```EDO_D```.)

(°) Caso confirmado consiste en un paciente cuya muestra resultó positiva a la búsqueda de SARS-CoV-2. (No es del todo claro en los datos oficiales si estos incluyen el caso de portadores que se mencionan en los CTD. El incremento en las cifras (y el listado del primer portador en la lista de casos oficiales) parece sugerir que sí es el caso.)

(°°) Caso recuperado consiste en un paciente que no presenta sintomatología y cuya segunda muestra ha dado negativo a la presencia de SARS-CoV-2.

(°°°) Pacientes no sospechosos de COVID-19 (i.e. que visitaron un país con tranmisión comunitaria activa y presentan sintomatología o que son contactos de pacientes con COVID-19) con sintomatología de enfermedad respiratoria aguda de etiología desconocida analizada al azar para detección de SARS-CoV-2.
(Nota: Esta definición cambió desde al menos el 23 de marzo; el número de estas pruebas se ha mantenido constante desde el 11 de marzo.)

### Notas:

- Hay algunos trabajos semejantes, pero diferentes en GitHub: [1](https://github.com/guzmart/covid19_mex), [2](https://github.com/alancarrillop/COVID19_Mexico).
- Los casos sospechosos sí se han presentado desglosados por entidad, sin embargo, a partir del 29 de febrero y hasta el 13 de marzo, no se cuentan con las tablas en las que se desglosan.
- La cuenta de casos locales difiere de la oficial en uno dado a que en algunos CTD un paciente se registra como "Contacto/Cuba" o "Contacto". Se elige reportarlo en esta base de datos como "Contacto" (Local).

¡Cualquier contribución  es bienvenida!
Contacto: carranco[punto]sga[arroba]ciencias[punto]unam[punto]mx

## English

Information of cases of COVID-19 distributed by the Mexican Federal Government reported by state.

The information is extracted of the Daily Technical Communiqués (CTD by its acronym in Spanish) published by the Federal Health Secretariat.

The data by day and state are contained in ```Mexico_COVID19.ods```. A ```.csv``` file with identical data is uploaded for convenience.

The columns are named according to the following format: ```EDO_{key}```. ```EDO``` is the [three-letter ISO code for the state](https://www.iso.org/obp/ui/#iso:code:3166:MX). ```key``` takes the following values:

- ```S```: represents the total number of suspect cases in the state.
- ```I```: represents the number of confirmed(°) cases, imported from abroad to the state.
- ```L```: represents the number of confirmed local cases in the state.
- *none*: represents the total number of cases in the state.
- ```R```: represents the number of confirmed cases that have recovered(°°) in the state. No information about the number of recovered cases per state is published from March 23 onwards.
- ```D```: represents the number of confirmed cases that have died in the state.

Aditionally, we have the following columns:

- ```Fecha```: date given in ISO format.
- ```Susp```: is the total number of suspect cases in the country. (It differs in one from the official tally on March 23 because patient 576 has no state or municipality registered.)
- ```Pos_I```:  is the total number of confirmed cases, imported from abroad into the country. (Sum of the ```EDO_I``` columns.)
- ```Pos_L```: is the total number of confirmed local cases. (Sum of the ```EDO_L``` columns.)
- ```Pos```: is the total number of confirmed cases in the country, independent of their origin. (Sum of the ```EDO_I``` and ```EDO_L``` columns.) (It differs from the official number of cases in the country by a unit from March 11 to March 13 because the official tally doesn't include the recovered case in Sinaloa on this period. It is again included after March 14, along all other recovered cases.)
- ```Pos_rep```: is the total number of officially reported confirmed cases in the country, independent of their origin. ( *Confirmados* in the CTD.)
- ```Susp_rep```: is the total number of officially suspected cases in the country. ( *Sospechosos* in the CTD.)
- ```Neg_rep```: is the total number of officially discarded (negative lab test) cases in the country. ( *Negativos* in the CTD.)
- ```IRAG_Test```: **no longer reported (2020-04-03)** is the total number of samples from random patients(°°°) whose sample was tested for SARS-CoV-2. (Reported in the CTD from February 25 onwards, announced on February 23.)
- ```Tested_total```: is the total number of tests done in the country, according to official data. (Sum of the ```Pos```, and ```Neg_rep``` columns.)
- ```Recovered```: is the total number of confirmed recovered cases in the country. (Sum of the ```EDO_R``` columns.)
No exact number of recovered patients has been reported from March 23 onwards.
- ```Deceased```: is the total number of confirmed deceased cases in the country. (Sum of the ```EDO_D``` columns.)

(°) A confirmed case consists in a patient whose sample tested positive for SARS-CoV-2. (It isn't fully clear in the official data if these numbers contain asymptomatic carriers (*portadores*) mentioned in the CTD. The increase in the numbers (and that the first carrier was listed in the official case list) suggests the numbers do reflect the mentioned carriers.)

(°°) A recovered case consists in a patient that no longer displays symptoms and whose second sample has tested negative for SARS-CoV-2.

(°°°) Non-suspicious patients for COVID-19 (i.e. that visited a country with active community transmission and present symptoms or that are contacts of patients with COVID-19) with acute respiratory illness with unknown etiology randomly screened for SARS-CoV-2.
(Note: This definition was changed since at least March 23; the number of these tests has been the same since March 11.)

### Notes:

- Similar, but different works are available in GitHub: [1](https://github.com/guzmart/covid19_mex), [2](https://github.com/alancarrillop/COVID19_Mexico).
- Suspect cases have been presented by state, however, from February 29 until March 13, we don't have the tables in which they are detailed.
- The count of local cases differs from the official one in one because in some of the CTD a patient is registered as "Contacto/Cuba" or "Contacto". We report them as "Contact" (local) in this database.

Any contribution is welcome!
Contact: carranco[dot]sga[at]ciencias[dot]unam[dot]mx
