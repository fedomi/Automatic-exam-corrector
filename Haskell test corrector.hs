-- Práctica de Programación Declarativa
-- Alumno: Fernando Domínguez Estévez
-- Titulación: Doble grado Informática-Matemáticas

-- Descripción de la práctica: En esta práctica tenemos que hacer un corrector automático de exámenes
-- tipo test en Haskell utilizando los conceptos de programación funcional aprendidos durante el curso.

-- Uso de la práctica: Para utilizar la interfaz de I/O, tan solo basta con llamar a 'main', y dará
-- comienzo el programa principal. Para probar cada función por separado, se dispone de una pequeña base
-- de datos destinada a hacer estas pruebas, que se puede encontrar en la parte de abajo del fichero.

--------------------------------------------------------------------------------------------------------
--APARTADO 1 : DEFINICION DE TIPOS
--Vamos a definir los tipos que necesitaremos:


--PREGUNTAS
--N_resp: Numero de respuestas
--C_resp: Respuesta correcta
--Puntuacion: Puntos que vale la pregunta
--Id_P: Identificador de la pregunta
type N_resp  = Float
type C_resp  = Int
type Puntuacion = Float
type Id_P = Int 

data Pregunta = Pregunta N_resp C_resp Puntuacion Id_P deriving Show

--Ahora definiremos funciones que, aplicadas al tipo Pregunta definido por nosotros, nos devuelven
--alguno de sus atributos. Estas seran muy utiles mas adelante
getRespuestaCorrecta :: Pregunta -> C_resp
getRespuestaCorrecta (Pregunta _ cr _ _) = cr

getNumeroRespuestas :: Pregunta -> N_resp
getNumeroRespuestas (Pregunta nr _ _ _) = nr

getPuntuacion :: Pregunta -> Puntuacion
getPuntuacion (Pregunta _ _ p _) = p

--TEST
--Id_T: Identificador del test
--[Pregunta]: Lista de preguntas del test
type Id_T = String

data Test = Test Id_T [Pregunta] deriving Show

--MODELOS DE EXAMEN
--Id_M: Identificador del modelo
--Orden: Lista de enteros que representa el orden de las preguntas en este modelo. Por ejemplo, la 
--       lista [2,3,1] indica que primero va la segunda pregunta, luego la tercera y por último la primera.
type Id_M = String
type Orden = [Int]

data Modelo = Modelo Orden Id_M Id_T deriving Show

getOrden :: Modelo -> Orden
getOrden (Modelo o _ _) = o

--RESPUESTAS
--Respuesta: Opción elegida en una pregunta. Cada RespuestaTest tiene una lista de Respuesta, que 
--           representan a las respuestas de cada pregunta.
--Estudiante: Estudiante que responde el test
--Modelo: Modelo del test respondido
--Id_T: Identificador del test respondido.
type Respuesta = Int

data RespuestaTest = RespuestaTest [Respuesta] Estudiante Modelo Id_T deriving Show

--ESTUDIANTE
--Nombre: Nombre del estudiante
--Apellido: Apellido del estudiante
--Id_E: Identificador del estudiante
type Nombre = String
type Apellido = String
type Id_E = String 

data Estudiante = Estudiante Nombre Apellido Id_E deriving Show

getNombre (Estudiante n _ _) = n

------------------------------------------------------------------------------------------------------
--APARTADO 2 : CORREGIR UN TEST

-- Vamos a definir el tipo Correccion, que necesitaremos para las funciones que corrigen los test
--Nota: Nota total del test, según las puntuaciones que tiene asociadas cada pregunta.
--Nota_10: Nota sobre 10, calculada a partir de la nota anterior.
--N_Contestadas: Numero de preguntas contestadas.
--N_Blanco: Numero de preguntas dejadas en blanco. 
type Nota = Float
type Nota_10 = Float
type N_Contestadas = Float
type N_Blanco = Float

data Correccion = Correccion Estudiante Nota Nota_10 N_Contestadas N_Blanco deriving Show

getEstudiante (Correccion e _ _ _ _) = e
getNota (Correccion _ n _ _ _) = n
getNota10 (Correccion _ _ n10 _ _) = n10
getNBlanco (Correccion _ _ _ _ nb) = nb
getNContestadas (Correccion _ _ _ nc _) = nc

correccionString c = (" --RESULTADOS DEL TEST-- " ++ " Alumno: " ++ (getNombre (getEstudiante c)) ++ "\n" ++ "  Puntuacion total: " ++ (show(getNota c)) ++ "\n" ++ "  Nota sobre 10: " ++ (show(getNota10 c)) ++ "\n")

--A continuación se presentan las funciones 'corrige' y 'evalua', encargadas de corregir un test. En ambas
--se tiene en cuenta el orden en el que puedan estar las respuestas, según el modelo de test respondido.

--'corrige' coge el test y las respuestas, y si las respuestas son para ese test,  llama a la funcion que 
--compara la solucion y las respuestas del alumno
corrige (Test id1 preguntas) (RespuestaTest respuestas alumno modelo id_test)
     | id1 == id_test = evalua preguntas respuestas (getOrden modelo) alumno 0 0 0 0
 
--'evalua' coge las preguntas y las respuestas y las compara para ver si coinciden, y calcula la nota
evalua [] _ _ alumno nota maxNota n_cont n_blanco = Correccion alumno nota (10*(nota/maxNota)) n_cont n_blanco 
evalua _ [] _ alumno nota maxNota n_cont n_blanco = Correccion alumno nota (10*(nota/maxNota)) n_cont n_blanco 
evalua (p:ps) (respAlumno:rs) (o:os) alumno nota maxNota n_cont n_blanco 
    | respAlumno == 0 = evalua (p:ps) rs os alumno nota (maxNota+(getPuntuacion ((p:ps) !! (o-1)))) n_cont (n_blanco+1)
    | getRespuestaCorrecta ((p:ps) !! (o-1)) == respAlumno = evalua (p:ps) rs os alumno (nota+(getPuntuacion ((p:ps) !! (o-1)))) (maxNota+(getPuntuacion ((p:ps) !! (o-1)))) (n_cont+1) n_blanco
    | otherwise = evalua (p:ps) rs os alumno (nota - (1/((getNumeroRespuestas ((p:ps) !! (o-1)))-1))) (maxNota+(getPuntuacion ((p:ps) !! (o-1)))) (n_cont+1) n_blanco

--APARTADO 3 : ESTADISTICAS----------------------------------------------------------------------
--Ya tenemos las funciones que corrigen un test con unas respuestas dadas. Ahora haremos otras funciones que 
--den una serie de estadísticas a partir de un conjunto de respuestas.

--Para ello necesitamos definir algunos tipos más

--N_SS: Número de sobresalientes
--N_NT: Número de notables
--N_AP: Número de aprobados
--N_SP: Número de suspensos
type N_SS = Int
type N_NT = Int
type N_AP = Int
type N_SP = Int

data Calificaciones = Calificaciones N_SS N_NT N_AP N_SP deriving Show

getSuspensos (Calificaciones _ _ _ ns) = ns
getAprobados (Calificaciones _ _ na _) = na
getNotables (Calificaciones _ nn _ _) = nn
getSobresalientes (Calificaciones nss _ _ _) = nss


showCalificaciones c = (" \n --RESULTADOS EN LAS CALIFICACIONES-- \n" ++ "  Numero de suspensos: " ++ (show (getSuspensos c)) ++ "\n" ++ "  Numero de aprobados: " ++ (show (getAprobados c)) ++ "\n" ++ "  Numero de notables: " ++ (show (getNotables c)) ++ "\n" ++ "  Numero de sobresalientes: " ++ (show (getSobresalientes c)) ++ "\n")


--Nota_Media: Nota media entre todas las respuestas proporcionadas
--Media_Respondidas: Media de preguntas respondidas en las respuestas proporcionadas
type Nota_Media = Float
type Media_Respondidas = Float

data Estadisticas = Estadisticas Nota_Media Media_Respondidas Calificaciones deriving Show-- Para hacer preubas estadistica a estadistica

getNotaMedia (Estadisticas nm _ _) = nm
getMediaRespondidas (Estadisticas _ mr _) = mr
getCalificaciones (Estadisticas _ _ c) = c

showEstadisticas e = ("--ESTADISTICAS DE LAS RESPUESTAS-- \n" ++ "  Nota Media: " ++ (show (getNotaMedia e)) ++ "\n" ++ "  Media de preguntas respondidas: " ++ (show (getMediaRespondidas e)) ++ "\n" ++ showCalificaciones (getCalificaciones e) ++ "\n")

estadisticas :: Test -> [RespuestaTest] -> Estadisticas
estadisticas t (r:rs) = Estadisticas (calculaMedia t (r:rs)) (calculaMediaRespondidas t (r:rs)) (Calificaciones (cuentaNotasEntreAyB t (r:rs) 9 11) (cuentaNotasEntreAyB t (r:rs) 5 7) (cuentaNotasEntreAyB t (r:rs) 7 9) (cuentaNotasEntreAyB t (r:rs) 0 5))

--Media de las notas-------------------------------------
calculaMedia :: Test -> [RespuestaTest] -> Float
calculaMedia t (r:rs) = calculaMediaAux t (r:rs) (length_T (r:rs))

calculaMediaAux :: Test -> [RespuestaTest] -> Float -> Float
calculaMediaAux t [] n = 0
calculaMediaAux t (r:rs) n = ((getNota10 (corrige t r))/ n) + (calculaMediaAux t rs n)

--Usamos esta funcion para poder dividir por 'n' sin problemas de tipo en calculaMediaAux 
length_T :: [RespuestaTest]->Float
length_T [] = 0
length_T (r:rs) = 1 + length_T rs
--------------------------------------------------------
--Media de preguntas respondidas------------------------

calculaMediaRespondidas :: Test -> [RespuestaTest] -> Float
calculaMediaRespondidas t (r:rs) = calculaMediaRespondidasAux t (r:rs) (length_T (r:rs))

calculaMediaRespondidasAux :: Test -> [RespuestaTest] -> Float -> Float
calculaMediaRespondidasAux t [] n = 0
calculaMediaRespondidasAux t (r:rs) n = ((getNContestadas (corrige t r))/ n) + (calculaMediaRespondidasAux t rs n)

--Numero de Suspensos, Aprobados, Notables y Sobresalientes
cuentaNotasEntreAyB :: Test -> [RespuestaTest] -> Float -> Float -> Int
cuentaNotasEntreAyB t [] a b = 0
cuentaNotasEntreAyB t (r:rs) a b 
     | (((getNota10 (corrige t r)) >= a) && ((getNota10 (corrige t r)) < b)) = 1 + cuentaNotasEntreAyB t rs a b
     | otherwise = cuentaNotasEntreAyB t rs a b

----------------------------------------------------------
-- Aquí definimos los tipos necesarios para el cálculo de frecuencias
type Acertadas = Float
type Falladas = Float
type Blanco = Float 
data FrecAbs = FrecAbs Acertadas Falladas Blanco deriving Show
data FrecRel = FrecRel Acertadas Falladas Blanco deriving Show

data Frecuencias = Frecuencias Id_P FrecAbs FrecRel deriving Show



-- Y aquí las funciones necesarias para estos cálculos. Trabajan de la siguiente manera: Para cada test, va cogiendo pregunta por pregunta, y para cada una de ellas,
-- calcula las frecuencias absolutas y relativas, contando el número de veces que se ha acertado, fallado o dejado en blanco, y luego devuelve los datos en 
-- el tipo 'Frecuencias' descrito más arriba

--frecuenciasTest :: Test -> [RespuestaTest] -> [Frecuencias]
frecuenciasTest (Test id1 preguntas) ((RespuestaTest respuestas alumno modelo id_test):rs)
    | id1 == id_test = frecuenciasTestAux preguntas ((RespuestaTest respuestas alumno modelo id_test):rs) (length_T ((RespuestaTest respuestas alumno modelo id_test):rs))

frecuenciasTestAux [] (r:rs) n = []
frecuenciasTestAux (p:ps) (r:rs) n = (calculaFrecuencias p (r:rs) n 0 0 0) : frecuenciasTestAux ps (r:rs) n

--calculaFrecuencias :: Pregunta -> [RespuestaTest] -> Float -> Float -> Float -> Frecuencias
calculaFrecuencias (Pregunta n_r c_r punt id) [] n n_a n_f n_b = Frecuencias id (FrecAbs n_a n_f n_b) (FrecRel (n_a/n) (n_f/n) (n_b/n))
calculaFrecuencias (Pregunta n_r c_r punt id) (((RespuestaTest respuestas alumno modelo id_test)):rs) n n_a n_f n_b
    | (buscarRespuesta id respuestas (getOrden modelo)) == c_r = calculaFrecuencias (Pregunta n_r c_r punt id) rs n (n_a+1) n_f n_b
    | (buscarRespuesta id respuestas (getOrden modelo)) == 0 = calculaFrecuencias (Pregunta n_r c_r punt id) rs n n_a n_f (n_b+1)
    | otherwise = calculaFrecuencias (Pregunta n_r c_r punt id) rs n n_a (n_f+1) n_b

buscarRespuesta id_p (r:rs) (o:os) 
    | o == id_p = r
    | otherwise = buscarRespuesta id_p rs os

---------------------------------------------------------------
-- Ahora definiremos los tipos y funciones para obtener las mejores/peores preguntas, y tambien mas/menos respondidas en blanco. Tenemos una funcion para buscar cada una de ellas, y al final estos datos
-- se dan encapsulados dentro del tipo 'Mejores'

--Mejor_P: Pregunta con mejores resultados (más veces acertada)
--Peor_P: Pregunta con peores resultados (más veces fallada)
--Mas_Blanco: Pregunta más veces dejada en blanco
--Menos_Blanco: Pregunta más veces respondida 
type Mejor_P = Int
type Peor_P = Int
type Mas_Blanco = Int
type Menos_Blanco = Int
data Mejores = Mejores Mejor_P Peor_P Mas_Blanco Menos_Blanco deriving Show

getMejor (Mejores mp _ _ _) = mp
getPeor (Mejores _ p _ _) = p
getMasB (Mejores _ _ masb _) = masb
getMenosB (Mejores _ _ _ menosb) = menosb


showMejores m = ("\n --ESTADISTICAS DE LAS PREGUNTAS-- \n" ++ "  Pregunta con mejores resultados: " ++ (show (getMejor m)) ++ "\n" ++ "  Pregunta con peores resultados: " ++ (show (getPeor m)) ++ "\n" ++ "  Pregunta mas veces en blanco: " ++ (show(getMasB m)) ++ "  Pregunta mas veces respondida: " ++ (show(getMenosB m)) ++ "\n")


mejores t (r:rs) = Mejores (buscaMejor (frecuenciasTest t (r:rs)) 0 0) (buscaPeor (frecuenciasTest t (r:rs)) 0 0) (buscaMasBlanco (frecuenciasTest t (r:rs)) 0 0) (buscaMenosBlanco (frecuenciasTest t (r:rs)) 0 1000)

buscaMejor [] act_p act_n = act_p
buscaMejor ((Frecuencias id (FrecAbs a f b) _):fs) act_p act_n  
    | a > act_n = buscaMejor fs id a
    | otherwise = buscaMejor fs act_p act_n

buscaPeor [] act_p act_n = act_p
buscaPeor ((Frecuencias id (FrecAbs a f b) _):fs) act_p act_n  
    | f > act_n = buscaPeor fs id f
    | otherwise = buscaPeor fs act_p act_n

buscaMasBlanco [] act_p act_n = act_p
buscaMasBlanco ((Frecuencias id (FrecAbs a f b) _):fs) act_p act_n 
    | b > act_n = buscaMasBlanco fs id b
    | otherwise = buscaMasBlanco fs act_p act_n

buscaMenosBlanco [] act_p act_n = act_p
buscaMenosBlanco ((Frecuencias id (FrecAbs a f b) _):fs) act_p act_n 
    | b < act_n = buscaMenosBlanco fs id b
    | otherwise = buscaMenosBlanco fs act_p act_n


---------------------------------------------------------------------
-- APARTADO 4: IO (ENTRADA/SALIDA)-----------------------------------


-- Main del programa con IO. Es el menú del programa, desde el cual accedemos al resto de opciones.
main :: IO ()
main = do
    putStrLn " ---------PRACTICA PD: CORRECTOR DE TEST ----------\n"
    putStrLn "  1- Corregir test "
    putStrLn "  2- Estadisticas"
    putStr "  Escoge una opcion: "
    opcion <- getLine
    case opcion of
        "1" -> correccion
        "2" -> calculaEstadisticas
        _ -> putStrLn "Adios \n"


-- Opcion 1 seleccionada, aquí introducimos los datos de un test y de la solución propuesta, y lo corrige. Para introducir los datos se ayuda de las funciones descritas después.
correccion :: IO ()
correccion = do
    putStr "\n Introduce los datos del test: \n"
    test <- introducirTest
    putStr "Introduce los datos de las respuestas: \n"
    respuestas <- introducirRespuestas
    putStrLn (correccionString (corrige test respuestas))

-- Introduce los datos de un test.
introducirTest :: IO Test
introducirTest = do
    putStr "Introduce el ID del test: "
    id_T <- getLine
    putStr "Introduce el numero de preguntas: "
    n <- getLine
    putStr "Ahora introducimos las preguntas del test: \n"
    p <- (introducirPreguntas (read n::Int) [])
    return (Test id_T p)

-- Devuelve la lista de preguntas
introducirPreguntas n p = do
    if n == 0 then (sequence p)
              else introducirPreguntas (n-1) ((introdP):p) 

--Introduce una nueva pregunta
introdP = do 
    putStr "Introduciendo pregunta nueva: \n"
    putStr "Introduce el numero de respuestas: "
    n_r <- getLine
    putStr "Introduce el numero de la respuesta correcta: "
    c_r <- getLine
    putStr "Introduce la puntuacion que suma la pregunta: "
    punt <- getLine
    putStr "Introduce el id de la pregunta: "
    id_P <- getLine
    return (Pregunta (read n_r::Float) (read c_r::Int) (read punt::Float) (read id_P :: Int))

--Introduce una nueva respuesta al test
introducirRespuestas = do
    putStr "Introduce el numero de preguntas que tiene el test: "
    n <- getLine
    putStr "Introduce las respuestas del test: \n"
    r <- introduceSolucion (read n::Int) []
    putStr "Introduce los datos del estudiante: \n"
    e <- introduceEstudiante
    putStr "Introduce los datos del modelo: \n"
    m <- (introduceModelo)
    putStr "Introduce el ID del test respondido: "
    id_T <- getLine
    return (RespuestaTest r e m id_T)

-- Devuelve la lista de respuestas al test
introducirArrayRespuestas n r = do
    if n == 0 then (sequence r)
              else introducirArrayRespuestas (n-1) (introducirRespuestas:r)

-- Devuelve la lista de opciones elegidas en el test
introduceSolucion n s = do
    if n == 0 then (sequence s)
       else introduceSolucion (n-1) (intrSol:s)

-- Introduce un nuevo entero. Se usa para introducir las soluciones en una respuesta al test
intrSol = do 
    putStr "Introduce otro: \n"
    r <- getLine
    return (read r::Int)

--Introduce los datos de un estudiante
introduceEstudiante :: IO Estudiante
introduceEstudiante = do
    putStr "Introduce el nombre: "
    n <- getLine
    putStr "Introduce el apellido: "
    a <- getLine
    putStr "Introduce el ID: "
    id_E <- getLine
    return (Estudiante n a id_E)

-- Introduce los datos de un modelo
introduceModelo :: IO Modelo
introduceModelo = do
    putStr "Introduce el numero de preguntas que tiene el test: "
    n <- getLine
    putStr "Introduce la colocacion de las preguntas (en orden): \n"
    r <- introduceSolucion (read n::Int) []
    putStr "Introduce el ID del modelo: "
    id_M <- getLine
    putStr "Introduce el ID del test asociado: "
    id_T <- getLine
    return (Modelo r id_M id_T)


-- Opcion 2: Se introducen los datos de un test, y una lista de soluciones propuestas, y se corrigen y muestran las estadisticas.
calculaEstadisticas = do
    putStr "Introduce los datos del test: \n"
    test <- introducirTest
    putStr "Introduce el numero de test resueltos: "
    n <- getLine
    putStr "Introduce los datos de las respuestas: \n"
    respuestas <- introducirArrayRespuestas (read n::Int) []
    putStr (showEstadisticas (estadisticas test respuestas))
    putStr (showMejores (mejores test respuestas))

---------------------------------------------------------------------
--DATOS--------------------------------------------------------------
--Estudiante = Estudiante Nombre Apellido Id_E
nando = (Estudiante "Nando" "Dominguez" "001")
will = (Estudiante "Will" "Smith" "002")

--RespuestaTest = RespuestaTest [Respuesta] Estudiante Modelo Id_T
--respuesta1 : Respuestas de nando para el modelo 1 del test 1
--respuesta2 : Respuestas de nando para el modelo 2 del test 1
respuesta1 = RespuestaTest [4,3,2,1,1,1,2,1,2,3] nando modelo1 "1"
respuesta2 = RespuestaTest [3,4,1,2,1,1,2,1,1,1] nando modelo2 "1"

respuesta3 = RespuestaTest [4,2,2,1,1,3,2,1,2,3] will modelo1 "1"
respuesta4 = RespuestaTest [2,4,1,2,3,1,2,1,2,3] will modelo2 "1"


arr_respuestas1 = [respuesta1, respuesta3]

--Test = Test Id_T [Pregunta]
test1 = Test "1" [pregunta1, pregunta2, pregunta3, pregunta4, pregunta5, pregunta6,pregunta7, pregunta8, pregunta9, pregunta10]

--Modelo = Modelo Orden Id_M Id_T
--modelo1 : Modelo 1 del test 1
--modelo2: Modelo 2 del test 1
modelo1 = Modelo [1,2,3,4,5,6,7,8,9,10] "M1" "1"
modelo2 = Modelo [2,1,4,3,6,5,7,8,9,10] "M2" "1"

--Pregunta = Pregunta N_resp C_resp Puntuacion Id_P 
pregunta1 = Pregunta 4 4 1 1
pregunta2 = Pregunta 4 3 1 2
pregunta3 = Pregunta 4 2 1 3
pregunta4 = Pregunta 4 1 1 4
pregunta5 = Pregunta 4 1 1 5
pregunta6 = Pregunta 4 1 1 6
pregunta7 = Pregunta 4 2 1 7
pregunta8 = Pregunta 4 1 1 8
pregunta9 = Pregunta 4 2 1 9
pregunta10 = Pregunta 4 3 1 10