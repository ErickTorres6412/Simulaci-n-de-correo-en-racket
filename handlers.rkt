#lang racket

;; Se requieren las bibliotecas necesarias para manejar el servidor web, las rutas y los templates.
(require web-server/http
         web-server/servlet
         web-server/servlet-env
         web-server/servlet/servlet-structs
         net/url
         "templates.rkt"  ;; Archivo que contiene las plantillas HTML
         "db.rkt"
        racket/date
        racket/runtime-path)     ;; Archivo que contiene funciones para interactuar con la base de datos

;; Exporta las funciones `dispatch` y `start` para que puedan ser utilizadas por otros módulos.
(provide dispatch start)

;; Estructura para almacenar los datos de sesión de un usuario (aquí solo el nombre de usuario).
(struct session-data (username) #:transparent)

;; Función que inicia el servidor y despacha las solicitudes entrantes.
(define (start request)
  (dispatch request))

;; Función para agregar ceros a los meses o días con un solo dígito
(define (agregar-ceros num)
  (if (< num 10)
      (string-append "0" (number->string num))
      (number->string num)))

;; Obtiene la fecha actual en formato YYYY-MM-DD.
(define (obtener-fecha-actual)
  (define ahora (current-date))
  (string-append 
   (number->string (date-year ahora)) "-"
   (agregar-ceros (date-month ahora)) "-"
   (agregar-ceros (date-day ahora))))

;; Definimos las rutas relativas
(define-runtime-path usuarios-file "usuarios.txt")
(define-runtime-path correos-dir "correos")

;; Función para verificar la consistencia de usuarios
(define (verificar-consistencia-usuarios)
  (define (contar-lineas archivo)
    (if (file-exists? archivo)
        (with-input-from-file archivo
          (lambda ()
            (length (port->lines (current-input-port)))))
        0))
  
  (define num-usuarios (contar-lineas usuarios-file))
  
  (define num-archivos-correo
    (if (directory-exists? correos-dir)
        (length (filter (lambda (f) (regexp-match? #rx"\\.txt$" f))
                        (directory-list correos-dir)))
        0))
  
  (unless (= num-usuarios num-archivos-correo)
    (error 'verificar-consistencia-usuarios
           "Inconsistencia detectada: ~a usuarios, ~a archivos de correo"
           num-usuarios
           num-archivos-correo)))

;; Función para manejar el error de consistencia
(define (manejar-error-consistencia exn)
  (displayln (format "Manejando error de consistencia: ~a" (exn-message exn)))
  (response/xexpr
   #:code 400
   `(html
     (head
      (title "Error 400 - Inconsistencia en el sistema")
      (style "body { font-family: Arial, sans-serif; text-align: center; padding-top: 50px; }
              h1 { color: #d9534f; }"))
     (body
      (h1 "Error 400 - Bad Request")
      (p "Se ha detectado una inconsistencia en el sistema de usuarios y correos.")
      (p ,(format "Error: ~a" (exn-message exn)))
      (p "Por favor, contacte al administrador del sistema.")))))

; Función que maneja las solicitudes entrantes y las redirige a las funciones correspondientes según la ruta.
(define (dispatch request)
  (with-handlers ([exn:fail? (lambda (exn)
                              (displayln (format "Error: ~a" (exn-message exn)))
                              (manejar-error-consistencia exn))])
  (verificar-consistencia-usuarios)
  (displayln "Entrando en dispatch")
  (let* ([uri (request-uri request)]
          [ruta (map path/param-path (url-path uri))]
          [correo-usuario (obtener-correo-usuario request)])
    (displayln (format "URI: ~a" uri))
    (displayln (format "Ruta solicitada: ~a" ruta))
    (displayln (format "Usuario (correo): ~a" correo-usuario))
    (cond
      [(or (empty? ruta) (equal? ruta '("")))
       (displayln "Manejando ruta raíz")
       (if (equal? (request-method request) #"POST")
           (manejar-login request)
           (renderizar-pagina (pagina-login)))]
      [(and (equal? (car ruta) "correo") (= (length ruta) 2))
       (let* ([id-correo (cadr ruta)]) ; Extraer ID de la ruta
         (displayln (format "Manejando ruta /correo/~a" id-correo))
         (if correo-usuario
             (let ([contenido (contenido-correo-seleccionado correo-usuario id-correo)]) ; Llamar a la función que muestra el contenido del correo
               (renderizar-pagina contenido)) ; Renderizar la página con el contenido del correo
             (redirect-to "/")))] ; Redirigir si no hay usuario autenticado
      [(equal? ruta '("crear"))
       (displayln "Manejando ruta /crear")
       (if (equal? (request-method request) #"POST")
           (manejar-crear-usuario request)
           (renderizar-pagina (pagina-crear-usuario)))]
      [(equal? ruta '("logout"))
       (displayln "Manejando ruta /logout")
       (manejar-logout request)]
      [(equal? ruta '("favicon.ico"))
       (displayln "Manejando solicitud de favicon.ico")
       (response/output
        (lambda (op) (void))
        #:headers (list (header #"Cache-Control" #"max-age=3600")))]
      ; Nuevas rutas para el contenido dinámico de la página de correo
      [(equal? ruta '("correo"))
       (displayln "Manejando ruta /correo")
       (if correo-usuario
           (renderizar-pagina (pagina-correo correo-usuario))
           (redirect-to "/"))]
      [(and (equal? (car ruta) "contenido") (= (length ruta) 2))
       (displayln (format "Manejando ruta /contenido/~a" (cadr ruta)))
       (if correo-usuario
           (response/xexpr (manejar-ruta (cadr ruta) correo-usuario))
           (redirect-to "/"))]
      [(and (equal? (car ruta) "contenido") (= (length ruta) 3))
        (let* ([seccion (cadr ruta)]
              [id-correo (caddr ruta)]) ; Extraer ID del correo de la ruta
          (displayln (format "Manejando ruta /contenido/~a/~a" seccion id-correo))
          (if correo-usuario
              (response/xexpr (manejar-ruta-con-id seccion id-correo correo-usuario))
              (redirect-to "/")))]

      [(equal? ruta '("enviar-correo"))
       (displayln "Manejando envío de correo")
       (if (and correo-usuario (equal? (request-method request) #"POST"))
           (manejar-envio-correo request)
           (redirect-to "/"))]
      [(equal? ruta '("vaciar-papelera"))
        (displayln "Manejando vaciado de papelera")
        (if (and correo-usuario (equal? (request-method request) #"POST"))
            (manejar-vaciar-papelera request)
            (redirect-to "/"))]
      [(equal? ruta '("recuperar-correo"))
      (displayln "Manejando recuperación de correo")
      (if (and correo-usuario (equal? (request-method request) #"POST"))
          (manejar-recuperar-correo request)
          (redirect-to "/"))]
      [(equal? ruta '("eliminar-correo"))
      (displayln "Manejando eliminación de correo")
      (if (and correo-usuario (equal? (request-method request) #"POST"))
          (manejar-eliminar-correo request)
          (redirect-to "/"))]
        [else
        (displayln "Ruta no encontrada")
        (renderizar-pagina (pagina-no-encontrada))]))))

;; Función para manejar las rutas y devolver el contenido adecuado
(define (manejar-ruta ruta correo-usuario)
  (cond
    [(equal? ruta "bandeja-entrada") (contenido-bandeja-entrada correo-usuario)]
    [(equal? ruta "enviados") (contenido-enviados correo-usuario)]
    [(equal? ruta "todos") (contenido-todos correo-usuario)]
    [(equal? ruta "papelera") (contenido-papelera correo-usuario)]
    [(equal? ruta "nuevo-correo") (contenido-nuevo-correo correo-usuario)]
    [else (contenido-bandeja-entrada correo-usuario)]))

(define (manejar-ruta-con-id ruta id-correo correo-usuario)
  (cond
    [(equal? ruta "ver-correo") 
     (let ([correo (obtener-correo-por-id correo-usuario id-correo)])
       (contenido-correo-seleccionado correo-usuario correo))] ; Mostrar correo específico
    [else (contenido-bandeja-entrada correo-usuario)])) ; Otros casos pueden regresar a la bandeja de entrada


;; Función para manejar la visualización de un correo
(define (manejar-ver-correo request)
  (let* ([correo-usuario (obtener-correo-usuario request)]
         [bindings (request-bindings request)]
         [id-correo (extract-binding/single 'id-correo bindings)])
    (displayln (format "Manejando ver correo: ~a para usuario: ~a" id-correo correo-usuario))
    (if (and correo-usuario id-correo)
        (let ([correo (obtener-correo-por-id correo-usuario (string->number id-correo))])
          (if correo
              ;; Si el correo existe, devolver su contenido
              (response/xexpr (contenido-correo-seleccionado correo-usuario correo))
              ;; Si no se encuentra el correo, redirigir a la página de correos
              (response/xexpr
               `(html
                 (head
                  (meta ((http-equiv "refresh") (content "0;url=/correo"))))
                 (body
                  (p "El correo no fue encontrado. Redirigiendo a la bandeja de entrada..."))))))
        ;; Si no hay usuario o id, redirigir a la página principal
        (response/xexpr
         `(html
           (head
            (meta ((http-equiv "refresh") (content "0;url=/"))))
           (body
            (p "No tienes acceso a este correo. Redirigiendo a la página principal...")))))))



;; Función para manejar la creación de un nuevo usuario.
(define (manejar-crear-usuario request)
  (displayln "Manejando solicitud de creación de usuario")
  ;; Obtiene los datos POST y los procesa.
  (let* ([datos (request-post-data/raw request)]
         [parametros (parsear-datos-post datos)])
    (displayln (format "Datos post sin procesar: ~a" datos))
    (displayln (format "Parámetros procesados: ~a" parametros))
    ;; Extrae los valores de los parámetros necesarios.
    (let ([nombre (assoc-ref parametros "nombre")]
          [correo (assoc-ref parametros "correo")]
          [contrasena (assoc-ref parametros "contrasena")])
      (displayln (format "nombre: ~a, correo: ~a, contraseña: ~a" nombre correo contrasena))
      ;; Si todos los parámetros son válidos, crea el usuario.
      (if (and nombre correo contrasena)
          (with-handlers ([exn:fail? (lambda (exn)
                                       (displayln (format "Error al crear usuario: ~a" (exn-message exn)))
                                       (renderizar-pagina (pagina-error (format "Error al crear usuario: ~a" (exn-message exn)))))])
            (let ([resultado (crear-usuario nombre correo contrasena)])
              (displayln (format "Resultado de crear usuario: ~a" resultado))
              (renderizar-pagina (pagina-resultado resultado))))
          ;; Si falta algún parámetro, muestra un error.
          (renderizar-pagina (pagina-error "Todos los campos son obligatorios."))))))

;; Función para manejar el login del usuario.
(define (manejar-login request)
  (displayln "Entrando en manejar-login")
  (let* ([datos (request-post-data/raw request)]
         [parametros (parsear-datos-post datos)]
         [correo (assoc-ref parametros "correo")]
         [contrasena (assoc-ref parametros "contrasena")])
    (displayln (format "Correo: ~a, Contraseña: ~a" correo contrasena))
    (displayln "Llamando a verificar-credenciales")
    (if (verificar-credenciales correo contrasena)
        (begin
          (displayln "Credenciales verificadas correctamente")
          (let ([session (session-data correo)])
            (response/xexpr
             `(html
               (head
                (meta ((http-equiv "refresh") (content "0;url=/correo"))))
               (body
                (p "Redirigiendo...")))
             #:headers (list (header #"Set-Cookie"
                                     (string->bytes/utf-8 
                                      (format "session=~a; Path=/" 
                                              (session-data-username session))))))))
        (begin
          (displayln "Credenciales inválidas")
          (renderizar-pagina (pagina-login "Credenciales inválidas"))))))

;; Función para manejar el logout del usuario.
(define (manejar-logout request)
  (displayln "Manejando logout")
  ;; Elimina la cookie de sesión y redirige a la página principal.
  (send/suspend
   (lambda (url)
     (redirect-to request "/" 
                  #:headers (list (make-header #"Set-Cookie"  ;; Elimina la cookie de sesión
                                               #"session=; Path=/; Expires=Thu, 01 Jan 1970 00:00:00 GMT"))))))

;; funcion para obtener el nombre del usuario
(define (obtener-nombre-usuario request)
  (displayln "Entrando en la función obtener-nombre-usuario")
  ; Obtiene los datos de sesión
  (let ([session (get-session request)])
    (displayln (format "Sesión: ~a" session))
    ; Si la sesión es válida, devuelve el nombre de usuario
    (if session
        (begin
          (displayln "Sesión encontrada, obteniendo nombre de usuario")
          (session-data-username session))
        (begin
          (displayln "No se encontró sesión, devolviendo nombre por defecto")
          "Invitado")))) 

; Función para obtener el correo del usuario
(define (obtener-correo-usuario request)
  (displayln "Entrando en la función obtener-correo-usuario")
  (let ([session (get-session request)])
    (displayln (format "Sesión: ~a" session))
    (if session
        (begin
          (displayln "Sesión encontrada, obteniendo correo de usuario")
          (session-data-username session))
        (begin
          (displayln "No se encontró sesión, devolviendo correo por defecto")
          "invitado@example.com"))))

;; Función para obtener los datos de la sesión desde la cookie.
(define (get-session request)
  (displayln "Entering get-session function")
  (displayln (format "Request headers: ~a" (request-headers request)))
  ;; Extrae las cookies del encabezado de la solicitud.
  (let ([cookies (request-headers request)])
    (displayln (format "Cookies: ~a" cookies))
    ;; Busca la cookie de sesión.
    (if (list? cookies)
        (let ([session-cookie (findf (lambda (header)
                                       (and (pair? header)
                                            (equal? (car header) 'cookie)))
                                     cookies)])
          (displayln (format "Session cookie: ~a" session-cookie))
          ;; Si la cookie de sesión está presente, extrae el valor de la sesión.
          (if session-cookie
              (let* ([cookie-value (cdr session-cookie)]
                     [session-match (regexp-match #rx"session=([^;]+)" cookie-value)])
                (displayln (format "Cookie value: ~a" cookie-value))
                (displayln (format "Session match: ~a" session-match))
                ;; Si se encuentra la sesión, devuelve los datos de la sesión.
                (if session-match
                    (session-data (cadr session-match))
                    (begin
                      (displayln "No session match found")
                      #f)))
              (begin
                (displayln "No session cookie found")
                #f)))
        (begin
          (displayln "Cookies is not a list")
          #f))))

;; Función para redirigir a otra página.
(define (redirect-to request path #:headers [extra-headers '()])
  (response/xexpr
   `(html
     (head
      (meta ((http-equiv "Refresh") (content ,(format "0;url=~a" path)))))
     (body
      (p "Redirecting...")))
   #:code 302
   #:headers (cons (header #"Location" (string->bytes/utf-8 path))
                   extra-headers)))

;; Función para procesar los datos POST.
(define (parsear-datos-post datos)
  (if (bytes? datos)
      (let ([parametros (string-split (bytes->string/utf-8 datos) "&")])
        (for/list ([param parametros])
          (let* ([kv (string-split param "=")]
                 [clave (and (>= (length kv) 1) (string->symbol (decodificar-url (first kv))))] 
                 [valor (and (>= (length kv) 2) (decodificar-url (second kv)))] )
            ;; Devuelve los pares clave-valor extraídos de los datos POST.
            (if (and clave valor)
                (cons (symbol->string clave) valor)
                (begin
                  (displayln (format "Parámetro inválido: ~a" param))
                  '("" . ""))))))
      (begin
        (displayln "Datos no son bytes")
        '())))

;; Función para decodificar URLs.
(define (decodificar-url str)
  (let ([hex-a-numero (lambda (hex) (string->number (string-append "#x" hex)))] )
    ;; Reemplaza los valores codificados en URL por sus caracteres originales.
    (regexp-replace* #px"%([0-9a-fA-F]{2})"
                     str
                     (lambda (_ hex) 
                       (string (integer->char (hex-a-numero hex)))))))

;; Función auxiliar para buscar en una lista asociativa.
(define (assoc-ref alist clave)
  (let ([par (assoc clave alist)])
    (and par (cdr par))))


(define (validar-contenido-correo destino asunto cuerpo)
  (and 
   ;; Verificar que ningún campo esté vacío
   (not (string=? destino ""))
   (not (string=? asunto ""))
   (not (string=? cuerpo ""))
   ;; Verificar límites de caracteres
   (<= (string-length asunto) 100)
   (<= (string-length cuerpo) 1000)
   ;; Verificar que no contengan el carácter |
   (not (string-contains? destino "|"))
   (not (string-contains? asunto "|"))
   (not (string-contains? cuerpo "|"))
   ;; Validar formato de correo electrónico (básico)
   (regexp-match? #rx"^[^@ ]+@[^@ ]+\\.[^@ ]+$" destino)))


;; Función para manejar el envío de un nuevo correo con validaciones
(define (manejar-envio-correo request)
  (let* ([bindings (request-bindings request)]
         [destino (extract-binding/single 'destino bindings)]
         [asunto (extract-binding/single 'asunto bindings)]
         [cuerpo (extract-binding/single 'cuerpo bindings)]
         [remitente (obtener-correo-usuario request)])
    
    ;; Validar el contenido antes de procesar
    (if (validar-contenido-correo destino asunto cuerpo)
        (let* ([fecha (obtener-fecha-actual)]
               [nuevo-correo (correo #f remitente destino asunto cuerpo fecha #f #f)]
               [ids (guardar-correo remitente nuevo-correo)]
               [nuevo-id-remitente (car ids)]
               [nuevo-id-destinatario (cdr ids)])
          ;; Redirigir después de guardar el correo
          (response/xexpr
           `(html
             (head
              (meta ((http-equiv "refresh") (content "0;url=/correo"))))
             (body
              (p "Redirigiendo...")))))
        ;; Si la validación falla, mostrar error
        (renderizar-pagina 
         (pagina-correo remitente 
                       #:error "Error: El correo no cumple con los requisitos de validación.")))))

;; Función para manejar el vaciado de la papelera
(define (manejar-vaciar-papelera request)
  (let ([correo-usuario (obtener-correo-usuario request)])
    (if correo-usuario
        (begin
          (vaciar-papelera correo-usuario)
          (response/xexpr
           `(html
             (head
              (meta ((http-equiv "refresh") (content "0;url=/correo"))))
             (body
              (p "Vaciando papelera y redirigiendo...")))))
        (redirect-to "/"))))

;; Función para manejar la recuperación de un correo de la papelera
(define (manejar-recuperar-correo request)
  (let* ([correo-usuario (obtener-correo-usuario request)]
         [bindings (request-bindings request)]
         [id-correo (string->number (extract-binding/single 'id-correo bindings))])
    (if (and correo-usuario id-correo)
        (begin
          (recuperar-correo-de-papelera correo-usuario id-correo)
          (response/xexpr
           `(html
             (head
              (meta ((http-equiv "refresh") (content "0;url=/correo"))))
             (body
              (p "Recuperando correo y redirigiendo...")))))
        (redirect-to "/"))))

;; Función para manejar la eliminación de un correo (moverlo a la papelera)
(define (manejar-eliminar-correo request)
  (let* ([correo-usuario (obtener-correo-usuario request)]
         [bindings (request-bindings request)]
         [id-correo (string->number (extract-binding/single 'id-correo bindings))])
    (if (and correo-usuario id-correo)
        (begin
          (mover-correo-a-papelera correo-usuario id-correo)
          (response/xexpr
           `(html
             (head
              (meta ((http-equiv "refresh") (content "0;url=/correo"))))
             (body
              (p "Moviendo correo a la papelera y redirigiendo...")))))
        (redirect-to "/"))))
