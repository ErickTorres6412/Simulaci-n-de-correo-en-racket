#lang racket

(require web-server/http)
(require "db.rkt")
(require racket/date)

(provide renderizar-pagina pagina-crear-usuario pagina-resultado pagina-error pagina-no-encontrada
         pagina-login pagina-correo contenido-bandeja-entrada contenido-enviados contenido-papelera
         contenido-nuevo-correo contenido-todos contenido-correo-seleccionado)

;; Función para renderizar una página
(define (renderizar-pagina xexpr)
  (response/xexpr xexpr))

;; Estilos CSS comunes para todas las páginas
(define estilos-comunes
  "
  body { font-family: Arial, sans-serif; line-height: 1.6; padding: 20px; background-color: #f0f0f0; }
  h1 { color: #333; }
  .container { background: #fff; padding: 20px; border-radius: 5px; box-shadow: 0 0 10px rgba(0, 0, 0, 0.1); }
  .button { display: inline-block; margin-top: 20px; padding: 10px 15px; background: #333; color: #fff; text-decoration: none; border-radius: 5px; }
  label { display: block; margin-top: 10px; }
  input { margin-top: 5px; padding: 8px; width: 100%; max-width: 300px; }
  form div { margin-bottom: 15px; }
  ")

; Función auxiliar para calcular la diferencia en días entre dos fechas
(define (dias-entre-fechas fecha1 fecha2)
  (let* ([time1 (date->seconds fecha1)]
         [time2 (date->seconds fecha2)]
         [diff-seconds (- time2 time1)])
    (inexact->exact (floor (/ diff-seconds 86400)))))

; Función para formatear la fecha
(define (formatear-fecha fecha-input)
  (define fecha-correo 
    (cond
      [(string? fecha-input)
       (let ([partes (map string->number (string-split fecha-input "-"))])
         (seconds->date (find-seconds 0 0 0 (list-ref partes 2) (list-ref partes 1) (list-ref partes 0))))]
      [(number? fecha-input)
       (seconds->date fecha-input)]
      [else (current-date)]))  ; Si no es ni string ni número, usamos la fecha actual
  
  (define fecha-actual (current-date))
  (define diferencia-dias (dias-entre-fechas fecha-correo fecha-actual))
  
  (cond
    [(= diferencia-dias 0) "hoy"]
    [(= diferencia-dias 1) "ayer"]
    [(<= diferencia-dias 7) (format "hace ~a días" diferencia-dias)]
    [(<= diferencia-dias 30) (format "hace ~a semanas" (quotient diferencia-dias 7))]
    [(<= diferencia-dias 365) (format "hace ~a meses" (quotient diferencia-dias 30))]
    [else (format "hace ~a años" (quotient diferencia-dias 365))]))

;; Página de inicio de sesión con validación y Bootstrap
(define (pagina-login [error #f])
  `(html
    (head
     (title "Iniciar sesión")
     (meta ([charset "utf-8"]))
     (meta ([name "viewport"] [content "width=device-width, initial-scale=1"]))
     (link ([rel "stylesheet"] [href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css"] [integrity "sha384-QWTKZyjpPEjISv5WaRU9OFeRpok6YctnYmDr5pNlyT2bRjXh0JMhjY6hW+ALEwIH"] [crossorigin "anonymous"]))
     (link ([rel "stylesheet"] [href "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css"]))
     (script ([src "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js"] [integrity "sha384-YvpcrYf0tY3lHB60NNkmXc5s9fDVZLESaAA55NDzOxhy9GkcIdslK1eN7N6jIeHz"] [crossorigin "anonymous"]))
     (style "
       body { background-color: #f8f9fa; }
       .card { box-shadow: 0 4px 8px rgba(0,0,0,0.1); }
       .card-header { background-color: #A31E32; color: white; }
       .btn-primary { background-color: #28a745; border-color: #007bff; }
       .btn-primary:hover { background-color: #218838; border-color: #1e7e34; }
     ")
     (script "
       function validarFormulario() {
        var correo = document.getElementById('correo').value;
        var contrasena = document.getElementById('contrasena').value;
        if (correo == '' || contrasena == '') {
          alert('Todos los campos son obligatorios.');
          return false;
        }
        if (correo.includes('|') || contrasena.includes('|')) {
          alert('El símbolo | no está permitido.');
          return false;
        }
         return true;
       }
     "))
    (body
     (div ([class "container"])
          (div ([class "row justify-content-center align-items-center min-vh-100"])
               (div ([class "col-md-6 col-lg-4"])
                    (div ([class "card border-0 rounded-3"])
                         (div ([class "card-header text-center py-3"])
                              (h2 ([class "mb-0"]) "Iniciar sesión"))
                         (div ([class "card-body p-4"])
                              (form ([action "/"] [method "POST"] [onsubmit "return validarFormulario();"])
                                    (div ([class "mb-3"])
                                         (label ([for "correo"] [class "form-label"]) "Correo:")
                                         (div ([class "input-group"])
                                              (span ([class "input-group-text"]) (i ([class "bi bi-person"])))
                                              (input ([type "text"] [id "correo"] [name "correo"] [class "form-control"] [placeholder "Ingrese su correo"]))))
                                    (div ([class "mb-3"])
                                         (label ([for "contrasena"] [class "form-label"]) "Contraseña:")
                                         (div ([class "input-group"])
                                              (span ([class "input-group-text"]) (i ([class "bi bi-lock"])))
                                              (input ([type "password"] [id "contrasena"] [name "contrasena"] [class "form-control"] [placeholder "Ingrese su contraseña"]))))
                                    (div ([class "d-grid gap-2"])
                                         (button ([type "submit"] [class "btn btn-primary btn-lg"]) "Iniciar sesión")))
                              (div ,(if error `(div ([class "alert alert-danger mt-3"]) ,error) '()))
                              (div ([class "text-center mt-3"])
                                   (a ([href "crear"]) "¿No tienes una cuenta? Registrarme"))))))))))

;; Pagina de principal de correo
(define (pagina-correo correo-usuario)
  `(html
    (head
     (title "Correo UNA")
     (meta ([charset "utf-8"]))
     (meta ([name "viewport"] [content "width=device-width, initial-scale=1"]))
     (link ([rel "stylesheet"] [href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css"] [integrity "sha384-QWTKZyjpPEjISv5WaRU9OFeRpok6YctnYmDr5pNlyT2bRjXh0JMhjY6hW+ALEwIH"] [crossorigin "anonymous"]))
     (link ([rel "stylesheet"] [href "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css"]))
     (script ([src "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js"] [integrity "sha384-YvpcrYf0tY3lHB60NNkmXc5s9fDVZLESaAA55NDzOxhy9GkcIdslK1eN7N6jIeHz"] [crossorigin "anonymous"]))
     (style "
       body { 
          background-color: #f8f9fa;
          min-height: 100vh;
          display: flex;
          flex-direction: column;
       }
       .sidebar { 
          background-color: #e9ecef; 
          min-height: 100%;
          position: sticky;
          top: 0;
          padding-top: 1rem;
       }
       .navbar { 
          background-color: #A31E32 !important;
          position: sticky;
          top: 0;
          z-index: 1000;
       }
       .navbar-brand, .navbar-nav .nav-link { 
          color: white !important; 
       }
       .search-bar { 
         margin-top: 20px;
         max-width: 990px;
         width: 100%;
       }
       .search-bar .form-control {
         height: 40px;
         font-size: 16px;
       }
       .search-bar .btn {
         height: 40px;
       }
       .sidebar .nav-link { color: #495057; transition: all 0.3s; }
       .sidebar .nav-link:hover, .sidebar .nav-link.active { 
         background-color: #A31E32; 
         color: white; 
         border-radius: 5px; 
       }
       .sidebar .nav-link i { margin-right: 10px; }
       .user-name {
         font-size: 1.1rem;
         font-weight: bold;
         cursor: pointer;
         margin-right: 10px;
       }
       .dropdown-menu {
         min-width: 100px;
         top: 100% !important;
         margin-top: 10px !important;
       }
       .dropdown-item {
         font-size: 0.9rem;
         padding: 0.25rem 1rem;
       }
       .container-fluid {
         flex: 1;
       }
       .email-layout {
         min-height: calc(100vh - 76px);
       }
       main {
         padding: 1rem;
         min-height: 100%;
       }
     ")
     (script "
      
      function validarFormularioCorreo() {
          var destino = document.getElementById('destino').value;
          var asunto = document.getElementById('asunto').value;
          var cuerpo = document.getElementById('cuerpo').value;
          
          // Validar campos vacíos
          if (destino == '' || asunto == '' || cuerpo == '') {
            alert('Todos los campos son obligatorios.');
            return false;
          }
          
          // Validar el símbolo '|'
          if (destino.includes('|') || asunto.includes('|') || cuerpo.includes('|')) {
            alert('El símbolo | no está permitido en ningún campo.');
            return false;
          }

          // Validar si el asunto excede los 50 caracteres
          if (asunto.split('').join('') !== asunto.substring(0, 50)) {
            alert('El asunto no puede exceder los 50 caracteres.');
            return false;
          }

          // Validar que el cuerpo del mensaje no exceda los 500 caracteres
          if (cuerpo.split('').join('') !== cuerpo.substring(0, 500)) {
            alert('El cuerpo del mensaje no puede exceder los 500 caracteres.');
            return false;
          }
          
          return true;
        }

        function validarFormularioRespuesta() {
          var cuerpo = document.getElementById('cuerpo-respuesta').value;
                 
          // Validar campo vacío
          if (cuerpo.trim() === '') {
            alert('El mensaje no puede estar vacío.');
            return false;
          }
                 
          // Validar el símbolo '|'
          if (cuerpo.includes('|')) {
            alert('El símbolo | no está permitido en el mensaje.');
            return false;
          }
                 
            return true;
        }

        function actualizarContador(elemento, idContador, maxLength) {
            var contador = document.getElementById(idContador);
            var actual = elemento.value.length;
            contador.innerHTML = actual.toString().concat(' de ' , maxLength.toString());
        }

       function cargarContenido(seccion) {
         fetch('/contenido/' + seccion)
           .then(function(response) {
             return response.text();
           })
           .then(function(html) {
             document.querySelector('main').innerHTML = html;
           })
           .catch(function(error) {
             console.error('Error:', error);
           });
       }
       function cargarContenidoConId(seccion, idCorreo) {
        fetch('/contenido/' + seccion + '/' + idCorreo)
          .then(function(response) {
            return response.text();
          })
          .then(function(html) {
            document.querySelector('main').innerHTML = html;
          })
          .catch(function(error) {
            console.error('Error:', error);
          });
      }

      document.addEventListener('DOMContentLoaded', function() {
        document.querySelectorAll('.nav-link').forEach(function(link) {
          link.addEventListener('click', function(e) {
            e.preventDefault();

            // Obtener los valores de los atributos data-seccion y data-id-correo
            const seccion = this.getAttribute('data-seccion');
            const idCorreo = this.getAttribute('data-id-correo');

            // Verificar si existe idCorreo
            if (idCorreo) {
              console.log('Cargando contenido de sección', seccion, 'con id', idCorreo);
              cargarContenidoConId(seccion, idCorreo);
            } else {
              cargarContenido(seccion);
            }

            // Manejo de la clase active en la navegación
            document.querySelectorAll('.nav-link').forEach(function(l) {
              l.classList.remove('active');
            });
            this.classList.add('active');
          });
        });

        // Cargar la bandeja de entrada por defecto al cargar la página (esta es la linea 214)
        cargarContenido('bandeja-entrada');

         function toggleDropdown() {
        document.getElementById('userDropdown').classList.toggle('show')
      }

        function cerrarSesion() {
            window.location.href = '/logout';
        }

        document.querySelector('.user-name').addEventListener('click', toggleDropdown)

        document.querySelector('.dropdown-item').addEventListener('click', cerrarSesion)

        window.addEventListener('click', function (event) {
            if (!event.target.matches('.user-name')) {
                const dropdown = document.getElementById('userDropdown');
                if (dropdown.classList.contains('show')) {
                    dropdown.classList.remove('show');
                }
            }
        });
       });
       
     "))
    (body
     (header ([class "navbar navbar-expand-lg navbar-dark"])
             (div ([class "container-fluid"])
                  (a ([class "navbar-brand"] [href "#"])
                     (i ([class "bi bi-envelope me-2"]))
                     "Correos UNA")
                  (div ([class "search-bar"])
                       (form ([class "d-flex"])
                             (input ([class "form-control me-2"] [type "search"] [placeholder "Buscar correo"] [aria-label "Buscar"]))
                             (button ([class "btn btn-outline-light"] [type "submit"]) "Buscar")))
                  (div ([class "d-flex align-items-center position-relative"])
                       (span ([class "text-white user-name"])
                             ,(format "~a" correo-usuario)
                             (i ([class "bi bi-caret-down-fill ms-2"])))
                       (div ([id "userDropdown"] [class "dropdown-menu dropdown-menu-end"])
                            (a ([class "dropdown-item"] [href "#"])
                               "Cerrar sesión")))))
     (div ([class "container-fluid"])
          (div ([class "row email-layout"])
               (nav ([class "col-md-3 col-lg-2 d-md-block sidebar collapse"])
                    (div ([class "position-sticky pt-3"])
                         (ul ([class "nav flex-column"])
                             (li ([class "nav-item"])
                                 (a ([class "nav-link active"] [href "#"] [data-seccion "bandeja-entrada"])
                                    (i ([class "bi bi-inbox"]))
                                    "Bandeja de entrada"))
                             (li ([class "nav-item"])
                                 (a ([class "nav-link"] [href "#"] [data-seccion "enviados"])
                                    (i ([class "bi bi-send"]))
                                    "Enviados"))
                             (li ([class "nav-item"])
                                 (a ([class "nav-link"] [href "#"] [data-seccion "todos"])
                                    (i ([class "bi bi-envelope-open"]))
                                    "Todos"))
                             (li ([class "nav-item"])
                                 (a ([class "nav-link"] [href "#"] [data-seccion "papelera"])
                                    (i ([class "bi bi-trash"]))
                                    "Papelera")))))
               (main ([class "col-md-9 ms-sm-auto col-lg-10 px-md-4"])
                     ,(contenido-bandeja-entrada correo-usuario)))))))

;; Contenido de la bandeja de entrada
(define (contenido-bandeja-entrada correo-usuario)
  (if correo-usuario
      (let ([correos (leer-correos-recibidos correo-usuario)])
        `(div
           (div ([class "d-flex justify-content-between flex-wrap flex-md-nowrap align-items-center pt-3 pb-2 mb-3 border-bottom"])
                (h1 ([class "h2"]) "Bandeja de entrada")
                (div ([class "btn-toolbar mb-2 mb-md-0"])
                     (button ([onclick "cargarContenido('nuevo-correo')"] [class "btn btn-primary"])
                             (i ([class "bi bi-envelope me-2"]) "")
                             "Nuevo Correo")))
           (div ([class "email-list"])
                ,@(map (lambda (correo)
                        `(div ([class "email-item p-2 border-bottom w-100 text-start"])
                              ; Botón para ver correo usando cargarContenido
                              (button ([class "w-100 text-start btn btn-light"]
                                  [onclick ,(string-append "cargarContenidoConId('ver-correo', '" (list-ref correo 0) "')")])
                                      (div ([class "d-flex w-100 justify-content-between"])
                                           (h5 ([class "mb-1"]) ,(list-ref correo 3))  ; Asunto
                                           (small ,(formatear-fecha (list-ref correo 5))))  ; Fecha
                                      (p ([class "mb-1"]) ,(string-append "Remitente: " (list-ref correo 1)))  ; Remitente
                                      (small ,(list-ref correo 4)))  ; Contenido breve
                              ; Formulario para eliminar correo
                              (form ([action "/eliminar-correo"] [method "post"])
                                    (input ([type "hidden"] [name "id-correo"] [value ,(list-ref correo 0)]))
                                    (button ([type "submit"] [class "btn btn-danger btn-sm"])
                                            (i ([class "bi bi-trash me-1"]) "")
                                            "Mover a Papelera"))))
                      correos))))
      `(div (h2 "Error: No se pudo obtener el correo del usuario"))))

(define (contenido-correo-seleccionado correo-usuario correo)
  (if (not correo) ; Verificar si correo es #f
      `(div (h2 "Error: El correo no fue encontrado"))
      (let* ((remitente (list-ref correo 1))
             (destinatario (list-ref correo 2))
             (asunto (list-ref correo 3))
             (cuerpo (list-ref correo 4))
             (fecha (list-ref correo 5))
             (en-papelera (let ((valor (list-ref correo 7)))
                            (cond
                              ((equal? valor "true") #t)
                              ((equal? valor "false") #f)
                              ((eq? valor #t) #t)
                              ((eq? valor #f) #f)
                              (else #f)))) ; Por defecto, asumimos que no está en papelera
             (es-remitente (equal? remitente correo-usuario))
             (etiqueta-remitente (if es-remitente "Remitido a:" "Remitente:"))
             (valor-remitente (if es-remitente destinatario remitente))
             (mostrar-boton-responder (and (not es-remitente) (not en-papelera))))
        `(div
           (div ([class "d-flex justify-content-between flex-wrap flex-md-nowrap align-items-center pt-3 pb-2 mb-3 border-bottom"])
                (h1 ([class "h2"]) "Ver Correo")
                (div ([class "btn-toolbar mb-2 mb-md-0"])
                     (button ([onclick "cargarContenido('bandeja-entrada')"] [class "btn btn-secondary"])
                             (i ([class "bi bi-arrow-left me-2"]) "")
                             "Volver")))
           (div ([class "email-content"])
                (div ([class "row mb-3"])
                     (div ([class "col"])
                          (label ([for "destino"] [class "form-label"]) ,etiqueta-remitente)
                          (input ([type "email"] [class "form-control"] [id "destino"] [name "destino"] [value ,valor-remitente] [readonly ""])))
                     (div ([class "col"])
                          (label ([for "asunto"] [class "form-label"]) "Asunto:")
                          (input ([type "text"] [class "form-control"] [id "asunto"] [name "asunto"] [value ,asunto] [readonly ""]))))
                (hr)
                (div ([class "mb-3"])
                     (label ([for "fecha"] [class "form-label"]) "Fecha:")
                     (input ([type "text"] [class "form-control"] [id "fecha"] [name "fecha"] [value ,fecha] [readonly ""])))
                (div ([class "email-body mb-3"])
                     (label ([for "cuerpo"] [class "form-label"]) "Mensaje:")
                     (textarea ([class "form-control"] [id "cuerpo"] [name "cuerpo"] [rows "5"] [readonly ""])
                               ,cuerpo))
                ,@(if mostrar-boton-responder
                      `((div ([class "mt-3"])
                             (button ([id "btn-responder"] [class "btn btn-primary"] 
                                      [onclick "console.log('Botón clickeado'); 
                                                var form = document.getElementById('formulario-respuesta'); 
                                                form.style.display = form.style.display === 'none' ? 'block' : 'none';"])
                                     (i ([class "bi bi-reply me-2"]) "")
                                     "Responder"))
                        (div ([id "formulario-respuesta"] [style "display: none;"])
                             (form ([id "form-respuesta"] [action "/enviar-correo"] [method "post"] [onsubmit "return validarFormularioRespuesta();"])
                                   (hr)
                                   (div ([class "row mb-3"])
                                        (div ([class "col"])
                                             (label ([for "destino-respuesta"] [class "form-label"]) "Destinatario:")
                                             (input ([type "email"] [class "form-control"] [id "destino-respuesta"] [name "destino"] [value ,remitente] [readonly ""])))
                                        (div ([class "col"])
                                             (label ([for "asunto-respuesta"] [class "form-label"]) "Asunto:")
                                             (input ([type "text"] [class "form-control"] [id "asunto-respuesta"] [name "asunto"] [value ,(string-append "Re: " asunto)] [readonly ""]))))
                                   (div ([class "mb-3"])
                                        (label ([for "cuerpo-respuesta"] [class "form-label"]) "Mensaje:")
                                        (textarea ([class "form-control"] [id "cuerpo-respuesta"] [name "cuerpo"] [rows "5"] [required ""])))
                                   (button ([type "submit"] [class "btn btn-primary"]) "Enviar"))))
                      '())
                (script "
                  console.log('Script cargado');
                "))))))

;; Contenido de los correos enviados
(define (contenido-enviados correo-usuario)
  (if correo-usuario
      (let ([correos-enviados (leer-correos-enviados correo-usuario)])
        `(div
          (div ([class "d-flex justify-content-between flex-wrap flex-md-nowrap align-items-center pt-3 pb-2 mb-3 border-bottom"])
            (h1 ([class "h2"]) "Correos Enviados")
              (div ([class "btn-toolbar mb-2 mb-md-0"])
                (button ([onclick "cargarContenido('nuevo-correo')"] [class "btn btn-primary"])
                  (i ([class "bi bi-envelope me-2"]) "")"Nuevo Correo")))
          (div ([class "email-list"])
               ,@(map (lambda (correo)
                        `(div ([class "email-item p-2 border-bottom w-100 text-start"])
                              (button ([class "w-100 text-start btn btn-light"]
                                       [onclick ,(string-append "cargarContenidoConId('ver-correo', '" (list-ref correo 0) "')")])
                                      (div ([class "d-flex w-100 justify-content-between"])
                                           (h5 ([class "mb-1"]) ,(list-ref correo 3))  ; Asunto
                                           (small ,(formatear-fecha (list-ref correo 5))))  ; Fecha
                                      (p ([class "mb-1"]) ,(string-append "Destinatario: " (list-ref correo 2)))  ; Destinatario
                                      (small ,(if (> (string-length (list-ref correo 4)) 50)
                                                  (string-append (substring (list-ref correo 4) 0 50) "...")
                                                  (list-ref correo 4))))  ; Contenido breve
                              (div ([class "mt-2"])
                                   (form ([action "/eliminar-correo"] [method "post"])
                                         (input ([type "hidden"] [name "id-correo"] [value ,(list-ref correo 0)]))
                                         (button ([type "submit"] [class "btn btn-danger btn-sm"])
                                                 (i ([class "bi bi-trash me-1"]) "")
                                                 "Mover a Papelera")))))
                      correos-enviados))))
      `(div (h2 "Error: No se pudo obtener el correo del usuario"))))

;; Contenido de todos los correos
(define (contenido-todos correo-usuario)
  (if correo-usuario
      (let ([todos-los-correos (leer-todos-los-correos correo-usuario)])
        `(div
          (div ([class "d-flex justify-content-between flex-wrap flex-md-nowrap align-items-center pt-3 pb-2 mb-3 border-bottom"])
               (h1 ([class "h2"]) "Todos los Correos")
               (div ([class "btn-toolbar mb-2 mb-md-0"])
                    (button ([onclick "cargarContenido('nuevo-correo')"] [class "btn btn-primary"])
                            (i ([class "bi bi-envelope me-2"]) "")"Nuevo Correo")))
          (div ([class "email-list"])
               ,@(map (lambda (correo)
                        `(div ([class "email-item p-2 border-bottom w-100 text-start"])
                              (button ([class "w-100 text-start btn btn-light"]
                                       [onclick ,(string-append "cargarContenidoConId('ver-correo', '" (list-ref correo 0) "')")])
                                      (div ([class "d-flex w-100 justify-content-between"])
                                           (h5 ([class "mb-1"]) ,(list-ref correo 3))  ; Asunto
                                           (small ,(formatear-fecha (list-ref correo 5))))  ; Fecha
                                      (p ([class "mb-1"])
                                         ,(string-append 
                                           (if (equal? (list-ref correo 6) "enviado") 
                                               "Para: " 
                                               "De: ")  ; Si es enviado, muestra "Para:", si es recibido, muestra "De:"
                                           (if (equal? (list-ref correo 6) "enviado") 
                                               (list-ref correo 2)  ; Destinatario si es enviado
                                               (list-ref correo 1))))  ; Remitente si es recibido
                                      (small ,(if (> (string-length (list-ref correo 4)) 50)
                                                  (string-append (substring (list-ref correo 4) 0 50) "...")  ; Contenido breve
                                                  (list-ref correo 4))))
                              (div ([class "mt-2"])
                                   (form ([action "/eliminar-correo"] [method "post"])
                                         (input ([type "hidden"] [name "id-correo"] [value ,(list-ref correo 0)]))
                                         (button ([type "submit"] [class "btn btn-danger btn-sm"])
                                                 (i ([class "bi bi-trash me-1"]) "")
                                                 "Mover a Papelera")))))
                      todos-los-correos))))
      `(div (h2 "Error: No se pudo obtener el correo del usuario"))))

;; Contenido de la papelera
(define (contenido-papelera correo-usuario)
  (let ([correos-papelera (obtener-correos-papelera correo-usuario)])
    `(div
      (div ([class "d-flex justify-content-between flex-wrap flex-md-nowrap align-items-center pt-3 pb-2 mb-3 border-bottom"])
           (h1 ([class "h2"]) "Papelera")
           (form ([action "/vaciar-papelera"] [method "post"] [class "btn-toolbar mb-2 mb-md-0"])
                 (button ([type "submit"] [class "btn btn-danger"]) "Vaciar Papelera")))
      (div ([class "email-list"])
           ,@(if (null? correos-papelera)
                 `((p "No hay correos en la papelera."))
                 (map (lambda (correo)
                        (let ([c (string-split correo "|")])
                          `(div ([class "email-item p-2 border-bottom w-100 text-start"])
                                (button ([class "w-100 text-start btn btn-light"]
                                         [onclick ,(string-append "cargarContenidoConId('ver-correo', '" (list-ref c 0) "')")])
                                        (div ([class "d-flex w-100 justify-content-between"])
                                             (h5 ([class "mb-1"]) ,(list-ref c 3))  ; asunto
                                             (small ,(list-ref c 5)))  ; fecha
                                        (p ([class "mb-1"]) ,(format "Remitente: ~a" (list-ref c 1)))  ; remitente
                                        (small "Contenido del correo eliminado..."))
                                (div ([class "mt-2"])
                                     (form ([action "/recuperar-correo"] [method "post"])
                                           (input ([type "hidden"] [name "id-correo"] [value ,(list-ref c 0)]))
                                           (button ([type "submit"] [class "btn btn-success btn-sm"]) "Recuperar"))))))
                      correos-papelera))))))

;; Página de creación de usuario con validación de formulario
(define (pagina-crear-usuario)
  `(html
    (head
     (title "Crear Usuario")
     (meta ([charset "utf-8"]))
     (meta ([name "viewport"] [content "width=device-width, initial-scale=1"]))
     (link ([rel "stylesheet"] [href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css"] [integrity "sha384-QWTKZyjpPEjISv5WaRU9OFeRpok6YctnYmDr5pNlyT2bRjXh0JMhjY6hW+ALEwIH"] [crossorigin "anonymous"]))
     (link ([rel "stylesheet"] [href "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css"]))
     (script ([src "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js"] [integrity "sha384-YvpcrYf0tY3lHB60NNkmXc5s9fDVZLESaAA55NDzOxhy9GkcIdslK1eN7N6jIeHz"] [crossorigin "anonymous"]))
     (style "
       body { background-color: #f8f9fa; }
       .card { box-shadow: 0 4px 8px rgba(0,0,0,0.1); }
       .card-header { background-color: #A31E32; color: white; }
       .btn-success { background-color: #28a745; border-color: #28a745; }
       .btn-success:hover { background-color: #218838; border-color: #1e7e34; }
       .email-input-group { display: flex; align-items: stretch; }
       .email-input-group .input-group { flex-grow: 1; }
       .email-domain { 
         display: flex; 
         align-items: center; 
         padding: 0 10px; 
         background-color: #e9ecef; 
         border: 1px solid #ced4da;
         border-left: none;
         border-top-right-radius: 0.25rem;
         border-bottom-right-radius: 0.25rem;
       }
       .email-input-group .form-control {
         border-top-right-radius: 0;
         border-bottom-right-radius: 0;
       }
     ")
     (script "
       function validarFormulario() {
         var nombre = document.getElementById('nombre').value;
         var correo = document.getElementById('correo').value;
         var contrasena = document.getElementById('contrasena').value;
         if (nombre == '' || correo == '' || contrasena == '') {
           alert('Todos los campos son obligatorios.');
           return false;
         }
         if (correo.includes('|') || contrasena.includes('|') || nombre.includes('|')) {
            alert('El símbolo | no está permitido.');
            return false;
          }
          if (correo.includes('@') || contrasena.includes('@') || nombre.includes('@')) {
            alert('El símbolo @ no está permitido.');
            return false;
          }
          if (correo.includes('.') || contrasena.includes('.') || nombre.includes('.')) {
            alert('El símbolo . no está permitido.');
            return false;
          }
          if (nombre.split('').join('') !== nombre.substring(0, 25)) {
            alert('El nombre no puede exceder los 25 caracteres.');
            return false;
          }
          if (correo.split('').join('') !== correo.substring(0, 15)) {
            alert('El correo no puede exceder los 15 caracteres.');
            return false;
          }
          if (contrasena.split('').join('') !== contrasena.substring(0, 14)) {
            alert('La contraseña no puede exceder los 14 caracteres.');
            return false;
          }
          if (contrasena.split('').join('') === contrasena.substring(0, 7)) {
            alert('La contraseña debe tener al menos 8 caracteres.');
            return false;
          }
         return true;
       }
     "))
    (body
     (div ([class "container"])
          (div ([class "row justify-content-center align-items-center min-vh-100"])
               (div ([class "col-md-6 col-lg-4"])
                    (div ([class "card border-0 rounded-3"])
                         (div ([class "card-header text-center py-3"])
                              (h2 ([class "mb-0"]) "Crear Nuevo Usuario"))
                         (div ([class "card-body p-4"])
                              (form ([action "/crear"] [method "POST"] [onsubmit "return validarFormulario();"])
                                    (div ([class "mb-3"])
                                         (label ([for "nombre"] [class "form-label"]) "Nombre:")
                                         (div ([class "input-group"])
                                              (span ([class "input-group-text"]) (i ([class "bi bi-person"])))
                                              (input ([type "text"] [id "nombre"] [name "nombre"] [class "form-control"] [placeholder "Ingrese su nombre"]))))
                                    (div ([class "mb-3"])
                                      (label ([for "correo"] [class "form-label"]) "Correo:")
                                        (div ([class "email-input-group"])
                                              (div ([class "input-group"])
                                                  (span ([class "input-group-text"]) (i ([class "bi bi-envelope"])))
                                                  (input ([type "text"] [id "correo"] [name "correo"] [class "form-control"] [placeholder "Nombre de usuario"])))
                                              (span ([class "email-domain"]) "@una.ac.cr")))
                                    (div ([class "mb-3"])
                                         (label ([for "contrasena"] [class "form-label"]) "Contraseña:")
                                         (div ([class "input-group"])
                                              (span ([class "input-group-text"]) (i ([class "bi bi-lock"])))
                                              (input ([type "password"] [id "contrasena"] [name "contrasena"] [class "form-control"] [placeholder "Ingrese su contraseña"]))))
                                    (div ([class "d-grid gap-2"])
                                         (button ([type "submit"] [class "btn btn-success btn-lg"]) "Crear usuario")))
                              (div ([class "text-center mt-3"])
                                   (a ([href "/"]) "¿Ya tienes una cuenta? Inicia sesión"))))))))))


(define (contenido-nuevo-correo correo-usuario)
  `(div
    (div ([class "d-flex justify-content-between flex-wrap flex-md-nowrap align-items-center pt-3 pb-2 mb-3 border-bottom"])
         (h1 ([class "h2"]) "Nuevo Correo"))
    (div ([class "alert alert-info"]) 
         "Límites: Asunto (50 caracteres), Mensaje (500 caracteres). No se permite el uso del carácter '|'.")
    (form ([action "/enviar-correo"] [method "post"] [onsubmit "return validarFormularioCorreo();"])
          (div ([class "mb-3"])
               (label ([for "destino"] [class "form-label"]) "Destinatario:")
               (input ([type "email"] 
                      [class "form-control"] 
                      [id "destino"] 
                      [name "destino"] 
                      [required ""])))
          (div ([class "mb-3"])
               (label ([for "asunto"] [class "form-label"]) "Asunto:")
               (input ([type "text"] 
                      [class "form-control"] 
                      [id "asunto"] 
                      [name "asunto"] 
                      [maxlength "100"]
                      [oninput "actualizarContador(this, 'contadorAsunto', 50)"]))
               (small ([id "contadorAsunto"] [class "text-muted"]) "0 de 50"))
          (div ([class "mb-3"])
               (label ([for "cuerpo"] [class "form-label"]) "Mensaje:")
               (textarea ([class "form-control"] 
                         [id "cuerpo"] 
                         [name "cuerpo"] 
                         [rows "5"] 
                         [maxlength "1000"]
                         [oninput "actualizarContador(this, 'contadorCuerpo', 500)"]))
               (small ([id "contadorCuerpo"] [class "text-muted"]) "0 de 500"))
          (button ([type "submit"] [class "btn btn-primary"]) "Enviar"))
    (script "function actualizarContador(elemento, idContador, maxLength) {
        var contador = document.getElementById(idContador);
        var actual = elemento.value.length;
        contador.innerHTML = actual.toString().concat(' de ', maxLength.toString());
    }")))

;; Página de resultado con mensaje de alerta
(define (pagina-resultado mensaje)
  `(html
    (head
     (title "Resultado")
     (meta ([charset "utf-8"]))
     (meta ([name "viewport"] [content "width=device-width, initial-scale=1"]))
     (link ([rel "stylesheet"] [href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css"] [integrity "sha384-QWTKZyjpPEjISv5WaRU9OFeRpok6YctnYmDr5pNlyT2bRjXh0JMhjY6hW+ALEwIH"] [crossorigin "anonymous"]))
     (link ([rel "stylesheet"] [href "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css"]))
     (script ([src "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js"] [integrity "sha384-YvpcrYf0tY3lHB60NNkmXc5s9fDVZLESaAA55NDzOxhy9GkcIdslK1eN7N6jIeHz"] [crossorigin "anonymous"]))
     (style "
       body { background-color: #f8f9fa; }
       .card { box-shadow: 0 4px 8px rgba(0,0,0,0.1); }
       .card-header { background-color: #A31E32; color: white; }
       .btn-primary { background-color: #28a745; border-color: #007bff; }
       .btn-primary:hover { background-color: #218838; border-color: #1e7e34; }
     ")
     (script ,(string-append "
       window.onload = function() {
         setTimeout(function() {
           const toast = new bootstrap.Toast(document.getElementById('successToast'));
           toast.show();
         }, 500);
       };
     ")))
    (body
     (div ([class "container"])
          (div ([class "row justify-content-center align-items-center min-vh-100"])
               (div ([class "col-md-6 col-lg-4"])
                    (div ([class "card border-0 rounded-3"])
                         (div ([class "card-header text-center py-3"])
                              (h2 ([class "mb-0"]) "¡Usuario Creado!")
                              (i ([class "bi bi-check-circle-fill fs-1 mt-2"])))
                         (div ([class "card-body p-4 text-center"])
                              (p ([class "lead"]) ,mensaje)
                              (div ([class "d-grid gap-2 mt-4"])
                                   (a ([href "/"] [class "btn btn-primary btn-lg"])
                                      (i ([class "bi bi-house-door me-2"]))
                                      "Volver al inicio")))))))
          
          ;; Toast de notificación
          (div ([class "position-fixed bottom-0 end-0 p-3"])
               (div ([id "successToast"] [class "toast"] [role "alert"] [aria-live "assertive"] [aria-atomic "true"])
                    (div ([class "toast-header bg-success text-white"])
                         (i ([class "bi bi-check-circle me-2"]))
                         (strong ([class "me-auto"]) "Éxito")
                         (button ([type "button"] [class "btn-close btn-close-white"] [data-bs-dismiss "toast"] [aria-label "Close"])))
                    (div ([class "toast-body"])
                         ,mensaje))))))

;; Página de error con mensaje de alerta
(define (pagina-error mensaje)
  `(html
    (head
     (title "Error")
     (style ,estilos-comunes)
     (script ,(string-append "window.onload = function() { alert('Error: " mensaje "'); };")))
    (body
     (div ([class "container"])
          (h1 "Error")
          (p ,mensaje)
          (a ((href "/") [class "button"]) "Volver al inicio")))))

;; Página no encontrada (404)
(define (pagina-no-encontrada)
  `(html
    (head
     (title "404 - No encontrado")
     (style ,estilos-comunes)
     (script "window.onload = function() { alert('Página no encontrada'); };"))
    (body
     (div ([class "container"])
          (h1 "404 - Página no encontrada")
          (p "Lo sentimos, la página que buscas no existe.")
          (a ((href "/") [class "button"]) "Volver al inicio")))))
