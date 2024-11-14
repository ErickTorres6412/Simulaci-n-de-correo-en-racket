#lang racket

(provide crear-usuario verificar-credenciales obtener-correo-usuario
         guardar-correo obtener-correos-usuario
         mover-correo-a-papelera recuperar-correo-de-papelera
         vaciar-papelera eliminar-correo
         correo correo-id correo-remitente correo-destino 
         correo-asunto correo-cuerpo correo-fecha correo-estado correo-en-papelera
         leer-todos-los-correos leer-correos-enviados leer-correos-recibidos obtener-correos-papelera  
         set-correo-estado! set-correo-en-papelera! obtener-correo-por-id)

(require racket/runtime-path)

;; Definición de la estructura correo
(struct correo (id remitente destino asunto cuerpo fecha estado en-papelera) #:mutable #:transparent) 

;; Definimos la ubicación del archivo que almacenará los datos de los usuarios
(define archivo-usuarios (build-path (current-directory) "usuarios.txt"))

;; Definimos la ubicación del directorio para los archivos de correo
(define-runtime-path directorio-correos "correos")

;; Aseguramos que el directorio de correos exista
(when (not (directory-exists? directorio-correos))
  (make-directory directorio-correos))

;; Función para obtener un correo específico por su ID
(define (obtener-correo-por-id correo-usuario id-correo)
  (let ([correos (leer-todos-correos correo-usuario)])
    (let ([correo (findf (lambda (correo)
                            (let ([correo-id (string->number (list-ref correo 0))])
                              (and (number? correo-id)
                                   (= correo-id (string->number id-correo)))))
                          correos)])
      (displayln (format "Correo encontrado: ~a" correo))  ; Agrega esta línea
      (displayln (format "Correo usuario: ~a" correo-usuario))  ; Agrega esta línea
      (displayln (format "ID correo: ~a" id-correo))  ; Agrega esta línea
      correo)))

;; Función para leer todos los correos, sin importar el estado ni si están en la papelera
(define (leer-todos-correos correo-usuario)
  (define archivo (build-path directorio-correos (string-append correo-usuario ".txt")))
  (if (file-exists? archivo)
      (with-input-from-file archivo
        (lambda ()
          (let loop ([correos '()])
            (let ([linea (read-line)])
              (if (eof-object? linea)
                  (reverse correos) ; Devolvemos los correos en orden inverso (de más reciente a más antiguo)
                  (let* ([linea-limpia (string-trim linea)]
                         [campos (string-split linea-limpia "|")])
                    (loop (cons campos correos))))))))
      '()))


;; Función para obtener el último ID utilizado
(define (ultimo-id)
  (if (file-exists? archivo-usuarios)
      (with-input-from-file archivo-usuarios
        (lambda ()
          (let loop ([ultimo-id 0])
            (let ([linea (read-line)])
              (if (eof-object? linea)
                  ultimo-id
                  (let* ([campos (string-split linea "|")]
                         [id (string->number (car campos))])
                    (loop (max ultimo-id id))))))))
      0))

;; Función para generar un nuevo ID
(define (generar-id)
  (+ (ultimo-id) 1))

;; Función para crear un usuario
(define (crear-usuario nombre correo contrasena)
  (let* ([id (generar-id)]
         [correo-completo (string-append correo "@una.ac.cr")]
         [datos-usuario (format "~a|~a|~a|~a\n" id nombre correo-completo contrasena)])
    (with-output-to-file archivo-usuarios
      #:exists 'append
      (lambda () (display datos-usuario)))
    ;; Crear el archivo de correo del usuario en el directorio de correos
    (let ([archivo-correo (build-path directorio-correos (string-append correo-completo ".txt"))])
      (with-output-to-file archivo-correo
        #:exists 'append
        (lambda () (void))))
    "Usuario creado exitosamente"))
    
;; Función para verificar las credenciales de un usuario
(define (verificar-credenciales correo contrasena)
  (with-input-from-file archivo-usuarios
    (lambda ()
      (let loop ()
        (let ([linea (read-line)])
          (if (eof-object? linea)
              #f
              (let* ([linea-limpia (string-trim linea)]        ; Elimina espacios y saltos de línea
                     [campos (string-split linea-limpia "|")]  ; Divide la línea en campos
                     [correo-archivo (list-ref campos 2)]      ; Correo almacenado en el archivo
                     [contrasena-archivo (list-ref campos 3)]) ; Contraseña almacenada en el archivo
                (if (and (= (length campos) 4)
                         (equal? correo-archivo correo)         ; Compara el correo
                         (equal? contrasena-archivo contrasena)) ; Compara la contraseña
                    #t
                    (loop)))))))))



;; Función para obtener el correo electrónico de un usuario
(define (obtener-correo-usuario nombre)
  (with-input-from-file archivo-usuarios
    (lambda ()
      (let loop ()
        (let ([linea (read-line)])
          (if (eof-object? linea)
              #f
              (let ([campos (string-split linea "|")])
                (if (equal? (cadr campos) nombre)
                    (caddr campos)
                    (loop)))))))))

;; Función para verificar si existe un usuario
(define (usuario-existe? correo-usuario)
  (file-exists? (build-path directorio-correos (string-append correo-usuario ".txt"))))

;; Función para obtener el siguiente ID disponible
(define (obtener-siguiente-id correos)
  (if (null? correos)
      1
      (+ 1 (apply max (map correo-id correos)))))


;; Función modificada para guardar un correo
(define (guardar-correo correo-usuario nuevo-correo)
  (if (usuario-existe? (correo-destino nuevo-correo))
      (let* ([archivo-remitente (build-path directorio-correos (string-append correo-usuario ".txt"))]
             [archivo-destino (build-path directorio-correos (string-append (correo-destino nuevo-correo) ".txt"))]
             [correos-remitente (obtener-correos-usuario correo-usuario)]
             [correos-destino (obtener-correos-usuario (correo-destino nuevo-correo))]
             [nuevo-id-remitente (obtener-siguiente-id correos-remitente)]
             [nuevo-id-destino (obtener-siguiente-id correos-destino)]
             [correo-remitente (struct-copy correo nuevo-correo [id nuevo-id-remitente] [estado "enviado"] [en-papelera #f])]
             [correo-destino (struct-copy correo nuevo-correo [id nuevo-id-destino] [estado "recibido"] [en-papelera #f])])
        ;; Guardar en el archivo del remitente
        (actualizar-archivo-correos archivo-remitente (cons correo-remitente correos-remitente))
        ;; Guardar en el archivo del destinatario
        (actualizar-archivo-correos archivo-destino (cons correo-destino correos-destino))
        (cons nuevo-id-remitente nuevo-id-destino)) ; Devuelve los nuevos IDs como un par
      (error "El destinatario no existe")))

;; Función para obtener todos los correos de un usuario
(define (obtener-correos-usuario correo-usuario)
  (let ([archivo (build-path directorio-correos (string-append correo-usuario ".txt"))])
    (if (file-exists? archivo)
        (with-input-from-file archivo
          (lambda ()
            (let loop ([correos '()])
              (let ([linea (read-line)])
                (if (eof-object? linea)
                    (reverse correos)
                    (let ([campos (string-split linea "|")])
                      (if (= (length campos) 8)
                          (loop (cons (apply correo (map (lambda (x) 
                                                           (cond
                                                             [(string->number x) (string->number x)]
                                                             [(equal? x "true") #t]
                                                             [(equal? x "false") #f]
                                                             [else x])) 
                                                         campos)) 
                                      correos))
                          (loop correos))))))))
        '())))

;; Función modificada para mover un correo a la papelera
(define (mover-correo-a-papelera correo-usuario id-correo)
  (let* ([archivo (build-path directorio-correos (string-append correo-usuario ".txt"))]
         [correos (obtener-correos-usuario correo-usuario)]
         [correo-actualizado (map (lambda (c)
                                    (if (= (correo-id c) id-correo)
                                        (struct-copy correo c [en-papelera #t])
                                        c))
                                  correos)])
    (actualizar-archivo-correos archivo correo-actualizado)
    (for-each (lambda (c)
                (when (= (correo-id c) id-correo)
                  (displayln (format "Correo movido a papelera: ~a" (correo-en-papelera c)))))
              correo-actualizado)))

;; Función para recuperar un correo de la papelera
(define (recuperar-correo-de-papelera correo-usuario id-correo)
  (let* ([archivo (build-path directorio-correos (string-append correo-usuario ".txt"))]
         [correos (obtener-correos-usuario correo-usuario)]
         [correo-actualizado (map (lambda (c)
                                    (if (= (correo-id c) id-correo)
                                        (struct-copy correo c 
                                                     [en-papelera #f]
                                                     [estado (if (equal? (correo-estado c) "recibido")
                                                                 "recibido"
                                                                 "enviado")])
                                        c))
                                  correos)])
    (actualizar-archivo-correos archivo correo-actualizado)))

;; Función para vaciar la papelera (elimina físicamente los correos)
(define (vaciar-papelera correo-usuario)
  (let* ([archivo (build-path directorio-correos (string-append correo-usuario ".txt"))]
         [correos (obtener-correos-usuario correo-usuario)]
         [correos-no-papelera (filter (lambda (c) (not (correo-en-papelera c))) correos)])
    (actualizar-archivo-correos archivo correos-no-papelera)
    (- (length correos) (length correos-no-papelera)))) ; Devuelve el número de correos eliminados

;; Función para eliminar un correo (solo lo mueve a la papelera)
(define (eliminar-correo correo-usuario id-correo)
  (mover-correo-a-papelera correo-usuario id-correo))

;; Función auxiliar para actualizar el archivo de correos
(define (actualizar-archivo-correos archivo correos)
  (with-output-to-file archivo
    #:exists 'replace
    (lambda ()
      (for-each (lambda (c)
                  (fprintf (current-output-port) "~a|~a|~a|~a|~a|~a|~a|~a\n"
                           (correo-id c)
                           (correo-remitente c)
                           (correo-destino c)
                           (correo-asunto c)
                           (correo-cuerpo c)
                           (correo-fecha c)
                           (correo-estado c)
                           (if (correo-en-papelera c) "true" "false")))
                correos))))

;; Función para leer solo los correos recibidos (excluyendo los de la papelera)
(define (leer-correos-recibidos correo-usuario)
  (define archivo (build-path directorio-correos (string-append correo-usuario ".txt")))
  (if (file-exists? archivo)
      (with-input-from-file archivo
        (lambda ()
          (let loop ([correos '()])
            (let ([linea (read-line)])
              (if (eof-object? linea)
                  (reverse correos) ; Devolvemos los correos en orden inverso (por ejemplo, de más reciente a más antiguo)
                  (let* ([linea-limpia (string-trim linea)]
                         [campos (string-split linea-limpia "|")]
                         [estado (if (>= (length campos) 7) (list-ref campos 6) "")]
                         [en-papelera (if (>= (length campos) 8) (list-ref campos 7) "")])
                    (if (and (equal? estado "recibido")
                             (equal? en-papelera "false"))
                        (loop (cons campos correos))
                        (loop correos))))))))
      '()))

;; Función para leer solo los correos enviados (excluyendo los de la papelera)
(define (leer-correos-enviados correo-usuario)
  (define archivo (build-path directorio-correos (string-append correo-usuario ".txt")))
  (if (file-exists? archivo)
      (with-input-from-file archivo
        (lambda ()
          (let loop ([correos '()])
            (let ([linea (read-line)])
              (if (eof-object? linea)
                  (reverse correos)
                  (let ([campos (string-split linea "|")])
                    (if (and (equal? (list-ref campos 6) "enviado")
                             (equal? (list-ref campos 7) "false"))
                        (loop (cons campos correos))
                        (loop correos))))))))
      '())) ; Retorna una lista vacía si el archivo no existe

;; Función para leer todos los correos (enviados y recibidos, excluyendo los de la papelera)
(define (leer-todos-los-correos correo-usuario)
  (define archivo (build-path directorio-correos (string-append correo-usuario ".txt")))
  (if (file-exists? archivo)
      (with-input-from-file archivo
        (lambda ()
          (let loop ([correos '()])
            (let ([linea (read-line)])
              (if (eof-object? linea)
                  (reverse correos)
                  (let ([campos (string-split linea "|")])
                    (if (equal? (list-ref campos 7) "false")
                        (loop (cons campos correos))
                        (loop correos))))))))
      '())) ; Retorna una lista vacía si el archivo no existe

;; Función para obtener los correos de la papelera
(define (obtener-correos-papelera correo-usuario)
  (define archivo (build-path directorio-correos (string-append correo-usuario ".txt")))
  (if (file-exists? archivo)
      (with-input-from-file archivo
        (lambda ()
          (let loop ([correos '()])
            (let ([linea (read-line)])
              (if (eof-object? linea)
                  (reverse correos)
                  (let ([campos (string-split linea "|")])
                    (if (equal? (list-ref campos 7) "true")
                        (loop (cons linea correos))
                        (loop correos))))))))
      '())) ; Retorna una lista vacía si el archivo no existe