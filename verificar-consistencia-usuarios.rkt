#lang racket

(require web-server/http
         racket/file)

(provide verificar-consistencia-usuarios manejar-error-consistencia)

(define (verificar-consistencia-usuarios)
  (define usuarios-file "usuarios.txt")
  (define correos-dir "correos")
  
  ; Contar usuarios en el archivo usuarios.txt
  (define num-usuarios
    (if (file-exists? usuarios-file)
        (with-input-from-file usuarios-file
          (lambda ()
            (length (port->lines (current-input-port)))))
        0))
  
  ; Contar archivos .txt en la carpeta correos/
  (define num-archivos-correo
    (if (directory-exists? correos-dir)
        (length (filter (lambda (f) (regexp-match? #rx"\\.txt$" f))
                        (directory-list correos-dir)))
        0))
  
  ; Verificar si los números coinciden
  (unless (= num-usuarios num-archivos-correo)
    (error 'verificar-consistencia-usuarios
           "Inconsistencia detectada: ~a usuarios, ~a archivos de correo"
           num-usuarios
           num-archivos-correo)))

; Función para manejar el error de consistencia
(define (manejar-error-consistencia exn)
  (response/xexpr
   #:code 400
   `(html
     (head
      (title "500 - Internal Server Error")
      (style "body { font-family: Arial, sans-serif; text-align: center; padding-top: 50px; }
              h1 { color: #d9534f; }"))
     (body
      (h1 "Error 400 - Bad Request")
      (p "Se ha detectado una inconsistencia en el sistema de usuarios y correos.")
      (p ,(format "Error: ~a" (exn-message exn)))
      (p "Por favor, contacte al administrador del sistema.")))))