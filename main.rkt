#lang racket

;; Importamos las bibliotecas necesarias
(require web-server/servlet
         web-server/servlet-env
         "handlers.rkt")

;; Definimos el puerto desde la variable de entorno 'PORT', con valor predeterminado 3000
(define puerto (string->number (or (getenv "PORT") "8080")))

;; Iniciamos el servidor web
(serve/servlet 
 start  ; La función 'start' está definida en handlers.rkt
 #:launch-browser? #f  ; No abrimos automáticamente el navegador
 #:quit? #f  ; El servidor no se detendrá automáticamente
 #:port puerto  ; El servidor escuchará en el puerto de la variable de entorno 'PORT'
 #:servlet-regexp #rx"")  ; Todas las rutas serán manejadas por nuestro servlet

;; Imprimimos un mensaje para indicar que el servidor está en funcionamiento
(displayln (string-append "Servidor iniciado en el puerto " (number->string puerto)))
