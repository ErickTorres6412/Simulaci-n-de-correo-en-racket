#lang racket

;; Importamos las bibliotecas necesarias
(require web-server/servlet
         web-server/servlet-env
         "handlers.rkt")

;; Iniciamos el servidor web
(serve/servlet 
 start  ; La función 'start' está definida en handlers.rkt
 #:launch-browser? #f  ; No abrimos automáticamente el navegador
 #:quit? #f  ; El servidor no se detendrá automáticamente
 #:port 8080  ; El servidor escuchará en el puerto 8080
 #:servlet-regexp #rx"")  ; Todas las rutas serán manejadas por nuestro servlet

;; Imprimimos un mensaje para indicar que el servidor está en funcionamiento
(displayln "Servidor iniciado en http://localhost:8080")