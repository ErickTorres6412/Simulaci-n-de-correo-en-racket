#lang racket

;; Importamos las bibliotecas necesarias
(require web-server/servlet
         web-server/servlet-env
         "handlers.rkt")

;; Función mejorada para obtener el puerto desde la variable de entorno
(define (get-port)
  (let ([port-str (getenv "PORT")])
    (if port-str
        (string->number port-str)
        8080))) ; Puerto por defecto si no hay variable de entorno

;; Iniciamos el servidor web con la configuración necesaria para Railway
(serve/servlet 
 start  ; La función 'start' está definida en handlers.rkt
 #:launch-browser? #f  ; No abrimos automáticamente el navegador
 #:quit? #f  ; El servidor no se detendrá automáticamente
 #:listen-ip #f  ; Esto permite escuchar en todas las interfaces
 #:port (get-port)  ; Usamos la función get-port para obtener el puerto
 #:servlet-regexp #rx""  ; Todas las rutas serán manejadas por nuestro servlet
 #:command-line? #t)  ; Permite que el servidor se ejecute desde línea de comandos

;; Imprimimos un mensaje para indicar que el servidor está en funcionamiento
(displayln (format "Servidor iniciado en el puerto ~a" (get-port)))