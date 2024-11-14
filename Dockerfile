# Usa una imagen base de Racket
FROM racket/racket:latest

# Establece el directorio de trabajo dentro del contenedor
WORKDIR /app

# Copia todos los archivos de tu proyecto al contenedor
COPY . /app

# Instala el paquete web-server
RUN raco pkg install --auto web-server

# Exponer el puerto en el que tu app se ejecutará
EXPOSE 8080

# Comando para ejecutar el archivo principal de tu aplicación en Racket
CMD ["racket", "main.rkt"]
