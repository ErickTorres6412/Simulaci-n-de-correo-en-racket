# Usa una imagen base de Racket
FROM racket/racket:latest

# Establece el directorio de trabajo dentro del contenedor
WORKDIR /app

# Copia todos los archivos de tu proyecto al contenedor
COPY . /app

# Instala el paquete web-server y otros paquetes necesarios
RUN raco pkg install --auto web-server

# Exponer el puerto que Railway asignará dinámicamente
ENV PORT=8080
EXPOSE $PORT

# Comando para ejecutar el archivo principal de tu aplicación en Racket
CMD ["racket", "main.rkt"]