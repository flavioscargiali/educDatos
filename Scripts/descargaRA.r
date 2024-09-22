descargaRA <- function(base = c(2023),file_path = getwd() ) {
  
  # Instala la librería si no la tienes
  if (!requireNamespace("zip", quietly = TRUE)) {
    install.packages("zip")
  }
  
  # Cargar la librería
  library(zip)
  
  anios <- seq(2013, 2023)
  path <- c(
    "https://www.argentina.gob.ar/sites/default/files/2023/09/2013_bases_usuarias_en_formato_csv.zip",
    "https://www.argentina.gob.ar/sites/default/files/2023/09/2014_bases_usuarias_en_formato_csv.zip",
    "https://www.argentina.gob.ar/sites/default/files/2023/09/2015_bases_usuarias_en_formato_csv.zip",
    "https://www.argentina.gob.ar/sites/default/files/2023/09/2016_bases_usuarias_en_formato_csv.zip",
    "https://www.argentina.gob.ar/sites/default/files/2023/09/2017_bases_usuarias_en_formato_csv.zip",
    "https://www.argentina.gob.ar/sites/default/files/2023/09/2018_bases_usuarias_en_formato_csv.zip",
    "https://www.argentina.gob.ar/sites/default/files/2023/09/2019_bases_usuarias_en_formato_csv.zip",
    "https://www.argentina.gob.ar/sites/default/files/2018/04/bases_usuarias_en_formato_csv.zip",
    "https://www.argentina.gob.ar/sites/default/files/2021_bases_usuarias_en_formato_csv.zip",
    "https://www.argentina.gob.ar/sites/default/files/bases_usuarias_2022_-_csv.zip",
    "https://www.argentina.gob.ar/sites/default/files/2024/07/2023_bases_usuarias_csv.zip"
  )
  
  descargas <- data.frame(anio = anios,
                          path = path)
  
  descargas <- subset(descargas, subset = anio %in% c(base))
  
  for (i in unique(descargas$anio)) {
    # URL del archivo ZIP
    url <-
      as.character(subset(descargas, subset = anio %in% c(i), select = path))
    
    # Nombre del archivo que se descargará
    destfile <- paste0(file_path, "/", i, ".zip")
    
    # Intentar descargar el archivo
    tryCatch({
      download.file(url, destfile)
      cat("Descargado:", destfile, "\n")
      
      # Intentar descomprimir el archivo
      tryCatch({
        unzip(destfile, exdir = paste0(file_path, "/RA/", i))
        cat("Descomprimido:", destfile, "\n")
        
        # Eliminar el archivo ZIP después de descomprimir
        file.remove(destfile)
        cat("Eliminado:",  paste0(file_path, "/RA/", i),
            ".zip\nSe ha creado el CSV.\nBuscar el archivo en:",paste0(file_path, "/RA/", i) )
        
      }, error = function(e) {
        cat("Error al descomprimir:",
            paste0(file_path, "/RA/", i),
            "\n")
      })
      
    }, error = function(e) {
      cat("Error al descargar:", url, "\n")
    })
    
    
  }
  
} 