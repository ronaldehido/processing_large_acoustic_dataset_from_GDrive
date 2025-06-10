###################################
#### ARCHIVOS EN GOOGLE DRIVE ####
###################################

#Ron Fernández
#2025-06-05

#archivos que estan en formato FLAC en una carpeta de Google Drive

#!!!!! Esta es una prueba con pocos archivos (un folder con audios de un solo día)

install.packages(c("googledrive", "httr"))

library(googledrive)
library(httr)


#otros paquetes
library(tuneR)
library(seewave)
library(av)
library(soundecology)
library(dplyr)


# Autenticación con Google Drive
drive_auth() #ingresa a tu cuenta de Google Drive
#previamente se te compartirá a tu cuenta la carpeta de trabajo para que puedas acceder a los archivos

#establecer los ID de las Unidades / Carpetas compartidas donde están los archivos
#!!! por ahora comparto directamente el ID para este ejemplo pero el resto de codigo ayuda a hacer busquedas dentro de Drive

#shared_drive_find() #identificar el ID de la carpeta compartida a utilizar
#sdrive_id <- readline(prompt = "Ingresa el ID de la Unidad compartida:") #copiar en la terminal el ID de la Unidad Compartida

#drive_find(n_max = 10, shared_drive = as_id(sdrive_id), type = "folder") #identificar el ID de la carpeta de trabajo
#folder_id <- readline(prompt = "Ingresa el ID de la carpeta en Google Drive:") #copiar en la terminal el ID de la carpeta de trabajo
folder_id <- "1Ai4tTS7bKACBUZo5F64IXS8Pk63L2f5x" #por ahora comparto directamente el ID de la carpeta de ejemplo

#folder_site_name <- readline(prompt = "Ingresa el nombre de la carpeta de trabajo en Google Drive (codifica sitio y fecha:")
folder_site_name <- "R501_240801_D2" #por ahora comparto directamente el nombre de la carpeta de ejemplo

files <- drive_ls(as_id(folder_id))


#folder de resultados, se puede buscar navegando con drive_find() o copiarla del navegador 
#folder_results <- readline(prompt = "Ingresa el ID de la carpeta de resultados en Google Drive:") #copiar en la terminal el ID de la carpeta donde se guardan los resultados
folder_results <- "1BXNLxKla-ei6uuLBHiSp5RfNTEQ6UMWz" # por ahora comparto directamente el ID de la carpeta

# Crear carpeta temporal local
temp_dir <- file.path(tempdir(), "gdrive_audio")
dir.create(temp_dir, showWarnings = FALSE)


# Inicializar dataframe
temp_table <- data.frame(
  archivo = character(),
  gdrive_id = character(),
  path_local = character(),
  NP = numeric(),
  ACI_1 = numeric(),
  BI = numeric(),
  NDSI = numeric(),
  stringsAsFactors = FALSE
)


# Procesar cada archivo
{
start_time <- Sys.time() 

for(i in seq_len(nrow(files))) {
  tryCatch({
    file_info <- files[i, ]
    print(paste("Procesando archivo", i, "de", nrow(files), ":", file_info$name))
    
    # Descargar archivo temporalmente
    file_local <- file.path(temp_dir, file_info$name)
    drive_download(as_id(file_info$id), path = file_local, overwrite = TRUE)
    
    # Convertir a WAV
    file_temp_wav <- file.path(temp_dir, paste0("temp_", i, ".wav"))
    av_audio_convert(file_local, file_temp_wav, format = "wav", 
                     channels = 1, sample_rate = 44100)
    
    # Leer audio
    wave <- readWave(file_temp_wav)
    
    # Calcular índices
    NP <- nrow(fpeaks(meanspec(wave, plot = FALSE), amp = c(1/70, 1/70), plot = FALSE))
    ACI_1 <- acoustic_complexity(wave)$AciTotAll_left
    BI <- bioacoustic_index(wave)$left_area
    NDSI <- ndsi(wave)$ndsi_left
    
    # Agregar resultados
    new_row <- data.frame(
      archivo = file_info$name,
      gdrive_id = file_info$id,
      path_local = file_local,
      NP = NP,
      ACI_1 = ACI_1,
      BI = BI,
      NDSI = NDSI,
      stringsAsFactors = FALSE
    )
    
    temp_table <- rbind(temp_table, new_row)
    
    # Limpiar archivos temporales del procesamiento
    file.remove(file_temp_wav)
    file.remove(file_local)  # También eliminar el FLAC descargado
    
    }, error = function(e) {
    print(paste("Error procesando archivo", i, ":", e$message))
      })
  
  }
  
  #mensajes finales indicando tiempo de proceso
  print("Procesamiento completado!")
  
  end_time <- Sys.time()
  
  total_time <- as.numeric(end_time - start_time, units = "secs")
  cat("\n--- Resumen de Tiempos ---\n")
  cat(paste0("Tiempo total del proceso: ", round(total_time, 2), " segundos.\n"))

  
  # Limpiar carpeta temporal completa
  unlink(temp_dir, recursive = TRUE)
  
  #
  #añadir columna con nombre de sitio/folder
  temp_table <- temp_table %>%
    mutate(site = folder_site_name)
  
  
  #renombrar la tabla de resultados
  x <- assign(paste0("TableAlphaIndices_",folder_site_name), temp_table)
  
  
  # exportar dataframe como archivo CSV (esta copia queda en la computadora local como respaldo y se requiere para subir a drive)
  exported_csv <- write.csv(x, file=paste0("TableAlphaIndices_",folder_site_name,".csv"))
  
  # Verificar si el csv se escribió correctamente
  if (!is.null(file.exists(path=paste0("TableAlphaIndices_",folder_site_name,".csv")))) {
    print(paste("Archivo CSV (",paste0("TableAlphaIndices_",folder_site_name,".csv"), ") escrito exitosamente:", exported_csv))
  } else {
    print("Error al escribir el archivo.")
  }
  
  
  
}



#subir archivo a drive
uploaded_file <- drive_upload(media=paste0("TableAlphaIndices_",folder_site_name,".csv"), name=paste0("TableAlphaIndices_",folder_site_name,".csv"), path=as_id(folder_results)) #el id de la carpeta de resultados en GDrive
 
 
# Verificar si el archivo se subió correctamente
if (!is.null(uploaded_file)) {
  print(paste("Archivo subido exitosamente:", uploaded_file$name))
} else {
  print("Error al subir el archivo.")
}

##FIN