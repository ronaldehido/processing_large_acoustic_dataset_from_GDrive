###################################
#### ARCHIVOS EN GOOGLE DRIVE ####
###################################
#archivos que estan en formato FLAC en una carpeta de Google Drive

#Prueba con pocos archivos

install.packages(c("googledrive", "httr"))

library(googledrive)
library(httr)


#otros paquetes
library(tuneR)
library(seewave)
library(soundecology)
library(dplyr)

# Autenticación con Google Drive
drive_auth() #ingresa a tu cuenta de Google Drive
#previamente se te compartirá a tu cuenta la carpeta de trabajo para que puedas acceder a los archivos

#establecer los ID de las Unidades / Carpetas compartidas donde están los archivos

#PRUEBA: usaremos una carpeta de prueba en una Unidad compartida con pocos archivos 

shared_drive_find() #identificar el ID de la carpeta compartida a utilizar
sdrive_id <- readline(prompt = "Ingresa el ID de la Unidad compartida:") #copiar en la terminal el ID de la Unidad Compartida

drive_find(n_max = 10, shared_drive = as_id(sdrive_id), type = "folder") #identificar el ID de la carpeta de trabajo
folder_id <- readline(prompt = "Ingresa el ID de la carpeta en Google Drive:")

folder_site_name <- readline(prompt = "Ingresa el nombre de la carpeta de trabajo en Google Drive (codifica sitio y fecha:")

files_gdrive <- drive_ls(as_id(folder_id))

#drive_ls(as_id(folder_id))


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

for(i in 1:nrow(files_gdrive)) {
  tryCatch({
    file_info <- files_gdrive[i, ]
    print(paste("Procesando archivo", i, "de", nrow(files_gdrive), ":", file_info$name))
    
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

  print("Procesamiento completado!")
  
  end_time <- Sys.time()
  
  total_time <- as.numeric(end_time - start_time, units = "secs")
  cat("\n--- Resumen de Tiempos ---\n")
  cat(paste0("Tiempo total del proceso: ", round(total_time, 2), " segundos.\n"))

  
  # Limpiar carpeta temporal completa
  unlink(temp_dir, recursive = TRUE)
  
}

#renombrar la tabla de resultados



x <- assign(name_folder, temp_table)
library(dplyr)
x <- x %>%
  mutate(site = folder_site_name)

write.csv(x, file=paste0(name_folder,".csv"))

# exportar dataframe como archivo CSV
 write.csv(name_dataframe,file=paste0(name_dataframe,".csv"))
 dataframe_export <- read.csv(name_dataframe,file=paste0(name_dataframe,".csv"))

 paste0("TableAlphaIndices_",folder_site_name) <- temp_table

  
#subir archivo a drive
uploaded_file <- drive_upload(media=paste0(name_folder,".csv"), name=paste0(name_folder,".csv"), path=as_id("1Kigtkbw09V3KKxerafy_6lAmv_1rx85U"))
# Verificar si el archivo se subió correctamente
if (!is.null(uploaded_file)) {
  print(paste("Archivo subido exitosamente:", uploaded_file$name))
} else {
  print("Error al subir el archivo.")
}