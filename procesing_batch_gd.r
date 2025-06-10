
# ======================================================================
#           PROCESAMIENTO POR LOTES
#           ARCHIVOS EN GOOGLE DRIVE (UNIDADES COMPARTIDAS) WORKSPACE
# ======================================================================

# Ron Fernández
# 2025-06-09

# NOTA 1: este código analiza archivos en FLAC que se convierten a WAV durante el procesamiento
# NOTA 2: esta es para correr un folder completo, correspondiente a un sitio.

# AQUI HAY DOS OPCIONES DE ACCESO
# 1) navegando por las carpetas de Drive
# 2) ya teniendo los ID y nombres de folders

# LIBRERIAS ----
{
  library(googledrive)
  library(httr)
  library(tuneR)
  library(seewave)
  library(av)
  library(soundecology)
  library(dplyr)
}


# 1 AUTENTICACION EN GOOGLE ----

drive_auth() # ingresa a tu cuenta de Google Drive...
# aunque ya te hayas autenticado se sugiere siempre reiniciar la autenticación 
# en cada nuevo ingreso para actualizar cualquier cambio en los folders

# 2 ESTABLECER LOS ID Y NOMBRES DE CARPETAS ----
# Aquí hay dos formas de hacerlo:
## 2.1 OPCION 1: Navegando -----
## Navegando entre unidades y carpetas...
shared_drive_find() # para observar Unidades Compartidas e identificar el ID...
sdrive_id <- readline(prompt = "Ingresa el ID de la Unidad compartida:") # asignar en terminal el ID de la Unidad Compartida a trabajar...

# navegar e identificar el ID de la carpeta de trabajo...
drive_find(n_max = 15, # cantidad de folders a mostrar (incluye subfolders)
           shared_drive = as_id(sdrive_id), # unidad compartida
           type = "folder")

# asignar en terminal el ID de la carpeta a trabajar...
# se puede ir reasignando el folder_id a medida que se va navegando entre subcarpetas hasta encontrar la de interés
folder_id <- readline(prompt = "Ingresa el ID de la carpeta en Google Drive:")


## 2.2 OPCION 2: con IDs establecidos -----
## En caso de tener los ID y nombres previamente...
## Nota: los ID y nombres se asignan desde la consola, !NO modifiques el código!

# Asigna el ID de la carpeta a trabajar en la consola
folder_id <- readline(prompt = "Ingresa el ID de la carpeta en Google Drive:")
# Asigna el nombre de la carpeta a trabajar en la consola
folder_site_name <- readline(prompt = "Ingresa el nombre de la carpeta:")
# Asigna en la consola el ID de la carpeta donde se guardarán los resultados
folder_results <- readline(prompt = "Ingresa el ID de la carpeta de resultados en Google Drive:")

# 3 FOLDERS Y TEMPORALES ----
#creamos una lista con los archivos
files <- drive_ls(as_id(folder_id))
nrow(files) #verificar cuantos archivos hay


# Crear carpeta temporal local
temp_dir <- file.path(tempdir(), "gdrive_audio")
dir.create(temp_dir, showWarnings = FALSE)

# 4 FILTRADO DE ARCHIVOS A PROCESAR ----
# En este caso filtramos archivos en horario entre 05:00 a 19:00
# y cada ciertos minutos específicos

# Definimos el conjunto de minutos permitidos
valid_time <- c(0, 6, 12, 18, 24, 30, 36, 42, 48, 54)

# Aplicamos el filtro sobre cada nombre de archivo
idx <- sapply(files$name, function(name) {
    # 1) Quitar la extensión (".flac", ".wav", etc.) → "20241216_142000"
    without_ext <- tools::file_path_sans_ext(name)
    # 2) Separar por "_" y tomar la parte "HHMMSS"
    parts   <- strsplit(without_ext, "_")[[1]]
    if (length(parts) < 2) return(FALSE)
    time   <- parts[length(parts)]  # ej. "142000"
    # 3) Extraer hora (pos 1–2) y minuto (pos 3–4):
    hour   <- as.numeric(substr(time, 1, 2))
    mins <- as.numeric(substr(time, 3, 4))
    # 4) Filtrar por rango de hora: de 5 a 19 (inclusive)
    if (is.na(hour) || hour < 5 || hour > 19) return(FALSE)
    # 5) Filtrar por minuto en el vector minutos_validos
    if (is.na(mins) || !(mins %in% valid_time)) return(FALSE)
    return(TRUE)
  })
files_filtered <- files[idx, ]

#reasigno el objeto a files para usarlo en el loop
files_all <- files
files <- files_filtered



# 5 CALCULAR INDICES ALPHA ----
# 5.1 Crear el dataframe -----
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


# 5.2 Procesar cada archivo
# He añadido un counter para estimar el tiempo de procesamiento
{
  start_time <- Sys.time()
  for(i in 1:nrow(files)) {
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
      NP <- nrow(fpeaks(meanspec(wave, plot = FALSE), amp = c(1 / 70, 1 / 70), plot = FALSE))
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
  x <- assign(paste0("TableAlphaIndices_", folder_site_name), temp_table)


  # exportar dataframe como archivo CSV 
  # (esta copia queda localmente como respaldo y se requiere para subir a drive)
  exported_csv <- write.csv(x, file = paste0("TableAlphaIndices_", folder_site_name, ".csv"))

  # Verificar si el csv se escribió correctamente
  if (!is.null(file.exists(path = paste0("TableAlphaIndices_", folder_site_name, ".csv")))) {
    print(paste("Archivo CSV (", paste0("TableAlphaIndices_", folder_site_name, ".csv"), ") escrito exitosamente:", exported_csv))
  } else {
    print("Error al escribir el archivo.")
  }


}




#subir archivo a drive

uploaded_file <- drive_upload(media = paste0("TableAlphaIndices_", folder_site_name, ".csv"),
                              name = paste0("TableAlphaIndices_", folder_site_name, ".csv"),
                              path = as_id(folder_results)) #el id de la carpeta de resultados en GDrive

# Verificar si el archivo se subió correctamente
if (!is.null(uploaded_file)) {
  print(paste("Archivo subido exitosamente:", uploaded_file$name))
} else {
  print("Error al subir el archivo.")
}

##FIN