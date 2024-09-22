pivotMatricula <- function(anio_base = 2023, file = "") {
  
  if (file == "") {
    stop("Deteniendo el procesamiento porque no se especifica ubicación de la carpeta 'RA'.")
  }
  
  bases_unidas <- list()
  
  path_bases <- grep(x = list.files(file, recursive = T),
                     pattern = "Base 2",
                     value = T)
  
  for (a in c(anio_base)) {
    print(a)
    
    # Filtro del año
    path <- grep(x = path_bases,
                 paste0(a),
                 value = T)
    
    print(path)
    
    # Carga de la base
    ra_matricula <-
      read.csv(paste0(file, path), sep = ";")
    
    for (i in c("lactantes",
                "deambulantes",
                "s2",
                "s3",
                "s4",
                "s5")) {
      # Modificación del nombre de algunas variables
      index <- grep(paste0("^", i, "$"), colnames(ra_matricula))
      names(ra_matricula)[[index]] <- paste0("X_", i)
      
    }
    
    anios <- c(
      "lactantes",
      "deambulantes",
      "s2",
      "s3",
      "s4",
      "s5",
      "1",
      "2",
      "3",
      "4",
      "5",
      "6",
      "7",
      "8",
      "9",
      "10",
      "11",
      "12",
      "1314",
      "20",
      "snu"
    )
    
    anios <- factor(anios, levels = unique(anios))
    
    anios <- data.frame (anios = anios)
    
    
    niveles <-
      c(
        "inicial_maternal",
        "inicial_infantes",
        "primaria6",
        "primaria7",
        "secundaria_cb6",
        "secundaria_cb7",
        "secundaria_completa6",
        "secundaria_completa7",
        "secundaria_orientada",
        "sec_inet",
        "snu",
        "snu_inet"
      )
    
    base <- ra_matricula[, c("ID1",
                             "provincia",
                             "sector",
                             "ambito",
                             niveles)]
    
    base <- merge(base, anios)
    
    # rowsum
    # prueba[["vacios"]] <- rowSums(prueba[niveles] != "" , na.rm = TRUE)
    
    # Inicial
    base[["nivel"]] <- ifelse(
      base[["inicial_maternal"]] == "X" &
        base[["anios"]] %in% c("lactantes", "deambulantes", "s2"),
      "Inicial Maternal",
      ifelse(
        base[["inicial_infantes"]] == "X" &
          base[["anios"]] %in% c("s3", "s4", "s5"),
        "Inicial Infantes",
        NA
      )
    )
    
    # Primario
    base[["nivel"]] <- ifelse(
      base[["primaria6"]] == "X" &
        base[["anios"]] %in% c("1", "2", "3", "4", "5", "6"),
      "Primaria 6",
      ifelse(
        base[["primaria7"]] == "X" &
          base[["anios"]] %in% c("1", "2", "3", "4", "5", "6", "7"),
        "Primaria 7",
        base[["nivel"]]
      )
    )
    
    # Secundario CB: saco 20
    base[["nivel"]] <- ifelse(
      base[["secundaria_cb6"]] == "X" &
        base[["anios"]] %in% c("7", "8", "9", "10", "11", "12", "1314"),
      "Secundaria CB6",
      ifelse(
        base[["secundaria_cb7"]] == "X" &
          base[["anios"]] %in% c("7", "8", "9", "10", "11", "12", "1314"),
        "Secundaria CB7",
        base[["nivel"]]
      )
    )
    
    # Secundario Completa: saco 20
    base[["nivel"]] <- ifelse(
      base[["secundaria_completa6"]] == "X" &
        base[["anios"]] %in% c("7", "8", "9", "10", "11", "12", "1314"),
      "Secundaria Completa 6",
      ifelse(
        base[["secundaria_completa7"]] == "X" &
          base[["anios"]] %in% c("7", "8", "9", "10", "11", "12", "1314"),
        "Secundaria Completa 7",
        base[["nivel"]]
      )
    )
    
    # Secundario Orientada: saco 20
    base[["nivel"]] <-
      ifelse(
        base[["secundaria_orientada"]] == "X" &
          base[["anios"]] %in% c("7", "8", "9", "10", "11", "12", "1314"),
        "Secundaria Orientada",
        base[["nivel"]]
      )
    
    # SEC INET
    base[["nivel"]] <-
      ifelse(base[["nivel"]] %in% c(grep("Secundaria", unique(base[["nivel"]]), value = T)) &
               base[["sec_inet"]] == "X",
             paste0(base[["nivel"]], "/ Inet"),
             base[["nivel"]])
    
    # SNU
    base[["nivel"]] <-
      ifelse(base[["snu"]] == "X" &
               base[["anios"]] %in% c("snu"),
             "snu",
             base[["nivel"]])
    
    # SNU INET
    
    base[["nivel"]] <-
      ifelse(base[["nivel"]] == "snu" &
               base[["snu_inet"]] == "X",
             paste0(base[["nivel"]], "/ Inet"),
             base[["nivel"]])
    
    base <- subset(
      x = base,
      select = c("ID1",
                 "provincia",
                 "sector",
                 "ambito",
                 "anios",
                 "nivel"),
      subset = !is.na(nivel)
    )
    
    lista <- list()
    
    for (n in c("X_", "v_", "r_", "s_", "X_sec")) {
      print(n)
      
      if (n != "X_") {
        nombres <- grep(paste0("^", n),
                        names(ra_matricula),
                        value = TRUE)
        
      } else {
        nombres <- grep(
          paste0("^", n, "(?!sec).*$"),
          names(ra_matricula),
          value = TRUE,
          perl = T
        )
      }
      
      for (i in nombres) {
        base_aux <- ra_matricula[, c("ID1", i)]
        
        names(base_aux)[[2]] <- n
        
        if (n == "X_sec") {
          anio_t <-
            gsub(
              pattern = paste0("^", n, "*"),
              x = i,
              replacement = ""
            )
          
        } else{
          anio_t <-
            gsub(
              pattern = paste0("^", n, "*"),
              x = i,
              replacement = ""
            )
          
        }
        
        base_aux[["anios"]] <-
          ifelse(anio_t == "deambu", "deambulantes", anio_t)
        
        # print(unique(base_aux$anios))
        # base_aux[[paste0("original_", n)]] <- n
        
        lista[[i]] <- base_aux
      }
      
      final <- do.call(rbind, lista)
      
      rownames(final) <- NULL
      
      
      
      base <- merge(base,
                    final,
                    by = c("ID1", "anios")
                    ,
                    all.x = T)
      
      
      lista <- list()
      
      print(nrow(base))
      
      base[["periodo"]] <- paste0(a)
      
    }
    
    
    bases_unidas[[paste0("Base_", a)]] <- base
    
    rm(list = setdiff(
      ls(),
      c(
        "base",
        "bases_unidas",
        "anio",
        "file",
        "path",
        "path_bases"
      )
    ))
    
  }
  
  final <- do.call(rbind, bases_unidas)
  
  # Mover la columna B a la primera posición
  col_index <- match("periodo", names(final))
  final <- final[c(col_index, setdiff(seq_along(final), col_index))]
  
  rm(list = setdiff(ls(), c("final")))
  
  # 
  # resumen <- aggregate(cbind(final$X_, final$r_,),
  #                      list(final$anios, final$periodo),
  #                      sum,
  #                      na.rm = T)
  
  rownames(final) <- NULL
  
  assign("matricula", final, envir = .GlobalEnv) 
}

# Correr función 
pivotMatricula(anio_base = 2013, 
               file = "C:/Users/flavi/Desktop/Educacion/RA/")
