# educDatos

**Objetivo**

Facilitar el acercamiento y el procesamiento de datos educativos con R de Argentina.

## Funcionalidades

##### Descargar tablas del Relevamiento Anual

```{r}
descargaRA(
  base = c(2013,2023),
  file_path = "")
```

-   *base:* especificar el año de descarga (disponibles desde 2013 a 2023)

-   *file_path:* especifcar el directorio de trabajo para efectuar la descarga

##### Pivotear tabla con matrícula, repitencia y secciones

```{r}
pivotMatricula(
  anio_base = 2013, 
  file = "")
```

-   *anio_base:* especificar el año de la tabla

-   *file:* especifcar el directorio de trabajo de la tabla
