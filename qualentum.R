# Paso 1: Configuración inicial

# Vectores de datos
energia <- c(rep("Renovable", 10), rep("No Renovable", 10))  # 10 valores para cada tipo de energía
consumo <- c(50, 45, 60, NA, 52, 55, 70, 65, NA, 60, 
             80, 75, 85, 72, 78, 65, 90, 85, 82, 88)  # Consumo diario en kWh, algunos valores NA
costo_kwh <- c(rep(0.12, 10), rep(0.15, 10))  # Costo por kWh para cada tipo de energía

# Paso 2: Limpieza de datos

# Reemplazar los valores faltantes (NA) con la mediana de consumo según tipo de energía
consumo[is.na(consumo) & energia == "Renovable"] <- median(consumo[energia == "Renovable"], na.rm = TRUE)
consumo[is.na(consumo) & energia == "No Renovable"] <- median(consumo[energia == "No Renovable"], na.rm = TRUE)

# Paso 3: Creación del dataframe
df_consumo <- data.frame(energia = energia, consumo = consumo, costo_kwh = costo_kwh)

# Paso 4: Cálculos
# Calcular el costo total por día (consumo * costo por kWh)
df_consumo$costo_total <- df_consumo$consumo * df_consumo$costo_kwh

# Calcular el total de consumo y el costo total por cada tipo de energía
total_consumo <- tapply(df_consumo$consumo, df_consumo$energia, sum)
total_costo <- tapply(df_consumo$costo_total, df_consumo$energia, sum)

# Calcular la media del consumo diario para cada tipo de energía
media_consumo <- tapply(df_consumo$consumo, df_consumo$energia, mean)

# Simular un aumento del 10% en el costo
df_consumo$ganancia <- df_consumo$costo_total * 1.1

# Paso 5: Resumen

# Ordenar el dataframe por la columna costo_total de manera descendente
df_ordenado <- df_consumo[order(-df_consumo$costo_total), ]

# Extraer las tres filas con el mayor costo_total
top_3_costos <- head(df_ordenado, 3)

# Crear la lista resumen_energia
resumen_energia <- list(
  df_ordenado = df_ordenado,  # Dataframe ordenado
  total_consumo = total_consumo,  # Total de consumo por tipo de energía
  total_costo = total_costo,  # Total de costo por tipo de energía
  top_3_costos = top_3_costos  # Top 3 filas con mayor costo total
)

# Mostrar el resumen final
resumen_energia
