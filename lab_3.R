library(dplyr)
library(tidyr)

data(mtcars)
df <- as.data.frame(mtcars)

df <- df %>%
  select(mpg, cyl, hp, gear) %>%
  filter(cyl > 4)
print("Paso 2: Selección y filtrado de filas")
print(df)

df <- df %>%
  arrange(desc(hp)) %>%
  rename(consumo = mpg, potencia = hp)
print("Paso 3: Ordenación y renombrado de columnas")
print(df)

df <- df %>%
  mutate(eficiencia = consumo / potencia) %>%
  group_by(cyl) %>%
  summarise(
    consumo_medio = mean(consumo, na.rm = TRUE),
    potencia_maxima = max(potencia, na.rm = TRUE)
  )
print("Paso 4: Nueva columna y agregación de datos")
print(df)

transmision_df <- data.frame(
  gear = c(3, 4, 5),
  tipo_transmision = c("Manual", "Automática", "Semiautomática")
)

df <- df %>%
  left_join(transmision_df, by = "gear")
print("Paso 5: Unión de dataframes")
print(df)

long_df <- df %>%
  pivot_longer(
    cols = c(consumo_medio, potencia_maxima),
    names_to = "medida",
    values_to = "valor"
  )
print("Paso 6.1: Transformación a formato largo")
print(long_df)

wide_df <- long_df %>%
  group_by(cyl, gear, tipo_transmision, medida) %>%
  summarise(valor = mean(valor, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = medida,
    values_from = valor
  )
print("Paso 6.2: Transformación a formato ancho")
print(wide_df)

print("Verificación final: Dataframe en formato ancho")
print(wide_df)

