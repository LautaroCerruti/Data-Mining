import re
import csv

# Lee el archivo de entrada
with open('results_svmp_parallel.txt', 'r') as f:
    lines = f.readlines()

# Lista para almacenar los datos procesados
data = []

# Procesa cada línea
for line in lines:
    match = re.search(r'cost (\S+) - gamma (\S+) - degree (\d+) - .* - error (\S+)', line)
    if match:
        cost = float(match.group(1))  # Convertir a número flotante
        gamma = float(match.group(2))  # Convertir a número flotante
        degree = int(match.group(3))  # Convertir a entero
        error = float(match.group(4))  # Convertir a número flotante
        data.append([cost, gamma, degree, error])

data.sort(key=lambda x: (x[0], x[1], x[2]))

# Escribe los datos en un archivo CSV
with open('salida.csv', 'w', newline='') as f:
    writer = csv.writer(f)
    writer.writerow(['cost', 'gamma', 'degree', 'error'])  # Escribir encabezados
    writer.writerows(data)

print("Archivo CSV generado como 'salida.csv'")