import pandas as pd

# Esse algoritmo tem o intuito de reunir todas as regiões metropolitanas de um arquivo csv para determinar a que regiao cada municipio pertence, gerando uma lista de todos os municipios presentes no arquivo separados pela região de onde vem

csv = "MICRODADOS.csv"

df = pd.read_csv(csv, sep=';')
df = df.drop_duplicates()

regioes_agrupadas = df.groupby("RegiaoMetropolitana")["MUN"].apply(lambda municipios: sorted(set(municipios)))

for regiao, municipios in regioes_agrupadas.items():
    print(f"{regiao}:")
    for municipio in municipios:
        print(f"{municipio}")