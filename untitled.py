# Criando um projeto inicial do zero utilizando python 

# Para instalar bibliotecas utilize o comando a seguir no terminal
# pip install nomedabiblioteca
# para chamar o comando no código, utilize
# Importa a biblioteca "matplotlib" para usar no seu código
# import matplotlib.pyplot as plt

#Dowload dos dados

import os
import tarfile
import urllib.request
import pandas as pd

DOWNLOAD_ROOT = "https://raw.githubusercontent.com/ageron/handson-ml2/master/"
HOUSING_PATH = os.path.join("datasets", "housing")
HOUSING_URL = DOWNLOAD_ROOT + "datasets/housing/housing.tgz"

def fetch_housing_data(housing_url=HOUSING_URL, housing_path=HOUSING_PATH):
    os.makedirs(housing_path, exist_ok=True)
    tgz_path = os.path.join(housing_path, "housing.tgz")
    urllib.request.urlretrieve(housing_url, tgz_path)
    housing_tgz = tarfile.open(tgz_path)
    housing_tgz.extractall(path=housing_path)
    housing_tgz.close()

def load_housing_data(housing_path=HOUSING_PATH):
    csv_path = os.path.join(housing_path, "housing.csv")
    return pd.read_csv(csv_path)

# chama a função para baixar e extrair os dados.
fetch_housing_data()

# carrega os dados em um DataFrame.
housing = load_housing_data()
# Uma rápida olhada nos dados 
housing.head()

# O comando info() é interessante para analisaruma rápida descrição dos dados
housing.info()

# O comando describe() também é útil nessa abordagem inicial
housing.describe()

# Outra maneira rápida de se ter uma ideia do tipo de dados é através do 
# hstograma dos dados

%matplotlib inline #somente Jupyter notebook
import matplotlib.pyplot as plt #importa a biblioteca
housing.hist(bins=50, figsize=(20,15)) # bins defini o número de barras do histograma
# figsize define o tamanho total da figura 
plt.show()
