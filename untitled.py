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
# histograma dos dados

%matplotlib inline #somente Jupyter notebook
import matplotlib.pyplot as plt #importa a biblioteca
housing.hist(bins=50, figsize=(20,15)) # bins defini o número de barras do histograma
# figsize define o tamanho total da figura 
plt.show()

# Criando conjunto de testes 

# Com o intuito de evitar o data snooping bias [viés data snooping] se separa
# os dados em um conjunto de treino e outro de testes. 
# No geral 20% ou 25% são separados para teste e o restante para treinar o modelo

#### Conjunto de testes

import numpy as np

def split_train_test(data, test_ratio):
  shuffled_indices = np.random.permutation(len(data))
  test_set_size = int(len(data)*test_ratio)
  test_indices = shuffled_indices[:test_set_size]
  train_indices = shuffled_indices[test_set_size:]
  return data.iloc[train_indices], data.iloc[test_indices]

train_set, test_set = split_train_test(housing, 0.2)
len(train_set)
len(test_set)


# Importante

from zlib import crc32

def test_set_check(identifier, test_ratio):
  return crc32(np.int64(identifier)) & 0xffffffff < test_ratio*2**32

def split_train_test_by_id(data, test_ratio, id_column):
  ids = data[id_column]
  in_test_set = ids.apply(lambda id_: test_set_check(id_, test_ratio))
  return data.loc[~in_test_set], data.loc[in_test_set]

# O conjunto de dados dos imóveis não tem uma coluna de identificação
# A solução símples é utilizar o índice dalinha como ID

housing_with_id = housing.reset_index() # Adiciona um index à coluna
train_set, test_set = split_train_test_by_id(housing_with_id, 0.2, "id")

# Durante o procesos de atualização dos dados, utilizar um índice de uma 
# linha como identificador pode ser arriscado, pois existe a possibilidade 
# de alguma linha ser excluída e ainda que os novos dados sejam anexados 
# ao final do conjunto de teste. Uma ideia é utiliar características que 
# não mudam ao longo do tempo, como por exemplo, latitude e longitude, que
# certamente serão estáveis ao longo de milhares de anos.

housing_with_id["id"] = housing["longitude"]*1000 + housing["latitude"]
# O motivo da multiplicação por 1000 é diferir valores de longitude e latitude
# de modo que valores próximos não tenham o mesmo id.

train_set, test_set = split_train_test_by_id(housing_with_id, 0.2, "id")
# Utilizar a função by_id garante que uma mesma casa sempre cairá no mesmo 
# conjunto (treino ou teste), não importa quantas vezes você rode o código 
# ou adicione novos dados.

# Funções importantes do Scikite-Learn

# train_test_split() faz o mesmo que a função split_train_test()
# random_state possibilita definir o gerador de sementes aleatórias

from sklearn.model_selection import train_test_split

train_set, test_set = train_test_split(housing, test_size = 0.2, random_state = 42)

housing["income_cat"] = pd.cut(housing["median_income"],
                                bins = [0., 1.5, 3.0, 4.5, 6., np.inf],
                                labels = [1, 2, 3, 4, 5])

housing["income_cat"].hist()


