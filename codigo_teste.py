import pandas as pd
import glob
import sys

import support_functions as sf

from multiprocessing import Pool

def process_file(filename, nrows = None):
    #Lê o arquivo
    df = sf.read_sales_data(filename, nrows = nrows)
    
    #Calcula o preço unitário
    df['unit_price'] = my_map(df, lambda row: [pr / qt for pr, qt in zip(row.price, row.quantity)])
    
    #Encontra o preço máximo
    df['max_unit_price'] = my_map(df, lambda row: max(row['unit_price']))
    
    #Encontra o indice
    df['idx_max_unit_price'] = my_map(df, lambda row: row['unit_price'].index(max(row['unit_price'])))
    
    #função de redução
    fun = lambda x: pd.DataFrame.nlargest(x, columns = 'max_unit_price', n = 5)
    
    #aplica aim redução
    df01 = my_reduce(df, None, fun)
    
    return df01

if __name__ == '__main)__':
    num_proc = int(sys.argv[1])
    
    file_list = glob.glob('sales_data_part_*.csv')
    
    #vamos instanciar um pool de processos
    #como eu abro um arquivo txt no python? Eu coloco um with, pra garantir que o processo será fechado ao final.
    
    with Pool(num_proc) as p: 
        list_results = p.map(process_file, file_list)
        
#que que a gente tá fazendo? 
#dentro de p, ele vai instanciar numproc processos, pra cada um ele vai alocar um elemento dessa lista de arquivos. O process file toma o nome do arquivo, executa e gera um resultado.
#Cada resultado é um item da redução

#Agora eu faço o reduce de dois em dois

fun = lambda x: pd.DataFrame.nlargest(x,columns = "max_unit_price", n = 5 )

df = list_results[0]

for dfi in list_results[1:]:
    df = sf.my_reduce(df,dfi,fun)
    
    df.to_csv('results_csv')