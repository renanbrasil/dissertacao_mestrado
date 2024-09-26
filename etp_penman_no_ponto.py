import pandas as pd
import numpy as np
#import matplotlib.pyplot as plt
import datetime as dt
import os
import math
from skyfield.api import Topos, load
from skyfield import almanac
from datetime import timedelta
from datetime import datetime
import calendar
from skyfield.api import load
import math
import requests
import json
#from google.cloud import bigquery
import time
import warnings
import xlsxwriter


#Função para calcular a radiação no topo da atmosfera (Q0)
def Radiacao_topo_atm(NDA, latitude):
    #Relação da distancia terra-sol
    d_D_2 = 1 + 0.033 * math.cos(math.radians((360/365 * NDA)))
    #Declinação solar
    declinacao= 23.45*math.sin(math.radians((360/365))*(NDA-80))
    #Ângulo do por do sol
    hn= math.degrees(math.acos(-math.tan(math.radians(latitude))*math.tan(math.radians(declinacao))))

    Qo= 37.6 * d_D_2 * (math.pi/180 * hn * math.sin(math.radians(latitude)) * math.sin(math.radians(declinacao)) + math.cos(math.radians(latitude)) * math.cos(math.radians(declinacao)) * math.sin(math.radians(hn)))
    return(d_D_2, declinacao,hn, Qo) 

#Função para calcular a radiação global (Qg)
def Qg(Q0, Tmax, Tmin):
    Qg = Q0 * 0.698 * (1 - math.exp(-0.014 * (Tmax-Tmin)**1.914))
    return(Qg)

#Função para calcular a temperatura no ponto de orvalho (TpO)
def TpO(Tm, RH):
    Td = Tm - ((100 - RH)/5)
    return(Td)

#Função para calcular o Saldo de Radiação ou Radiação Liquida (Rn) e Evapotranspiração por Penman-Monteith

def Rn_ETP_2(Tm,altitude,Q0,Tmax,Tmin,Ur,vvm):
    albedo= 0.23
    # umidade corrigida
    es = 0.6108 * 10**((7.5 * Tm)/(Tm + 237.3))
    # ea
    ea= Ur * es/100
    #dpv
    dpv = es-ea
    Qghs = 0.16 * np.sqrt(Tmax-Tmin) * Q0
    Qgbc = Q0 * 0.698 * (1 - math.exp(-0.014 * (Tmax-Tmin)**1.914))
    #Radiação no topo da atmosfera corrigida ( Qocor)
    Qgcs = (0.75 +(2*10**-5)* altitude )* Q0
    # balanço de ondas longas
    BOL = -((4.903 * 10**(-9)) * ((Tmax + 273.15)**4 + (Tmin + 273.15)**4)/2 * (0.34 - 0.14 * np.sqrt(ea)) * (1.35 * (Qgbc/Qgcs) - 0.35))
    BOC = Qgbc * (1-albedo)
    SR = BOC + BOL
    G = SR * 0.05
    P = 101.3 * ((293 - 0.0065 * altitude)/293)**5.26
    y = 0.665 * (10**-3) * P
    S = 4098 * (0.6108 * np.exp((17.27 * Tm)/(Tm + 237.3)))/(Tm + 237.3)**2
    ETP = (0.408 * S * (SR - G) + (y * 900/(Tm + 273) * vvm * dpv))/(S + (y * (1 + 0.34 * vvm)))

    return(es,ea,dpv,Qghs,Qgbc,Qgcs,BOL,BOC,SR,G,P,y,S,ETP)

#Função para calcular o Saldo de Radiação ou Radiação Liquida (Rn) e Evapotranspiração por Penman-Monteith
def Rn_ETP(TpO,altitude,Q0,Qg,Tmax,Tmin,Tm,Ur,vvm):
    albedo= 0.23
    # umidade corrigida
    es = 0.611 * np.exp((17.27 * TpO)/(TpO + 237.3))

    #Radiação no topo da atmosfera corrigida ( Qocor)
    Qocor = 0.75 + 0.00002 * altitude * Q0
    # Radiação liquida ajustadar por Allen 1994
    Rn = (Qg*(1-albedo))-(1.35*(Qg/Qocor)-0.35)*(0.35-0.14*np.sqrt(es))*(5.67*10**(-8))*((Tmax**4+Tmin**4)/2)

    # New es
    espm=0.6108*10**((7.5*Tm)/(237.3+Tm))
    # s
    s=4098*espm/(Tm+237.3)**2
    # ea
    ea=espm*Ur/100
    numerador=((0.408*s*(Rn-0.5))+((0.063*vvm)*(espm-ea))/(Tm+275))
    denominador=s+0.063*(1+0.34*vvm)
    ETP=numerador/denominador
    return(Qocor,Rn,ETP)


def n_horas (NDA, LAT):
    SIG = 23.45 * math.sin(math.radians((360/365)*(NDA-81)))
    HN = math.acos(-math.tan(math.radians(LAT)) * math.tan(math.radians(SIG))) * 180 / math.pi
    NHORA = 2 * HN / 15
    return NHORA


def NDA (mes):
    dias_meses = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    total = 0

    for i in range(mes - 1):
        total += dias_meses[i]

    return total + 1 


# OBTENDO O ARQUIVO POSIÇÕES E VOLOCIDADES DOS PLANETAS E OUTROS CORPOS CELESTES
def save_ephemeris_to_disk(filename):
    load(filename)

save_ephemeris_to_disk('de421.bsp')
planets = load('D:/OneDrive - Suzano S A/Documents/Padrão - Renan/mestrado/PROJETO/de421.bsp')

# INDICE ANUAL DE CALOR 
def wb_i(n_meses, tmed):
    calculated_temps = round(n_meses*(0.2 *tmed)**1.514, 3)
    return calculated_temps

# INDICE ANUAL DE CALOR 
# def wb_i( tmed):
#     calculated_temps = round(12*(0.2 *tmed)**1.514, 3)
#     return calculated_temps

# COEFICIENTE DE AJUSTE DO INDICE DE CALOR
def wb_a(i):
    calculated_i = 0.49239 + (0.018) * i - (7.71 * (10 ** -5)) * (i ** 2) + (6.75 * (10 ** -7)) * (i ** 3)
    return round(calculated_i, 3)


# # MEDIA DO NUMERO DE HORAS MENSAL 
def n_hours(date, lat, lon):
    ts = load.timescale()
    planets = load('de421.bsp')
    earth = planets['earth']
    t0 = ts.utc(date.year, date.month, date.day)
    t1 = ts.utc(date.year, date.month, date.day + 1)
    t, y = almanac.find_discrete(t0, t1, almanac.sunrise_sunset(planets, Topos(latitude_degrees=lat, longitude_degrees=lon)))
    day = 0.0
    for yi, ti in zip(y, t):
        if yi:
            start = ti.utc_datetime()
        else:
            end = ti.utc_datetime()
            day += (end - start).total_seconds()
    return round(day / 3600, 1)

# NUMERO DE DIAS DO MES
def n_days(date):
    year = date.year
    month = date.day
    _, num_days = calendar.monthrange(year, month)
    return num_days
#data = pd.to_datetime('2023-01-04')
#n_days(data)

# CALCULO DE EVAPOTRANSPIRAÇÃO PELO METODO DE THORNTHWAITE
def etp(t_med,  n_hours, n_days,i, a):

    # Se a temperatura média é menor que 26.5
    if t_med < 26.5:
        etp_value = round(16 * ((10 * (abs(t_med) / i)) ** a) * (n_hours / 12) * (n_days / 30), 2)
    # Se a temperatura média é maior ou igual a 26.5
    else:
        etp_value = round((-415.85 + 32.24 * t_med - 0.43 * t_med ** 2) * (n_hours / 12) * (n_days / 30), 2)
    return etp_value


def balac_hidri(bhdados):
    
    P=bhdados['PRECTOTCORR'] # precipitação
    ETP=bhdados['etp'] # evapotranspiração
    BH = pd.DataFrame({'ANO':bhdados['ano'],'MES':bhdados['mes'],'DAY':bhdados['dia'],'P':P, 'ETP':ETP})
    BH['CAD']=bhdados[['CAD']]
    BH['P-ETP']=P-ETP
    BH['NAC']=np.nan
    BH['ARM']=np.nan
    BH['ID']=bhdados['ID']
    for dia in range(0,len(BH)):

        if dia==0:
            NACant=0
            ARMant=BH.loc[dia,'CAD']
        # Recomecando o calculo do ARM
        elif dia >= 2:
            if np.isnan(BH.loc[dia-2,'ARM'])==True:
                NACant=0
                ARMant=BH.loc[dia,'CAD']
            else:
                pass
        else:
            NACant=BH.loc[dia-1,'NAC']
            ARMant=BH.loc[dia-1,'ARM']
    
        if BH.loc[dia,'P-ETP']<0:
            BH.loc[dia,'NAC']=NACant+BH.loc[dia,'P-ETP']
            BH.loc[dia,'ARM']=BH.loc[dia,'CAD']*math.exp(BH.loc[dia,'NAC']/BH.loc[dia,'CAD'])
            if BH.loc[dia,'ARM']>BH.loc[dia,'CAD']:
                BH.loc[dia,'ARM']=BH.loc[dia,'CAD']
            
        if BH.loc[dia,'P-ETP']>=0:
            BH.loc[dia,'ARM']=BH.loc[dia,'P-ETP']+ARMant
            if BH.loc[dia,'ARM']>BH.loc[dia,'CAD']:
                BH.loc[dia,'ARM']=BH.loc[dia,'CAD']
            BH.loc[dia,'NAC']=BH.loc[dia,'CAD']*np.log(BH.loc[dia,'ARM']/BH.loc[dia,'CAD'])

        BH.loc[dia,'ALT']=BH.loc[dia,'ARM']-ARMant
    
        if BH.loc[dia,'ALT']<0:
            BH.loc[dia,'ETR']=BH.loc[dia,'P']+abs(BH.loc[dia,'ALT'])
        else:
            BH.loc[dia,'ETR']=BH.loc[dia,'ETP']

        BH.loc[dia,'DEF']=BH.loc[dia,'ETP']-BH.loc[dia,'ETR']
    
        if BH.loc[dia,'ARM']<BH.loc[dia,'CAD']:
            BH.loc[dia,'EXC']=0
        else:
            BH.loc[dia,'EXC']=BH.loc[dia,'P-ETP']-BH.loc[dia,'ALT']
    return round(BH,2)

dados_climaticos = pd.read_csv('D:/OneDrive - Suzano S A/Documents/Padrão - Renan/mestrado/PROJETO/output/parte2_select_dados_nasa_parcela_fazenda.csv')

key = dados_climaticos['fazenda'].unique().tolist()
#key = key[:10]
#len(key)

df_balanco_PENMAN =[]

for i in key:
    dados_aux = dados_climaticos[dados_climaticos['fazenda'] == i].reset_index(drop = True)

    dados_aux.sort_values(by=['YEAR', 'MM', 'DD'], inplace=True)
    dados_aux.reset_index(drop=True, inplace=True)
    dados_aux.rename(columns={'YEAR':'ano', 'MM':'mes', 'DD':'dia'}, inplace=True)
    dados_aux['ID'] = dados_aux['fazenda']
    dados_aux['CAD'] = dados_aux['cad']
    dados_aux['Altitude'] = dados_aux['altitude']
    dados_aux['Data'] = pd.to_datetime(dados_aux['YYYYMMDD'], format='%Y-%m-%d')
    dados_aux['Data'] = dados_aux['Data'].apply(lambda x: x.to_pydatetime())
    dados_aux['ano'] = dados_aux['Data'].apply(lambda x: x.year)
    dados_aux['mes'] = dados_aux['Data'].apply(lambda x: x.month)
    dados_aux['dia'] = dados_aux['Data'].apply(lambda x: x.day)
    #dados_climaticos = dados_climaticos[dados_climaticos['ano'] == 2023].reset_index(drop=True)
    dados_aux.sort_values(by=['ano', 'mes', 'dia'], inplace=True)
    dados_aux.reset_index(drop=True, inplace=True)

    # gerando df auxiliar anual 
    auxiliar_imp = dados_aux.copy()
    auxiliar_imp = auxiliar_imp.groupby(['ano']).agg({'T2M':'mean', 'mes':'count','dia':'count'}).reset_index()
    auxiliar_imp = auxiliar_imp.rename(columns={'T2M':'tmed_anual', 'mes':'n_meses', 'dia':'n_dias'})
    dados_aux = pd.merge(dados_aux, auxiliar_imp, on=['ano'])

    dados_aux['Tpo']= dados_aux.apply(lambda row: TpO(row['T2M'],row['RH2M']), axis=1)
    dados_aux['Qo'] = dados_aux.apply(lambda row: Radiacao_topo_atm(row['DOY'],row['LAT'])[3], axis=1)
    dados_aux['Qg'] = dados_aux.apply(lambda row: Qg(row['Qo'],row['T2M_MAX'],row['T2M_MIN']), axis=1)
    dados_aux['SR'] = dados_aux.apply(lambda row: Rn_ETP_2(row['T2M'],row['Altitude'],row['Qo'],row['T2M_MAX'],row['T2M_MIN'],row['RH2M'],row['WS2M'])[8], axis=1)
    # EVAPOTRANSPIRAÇÃO CALCULADA PELO METODO DE PENMAN-MONTEITH
    dados_aux['etp'] = dados_aux.apply(lambda row: Rn_ETP_2(row['T2M'],row['Altitude'],row['Qo'],row['T2M_MAX'],row['T2M_MIN'],row['RH2M'],row['WS2M'])[13], axis=1)

    balanco = balac_hidri(dados_aux[['ID', 'Data', 'CAD', 'ano', 'mes','dia', 'PRECTOTCORR', 'etp']])
    df_balanco_PENMAN.append(balanco)
    #else:
    print('Interacao: ', i,'concluída')

RESULT_PENMAN = pd.concat(df_balanco_PENMAN)

with pd.ExcelWriter('D:/OneDrive - Suzano S A/Documents/Padrão - Renan/mestrado/PROJETO/output/parte2_select_penman_dados_nasa_parcela_fazenda.xlsx') as writer:
    RESULT_PENMAN.to_excel(writer, sheet_name='dados')
    writer.close()


    # df_balanco_PENMAN =[]
    # for id in dados_climaticos['ID'].unique():
    # dfcs = dados_climaticos[dados_climaticos['ID'] == id].reset_index()
    # balanco = balac_hidri(dfcs[['ID', 'Data', 'CAD', 'ano', 'mes','dia', 'PRECTOTCORR', 'etp']])
    # df_balanco_PENMAN.append(balanco)
    # #else:
    # print('Estação: ', id,'concluída')
    # type(df_balanco_PENMAN)
    # RESULT_PENMAN = pd.concat([dados_climaticos, df_balanco_PENMAN[0]], axis=1)
    # RESULT_PENMAN.rename(columns={'etp':'ETP_PENMAN'}, inplace=True)
    # RESULT_PENMAN.rename(columns={'ARM':'ARM_MM_PENMAN', 
    #                                     'ALT':'ALT_MM_PENMAN', 
    #                                     'ETR':'ETR_MM_PENMAN', 
    #                                     'DEF':'DEF_MM_PENMAN',
    #                                     'EXC':'EXC_MM_PENMAN'}, inplace=True)






# calculando numero de dias juliano

#data = pd.to_datetime('2023-02-04')

#jd = data.timetuple().tm_yday