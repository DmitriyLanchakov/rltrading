Предполагаем сначала потренироваться на простой и всем известной системе: после открытия сессии на RI (утром) заходить в сторону импульса (если он есть). Заранее известно, что она работает, и имеет смысл. Используем минутные данные, внутрь минуты не заглядываем (пока). Формализуем эту систему, выделим факторы, и попробуем найти ее нейросетью. Заодно посмотрим, не обнаружится ли других систем в том же пространстве факторов.

Сначала возьмем ее упрощенный вариант: если 1-я минута > 500 пунктов, встать в ту же сторону на 15 минут.

Для иллюстрации, псевдо-код, работающий по открытиям свечей:

`delta = (Close[1] - Open[1])/Point_Size;
`if (Time==1001 && delta > 500) Buy (15);
`if (Time==1001 && delta < -500) Sell (15);

Пока получается два фактора:

1. время входа Time = 1001..2349

Есть два варианта его представления: линейный (сколько минут прошло с открытия) и в виде часа и минуты раздельно. Этот фактор не всегда плавно влияет на эквити. Кроме того, этот фактор уникален - является мощнейшим сепаратором отдельных систем, он не классифицируется "в лоб" на две группы, как большинство других факторов. Позже мы решим, как с ним лучше поступить.

2. размер 1-й минуты в пунктах (потом добавим нормирование)

3. и один параметр "время удержания", который не является классифицирующим, но тоже влияет на эквити.

Позже добавим другие факторы: диапазон предыдущего дня, направленность и длительность предыдущего движения и, возможно, еще что-нибудь.

Предполагается предсказывать состояние системы (в рынке, вне рынка) и знак последующего движения ( =направление сделки, бай-селл), вероятно, это будут делать две отдельные сети.


```python
%matplotlib inline
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from datetime import datetime, date, timedelta


# Читаем из файла
dateparse = lambda x: pd.datetime.strptime(x, '%m/%d/%y %H%M')
RI = pd.read_csv('RI.txt', parse_dates={'DT': ['Date', 'Time']}, date_parser=dateparse)
```


```python
# Отображаем исходные данные
plt.figure(figsize=(20,10))
plt.plot(RI.DT, RI.Close, label='RI')
legend = plt.legend(loc='upper center', shadow=True)
```


![png](output_2_0.png)



```python
# Строим таблицу факторов
R = pd.DataFrame(columns=('DT','Gap','Timeout','Profit'))

# Делим на два фактора
D = date(2000, 1,1)
for i in RI.index:
    dt = RI.DT[i].date() - D 
    if dt.days > 0:  # переход через день
        D = RI.DT[i].date()
        G = RI.Close[i] - RI.Open[i]

        for x in range(1,30):   # пробегаем все таймауты от 1 до 30 минут
            R1 = pd.Series({'DT':D, 'Gap':G, 'Timeout':x, 'Profit':np.sign(G) * (RI.Close[i+x] - RI.Close[i])})
            R = R.append(R1, ignore_index=True)

R.to_csv('Factors.csv')
R.head()
```




<div>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>DT</th>
      <th>Gap</th>
      <th>Timeout</th>
      <th>Profit</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>2006-01-10</td>
      <td>1000</td>
      <td>1</td>
      <td>255</td>
    </tr>
    <tr>
      <th>1</th>
      <td>2006-01-10</td>
      <td>1000</td>
      <td>2</td>
      <td>3000</td>
    </tr>
    <tr>
      <th>2</th>
      <td>2006-01-10</td>
      <td>1000</td>
      <td>3</td>
      <td>2000</td>
    </tr>
    <tr>
      <th>3</th>
      <td>2006-01-10</td>
      <td>1000</td>
      <td>4</td>
      <td>2000</td>
    </tr>
    <tr>
      <th>4</th>
      <td>2006-01-10</td>
      <td>1000</td>
      <td>5</td>
      <td>2000</td>
    </tr>
  </tbody>
</table>
</div>




```python
# Точечный график прибылей/убытков
plt.figure(figsize=(20,10))
plt.scatter(R.Timeout.add(np.random.normal(-0.15, 0.15, R.Timeout.count())), np.log(abs(R.Gap)), 
            c=R.Profit, s=np.log2(R.Profit)*6, edgecolors='none', alpha=0.5, cmap=plt.cm.Spectral)
```




    <matplotlib.collections.PathCollection at 0x2ad5e8d0>




![png](output_4_1.png)



```python
# Отображаем график эквити для гэпа 500 пп и таймаута 20 минут
R1 = R[(np.abs(R.Gap) > 500)&(R.Timeout==25)].reset_index()
R1.to_csv('sum.csv')
R1.head()
```




<div>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>index</th>
      <th>DT</th>
      <th>Gap</th>
      <th>Timeout</th>
      <th>Profit</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>24</td>
      <td>2006-01-10</td>
      <td>1000</td>
      <td>25</td>
      <td>1650</td>
    </tr>
    <tr>
      <th>1</th>
      <td>227</td>
      <td>2006-01-19</td>
      <td>-560</td>
      <td>25</td>
      <td>-800</td>
    </tr>
    <tr>
      <th>2</th>
      <td>256</td>
      <td>2006-01-20</td>
      <td>940</td>
      <td>25</td>
      <td>-60</td>
    </tr>
    <tr>
      <th>3</th>
      <td>285</td>
      <td>2006-01-23</td>
      <td>-900</td>
      <td>25</td>
      <td>-1575</td>
    </tr>
    <tr>
      <th>4</th>
      <td>314</td>
      <td>2006-01-24</td>
      <td>800</td>
      <td>25</td>
      <td>-360</td>
    </tr>
  </tbody>
</table>
</div>




```python
# Профит по отфильтрованному варианту
plt.figure(figsize=(20,10))
plt.plot(R1.DT, R1.Profit.cumsum())

# Суммарный профит по 30 контрактам (по одному на каждый таймаут)
plt.plot(R.DT, R.Profit.cumsum()/30, color='red')
```




    [<matplotlib.lines.Line2D at 0x2a9efa90>]




![png](output_6_1.png)



```python
# Готовим классификаторы
from sklearn.neighbors import KNeighborsClassifier
from sklearn.svm import SVC
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier, AdaBoostClassifier
from sklearn.preprocessing import StandardScaler

names = ["Nearest Neighbors", "RBF SVM", "Random Forest", "AdaBoost"]
classifiers = [KNeighborsClassifier(3),
    SVC(gamma=2, C=1),
    RandomForestClassifier(max_depth=5, n_estimators=10, max_features=1),
    AdaBoostClassifier()]
```


```python
Eq = pd.DataFrame(columns=['DT'])       # Структура с результатами всех методов классификации (колонки добавляются)

for year in range(2007, 2016):
    # Тренировочный набор - предыдущий год
    R_train = R[(R.DT > date(year - 1, 1, 1)) & (R.DT < date(year, 1, 1))].reset_index()
    X_train = StandardScaler().fit_transform(np.c_[R_train.Gap, R_train.Timeout])
    Y_train = np.sign(R_train.Profit)

    # Тестовый набор - текущий год
    R_test = R[(R.DT > date(year, 1, 1)) & (R.DT < date(year + 1, 1, 1))].reset_index()
    X_test = StandardScaler().fit_transform(np.c_[R_test.Gap, R_test.Timeout])
    Y_test = np.sign(R_test.Profit)

    # Перебираем все подключенные классификаторы
    E = pd.DataFrame({'DT': R_test.DT})
    for name, clf in zip(names, classifiers):
        clf.fit(X_train, Y_train)
        score = clf.score(X_test, Y_test)
        Y_test = clf.predict(X_test)
        E[name] = R_test.Profit * Y_test

    # Добавляем результаты в склееную таблицу
    Eq = Eq.append(E)
```


```python
# Отображаем все результаты
fig = plt.figure(figsize=(28,18))
for name in names:
    plt.plot(Eq.DT, Eq[name].cumsum()/30, label=name)

plt.plot(R.DT, R.Profit.cumsum()/30, label='average')
legend = plt.legend(loc='upper center', shadow=True)
```


![png](output_9_0.png)



```python

```
