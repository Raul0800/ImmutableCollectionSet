# ImmutableCollectionSet


Реализована неизменяемая структура данных `Set` с двумя методами:

1) Метод для добавления элемента в множество. Метод принимает новый элемент множества и возвращает новое множество, содержащее данный элемент.

2) Метод преобразования множества в список. Результат работы метода - объект типа `scala.List` который содержит все элементы из множества.

Выполнены дополнительные условия:
1) Оба метода реализованы с помощью рекурентных алгоритмов

2) Множество параметризовано типом элементов, которые могут содержаться в множестве

3) Сложность добавления элемента меньше O(n)

4) Написаны модульные тесты, проверяющие корректность реализации и написаны property-base тесты

Пример запуска либо в Main.scala, либо в тестах