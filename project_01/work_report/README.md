# WorkReport -- курсовая работа

Мы прошли весь курс, и пришло время для курсовой работы. Это будет полноценный проект, результатом которого будет полезная программа (скрипт). Я сам пользуюсь этим скриптом, может и вам пригодится.

## Суть проекта

Всем хорошо знакома такая простая вещь, как TODO-list: список задач. Он может быть простым текстовым файлом, а может быть сложной системой, координирующей работу больших комманд, как, например JIRA.

Мы будем делать похожую вещь -- DONE-list. То есть список задач, которые уже сделаны. Зачем нужен такой список? Чтобы формировать по нему отчеты и анализировать нашу продуктивность.

Да, эту функцию может выполнять та же JIRA. Но это слишком громоздкий инструмент для индивидуального использования. Попробуем другой подоход.

Я делаю DONE-list, или report, в текстовом файле в формате markdown. Выглядит это так:

```markdown
# May

## 3 mon
[DEV] Review Pull Requests - 27m
[COMM] Daily Meeting - 15m
[DEV] Implement filters for Logstash - 39m
[DEV] Implement filters for Logstash - 57m
[OPS] Setup Elasticsearch - 39m
[OPS] Setup Kibana - 17m
[DEV] Learn how to search for logs in Kibana - 34m
[DOC] Write a document about logs in ELK - 22m

## 4 tue
[COMM] Daily Meeting - 31m
[DOC] TASK-42 Read BA documents - 15m
[DOC] Read Service T API documents - 19m
[COMM] Sprint Review & Retro - 1h 35m
[DOC] Read Service T API documents - 11m
[DEV] TASK-42 Implement feature - 46m
[DEV] TASK-42 Test - 22m
[DEV] TASK 42 Fix and test - 37m

## 5 wed
[DEV] Review Pull Requests - 39m
[COMM] Daily Meeting - 16m
[DEV] Task-43 Impletent feature - 31m
[DEV] Task-43 Impletent feature - 54m
[DEV] Task-43 write tests - 38m
[DOC] Task-44 read BA documents - 32m
[COMM] Task-44 discuss with BA - 21m
[DEV] Task-44 Implement - 52m
```

Чтобы собрать статистику, нужно распарсить этот файл. В этом и состоит наш проект.

Проект реализует консольную утилиту, которая на вход получает файл report.md, а на выходе отдает статистику по нему.

Пример:

```shell
$ work_report -m 5 -d 3 test/sample/report-1.md
Day: 3 mon
 - DEV: Review Pull Requests - 27m
 - COMM: Daily Meeting - 15m
 - DEV: Implement filters for Logstash - 39m
 - DEV: Implement filters for Logstash - 57m
 - OPS: Setup Elasticsearch - 39m
 - OPS: Setup Kibana - 17m
 - DEV: Learn how to search for logs in Kibana - 34m
 - DOC: Write a document about logs in ELK - 22m
   Total: 4h 10m

Month: May
 - COMM: 2h 58m
 - DEV: 7h 56m
 - DOC: 1h 39m
 - EDU: 0
 - OPS: 56m
 - WS: 0
   Total: 13h 29m, Days: 3, Avg: 4h 29m
```

Статистика включает:
- суммарное время за день;
- суммарное время за месяц;
- время по каждой категории за месяц;
- количество отработанных дней;
- среднее время за день.

## Формат файла

На верхнем уровне иерархии указаны месяцы (год не поддерживается, для этого можно просто иметь разные файлы):

```
# March
...
# April
...
```

Для каждого месяца указаны дни:

```
# April

## 15 thu
...
## 16 fri
...
```

Для каждого дня указаны задачи:

```
## 16 fri
[DEV] TASK-20 implementation - 17m
[COMM] Daily Meeting - 22m
[DEV] TASK-19 investigate bug - 43m
...
```

Задача представлена категорией, описанием и затраченым временем.

Категории не могут быть произвольными, они представлены фиксированным списком:
- COMM -- коммуникации, рабочее общение (митинги, почта, мессендеры и пр);
- DEV -- разработка и тестирование;
- OPS -- оперирование (стейджинг и прод) серверов;
- DOC -- документация (чтение и написание);
- WS -- workspace, настройка рабочего окружения;
- EDU -- обучения (себя и других).

Время представлено в часах и минутах: 43m, 1h 39m, 4h 29m. Других вариантов (секунды, дни и пр) нет.

## Использование скрипта

Тут все довольно просто:

```shell
$ work_report --help
USAGE:
    work_report [OPTIONS] <path/to/report.md>
OPTIONS:
    -m, --month <M>  Show report for month (int), current month by default
    -d, --day <D>    Show report for day (int), current day by default
    -v, --version    Show version
    -h, --help       Show this help message
```

Примеры:

```shell
$ work_report test/sample/report-1.md
$ work_report -m 5 test/sample/report-1.md
$ work_report -d 3 test/sample/report-1.md
$ work_report -m 5 -d 3 test/sample/report-1.md
$ work_report --month=5 --day=3 test/sample/report-1.md
$ work_report --version
$ work_report --help
```

## Что нужно сделать

Я рекомендую начать с моделирования предметной области. 

Вам нужно решить, какими сущностями должна быть представлена предметная область, и из чего эти сущности состоят. Это могут быть, например: "задача", "время выполнения", "категория", "отчет за день", "отчет за месяц". Тут все на ваше усмотрение.

Затем нужно реализовать парсер, который на вход принимает текстовый файл, а на выходе данные, описаные в рамках модели. Например, строка:

```markdown
[DEV] TASK-42 Implement feature - 46m
```

должна превратиться в объект "задача", а группа строк:

```markdown
## 4 tue
[COMM] Daily Meeting - 31m
[DOC] TASK-42 Read BA documents - 15m
[DOC] Read Service T API documents - 19m
[COMM] Sprint Review & Retro - 1h 35m
[DOC] Read Service T API documents - 11m
[DEV] TASK-42 Implement feature - 46m
[DEV] TASK-42 Test - 22m
[DEV] TASK 42 Fix and test - 37m
```

должна превратиться в объект "отчет за день".

Затем вам нужно реализовать запросы к вашим данным, сформировать и вывести на консоль отчеты за день и за месяц.

Вы начинаете проект не с нуля, часть кода уже написана. Это код, не имеющий прямого отношения к задаче, а выполняющий вспомогательные функции: парсинг и обработка входящих аргументов, вывод справки и др.

При желании вы можете этот код удалить и сделать проект полностью с нуля. Тогда вам нужно будет немного поправить имеющиеся юнит-тесты, так как они делают предположения о наличии в проекте модулей и функций с конкретными названиями. Интеграционные тесты исправлять не придется, они тестируют программу снаружи, как черный ящик.

## Тестирование проекта

Обычный способ сборки Эликсир проекта, это релиз (release):

```shell
mix release
```

При этом создается системный сервис, предназначеный для постоянной работы в фоновом режиме.

В нашем случае этот способ не подходит. Мы соберем консольный скрипт (escript):

```shell
mix escript.build
```

При этом создается один бинарный файл, который включает в себя байткод всех модулей проекта, всех зависимостей, конфиги и другие ресурсы. Этот файл можно запускать на любой машине, где установлен Эрланг. (Эликсир не обязательно должен быть установлен, нужные модули Эликсир добавляются в бинарный файл).

Для запуска тестов нужно вызывать:

```shell
mix test
```

В проекте есть юнит-тесты и интеграционные тесты.

Юнит-тесты `test/parser_test.exs` и `test/formatter_test.exs`. Они проверяют парсинг и форматирование времени.

```elixir
  test "parse time" do
    assert Parser.parse_time("1m") == 1
    assert Parser.parse_time("5m") == 5
    assert Parser.parse_time("12m") == 12
    ...
    assert Parser.parse_time("1h 90m") == 150
    assert Parser.parse_time("3h") == 180
    ...

  test "format time" do
    assert Formatter.format_time(0) == "0"
    assert Formatter.format_time(1) == "1m"
    assert Formatter.format_time(12) == "12m"
    ...
    assert Formatter.format_time(90) == "1h 30m"
    assert Formatter.format_time(140) == "2h 20m"
    ...
```

Эти тесты подразумевают, что в проекте есть модули `WorkReport.Parser` и `WorkReport.Formatter` с соответствующими функциями. Вам не обязательно делать именно так, вы можете создавать любые модули и функции. Просто нужно будет поправить эти тесты.

Юнит-тесты добавлены скорее для примера. Разумеется, этого мало, стоит добавить больше тестов.

Интеграционные тесты `test/integration_test.exs` не делают никаких предположений о внутреннем устройстве проекта. Они запускают скрипт с определенными аргументами и сравнивают полученый ответ с ожидаемым. В папке `test/sample` лежат примеры report-файлов и файлы с ожидаемыми ответами.
