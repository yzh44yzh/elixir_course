# MyMusicBand

Давайте соберём музыкальную группу и сыграем небольшой концерт :)

Группа (Band) состоит из трёх музыкантов:

- вокалист (Vocalist),
- гитарист (Guitarist),
- и барабанщик (Drummer).

У каждого музыканта своя партия, которая представлена списком звуков:

```elixir
sounds = [:BOOM, :Ts, :Doom, :Ts]
```

Музыкант изучает эту партию:

```elixir
{:ok, drummer} = Drummer.init(sounds)
```

И затем играет бесконечно по кругу:

```elixir
{:BOOM, drummer} = Drummer.next(drummer)
{:Ts, drummer} = Drummer.next(drummer)
{:Doom, drummer} = Drummer.next(drummer)
{:Ts, drummer} = Drummer.next(drummer)
{:BOOM, drummer} = Drummer.next(drummer)
{:Ts, drummer} = Drummer.next(drummer)
{:Doom, _drummer} = Drummer.next(drummer)
```

У каждого музыканта свой набор звуков, которые он может исполнить. Смотри `MyMusicBand.Model.Sound`. Если дать музыканту партию с другими звуками, то он не сможет её играть:

```elixir
sounds = [:BOOM, :Ts, :Doom, :BoBoom, :Ts, :Ts, :Woooom, :Ts]
{:error, [{4, :BoBoom}, {7, :Woooom}]} = Drummer.init(sounds)
```

То есть, функция инициализации должна проверить все звуки, и вернуть список неправильных звуков с указанием их позиции в списке. (Позиции считаются от 1 а не от 0).

Когда каждый музыкант готов играть свою партию:

```elixir
{:ok, vocalist} = Vocalist.init([:'A-a-a', :'O-o-o', ...])
{:ok, guitarist} = Guitarist.init([:A, :D, ...])
{:ok, drummer} = Drummer.init([:BOOM, :Ts, ...])
```

их можно собрать вместе:

```elixir
band = 
  Band.init()
  |> Band.add_player(vocalist)
  |> Band.add_player(guitarist)
  |> Band.add_player(drummer)
```

и играть все партии вместе:

```elixir
{[:'A-a-a', :A, :BOOM], band} = Band.next(band)
{[:'O-o-o', :D, :Ts], band} = Band.next(band)
```

При этом нужно учесть, что партии могут быть разной длины.

Вам нужно реализовать модули `Vocalist`, `Guitarist`, `Drummer` и `Band` так, чтобы они прошли тесты. Модуль `Sound` уже реализован, его менять не нужно (хотя можете добавить свои звуки при желании).

Если считаете нужным, можете добавить ещё какие-то модули.
