# haskell-asyncplay

Do some async crap nicely

04:55 < Qgosh> Ashy: i'd like to know how you'd go 'here are 500 lazy objects' 'i can only resolve them 8 at a time, or i/o disaster' 'now i want to get 800 different objects with the reduce of that last lot' in a clean manner in haskell
04:56 < Qgosh> the issue is resource consumption logic for i/o
04:56 < Qgosh> when its all async
04:56 < Qgosh> becomes a mess


## Running
```
$ ./createdatabase.sh
$ stack build
$ stack exec haskell-asyncplay-exe
```
