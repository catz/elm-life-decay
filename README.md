### Elm Life Decay

Implementation of [Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) but instead of dying right away, cells decay slowly into the background.

Game of Life code ported to elm from [game-of-life-with-decay](https://joy.recurse.com/posts/886-game-of-life-with-decay)

![life](/life.gif)


### Local start

```sh
elm-live src/Main.elm -o --dir=public -- --output=public/elm.js
```
