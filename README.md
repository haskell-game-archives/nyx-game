# nyx

A bullet hell game written in Haskell.

Currently works on Linux and OS X.

* [Animated gif](https://streamable.com/0biaj)


## Controls:

* Arrows to move
* Z to shoot
* X to squeeze
* C to restart from checkpoint


## How to Run

### From Source

You will need [Stack](https://haskellstack.org).

#### Ubuntu:

```sh
sudo apt install libsdl2-dev libsdl2-ttf-dev libsdl2-image-dev libsdl2-mixer-dev
make build
make exec
```

#### OS X

```sh
brew install sdl2 sdl2_ttf sdl2_image sdl2_mixer
make build
make exec
```

### From Compiled Binary

> Binaries available [here](https://gitlab.com/gilmi/haskell-play/releases).

#### Ubuntu:

```sh
sudo apt install libsdl2-2.0-0 libsdl2-ttf-2.0-0 libsdl2-image-2.0-0 libsdl2-mixer-2.0-0
./shmup
```

#### OS X

```sh
brew install sdl2 sdl2_ttf sdl2_image sdl2_mixer
./shmup
```
