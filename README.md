Reprocessing Example
---

This project is base of off [Reprocessing](https://github.com/schmavery/reprocessing).
- a Reasonml wrapper for webgl.

Demos are on other branches:

- [Flappy Bird](https://github.com/bsansouci/reprocessing-example/tree/livestream-flappybird)
- [2048](https://github.com/bsansouci/reprocessing-example/tree/2048)

This is the current project;
- [Physics](https://github.com/Lyonsclay/game-physics-reprocessing/tree/physics)

...and more on the [Reprocessing repo](https://github.com/schmavery/reprocessing#projects-using-reprocessing).

First you will need to install the [reason-cli](https://github.com/reasonml/reason-cli)

Also, here is an in depth guide to setting up an Ocaml development environment
including integrating with different editors including vim.

```opam install ocp-indent```

## How to
```
git clone git@github.com:Lyonsclay/game-physics-reprocessing.git
```

### Install

```
npm install
```

### Checkout `physics` branch

```
git checkout physics
```

### Build
```
npm run build
```

### Start
```
npm start
```

To build to JS run `npm run build:web` and then run a static server, like `python -m SimpleHTTPServer` and go to `localhost:8000`. If you're using safari you can simply open the `index.html` and tick `Develop > Disable Cross-Origin Restrictions`.

To build to native run `npm run build:native` and run `npm run start:native`

The build system used is [bsb-native](https://github.com/bsansouci/bsb-native).
