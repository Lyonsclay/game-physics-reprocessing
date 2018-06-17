open Reprocessing;

type stateT = {
  playerY: float,
  driftV: float,
};

let setup = env => {
  Env.size(~width=600, ~height=600, env);
  {playerY: 0.0, driftV: 20.0};
};

let draw = ({playerY, driftV} as state, env) => {
  Draw.background(Utils.color(~r=19, ~g=217, ~b=229, ~a=255), env);
  Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);
  Draw.rectf(~pos=(150., playerY), ~width=100., ~height=100., env);

  {
    playerY: playerY > 500. ? 0.0 : playerY +. driftV *. Env.deltaTime(env),
    /* driftV: driftV -. 50.0 *. Env.deltaTime(env), */
    driftV:
      Env.keyPressed(K, env) ?
        driftV -. 5.0 *. Env.deltaTime(env) :
        driftV +. 5.0 *. Env.deltaTime(env),
  };
};

run(~setup, ~draw, ());
