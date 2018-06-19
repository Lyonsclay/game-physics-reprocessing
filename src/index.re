open Reprocessing;

type positionT = {
  x: float,
  y: float
};
type deltaT = float;
type velocityT = {
  x: float,
  y: float
};
type accelerationT = {
  x: float,
  y: float
};

/* classical physics */
let speed = (v: velocityT) : float => sqrt(v.x *. v.x +. v.y *. v.y);

type bodyT = {
  position: positionT,
  velocity: velocityT
};

type state = {player: bodyT};

/* This will be used as a maximum speed limit for all objects/bodies. */
let terminalSpeed: float = 2000.0;
let gravity: accelerationT = {y: 50.0, x: 0.0};

let computeVelocity =
  (velocity: velocityT, acceleration: accelerationT, time: deltaT) : velocityT
  => {
  x: velocity.x +. acceleration.x *. time,
  y: velocity.y +. acceleration.y *. time
};

let getNewVelocity =
    (velocity: velocityT, acceleration: accelerationT, elapsedTime: float)
  : velocityT
    =>
  switch (velocity) {
  | velocityT when speed(velocity) > terminalSpeed => {x: 0.0, y: 0.0}
  | velocityT => computeVelocity(velocity, acceleration, elapsedTime)
  };

let setup = env => {
  Env.size(~width=600, ~height=600, env);
  let player: bodyT =
    {
      position: {
        x: 150.0,
        y: 50.0
      },
      velocity: {
        x: 0.0,
        y: 20.0
      }
    };
  ();
};

let draw = (state, env) => {
  let {player} = state;
  Draw.background(Utils.color(~r=19, ~g=217, ~b=229, ~a=255), env);
  Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);
  let posX = player.position.x;
  let posY = player.position.y;
  Draw.rectf(~pos=(posX, posY), ~width=100., ~height=100., env);
  let p = Env.key(Space, env) ? "true" : "false";

  let status =
    "posX: "
    ++ string_of_float(player.position.x)
    ++ ", posY: "
    ++ string_of_float(player.position.y)
    ++ "\n"
    ++ "deltaX: "
    ++ string_of_float(player.velocity.x)
    ++ " deltay: "
    ++ string_of_float(player.velocity.y)
    ++ "\n";
  /* Draw.text(~body=p, ~pos=(50, 50), env); */
  Draw.text(~body=status, ~pos=(150, 50), env);
  let {velocity} = player;
  let up: accelerationT = { x: 0.0, y: (-50.0) };
  let down: accelerationT = { x: 0.0, y: 50.0 };
  {
    player: {
      position: {
        x: 0.0,
        y:
          player.position.y > 500. ?
            500.0 : player.position.y +. velocity.y *. Env.deltaTime(env)
      },
      velocity: getNewVelocity(velocity, down, Env.deltaTime(env))
        /* Env.key(Space, env) ? */
          /* getNewVelocity(velocity, up, Env.deltaTime(env)) : */
          /* getNewVelocity(velocity, down, Env.deltaTime(env)) */
    }
  };
};

run(~setup, ~draw, ());
