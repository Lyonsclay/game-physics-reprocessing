open Reprocessing;

type positionT = {
  x: float,
  y: float,
};
type deltaT = float;
type velocityT = {
  x: float,
  y: float,
};
type accelerationT = {
  x: float,
  y: float,
};

/* classical physics */
let speed = (v: velocityT) : float => sqrt(v.x *. v.x +. v.y *. v.y);

type bodyT = {
  position: positionT,
  velocity: velocityT,
};

type stateT = {player: bodyT};

/* This will be used as a maximum speed limit for all objects/bodies. */
let terminalSpeed: float = 250.0;
let gravity: accelerationT = {y: 50.0, x: 0.0};

let computeVelocity =
    (velocity: velocityT, acceleration: accelerationT, time: deltaT)
    : velocityT => {
  x: velocity.x +. acceleration.x *. time,
  y: velocity.y +. acceleration.y *. time,
};

let getNewPosition = ({velocity, position}, elapsedTime: float) : positionT => {
  let margin: float = 0.01;
  let newY =
    switch (position.y) {
    | _ when position.y < margin && velocity.y < 0.0 => 0.0
    | _ when position.y > 500.0 -. margin && velocity.y > 0.0 => 500.0
    | _ => position.y +. velocity.y *. elapsedTime
    };
  {y: newY, x: position.x};
};

let getNewVelocity =
    ({velocity, position}, acceleration: accelerationT, elapsedTime: float)
    : velocityT => {
  let margin: float = 0.01;

  switch (velocity) {
  | _ when speed(velocity) > terminalSpeed => {x: 0.0, y: 0.0}
  | _ when position.y < margin && position.y === 500.0 => {x: 0.0, y: 0.0}
  | _ => computeVelocity(velocity, acceleration, elapsedTime)
  };
};
let initialPlayer: bodyT = {
  position: {
    x: 150.0,
    y: 50.0,
  },
  velocity: {
    x: 0.0,
    y: 20.0,
  },
};
let setup = env : stateT => {
  Env.size(~width=600, ~height=600, env);
  {player: initialPlayer};
};

let draw = ({player}, env) => {
  Draw.background(Utils.color(~r=19, ~g=217, ~b=229, ~a=255), env);
  Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);
  let posX = player.position.x;
  let posY = player.position.y;
  Draw.rectf(~pos=(posX, posY), ~width=100., ~height=100., env);
  /* let p = Env.key(Space, env) ? "true" : "false"; */

  let posStatus =
    "posX: "
    ++ string_of_float(player.position.x)
    ++ ", posY: "
    ++ string_of_float(player.position.y);
  let deltaStatus =
    "deltaX: "
    ++ string_of_float(player.velocity.x)
    ++ " deltay: "
    ++ string_of_float(player.velocity.y);
  let speed = "speed: " ++ string_of_float(speed(player.velocity));

  Draw.text(~body=speed, ~pos=(150, 50), env);
  Draw.text(~body=deltaStatus, ~pos=(150, 150), env);
  Draw.text(~body=posStatus, ~pos=(150, 200), env);

  /* let {velocity} = player; */
  let up: accelerationT = {x: 0.0, y: (-50.0)};
  let down: accelerationT = {x: 0.0, y: 50.0};
  /* let margin: float = 0.01; */
  let newPlayer = {
    position: getNewPosition(player, Env.deltaTime(env)),
    velocity:
      Env.key(Space, env) ?
        getNewVelocity(player, up, Env.deltaTime(env)) :
        getNewVelocity(player, down, Env.deltaTime(env)),
  };
  {player: newPlayer};
};

run(~setup, ~draw, ());
