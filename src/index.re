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
let speed = (v : velocityT): float => sqrt(v.x *. v.x + v.y *. v.y);

type bodyT = {
  position: positionT,
  velocity: velocityT,
};

type stateT = {
  player: bodyT
}

/* This will be used as a maximum speed limit for all objects/bodies. */
let terminalSpeed: float = 2000.0;
let gravity: acceleration = { y: 50.0, x: 0.0 };

let computeVelocity = ( velocity: velocityT, acceleration: accelerationT, time: deltaT ) => {
   {
    x: velocity.x +. acceleration.x *. time,
    y: velocity.y +. acceleration.y *. time,
  }
}

let getNewVelocity =
    (velocity: velocityT, acceleration: float, elapsedTime: float) =>
  switch (velocity) {
  /* | float when speed(velocity) > terminalSpeed => velocity, */
  | float when speed(velocity) > terminalSpeed => { x: 20.0, y: 20.0 },
  | float => computeVelocity(velocity, acceleration, elapsedTime)
  };

let setup = env => {
  Env.size(~width=600, ~height=600, env);
  let player: bodyT =
    {
      postion: {
        x: 150.0,
        y: 50.0,
      },
      velocity: {
        x: 0.0,
        y: 20.0,
      },
    }(
      player,
    );
  ();
};

let draw = ({player} as state, env) => {
  Draw.background(Utils.color(~r=19, ~g=217, ~b=229, ~a=255), env);
  Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);
  Draw.rectf(~pos=(player.position.x, player.postion.y), ~width=100., ~height=100., env);
  let p = Env.key(Space, env) ? "true" : "false";
  Draw.text(~body=p, ~pos=(50, 50), env);
  {
    player: {
    position: {
      x: 0.0,
      y: position.y > 500. ? 500.0 : position.y +. velocity.y *. Env.deltaTime(env)
    },
    velocity: Env.key(Space, env) ?
      getNewVelocity(driftV, -50.0, Env.deltaTime(env)) :
      getNewVelocity(driftV, 50.0, Env.deltaTime(env)),
    }
  };
};

run(~setup, ~draw, ());
