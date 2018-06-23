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

/* type keyMapT = { */
/*  UP */
/*  DOWN */
/*  LEFT */
/*  RIGHT */
/* } */

/* classical physics */
let speed = (v: velocityT) : float => sqrt(v.x *. v.x +. v.y *. v.y);

type bodyT = {
  position: positionT,
  velocity: velocityT,
  acceleration: accelerationT,
};

type stateT = {
  player: bodyT,
  /* poop: bodyT, */
};

/* This will be used as a maximum speed limit for all objects/bodies. */
let terminalSpeed: float = 350.0;
let gravity: accelerationT = {x: 0.0, y: 10.0};
let screenWidth: int = 1680;
let screenHeight: int = 1000;
let pWidth: float = 50.0;
let pHeight: float = 50.0;
let poopWidth: float = 5.0;
let poopHeight: float = 7.0;
let maxPosX: float = float_of_int(screenWidth) -. pWidth;
let maxPosY: float = float_of_int(screenHeight) -. pHeight;
let playerThrust: float = 80.0;

let computeVelocity =
    (velocity: velocityT, acceleration: accelerationT, time: deltaT)
    : velocityT => {
  x: velocity.x +. acceleration.x *. time,
  y: velocity.y +. acceleration.y *. time,
};

let getNewPosition = ({velocity, position}, elapsedTime: float) : positionT => {
  /* let margin: float = 0.01; */
  let newX =
    switch (position.x) {
    | _ when position.x === 0.0 => 0.0
    | _ when position.x === maxPosX => maxPosX
    | _ => position.x +. velocity.x *. elapsedTime
    };
  let newY =
    switch (position.y) {
    | _ when position.y === 0.0 => 0.0
    | _ when position.y === maxPosY => maxPosY
    | _ => position.y +. velocity.y *. elapsedTime
    };

  {x: newX, y: newY};
};

let getPlayerAcceleration = env : accelerationT => {
  let accelerationX: float =
    switch (env) {
    | _ when Env.key(H, env) === true && Env.key(L, env) === true => 0.0
    | _ when Env.key(H, env) === true => (-1.0) *. playerThrust
    | _ when Env.key(L, env) === true => playerThrust
    | _ => 0.0
    };
  let accelerationY: float =
    switch (env) {
    | _ when Env.key(J, env) === true && Env.key(K, env) === true => 0.0
    | _ when Env.key(J, env) === true => playerThrust
    | _ when Env.key(K, env) === true => (-1.0) *. playerThrust
    | _ => 0.0
    };

  {x: accelerationX, y: accelerationY +. gravity.y};
};
let getNewVelocity =
    ({velocity, position, acceleration}, elapsedTime: float)
    : velocityT =>
  switch (velocity) {
  | _ when position.y <= 0.0 && velocity.y < 0.0 => {x: velocity.x, y: 0.0}
  | _ when position.x <= 0.0 && velocity.x < 0.0 => {x: 0.0, y: velocity.y}
  | _ when position.y >= maxPosY && velocity.y > 0.0 => {
      x: velocity.x,
      y: 0.0,
    }
  | _ when position.x >= maxPosX && velocity.x > 0.0 => {
      x: 0.0,
      y: velocity.y,
    }
  | _ when speed(velocity) >= terminalSpeed => velocity
  | _ => computeVelocity(velocity, acceleration, elapsedTime)
  };

let getNewPoop = (player: bodyT, poop: bodyT, env) : bodyT => {
  let elapsedTime: float = Env.deltaTime(env);
  let acceleration: accelerationT = {x: 0.0, y: (-80.0)};
  let deltaVelocity: velocityT = {
    x: acceleration.x *. elapsedTime,
    y: acceleration.y *. elapsedTime,
  };
  let velocity: velocityT =
    switch (env) {
    | _ when Env.key(Space, env) === true => {x: player.velocity.x, y: 0.0}
    | _ => {
        x: poop.velocity.x +. deltaVelocity.x,
        y: poop.velocity.y +. deltaVelocity.y,
      }
    };
  /* creates a syntax parsing error */
  /* if the right hand of the following equation is used as a param for getNewPosition */
  let tempBody: bodyT = {velocity, position: poop.position, acceleration};
  let position: positionT = getNewPosition(tempBody, elapsedTime);

  {position, velocity, acceleration};
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
  acceleration: {
    x: 0.0,
    y: 0.0,
  },
};
let initialPoop: bodyT = {
  position: {
    x: 40.0,
    y: 40.0,
  },
  velocity: {
    x: 0.0,
    y: 0.0,
  },
  acceleration: {
    x: 0.0,
    y: 0.0,
  },
};
let setup = env : stateT => {
  Env.size(~width=screenWidth, ~height=screenHeight, env);
  /* {player: initialPlayer, poop: initialPoop}; */
  {player: initialPlayer};
};
let draw = ({player}, env) => {
  Draw.background(Utils.color(~r=19, ~g=217, ~b=229, ~a=255), env);
  Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);
  let posX = player.position.x;
  let posY = player.position.y;
  Draw.rectf(~pos=(posX, posY), ~width=pWidth, ~height=pHeight, env);
  /* let poopX = poop.position.x; */
  /* let poopY = poop.position.y; */
  /* Draw.rectf(~pos=(poopX, poopY), ~width=poopWidth, ~height=poopHeight, env); */
  let newPlayer: bodyT = {
    position: getNewPosition(player, Env.deltaTime(env)),
    velocity: getNewVelocity(player, Env.deltaTime(env)),
    acceleration: getPlayerAcceleration(env),
  };
  /* let newPoop: bodyT = getNewPoop(player, poop, env); */
  /* {player: newPlayer, poop}; */
  {player: newPlayer};
};

/* run(~setup, ~draw, ()); */
/* let debugPlayerDisplay = (player, env) => { */
/*   let posStatus = */
/*     "posX: " */
/*     ++ string_of_float(player.position.x) */
/*     ++ ", posY: " */
/*     ++ string_of_float(player.position.y); */
/*   let deltaStatus = */
/*     "deltaX: " */
/*     ++ string_of_float(player.velocity.x) */
/*     ++ " deltay: " */
/*     ++ string_of_float(player.velocity.y); */
/*   let accelerationStatus = */
/*     "accelerationX: " */
/*     ++ string_of_float(player.acceleration.x) */
/*     ++ "   accelerationY:  " */
/*     ++ string_of_float(player.acceleration.y); */
/*   let speed = "speed: " ++ string_of_float(speed(player.velocity)); */

/*   Draw.text(~body=speed, ~pos=(150, 50), env); */
/*   Draw.text(~body=deltaStatus, ~pos=(150, 150), env); */
/*   Draw.text(~body=posStatus, ~pos=(150, 200), env); */
/*   Draw.text(~body=accelerationStatus, ~pos=(150, 250), env); */
/* }; */
