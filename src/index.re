open Reprocessing;
/* open Reprocessing_Common; */

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
/* This will be used as a maximum speed limit for all objects/bodies. */
let terminalSpeed: float = 350.0;
let gravityY: float = 220.0;
let gravity: accelerationT = {x: 0.0, y: gravityY};
let playerThrust: float = 350.0;
let screenWidth: int = 1680;
let screenHeight: int = 1000;
let pWidth: float = 50.0;
let pHeight: float = 50.0;
let poopWidth: float = 5.0;
let poopHeight: float = 7.0;
let maxPosX: float = float_of_int(screenWidth) -. pWidth;
let maxPosY: float = float_of_int(screenHeight) -. pHeight;

type directionT =
  | UP
  | DOWN
  | LEFT
  | RIGHT
  | NONE;

let keyMap: Reprocessing_Common.KeySet.elt => directionT =
  fun
  | H => LEFT
  | J => DOWN
  | K => UP
  | L => RIGHT
  | Nothing => NONE
  | _ => NONE;

let thrustMap: directionT => velocityT =
  fun
  | UP => {x: 0.0, y: (-50.0)}
  | DOWN => {x: 0.0, y: 50.0}
  | LEFT => {x: (-50.0), y: 0.0}
  | RIGHT => {x: 50.0, y: 0.0}
  | NONE => {x: 0.0, y: 0.0};

let accelerationMap: directionT => accelerationT =
  fun
  | UP => {x: 0.0, y: (-50.0)}
  | DOWN => {x: 0.0, y: 50.0}
  | LEFT => {x: (-50.0), y: 0.0}
  | RIGHT => {x: 50.0, y: 0.0}
  | NONE => {x: 0.0, y: 0.0};

/* classical physics */
let speed = (v: velocityT) : float => sqrt(v.x *. v.x +. v.y *. v.y);

type bodyT = {
  position: positionT,
  velocity: velocityT,
  acceleration: accelerationT,
};

type stateT = {
  player: bodyT,
  poop: bodyT,
};

let debugPlayerDisplay = (player: bodyT, env) => {
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
  let accelerationStatus =
    "accelerationX: "
    ++ string_of_float(player.acceleration.x)
    ++ "   accelerationY:  "
    ++ string_of_float(player.acceleration.y);
  let speed = "speed: " ++ string_of_float(speed(player.velocity));

  Draw.text(~body=speed, ~pos=(150, 50), env);
  Draw.text(~body=deltaStatus, ~pos=(150, 150), env);
  Draw.text(~body=posStatus, ~pos=(150, 200), env);
  Draw.text(~body=accelerationStatus, ~pos=(150, 250), env);
};

let computeVelocity =
    (velocity: velocityT, acceleration: accelerationT, time: deltaT)
    : velocityT => {
  x: velocity.x +. acceleration.x *. time,
  y: velocity.y +. acceleration.y *. time,
};

let getNewPosition = ({velocity, position}, deltaTime: float) : positionT => {
  /* let margin: float = 0.01; */
  let newX =
    switch (position.x) {
    | _ when position.x === 0.0 => 0.0
    | _ when position.x === maxPosX => maxPosX
    | _ => position.x +. velocity.x *. deltaTime
    };
  let newY =
    switch (position.y) {
    | _ when position.y === 0.0 => 0.0
    | _ when position.y === maxPosY => maxPosY
    | _ => position.y +. velocity.y *. deltaTime
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
let getNewPlayerVelocity =
    ({velocity, position, acceleration}, deltaTime: float)
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
  | _ => computeVelocity(velocity, acceleration, deltaTime)
  };

let getKeysPressed = env : list(directionT) =>
  List.map(key => Env.key(key, env) ? key : Nothing, [H, J, K, L])
  |> List.map(keyMap);

let getAccelerationList = (keys: list('a)) : list(accelerationT) =>
  List.map(accelerationMap, keys);

let rec calculatePlayerAcceleration =
        (accelerationList: list(accelerationT))
        : accelerationT =>
  switch (accelerationList) {
  | [head, ...tail] when List.length(tail) === 0 => head
  | [head, ...tail] when List.length(tail) > 0 => {
      x: head.x +. calculatePlayerAcceleration(tail).x,
      y: head.y +. calculatePlayerAcceleration(tail).y,
    }
  | _ => {x: 0.0, y: 0.0}
  };

let getThrustList = (keys: list('a)) : list(velocityT) =>
  List.map(thrustMap, keys);
/* // Add all velocities in list into one resultant velocity. */
let rec calculateVelocity = (thrustList: list(velocityT)) : velocityT =>
  switch (thrustList) {
  | [head, ...tail] when List.length(tail) === 0 => head
  | [head, ...tail] when List.length(tail) > 0 => {
      x: head.x +. calculateVelocity(tail).x,
      y: head.y +. calculateVelocity(tail).y,
    }
  | _ => {x: 0.0, y: 0.0}
  };

let getNewBirdy = (bird: bodyT, env) : bodyT => {
  let deltaTime: float = Env.deltaTime(env);
  let keys: list('a) = getKeysPressed(env);
  let birdVelocity: velocityT = bird.velocity;
  let thrustList: list(velocityT) = getThrustList(keys);
  let accelerationList: list(accelerationT) = getAccelerationList(keys);
  let acceleration: accelerationT =
    calculatePlayerAcceleration(accelerationList);
  /* // This is before applying acceleraton over deltaTime. */
  let velocity: velocityT = calculateVelocity([birdVelocity, ...thrustList]);
  let birdBody = {position: bird.position, velocity, acceleration};
  let newVelocity = getNewPlayerVelocity(birdBody, deltaTime);
  let newPosition = getNewPosition(birdBody, deltaTime);
  {position: newPosition, velocity: newVelocity, acceleration};
  /* bird; */
};

let getNewPoop = (player: bodyT, poop: bodyT, env) : bodyT => {
  let deltaTime: float = Env.deltaTime(env);
  let deltaVelocity: velocityT = {
    x: player.velocity.x +. poop.acceleration.x *. deltaTime,
    y: player.velocity.y +. gravityY *. deltaTime,
  };
  Draw.text(~body="delta velocity of y", ~pos=(200, 800), env);
  Draw.text(~body=string_of_float(deltaVelocity.y), ~pos=(400, 900), env);
  let isDropping: bool =
    poop.position.y < float_of_int(screenHeight) && poop.position.y > (-1.0);
  let pressSpaceKey: bool = Env.key(Space, env) === true;
  Draw.text(~body=string_of_bool(isDropping), ~pos=(400, 400), env);
  let position: positionT =
    switch ([pressSpaceKey, isDropping]) {
    | [false, false] => {x: (-40.0), y: (-40.0)}
    | [true, false] => {
        x: player.position.x +. pWidth /. 2.0,
        y: player.position.y +. pHeight,
      }
    | [_, true] => {
        x: poop.position.x +. poop.velocity.x *. deltaTime,
        y: poop.position.y +. poop.velocity.y *. deltaTime,
      }
    | _ => {x: (-40.0), y: (-40.0)}
    };
  let velocity: velocityT =
    switch ([pressSpaceKey, isDropping]) {
    | [false, false] => {x: 0.0, y: 0.0}
    | [true, false] => {
        x: player.velocity.x,
        y: player.velocity.y > 0.0 ? player.velocity.y : 0.0,
      }
    | [_, true] => {
        x: poop.velocity.x +. deltaVelocity.x,
        y: poop.velocity.y +. deltaVelocity.y,
      }
    | _ => {x: 0.0, y: 0.0}
    };

  Draw.text(~body=string_of_float(velocity.x), ~pos=(500, 600), env);
  Draw.text(~body=string_of_float(velocity.y), ~pos=(600, 600), env);
  Draw.text(~body=string_of_float(deltaTime), ~pos=(700, 600), env);

  /* creates a syntax parsing error */
  /* if the right hand of the following equation is used as a param for getNewPosition */
  /* let tempBody: bodyT = {velocity, position: poop.position, acceleration}; */
  /* let position: positionT = getNewPosition(tempBody, deltaTime); */

  {position, velocity, acceleration: gravity};
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
    x: (-40.0),
    y: (-40.0),
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
  {player: initialPlayer, poop: initialPoop};
};

let draw = ({player, poop}, env) => {
  Draw.background(Utils.color(~r=19, ~g=217, ~b=229, ~a=255), env);
  debugPlayerDisplay(poop, env);
  Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);
  let posX = player.position.x;
  let posY = player.position.y;
  Draw.rectf(~pos=(posX, posY), ~width=pWidth, ~height=pHeight, env);
  Draw.fill(Utils.color(~r=241, ~g=255, ~b=254, ~a=255), env);
  let poopX = poop.position.x;
  let poopY = poop.position.y;
  let center = (poopX, poopY);
  Draw.ellipsef(~center, ~radx=poopWidth, ~rady=poopHeight, env);
  /* let deltaTime: float = Env.deltaTime(env); */
  /* let newPlayer: bodyT = { */
  /*   position: getNewPosition(player, deltaTime), */
  /*   velocity: getNewPlayerVelocity(player, deltaTime), */
  /*   acceleration: getPlayerAcceleration(env), */
  /* }; */
  let newPlayer = getNewBirdy(player, env);
  let newPoop: bodyT = getNewPoop(player, poop, env);
  {player: newPlayer, poop: newPoop};
};

run(~setup, ~draw, ());
