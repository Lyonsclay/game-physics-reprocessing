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
/* This will be used as a maximum speed limit for all objects/bodies. */
let terminalSpeed: float = 250.0;
let gravityY: float = 150.0;
let gravity: accelerationT = {x: 0.0, y: gravityY};
/* birdy acceleration */
let playerThrust: float = 350.0;

/* velocity to add when switching directions */
/* compensates for slow application of natural decelerationj */
let birdBoost: float = 10.0;
let screenWidth: int = 1680;
let screenHeight: int = 1000;
let playerWidth: float = 50.0;
let playerHeight: float = 50.0;
let poopWidth: float = 5.0;
let poopHeight: float = 7.0;
let maxPosX: float = float_of_int(screenWidth) -. playerWidth;
let maxPosY: float = float_of_int(screenHeight) -. playerHeight;

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

let velocityMap: directionT => velocityT =
  fun
  | UP => {x: 0.0, y: (-1.0) *. birdBoost}
  | DOWN => {x: 0.0, y: birdBoost}
  | LEFT => {x: (-1.0) *. birdBoost, y: 0.0}
  | RIGHT => {x: birdBoost, y: 0.0}
  | NONE => {x: 0.0, y: 0.0};

let accelerationMap: directionT => accelerationT =
  fun
  | UP => {x: 0.0, y: (-1.0) *. playerThrust}
  | DOWN => {x: 0.0, y: playerThrust}
  | LEFT => {x: (-1.0) *. playerThrust, y: 0.0}
  | RIGHT => {x: playerThrust, y: 0.0}
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

let debugDisplay = (player: bodyT, env) => {
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
  let speed = speed(player.velocity);
  let playerSpeed = "speed: " ++ string_of_float(speed);
  let excedesTerminal: bool = speed > terminalSpeed -. 10.0;
  let speedLimit =
    "Excedes speed limit : " ++ string_of_bool(excedesTerminal);

  Draw.text(~body=playerSpeed, ~pos=(150, 50), env);
  Draw.text(~body=deltaStatus, ~pos=(150, 150), env);
  Draw.text(~body=posStatus, ~pos=(150, 200), env);
  Draw.text(~body=accelerationStatus, ~pos=(150, 250), env);
  Draw.text(~body=speedLimit, ~pos=(150, 350), env);
};

let computeVelocity =
    (velocity: velocityT, acceleration: accelerationT, time: deltaT)
    : velocityT => {
  x: velocity.x +. acceleration.x *. time,
  y: velocity.y +. acceleration.y *. time,
};

let getNewPosition = ({velocity, position}, deltaTime: float) : positionT => {
  /* let margin: float = 0.01; */
  let newPosX: float = position.x +. velocity.x *. deltaTime;
  let newPosY: float = position.y +. velocity.y *. deltaTime;
  let newX =
    switch () {
    | _ when newPosX < 1.0 => 0.0
    | _ when newPosX > maxPosX -. 1.0 => maxPosX
    | _ => newPosX
    };
  let newY =
    switch (newPosY) {
    | _ when newPosY < 1.0 => 0.0
    | _ when newPosY > maxPosY -. 1.0 => maxPosY
    | _ => newPosY
    };
  {x: newX, y: newY};
};

let getNewPlayerVelocity =
    ({velocity, position, acceleration}, deltaTime: float)
    : velocityT => {
  let maxLeft: bool = position.x < 1.0;
  let maxRight: bool =
    position.x > float_of_int(screenWidth) -. playerWidth -. 1.0;
  let maxUp: bool = position.y < 1.0;
  let maxDown: bool =
    position.y > float_of_int(screenHeight) -. playerHeight -. 1.0;
  let excedesTerminal: bool = speed(velocity) > terminalSpeed -. 10.0;
  let velocityX: float =
    switch (excedesTerminal) {
    | true when velocity.x > 0.0 => velocity.x -. 1.0
    | true when velocity.x < 0.0 => velocity.x +. 1.0
    | false when maxLeft && velocity.x < 0.0 => 0.0
    | false when maxRight && velocity.x > 0.0 => 0.0
    | false => velocity.x +. acceleration.x *. deltaTime
    | _ => 0.0
    };
  let velocityY: float =
    switch (excedesTerminal) {
    | true when velocity.y > 0.0 => velocity.y -. 1.0
    | true when velocity.y < 0.0 => velocity.y +. 1.0
    | false when maxLeft && velocity.x < 0.0 => 0.0
    | false when maxUp && velocity.y < 0.0 => 0.0
    | false when maxDown && velocity.y > 0.0 => 0.0
    | false => velocity.y +. acceleration.y *. deltaTime
    | _ => 0.0
    };

  {x: velocityX, y: velocityY};
};

let getKeysPressed = env : list(directionT) =>
  List.map(key => Env.key(key, env) ? key : Nothing, [H, J, K, L])
  |> List.map(keyMap);

let getAccelerationList = (keys: list(directionT)) : list(accelerationT) =>
  List.map(accelerationMap, keys);

let rec calculatePlayerAcceleration =
        (accelerationList: list(accelerationT))
        : accelerationT => {
  let acceleration: accelerationT =
    switch (accelerationList) {
    | [head, ...tail] when List.length(tail) === 0 => head
    | [head, ...tail] when List.length(tail) > 0 => {
        x: head.x +. calculatePlayerAcceleration(tail).x,
        y: head.y +. calculatePlayerAcceleration(tail).y,
      }
    | _ => {x: 0.0, y: 0.0}
    };

  /* acceleration.y >= playerThrust ? */
  /* playerThrust : acceleration.y +. playerThrust; */
  {x: acceleration.x, y: acceleration.y};
};

let calculateAcceleration = (keys: list('a)) : list(accelerationT) =>
  List.map(accelerationMap, keys);
/* Add all velocities in list into one resultant velocity. */
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

  let thrustList: list(accelerationT) = getAccelerationList(keys);
  /* let thrusccelerations: list(accelerationT) = getAccelerationList(keys); */
  let accelerationList = [bird.acceleration, ...thrustList];
  let acceleration: accelerationT =
    /* speed(birdVelocity) >= terminalSpeed ? */
    calculatePlayerAcceleration(accelerationList);
  /* This is before applying acceleraton over deltaTime. */
  let velocity: velocityT =
    speed(bird.velocity) > terminalSpeed ?
      bird.velocity : computeVelocity(birdVelocity, acceleration, deltaTime);
  let birdBody = {position: bird.position, velocity, acceleration};
  let newVelocity = getNewPlayerVelocity(birdBody, deltaTime);
  let newPosition = getNewPosition(birdBody, deltaTime);
  {position: newPosition, velocity: newVelocity, acceleration};
  /* bird; */
};

let getNewPoop = (player: bodyT, poop: bodyT, env) : bodyT => {
  let deltaTime: float = Env.deltaTime(env);
  /* let deltaVelocity: velocityT = { */
  /*   x: player.velocity.x +. poop.acceleration.x *. deltaTime, */
  /*   y: player.velocity.y +. gravityY *. deltaTime, */
  /* }; */
  /* Draw.text(~body="delta velocity of y", ~pos=(200, 800), env); */
  /* Draw.text(~body=string_of_float(deltaVelocity.y), ~pos=(400, 900), env); */
  let isDropping: bool =
    poop.position.y < float_of_int(screenHeight) && poop.position.y > (-1.0);
  let pressSpaceKey: bool = Env.key(Space, env) === true;
  let position: positionT =
    switch ([pressSpaceKey, isDropping]) {
    | [false, false] => {x: (-40.0), y: (-40.0)}
    | [true, false] => {
        x: player.position.x +. playerWidth /. 2.0,
        y: player.position.y +. playerHeight,
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
        x: poop.velocity.x,
        y: poop.velocity.y +. gravityY *. deltaTime,
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
  let acceleration: accelerationT =
    switch (player) {
    | _ when player.acceleration.y < 0.0 => gravity
    | _ when player.acceleration.y >= 0.0 => {
        x: 0.0,
        y: player.acceleration.y +. gravityY,
      }
    | _ => gravity
    };

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
  debugDisplay(player, env);
  Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);
  let posX = player.position.x;
  let posY = player.position.y;
  let pWidth = playerWidth;
  let pHeight = playerHeight;
  Draw.rectf(~pos=(posX, posY), ~width=pWidth, ~height=pHeight, env);
  Draw.fill(Utils.color(~r=241, ~g=255, ~b=254, ~a=255), env);
  let poopX = poop.position.x;
  let poopY = poop.position.y;
  let center = (poopX, poopY);
  Draw.ellipsef(~center, ~radx=poopWidth, ~rady=poopHeight, env);
  let newBirdy = getNewBirdy(player, env);
  let newPoop: bodyT = getNewPoop(player, poop, env);
  {player: newBirdy, poop: newPoop};
};

run(~setup, ~draw, ());
