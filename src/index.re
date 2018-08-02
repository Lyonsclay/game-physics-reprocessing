open Reprocessing;
open Types;
/* include Picnic; */
/* open Picnic; */
/* include Beebo; */

/* Newtonian physics */
let speed = (v: velocityT) : float => sqrt(v.x *. v.x +. v.y *. v.y);
/* This will be used as a maximum speed limit for all objects/bodies. */
let terminalSpeed: float = 350.0;
let gravityY: float = 150.0;
let gravity: accelerationT = {x: 0.0, y: gravityY};
/* birdy acceleration */
let playerThrust: float = 350.0;

/* velocity to add when switching directions */
/* compensates for slow application of natural deceleration */
let birdBoost: float = 40.0;
let screenWidth: int = 1680;
let screenHeight: int = 1000;
let playerWidth: float = 50.0;
let playerHeight: float = 50.0;
let poopWidth: float = 5.0;
let poopHeight: float = 7.0;
let maxPosX: float = float_of_int(screenWidth) -. playerWidth;
let maxPosY: float = float_of_int(screenHeight) -. playerHeight;
let keyMap: Reprocessing_Common.KeySet.elt => directionT =
  fun
  | H => LEFT
  | J => DOWN
  | K => UP
  | L => RIGHT
  | Nothing => NONE
  | _ => NONE;

let keyStringMap: Reprocessing_Common.KeySet.elt => string =
  fun
  | H => "H"
  | J => "J"
  | K => "K"
  | L => "L"
  | Space => "Space"
  | Nothing => ""
  | _ => "";

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

let motivationMap: motivationT => string =
  fun
  | PICNIC => "Picnic"
  | LEAVE => "Leave"
  | FORAGE => "Forage"
  | HUNT => "Hunt"
  | RAGE => "Rage";

type stateT = {
  birdy: bodyT,
  poops: list(bodyT),
  picnic: picnicT,
  picnicker: picnickerT,
};

let rec keyMapString = (keysMap: list(string)) : string =>
  switch (keysMap) {
  | [] => ""
  | [head, ...tail] when List.length(tail) === 0 => head
  | [head, ...tail] when List.length(tail) > 0 => head ++ keyMapString(tail)
  | _ => ""
  };

let getKeyMap = env : list(string) =>
  List.map(key => Env.key(key, env) ? key : Nothing, [H, J, K, L, Space])
  |> List.map(keyStringMap);

let debugDisplay = (player: bodyT, env) => {
  /* [%bs.raw {| console.log('here is some javascript for you') |}]; */
  let spaceKey: bool = Env.key(Space, env);
  let keys: string = getKeyMap(env) |> keyMapString;
  let pressedKeys = "Pressed keys : " ++ keys;
  let spaceStatus = "spacebar: " ++ string_of_bool(spaceKey);
  let posXStatus =
    "posX: " ++ string_of_int(int_of_float(player.position.x));
  let posYStatus =
    "posY: " ++ string_of_int(int_of_float(player.position.y));
  let deltaXStatus =
    "deltaX: " ++ string_of_int(int_of_float(player.velocity.x));
  let deltaYStatus =
    "deltay: " ++ string_of_int(int_of_float(player.velocity.y));
  let accelerationXStatus =
    "accelerationX: " ++ string_of_int(int_of_float(player.acceleration.x));
  let accelerationYStatus =
    "accelerationY:  " ++ string_of_int(int_of_float(player.acceleration.y));
  let speed = speed(player.velocity);
  let playerSpeed = "speed: " ++ string_of_int(int_of_float(speed));
  let excedesTerminal: bool = speed > terminalSpeed -. 10.0;
  let speedLimit =
    "Excedes speed limit : " ++ string_of_bool(excedesTerminal);

  Draw.text(~body=pressedKeys, ~pos=(150, 50), env);
  Draw.text(~body=playerSpeed, ~pos=(150, 100), env);
  Draw.text(~body=deltaXStatus, ~pos=(150, 150), env);
  Draw.text(~body=deltaYStatus, ~pos=(150, 200), env);
  Draw.text(~body=posXStatus, ~pos=(150, 250), env);
  Draw.text(~body=posYStatus, ~pos=(150, 300), env);
  Draw.text(~body=accelerationXStatus, ~pos=(150, 350), env);
  Draw.text(~body=accelerationYStatus, ~pos=(150, 400), env);
  Draw.text(~body=speedLimit, ~pos=(150, 450), env);
  Draw.text(~body=spaceStatus, ~pos=(150, 500), env);
};

let getNewPosition = ({velocity, position}, deltaTime: float) : positionT => {
  /* let margin: float = 0.01; */
  let newPosX: float = position.x +. velocity.x *. deltaTime;
  let newPosY: float = position.y +. velocity.y *. deltaTime;
  let newX =
    switch () {
    | _ when newPosX < 1.0 && velocity.x <= 0.0 => 0.0
    | _ when newPosX > maxPosX -. 1.0 && velocity.x >= 0.0 => maxPosX
    | _ => newPosX
    };
  let newY =
    switch (newPosY) {
    | _ when newPosY < 1.0 && velocity.y <= 0.0 => 0.0
    | _ when newPosY > maxPosY -. 1.0 && velocity.y <= 0.0 => maxPosY
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
    | true when velocity.x > 0.0 && velocity.x > abs_float(velocity.y) =>
      velocity.x -. 20.0
    | true when velocity.x > 0.0 => velocity.x -. 1.0
    | true
        when
          velocity.x < 0.0 && abs_float(velocity.x) > abs_float(velocity.y) =>
      velocity.x +. 20.0
    | true when velocity.x < 0.0 => velocity.x +. 1.0
    | false when maxLeft && velocity.x < 0.0 => 0.0
    | false when maxRight && velocity.x > 0.0 => 0.0
    | false => velocity.x +. acceleration.x *. deltaTime
    | _ => 0.0
    };
  let velocityY: float =
    switch (excedesTerminal) {
    | true when velocity.y > 0.0 && velocity.y > abs_float(velocity.x) =>
      velocity.y -. 20.0
    | true when velocity.y > 0.0 => velocity.y -. 1.0
    | true
        when
          velocity.y < 0.0 && abs_float(velocity.y) > abs_float(velocity.x) =>
      velocity.y +. 20.0
    | true when velocity.y < 0.0 => velocity.y +. 1.0
    | false when maxUp && velocity.y < 0.0 => 0.0
    | false when maxDown && velocity.y > 0.0 => 0.0
    | false => velocity.y +. acceleration.y *. deltaTime
    | _ => 0.0
    };

  {x: velocityX, y: velocityY};
};

let getKeysPressed = env : list(directionT) =>
  List.map(key => Env.key(key, env) ? key : Nothing, [H, J, K, L, Space])
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

  {x: acceleration.x, y: acceleration.y};
};

let getNewBirdy = (bird: bodyT, env) : bodyT => {
  let deltaTime: float = Env.deltaTime(env);
  let keys: list('a) = getKeysPressed(env);
  let thrustList: list(accelerationT) = getAccelerationList(keys);
  let accelerationList = [gravity, ...thrustList];
  let acceleration: accelerationT =
    calculatePlayerAcceleration(accelerationList);
  let velocity = getNewPlayerVelocity(bird, deltaTime);
  let birdBody = {position: bird.position, velocity, acceleration};
  let newPosition = getNewPosition(birdBody, deltaTime);

  {position: newPosition, velocity, acceleration};
};

let addNewPoop = (player: bodyT) : bodyT => {
  let position: positionT = {
    x: player.position.x +. playerWidth /. 2.0,
    y: player.position.y +. playerHeight,
  };
  let velocity: velocityT = {
    x: player.velocity.x,
    y: player.velocity.y > 100.0 ? player.velocity.y : 100.0,
  };

  {position, velocity, acceleration: gravity};
};

let filterOffScreen = (poop: bodyT) : bool =>
  switch (poop.position) {
  | {x, _} when x < 0.0 -. poopWidth => false
  | {x, _} when x > float_of_int(screenWidth) +. poopWidth => false
  | {y, _} when y > float_of_int(screenHeight) +. poopHeight => false
  | _ => true
  };

let canAddPoop = (poops: list(bodyT), player: bodyT) : bool =>
  switch (poops) {
  | [] => true
  | _ when List.length(poops) > 7 => false
  | [head, ..._] when head.position.y -. player.position.y > 80.0 => true
  | _ => false
  };

let getNewPoops = (player: bodyT, poops: list(bodyT), env) : list(bodyT) => {
  let deltaTime: float = Env.deltaTime(env);
  let pressedSpaceKey: bool = Env.key(Space, env);
  let addPoop: bool = pressedSpaceKey && canAddPoop(poops, player);
  let updatePoop = (poop: bodyT) : bodyT => {
    let position: positionT = {
      x: poop.position.x +. poop.velocity.x *. deltaTime,
      y: poop.position.y +. poop.velocity.y *. deltaTime,
    };
    let velocity: velocityT = {
      x: poop.velocity.x,
      y: poop.velocity.y +. gravityY *. deltaTime,
    };

    {position, velocity, acceleration: gravity};
  };
  let poopList = List.filter(filterOffScreen, poops) |> List.map(updatePoop);

  addPoop ? [addNewPoop(player), ...poopList] : poopList;
};

let drawPoop = (env, poop: bodyT) => {
  let poopX = poop.position.x;
  let poopY = poop.position.y;
  let center = (poopX, poopY);
  Draw.ellipsef(~center, ~radx=poopWidth, ~rady=poopHeight, env);
};

let cherryMap =
  List.map(
    l => (Random.float(l), Random.float(l)),
    [30.0, 30.0, 30.0, 30.0, 30.0, 30.0, 30.0],
  );

let drawPicnic = (picnic: picnicT, env) => {
  let posX = picnic.position.x;
  let posY = picnic.position.y;
  /* print_int(List.nth(cherryMap, 5)); */
  if (picnic.blanket) {
    Draw.fill(Utils.color(~r=241, ~g=215, ~b=234, ~a=205), env);
    Draw.rectf(~pos=(posX, posY), ~width=190.0, ~height=5.0, env);
  };
  if (picnic.basket) {
    Draw.fill(Utils.color(~r=121, ~g=25, ~b=24, ~a=255), env);
    Draw.rectf(
      ~pos=(posX +. 80.0, posY -. 25.0),
      ~width=35.0,
      ~height=25.0,
      env,
    );
  };
  if (picnic.watermelon) {
    Draw.fill(Utils.color(~r=21, ~g=205, ~b=24, ~a=255), env);
    Draw.ellipsef(
      ~center=(posX +. 20.0, posY -. 20.0),
      ~radx=40.0,
      ~rady=20.0,
      env,
    );
  };
  if (picnic.cherries) {
    Draw.fill(Utils.color(~r=210, ~g=5, ~b=24, ~a=255), env);
    List.iter(
      ((x, y)) =>
        Draw.ellipsef(
          ~center=(posX +. 80.0 +. x, posY +. y -. 40.0),
          ~radx=5.0,
          ~rady=5.0,
          env,
        ),
      cherryMap,
    );
  };
};

let detectHit = (player: bodyT, poop: bodyT, env) : bool => {
  let {x: poopX, y: poopY}: positionT = poop.position;
  let {x: playerX, y: playerY}: positionT = player.position;
  let poopPosX = "poopX : " ++ string_of_float(poopX);
  let poopPosY = "poopY : " ++ string_of_float(poopY);
  let playerPosX = "playerX : " ++ string_of_float(playerX);
  let playerPosY = "playerY : " ++ string_of_float(playerY);

  Draw.text(~body=poopPosX, ~pos=(550, 75), env);
  Draw.text(~body=poopPosY, ~pos=(550, 125), env);
  Draw.text(~body=playerPosX, ~pos=(550, 175), env);
  Draw.text(~body=playerPosY, ~pos=(550, 225), env);

  abs_float(poopX -. playerX) < 10.01 && abs_float(poopY -. playerY) < 10.01;
};
let updatePicnicker =
    (
      {body, motivation}: picnickerT,
      picnic: picnicT,
      bird: bodyT,
      poops: list(bodyT),
      env,
    )
    : picnickerT => {
  let posX = body.position.x;
  let posY = body.position.y;
  let picnicX = picnic.position.x;
  let deltaTime = Env.deltaTime(env);
  let newVelocityX: float =
    switch (motivation) {
    | PICNIC when posX > picnicX => (-30.0)
    | PICNIC when posX < picnicX => 30.0
    | LEAVE when posX > picnicX => 30.0
    | LEAVE when posX < picnicX => (-30.0)
    | FORAGE => Random.bool() ? (-10.0) : 10.0
    | RAGE => Random.bool() ? (-30.0) : 30.0
    | HUNT when posX > bird.position.x => (-5.0)
    | HUNT when posX < bird.position.x => 5.0
    | _ => 0.0
    };

  let hits: list(bodyT) =
    List.filter(poop => detectHit(body, poop, env), poops);
  let hitsCount: int = List.length(hits);
  let headShot =
    List.exists(
      (poop: bodyT) => (poop.position.y > 0.75 *. poopHeight: bool),
      hits: list(bodyT),
    );

  /* let message: string = "hits"; */
  for (i in 0 to List.length(hits) - 1) {
    let posX = "posX : " ++ string_of_float(List.nth(hits, i).position.x);
    let posY = "posY : " ++ string_of_float(List.nth(hits, i).position.y);

    Draw.text(~body=posX, ~pos=(250, 25 * i), env);
    Draw.text(~body=posY, ~pos=(250, 25 * i + 50), env);
  };
  let newMotivation: motivationT =
    switch (hitsCount) {
    | _ when headShot => RAGE
    | _ when hitsCount > 2 => RAGE
    | 0 => motivation
    | 2 => LEAVE
    | _ => motivation
    };
  {
    body: {
      velocity: {
        x: newVelocityX,
        y: 0.0,
      },
      position: {
        x: posX +. newVelocityX *. deltaTime,
        y: posY,
      },
      acceleration: {
        x: 0.0,
        y: 0.0,
      },
    },
    motivation: newMotivation,
  };
};

let drawPicnicker = ({body}: picnickerT, env) => {
  let posX = body.position.x;
  let posY = body.position.y;
  let pWidth = 25.0;
  let pHeight = 100.0;
  Draw.fill(Utils.color(~r=141, ~g=96, ~b=214, ~a=25), env);
  Draw.rectf(~pos=(posX, posY), ~width=pWidth, ~height=pHeight, env);
};

/*  //////////////////////////// */
/*  /// Initiate New Game ///// */
/* /////////////////////////// */

let initialBirdy: bodyT = {
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

let picnic: picnicT = {
  position: {
    x: float_of_int(screenWidth) /. 2.0,
    y: float_of_int(screenHeight),
  },
  blanket: true,
  basket: true,
  watermelon: true,
  cherries: true,
};

let picnicker: picnickerT = {
  body: {
    position:
      /* x: 800.0, */
      {
        x: float_of_int(screenWidth) -. 300.0,
        y: float_of_int(screenHeight) -. 100.0,
      },
    velocity: {
      x: (-10.0),
      y: 0.0,
    },
    acceleration: {
      x: 0.0,
      y: 0.0,
    },
  },
  motivation: PICNIC,
};

let setup = env : stateT => {
  Env.size(~width=screenWidth, ~height=screenHeight, env);
  {birdy: initialBirdy, poops: [], picnic, picnicker};
};

let draw = ({birdy, poops, picnic, picnicker}, env) => {
  Draw.background(Utils.color(~r=19, ~g=217, ~b=229, ~a=255), env);
  /* debugDisplay(birdy, env); */
  debugDisplay(picnicker.body, env);
  Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);
  let posX = birdy.position.x;
  let posY = birdy.position.y;
  let pWidth = playerWidth;
  let pHeight = playerHeight;
  Draw.rectf(~pos=(posX, posY), ~width=pWidth, ~height=pHeight, env);
  drawPicnicker(picnicker, env);
  drawPicnic(picnic, env);
  Draw.fill(Utils.color(~r=241, ~g=255, ~b=254, ~a=255), env);
  List.iter(drawPoop(env), poops);

  let newBirdy: bodyT = getNewBirdy(birdy, env);
  let newPoops: list(bodyT) = getNewPoops(birdy, poops, env);
  let newPicnicker: picnickerT =
    updatePicnicker(picnicker, picnic, birdy, newPoops, env);
  {birdy: newBirdy, poops: newPoops, picnic, picnicker: newPicnicker};
};

run(~setup, ~draw, ());
