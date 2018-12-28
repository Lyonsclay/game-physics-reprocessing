open Reprocessing;
/* Global types */
open Types;
/* Global definitions */
open Game;

let rec keyMapString = (keysMap: list(string)): string =>
  switch (keysMap) {
  | [] => ""
  | [head, ...tail] when List.length(tail) === 0 => head
  | [head, ...tail] when List.length(tail) > 0 => head ++ keyMapString(tail)
  | _ => ""
  };

let getKeyMap = env: list(string) =>
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

let getKeysPressed = env: list(directionT) =>
  List.map(key => Env.key(key, env) ? key : Nothing, [H, J, K, L, Space])
  |> List.map(keyMap);
