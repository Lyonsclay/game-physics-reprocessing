open Reprocessing;
open Types;

/* Newtonian physics */
let speed = (v: velocityT) : float => sqrt(v.x *. v.x +. v.y *. v.y);
/* This will be used as a maximum speed limit for all objects/bodies. */
let terminalSpeed: float = 350.0;
let gravityY: float = 150.0;
let gravity: accelerationT = {x: 0.0, y: gravityY};
/* birdy acceleration */
let playerThrust: float = 350.0;
/* air friction */
let playerDrag: accelerationT = {x: 20.0, y: 0.0};

/* birdBoost - velocity to add when switching directions */
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
