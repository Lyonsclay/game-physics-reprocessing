open Reprocessing;
/* Global types */
open Types;
/* Global definitions */
open Game;
open IO;
open Picnicker;
open Picnic;
open Birdy;
open Poops;

let getAccelerationList = (keys: list(directionT)): list(accelerationT) =>
  List.map(accelerationMap, keys);

let rec calculateBirdyAcceleration =
        (accelerationList: list(accelerationT)): accelerationT => {
  let acceleration: accelerationT =
    switch (accelerationList) {
    | [head, ...tail] when List.length(tail) === 0 => head
    | [head, ...tail] when List.length(tail) > 0 => {
        x: head.x +. calculateBirdyAcceleration(tail).x,
        y: head.y +. calculateBirdyAcceleration(tail).y,
      }
    | _ => {x: 0.0, y: 0.0}
    };

  {x: acceleration.x, y: acceleration.y};
};

let getNewBirdy = (bird: bodyT, env): bodyT => {
  let deltaTime: float = Env.deltaTime(env);
  let keys: list('a) = getKeysPressed(env);
  let thrustList: list(accelerationT) = getAccelerationList(keys);
  let accelerationList = [gravity, ...thrustList];
  let acceleration: accelerationT =
    calculateBirdyAcceleration(accelerationList);
  let velocity = getNewBirdyVelocity(bird, deltaTime, keys);
  let birdBody = {position: bird.position, velocity, acceleration};
  let newPosition = getNewBirdyPosition(birdBody, deltaTime);

  {position: newPosition, velocity, acceleration};
};

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

let setup = env: stateT => {
  Env.size(~width=screenWidth, ~height=screenHeight, env);
  {birdy: initialBirdy, poops: [], picnic, picnicker};
};

let draw = ({birdy, poops, picnic, picnicker}, env) => {
  Draw.background(Utils.color(~r=19, ~g=217, ~b=229, ~a=255), env);
  /* debugDisplay(birdy, env); */
  /* logging data */
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
