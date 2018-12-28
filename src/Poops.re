open Reprocessing;
open Types;
open Game;

/*  ///////////////////////////////////////  */
/*  \\\\ Poop functions \ calculations \\\\  */
/*  ///////////////////////////////////////  */

let addNewPoop = (player: bodyT): bodyT => {
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

let filterOffScreen = (poop: bodyT): bool =>
  switch (poop.position) {
  | {x, _} when x < 0.0 -. poopWidth => false
  | {x, _} when x > float_of_int(screenWidth) +. poopWidth => false
  | {y, _} when y > float_of_int(screenHeight) +. poopHeight => false
  | _ => true
  };

let canAddPoop = (poops: list(bodyT), player: bodyT): bool =>
  switch (poops) {
  | [] => true
  | _ when List.length(poops) > 7 => false
  | [head, ..._] when head.position.y -. player.position.y > 80.0 => true
  | _ => false
  };

let getNewPoops = (player: bodyT, poops: list(bodyT), env): list(bodyT) => {
  let deltaTime: float = Env.deltaTime(env);
  let pressedSpaceKey: bool = Env.key(Space, env);
  let addPoop: bool = pressedSpaceKey && canAddPoop(poops, player);
  let updatePoop = (poop: bodyT): bodyT => {
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
