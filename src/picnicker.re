open Reprocessing;
open Types;
open Game;

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
    | RAGE when body.velocity.x > 0.0 => Random.int(100) > 97 ? (-31.0) : 50.0
    | RAGE when body.velocity.x < 0.0 => Random.int(100) > 97 ? 31.0 : (-50.0)
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
  let newPositionX: float =
    switch (posX) {
    | _ when posX > float_of_int(screenWidth) => float_of_int(screenWidth)
    | _ when posX < 0.0 => 0.0
    | _ => posX +. newVelocityX *. deltaTime
    };

  /* let newPositionY: positionT = */
  /*   switch (posY) { */
  /*   | _ when posY > screenHeight => screenHeight */
  /*   | _ when posY < 0 => 0 */
  /*   | _ => posY +. newVelocityY *. deltaTime */
  /*   }; */

  let newMotivation: motivationT =
    switch (hitsCount) {
    | _ when headShot => RAGE
    | _ when hitsCount > 4 => RAGE
    | 0 => motivation
    | 2 => LEAVE
    | 3 => LEAVE
    | 4 => LEAVE
    | _ => motivation
    };
  {
    body: {
      velocity: {
        x: newVelocityX,
        y: 0.0,
      },
      position: {
        x: newPositionX,
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

/* let advancePosition = (picnicker: bodyT) : bodyT => {}; */
