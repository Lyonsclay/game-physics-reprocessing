open Reprocessing;
open Types;

let updatePicnicker =
    (
      {body, motivation}: picnickerT,
      picnic: picnicT,
      bird: bodyT,
      deltaTime: float,
    ) => {
  let xPos = body.position.x;
  let newXVelocity: float =
    switch (motivation) {
    | PICNIC when xPos > picnic.position.x => (-20.0)
    | PICNIC when xPos < picnic.position.x => 20.0
    | LEAVE when xPos > picnic.position.x => 30.0
    | LEAVE when xPos < picnic.position.x => (-30.0)
    | FORAGE => Random.bool() ? (-10.0) : 10.0
    | HUNT when xPos > bird.position.x => (-5.0)
    | HUNT when xPos < bird.position.x => 5.0
    | _ => 0.0
    };

  {
    velocity: {
      x: newXVelocity,
      y: 0.0,
    },
    position: {
      x: newXVelocity *. deltaTime,
      y: 0.0,
    },
    acceleration: {
      x: 0.0,
      y: 0.0,
    },
  };
};

let drawPicnicker = (picnicker: picnickerT, picnic: picnicT, bird: bodyT, env) => {
  let deltaTime: float = Env.deltaTime(env);
  let newPicnicker = updatePicnicker(picnicker, picnic, bird, deltaTime);
  let posX = newPicnicker.position.x;
  let posY = newPicnicker.position.y;
  let pWidth = 25.0;
  let pHeight = 100.0;
  Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=25), env);
  Draw.rectf(~pos=(posX, posY), ~width=pWidth, ~height=pHeight, env);
};

/* let advancePosition = (picnicker: bodyT) : bodyT => {}; */
