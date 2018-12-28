/* open Reprocessing; */
open Types;
open Game;

/*  ////////////////////////////////////////  */
/*  \\\\ Birdy functions \ calculations \\\\  */
/*  ////////////////////////////////////////  */

let getNewBirdyPosition = ({velocity, position}, deltaTime: float): positionT => {
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

let getNewBirdyVelocity =
    ({velocity, position, acceleration}, deltaTime: float): velocityT => {
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
