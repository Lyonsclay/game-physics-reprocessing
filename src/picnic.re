open Reprocessing;
open Types;

/* let drawPicnic = (picnic: picnicT, env) => { */
/*   let posX = picnic.position.x; */
/*   let posY = picnic.position.y; */

/*   if (picnic.blanket) { */
/*     Draw.fill(Utils.color(~r=251, ~g=255, ~b=204, ~a=255), env); */
/*     Draw.rectf(~pos=(posX, posY), ~width=150.0, ~height=10.0, env); */
/*   }; */
/*   if (picnic.basket) { */
/*     Draw.fill(Utils.color(~r=21, ~g=25, ~b=24, ~a=255), env); */
/*     Draw.rectf(~pos=(posX, posY), ~width=35.0, ~height=25.0, env); */
/*   }; */
/*   if (picnic.watermelon) { */
/*     Draw.fill(Utils.color(~r=21, ~g=205, ~b=24, ~a=255), env); */
/*     Draw.ellipsef(~center=(posX +. 20.0, 20.0), ~radx=40.0, ~rady=20.0, env); */
/*   }; */
/*   if (picnic.cherries) { */
/*     Draw.fill(Utils.color(~r=210, ~g=5, ~b=24, ~a=255), env); */
/*     for (i in 1 to 10) { */
/*       let x = Random.float(20.0); */
/*       let y = Random.float(20.0); */
/*       Draw.ellipsef( */
/*         ~center=(posX +. 80.0 +. x, 15.0 +. y), */
/*         ~radx=5.0, */
/*         ~rady=5.0, */
/*         env, */
/*       ); */
/*     }; */
/*   }; */
/* }; */

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
