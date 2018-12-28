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

type bodyT = {
  position: positionT,
  velocity: velocityT,
  acceleration: accelerationT,
};

type directionT =
  | UP
  | DOWN
  | LEFT
  | RIGHT
  | NONE;

type picnicT = {
  position: positionT,
  blanket: bool,
  basket: bool,
  watermelon: bool,
  cherries: bool,
};

type motivationT =
  | PICNIC
  | LEAVE
  | FORAGE
  | HUNT
  | RAGE;

type bodyZonesT =
  | HEAD
  | CHEST
  | BICEPS
  | FOREARMS
  | HANDS
  | BELLY
  | HIPS
  | THIGHS
  | CALVES
  | FEET;

/* type impactT = list */
type picnickerT = {
  body: bodyT,
  motivation: motivationT,
};

type stateT = {
  birdy: bodyT,
  poops: list(bodyT),
  picnic: picnicT,
  picnicker: picnickerT,
};

