const encrypted = require('./encrypted');

const keypad = [
  [1, 2, 3],
  [4, 5, 6],
  [7, 8, 9]
];

const keypad2 = [
  ["#", "#", "1", "#", "#"],
  ["#", "2", "3", "4", "#"],
  ["5", "6", "7", "8", "9"],
  ["#", "A", "B", "C", "#"],
  ["#", "#", "D", "#", "#"]
]

const moveReducer = ({x, y}, step) => {
  return {x: x+step.x, y: y+step.y};
};

const constraintReducer = ({x, y}, {x: sX, y: sY}) => {
  const newX = Math.min(Math.max(x, 0), 2);
  const newY = Math.min(Math.max(y, 0), 2);
  return {x: newX, y: newY};
};

const constraintReducer2 = ({x: i, y: j}, {x: sX, y: sY}) => {
  if (!(j>=(2-i) && j<=(2+i) && j>=(2-(4-i)) && j<=(2+(4-i))))
    return {x: i-sX, y: j-sY};
  else
    return {x:i, y:j};
};

const decode = (char) => {
  return ({
    'U': {x: 0, y: -1},
    'D': {x: 0, y: 1},
    'R': {x: 1, y: 0},
    'L': {x: -1, y: 0}
  })[char];
};

let initialState = {
  //pos: {x: 1, y: 1},
  pos: {x:0, y:2},
  code: []
};

const keys =
  encrypted
    .split("\n")
    .map((l) => l.split(''))
    .reduce((state, encryptedKey) => {
      const finalPosition =
        encryptedKey
          .map(decode)
          .reduce((s, move) => {
            const movedState = moveReducer(s, move);
            const constrainedState = constraintReducer2(movedState, move);
            return constrainedState;
          }, state.pos);
      const finalKey = keypad2[finalPosition.y][finalPosition.x];
      return {
        pos: finalPosition,
        code: [...state.code, finalKey]
      };
    }, initialState).code.reduce((acc, key) => acc+key, '');

console.log('Answer:', keys);
