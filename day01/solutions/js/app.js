const data = 'R5, L5, R5, R3'//require('./data');

const initialState = {
  pos: {x: 0, y: 0},
  direction: 0,
  path: [{x: 0, y: 0}],
  firstDouble: null
}

const moveReducer = (state, move) => {
  const newPos = {x: 0, y: 0}; //stub
  //console.log(state.path[0].x == newPos.x && state.path[0].y == newPos.y)
  const isFirstDouble = state
    .path
    .filter((pos) => pos.x == newPos.x && pos.y == newPos.y).length!=0
  && state.firstDouble==null;
  return {
    pos: newPos,
    direction: state.direction,
    path: [...state.path, newPos],
    firstDouble: (isFirstDouble) ? newPos : null
  }
};

const finalPosition = data
  .split(', ')
  .map((command) => {
    const [turn, number] = command.split('');
    return {turn: (turn=='R')?1:-1, number:parseInt(number)};
  }).reduce((state, move) => {
    return moveReducer(state, move);
  }, initialState);

console.log(finalPosition);
