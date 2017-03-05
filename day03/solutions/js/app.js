const _ = require('lodash');

const data = require('./data');

const triangles = data
  .split('\n')
  .map((line) => {
    return line.split(' ').filter((n) => n!='').map((n) => parseInt(n));
  })

const splittedTriangles = _.chunk(triangles, 3)
  .map((threeTriangles) => {
    return _.zip(...threeTriangles);
  })

const flatTriangles = _.flatten(splittedTriangles);

const numberOfRealTriangles = flatTriangles.filter(([a, b, c]) => a+b>c && a+c>b && b+c>a).length;


console.log(numberOfRealTriangles);
