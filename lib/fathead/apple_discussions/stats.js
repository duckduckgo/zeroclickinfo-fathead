"use strict";
const fs = require('fs');

const raw_data = fs.readFileSync('./output.json', 'utf8');
const json_data = JSON.parse(raw_data);
const number_of_objects = Object.keys(json_data).length;

// the counter flags
let recommended = 0;
let helpful = 0;
let solved = 0;
let couldnt_parse = 0;

for(var key in json_data) {

  if (json_data[key].title === '') {
      couldnt_parse++;
  } 

  // recommended?
  if (json_data[key].recommended === true) {
      recommended++;
  }

  // helpful?
  if (json_data[key].helpful === true) {
      helpful++;
  }

  // solved?
  if (json_data[key].solved === true) {
      solved++;
  }
}

const recommended_perc = Math.round(recommended / number_of_objects * 100).toFixed(2);
const helpful_perc = Math.round(helpful / number_of_objects * 100).toFixed(2);
const solved_perc = Math.round(solved / number_of_objects * 100).toFixed(2);

/*
 *  THE OUTPUT STATS
 */
console.log('\n\n');
console.log('APPLE DISCUSSION STATISTICS');
console.log('<<'.repeat(20));

console.log(`${number_of_objects} objects`);
console.log(`Recommended: ${recommended} (${recommended_perc}%)`);
console.log(`Helpful: ${helpful} (${helpful_perc}%)`);
console.log(`Solved: ${solved} (${solved_perc}%)`);
console.log(`Couldn't parse ${couldnt_parse}`);

console.log('>>'.repeat(20));
console.log('\n\n');
