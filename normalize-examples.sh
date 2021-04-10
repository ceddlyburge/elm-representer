sh make.sh

cat example-originals/Anagram.elm        | node src/cli.js > examples-normalized/Anagram.elm
cat example-originals/Bob.elm            | node src/cli.js > examples-normalized/Bob.elm
cat example-originals/GradeSchool.elm    | node src/cli.js > examples-normalized/GradeSchool.elm
cat example-originals/Hamming.elm        | node src/cli.js > examples-normalized/Hamming.elm
cat example-originals/Leap.elm           | node src/cli.js > examples-normalized/Leap.elm
cat example-originals/Pangram.elm        | node src/cli.js > examples-normalized/Pangram.elm
cat example-originals/Raindrops.elm      | node src/cli.js > examples-normalized/Raindrops.elm
cat example-originals/SumOfMultiples.elm | node src/cli.js > examples-normalized/SumOfMultiples.elm
cat example-originals/Triangle.elm       | node src/cli.js > examples-normalized/Triangle.elm
cat example-originals/TwelveDays.elm     | node src/cli.js > examples-normalized/TwelveDays.elm
