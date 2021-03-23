sh make.sh

node src/cli.js "$(cat example-originals/Anagram.elm)" > examples-normalized/Anagram.elm
node src/cli.js "$(cat example-originals/Bob.elm)" > examples-normalized/Bob.elm
node src/cli.js "$(cat example-originals/GradeSchool.elm)" > examples-normalized/GradeSchool.elm
node src/cli.js "$(cat example-originals/Hamming.elm)" > examples-normalized/Hamming.elm
node src/cli.js "$(cat example-originals/Leap.elm)" > examples-normalized/Leap.elm
node src/cli.js "$(cat example-originals/Pangram.elm)" > examples-normalized/Pangram.elm
node src/cli.js "$(cat example-originals/Raindrops.elm)" > examples-normalized/Raindrops.elm
node src/cli.js "$(cat example-originals/SumOfMultiples.elm)" > examples-normalized/SumOfMultiples.elm
node src/cli.js "$(cat example-originals/Triangle.elm)" > examples-normalized/Triangle.elm
node src/cli.js "$(cat example-originals/TwelveDays.elm)" > examples-normalized/TwelveDays.elm
