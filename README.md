(The following was taken from the [assignment page for the 8th homework assignment](https://course.ccs.neu.edu/cs2500f19/hw10.html) as part of Northeastern University's CS2500 - Fundamentals of Computer Science, I curriculum for the fall semester of the 2019 school year. This homework was completed by David Malone and [Cameron Boggio](https://github.com/undercovercar0t) and was due on Noveember 15th, 2019.)

## Exercise 3
It is now time to complete your project for this semester.

First, review Homework 5 to remind yourself of the ideas, terms, and data definitions involved.

Next, download [this file](https://course.ccs.neu.edu/cs2500/f19projectdata.zip) which contains two folders ("test" and "train") of input files. Each file name has the following format: d_<id>_<digit>.txt where <digit> is the correct label for the file and <id> depends on the folder. In the test folder the id was related to the original dataset and can be ignored. In the train folder the ids are sequential from 1-30, meaning that you have 30 examples for each digit.
Your overall goal will be to design the mnist world program, which takes a Number and a String. The number represents how many training files there are per digit, and the string is the path to the testing file that should be used. This function should then execute the nearest-neighbor algorithm: compute all the neighbors (by comparing the indicated testing to each of the training), find the "best" (the one with the smallest distance), and run the interactive visual program described in Homework 5. In this program the training image is shown, as well as the nearest neighbor and predicted label, and the user can scroll left/right through all the other neighbors using the keyboard, viewing the image as well as the distance. Finally, ```mnist``` should return the digit of the nearest neighbor (i.e., the prediction of the classification algorithm).
This probably seems like a lot of work, but you’ve actually done most of the work in earlier homework assignments. Here are some reminders...

* Reading in files: look to Homework 8 for ```training-fnames```, which produces a list of file names based upon the pattern described above. Also, look to ```fname->label``` for getting the digit from the filename. In Homework 9, you designed read-lolon which will be useful for getting the contents of the files into a list of lists of numbers. And Homework 8 ```flatten``` is useful for converting from a Bitmap to an Instance. Finally, Homework 8 had ```bitmap->image```, which might be useful for producing an image from a Bitmap.

* Distance between Training/Testing: for distance, you should use Euclidean distance, which is basically Pythagorean distance but can take coordinates with more than 2 dimensions. The formula for Euclidean distance looks like this: ```distance = (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2)) (sqr (- z1 z2)) ...))``` If you look back to Homework 9, ```map-2list``` can be really handy in computing this.

* Nearest neighbor: given a list of neighbors, the nearest is the one with the smallest distance. Look to ```smallest-of-list-by-f``` in Homework 8 for a great start.

* Scrolling left/right in a list: in Homework 8, look to ```next-index``` and ```prev-index``` to help with this.

* Returning the prediction: in Homework 8, look to ```return-former``` as one way to make sure ```mnist``` returns what you want, which may or may not be returned by big-bang.

Warning: when you are designing your world state, avoid putting all the neighbors in your representation - DrRacket will be very slow. Instead, see if you can’t just keep track of where your are in the list.

In case you are curious, the image shown in HW5 was produced using the following call: ```(mnist 10 "test/d_55555_9.txt")```
