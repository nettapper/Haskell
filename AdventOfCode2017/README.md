# AoC 2017
http://adventofcode.com/2017

# Running the code
- First get `stack`
- execute `stack filename.hs`
- then enter the appropriate input

# Questions
## 1 Capcha
``` text
λ git master* → stack 1.captcha.hs
111
the captcha of 111 is: 3
------

captcha
  1122 should be 3
  1111 should be 4
  1234 should be 0
  91212129 should be 3

Finished in 0.0015 seconds
4 examples, 0 failures
```
## 2 Checksum
Enter you input line by line  
press return after the last line  
then press `ctrl-d` to signal that you're done inputting  
``` text
λ git master* → stack 2.checksum.hs
5 1 9 5
7 5 3
2 4 6 8
18
---

calcChecksum
  example given should be 18
mySplit
  should return a empty for an empty
  should 4 elems given "5 1 9 5"

Finished in 0.0020 seconds
3 examples, 0 failures
```
