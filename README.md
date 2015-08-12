## Huptime

Simple program telling you when it's time to go home. Uses `last` command to get your first login today and then tells you how much time have you already spent at work and how much do you have left.

Assumes 8 hour working day.

## Example

    $ ./huptime 
    start | elaps | now   | rem   | end
    08:49 | 09:21 | 18:10 |(01:21)| 16:49 

(parentheses denote negative value.)

## Running

Simply run it:

    $ runhaskell huptime.hs username

Or compile and then run:

    $ ghc -Wall -Werror huptime.hs  
    $ ./huptime username

## Known issues

Log rotation - shown login may not be your actual first login the day the logs rotated.

## Motivation

  * [Hammock Driven Development - Rich Hickey](https://www.youtube.com/watch?v=f84n5oFoZBc)
  * [Go the Fuck Home: Engineering Work/Life Balance - Pam Selle](https://www.youtube.com/watch?v=YBoS-svKdgs).

## License

**The MIT License** - see LICENCE file.
