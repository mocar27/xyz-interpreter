# Xyz-Interpreter

Project initiated using Stack.

## Usage

Hello

## Files

*Main* file is the main file taking the input and passing it to interpreter.

*Interpreter* file is the file that runs Parser & Lexer on given input, 
then if it returns success it runs TypeChecker, and if the result is successful
it runs the Evaluator. 

## Some nuances / changes to initial grammar

It is not permitted to create Void function. Function must have a type and must return something.

For evaluator to be successful in given input program there has to be `main` function defined.

Reference work in general, just we have to have prefix 'var' in the function argument. 
(The only time it does not work, is if you pass actually just a number, eg. 1, 2, 3... etc., not able to pass const) (My bad :( )
Rest of the types passing work as intended.

## Grammar

Hello

## Sources used during programming

- My own laboratory assignments codes
- Provided lab descriptions on Moodle (Monads 1., Monads 2., Lab Haskell-Extra, Types)
- https://wiki.haskell.org/wikiupload/c/c6/ICMI45-paper-en.pdf
- https://learnyouahaskell.com/chapters
- https://jameshfisher.com/2018/03/06/an-interpreter-in-haskell/ 
- https://bor0.wordpress.com/2019/03/15/writing-a-simple-evaluator-and-type-checker-in-haskell/
- https://pappasbrent.com/blog/2022/06/11/writing-an-interpreter-in-haskell.html

## Concluding remarks

*None*
