# BreakOut with Haskell

## Proposal
### Goal

To implement a Brick Out game with Haskell with basic UI.
Construct winning logic and auto generated bricks for different game level 
Implement game logic for ball reflection when hit the brick and change its move path base on hitting angle 

### Description

Our project is based on Haskell and the 'brick' library to complete a game of hitting bricks using the bounce of pinballs. We will create a command-line game environment, using characters to simulate target bricks and pinballs. Players need to use the left and right keys of the keyboard to control the movement of the bottom board, bounce the pinball and eliminate the bricks.

### Approach

We plan to use haskell to implement the game logic including position updating, collision detection, paddle movement and score mechanism, during which we will get a deeper insight of the haskell language.

### Member

This project will be done by group 9 of CSE230-22fall with members of Xiaolong Dai, Junliang Liu, Shujing Zhang, Yinliang Wang.

## Updates

### Architecture

Our project will use the MVC architecture. The project is mainly divided into three parts: 1. Model, which defines the data structures. 2. View, which defines the display and generates the Terminal UI (TUI) with the 'bricks' library. 3. Control, which contains the control logic and the interaction logic. 
<center><img width=75% src="mvc.png"></img></center>

### Challenges
After doing some further investigation into the Haskell library, we found that the UI library we originally planned to use might not give the better effect. We planned to use TUI but the moving path of the ball is mostly diagonal. As TUI is based on character, we have to round the location of the ball.  

### Timeline

11/25 Finish basic functions, including models, control units and part of the visualization.  
12/2  Add more on the TUI of the project.   
12/6  Finish project and prepare for the presentation  

### Expectations

We expect to meet our goals until the deadline  



#### Reference 
1. [MVC](https://developer.mozilla.org/en-US/docs/Glossary/MVC)  

