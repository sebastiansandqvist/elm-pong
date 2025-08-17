module Main exposing (main)

import Basics exposing (abs, modBy)
import Playground exposing (game, move, moveX, moveY, rectangle, black, white, blue, words)
import Set
import String exposing (fromInt)


gameWidth = 600
gameHeight = 400
ballSize = 6
paddleHeight = 50
paddleWidth = 6
leftPaddleX = -gameWidth / 2 + paddleWidth / 2
rightPaddleX = gameWidth / 2 - paddleWidth / 2
initialBallSpeed = 1


type alias Model =
  { scoreL : Int
  , scoreR : Int
  , ballX : Float
  , ballY : Float
  , leftY : Float
  , rightY : Float
  , ballDx : Float
  , ballDy : Float
  , ballSpeed : Float
  }


initialState : Model
initialState =
  { scoreL = 0
  , scoreR = 0
  , ballX = 0
  , ballY = 0
  , leftY = 0
  , rightY = 0
  , ballDx = initialBallSpeed
  , ballDy = initialBallSpeed
  , ballSpeed = initialBallSpeed
  }


main =
  game render update initialState


render { screen } { ballX, ballY, leftY, rightY, scoreL, scoreR } =
  [ -- blackout
    rectangle black screen.width screen.height
  -- game area
  , rectangle blue gameWidth gameHeight
  -- left paddle
  , rectangle white paddleWidth paddleHeight
      |> move leftPaddleX leftY
      -- |> moveX leftPaddleX
      -- |> moveY leftY
  -- right paddle
  , rectangle white paddleWidth paddleHeight
      |> moveX rightPaddleX
      |> moveY rightY
  -- ball
  , rectangle white ballSize ballSize
      |> move ballX ballY
  , words white (fromInt scoreL)
      |> move -30 ((gameHeight / 2) + 20)
  , words white (fromInt scoreR)
      |> move 30 ((gameHeight / 2) + 20)
  ]


updatePlayers keyboard pong =
  let
    paddleSpeed = 3
    isPressingUp = keyboard.up
    isPressingDown = keyboard.down
    isPressingW = Set.member "w" keyboard.keys
    isPressingS = Set.member "s" keyboard.keys
    newLeftY =
      if isPressingW then
        pong.leftY + paddleSpeed
      else if isPressingS then
        pong.leftY - paddleSpeed
      else
        pong.leftY
    newRightY =
      if isPressingUp then
        pong.rightY + paddleSpeed
      else if isPressingDown then
        pong.rightY - paddleSpeed
      else
        pong.rightY
    clampUpper = gameHeight / 2 - paddleHeight / 2
    clampLower = -gameHeight / 2 + paddleHeight / 2
  in
  { pong
    | leftY = clamp clampLower clampUpper newLeftY
    , rightY = clamp clampLower clampUpper newRightY
  }


getVerticalCollisionDy pong =
  if pong.ballY < (-gameHeight / 2 + ballSize / 2) then
    1
  else if pong.ballY > (gameHeight / 2 - ballSize / 2) then
    -1
  else if pong.ballDy > 0 then
    1
  else
    -1


getPaddleCollisionDx pong =
  let
    isCollidingRightX = abs (pong.ballX - rightPaddleX) < paddleWidth / 2 + ballSize / 2
    isCollidingLeftX = abs (pong.ballX - leftPaddleX) < paddleWidth / 2 + ballSize / 2
    isCollidingRightY = abs (pong.ballY - pong.rightY) < paddleHeight / 2 + ballSize / 2
    isCollidingLeftY = abs (pong.ballY - pong.leftY) < paddleHeight / 2 + ballSize / 2
  in
  if isCollidingRightX && isCollidingRightY then
    -(abs pong.ballSpeed) - 0.5
  else if isCollidingLeftX && isCollidingLeftY then
    abs pong.ballSpeed + 0.5
  else
    pong.ballDx


handleGoal pong =
  let
    offscreenRight = pong.ballX - ballSize / 2 > gameWidth / 2
    offscreenLeft = pong.ballX + ballSize / 2 < -gameWidth / 2
    totalScore = pong.scoreL + pong.scoreR
  in
  if offscreenRight then
    { initialState
      | scoreR = pong.scoreR
      , scoreL = pong.scoreL + 1
      , ballDx =
          if modBy 2 totalScore == 0 then
            -1
          else
            1
    }
  else if offscreenLeft then
    { initialState
      | scoreL = pong.scoreL
      , scoreR = pong.scoreR + 1
      , ballDx =
          if modBy 2 totalScore == 0 then
            -1
          else
            1
    }
  else
    pong


updateBall pong =
  let
    newDx = getPaddleCollisionDx pong
    newSpeed = abs newDx
    newDySign = getVerticalCollisionDy pong
    newDy = newSpeed * newDySign
  in
  { pong
    | ballX = pong.ballX + newDx
    , ballY = pong.ballY + newDy
    , ballDx = newDx
    , ballDy = newDy
    , ballSpeed = newSpeed
  }


update { keyboard } pong =
  updatePlayers keyboard pong
    |> updateBall
    |> handleGoal
