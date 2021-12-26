define(function(require, exports, module) {
  module.exports = Player;

  var Key = require("./key")
    , Direction = require("./direction");

  function Player(game) {
    this.game = game;

    this.image = new Image("./assets/fighter.png");
    this.game.resources.add(this.image);

    this.x = 0;
    this.y = 0;

    this.pixelX = 10;
    this.pixelY = 10;

    this.animationStep = 0;
  }

  Player.prototype.update = function() {
    if (!this.isWalking()) {
      this.handleInput();
    }

    if (this.isWalking()) {
      // Increase the animation step.
      this.animationStep = ++this.animationStep % 60;

      if (this.x * 32 > this.pixelX) {
        this.pixelX++;
      } else if (this.x * 32 < this.pixelX) {
        this.pixelX--;
      }

      if (this.y * 32 > this.pixelY) {
        this.pixelY++;
      } else if (this.y * 32 < this.pixelY) {
        this.pixelY--;
      }
    } else {
      // Reset the animation step.
      this.animationStep = 0;
    }
  };

  Player.prototype.handleInput = function() {
    var keyboard = this.game.keyboard, finalAction, action, inputs = {
      'moveDown': keyboard.isDown(Key.DOWN),
      'moveUp': keyboard.isDown(Key.UP),
      'moveLeft': keyboard.isDown(Key.LEFT),
      'moveRight': keyboard.isDown(Key.RIGHT)
    };

    for (action in inputs) {
      if (inputs[action]) {
        if (!finalAction || inputs[finalAction] < inputs[action]) {
          finalAction = action;
        }
      }
    }

    this[finalAction] && this[finalAction]();
  };

  Player.prototype.isWalking = function() {
    return this.x * 32 != this.pixelX || this.y * 32 != this.pixelY;
  };

  Player.prototype.moveDown = function() {
    this.y += 1;
    this.direction = Direction.DOWN;
  };

  Player.prototype.moveUp = function() {
    this.y -= 1;
    this.direction = Direction.UP;
  };

  Player.prototype.moveLeft = function() {
    this.x -= 5;
    this.direction = Direction.LEFT;
  };

  Player.prototype.moveRight = function() {
    this.x += 1;
    this.direction = Direction.RIGHT;
  };

  Player.prototype.draw = function(context) {
    var offsetX = Math.floor(this.animationStep / 15) * 32, offsetY = 0;

    switch(this.direction) {
      case Direction.UP:
        offsetY = 48 * 3;
        break;
      case Direction.RIGHT:
        offsetY = 48 * 2;
        break;
      case Direction.LEFT:
        offsetY = 48;
        break;
    }

    context.drawImage(this.image.data, offsetX, offsetY, 32, 48, this.pixelX, this.pixelY, 32, 48);
  };
});
