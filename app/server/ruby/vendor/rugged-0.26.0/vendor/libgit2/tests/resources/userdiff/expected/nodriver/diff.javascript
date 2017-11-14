diff --git a/files/file.javascript b/files/file.javascript
index 0965b37..5391797 100644
--- a/files/file.javascript
+++ b/files/file.javascript
@@ -4,4 +4,3 @@ define(function(require, exports, module) {
   var Key = require("./key")
-    , Direction = require("./direction")
-    , Image = require("./image");
+    , Direction = require("./direction");
 
@@ -16,4 +15,4 @@ define(function(require, exports, module) {
 
-    this.pixelX = 0;
-    this.pixelY = 0;
+    this.pixelX = 10;
+    this.pixelY = 10;
 
@@ -82,3 +81,3 @@ define(function(require, exports, module) {
   Player.prototype.moveLeft = function() {
-    this.x -= 1;
+    this.x -= 5;
     this.direction = Direction.LEFT;
@@ -106,3 +105,3 @@ define(function(require, exports, module) {
 
-    context.drawImage(this.image.data, offsetX, offsetY, 32, 48, this.pixelX, this.pixelY - 16, 32, 48);
+    context.drawImage(this.image.data, offsetX, offsetY, 32, 48, this.pixelX, this.pixelY, 32, 48);
   };
