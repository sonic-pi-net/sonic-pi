diff --git a/files/file.javascript b/files/file.javascript
index b9f1286..7cd3c5a 100644
--- a/files/file.javascript
+++ b/files/file.javascript
@@ -16,3 +16,4 @@ function getViewportH ()
     var client = docElem['clientHeight'],
-      inner = window['innerHeight'];
+      inner = window['innerHeight'],
+      sample = window['otherProperty'];
 
@@ -27,3 +28,3 @@ function getOffset (el)
       if (!isNaN(el.offsetTop)) {
-        offsetTop += el.offsetTop;
+        offsetTop += el.offsetTop + 1;
       }
@@ -43,8 +44,7 @@ function isElementInViewport (el, h)
         viewed = scrolled + getViewportH(),
-        elH = el.offsetHeight,
         elTop = getOffset(el).top,
-        elBottom = elTop + elH,
+        elBottom = elTop + el.offsetHeight,
         h = h || 0;
 
-    return (elTop + elH * h) <= viewed && (elBottom) >= scrolled;
+    return (elTop + el.offsetHeight * h) <= viewed && (elBottom) >= scrolled;
   }
@@ -60,4 +60,2 @@ _init: function ()
 
-  //  Initialize all scrollreveals, triggering all
-  //  reveals on visible elements.
       this.elems.forEach(function (el, i) {
@@ -71,3 +69,3 @@ var scrollHandler = function ()
             self._scrollPage();
-          }, 60);
+          }, 61);
         }
@@ -101,2 +99,3 @@ _scrollPage: function ()
         this.scrolled = false;
+		this.tested = true;
     },
