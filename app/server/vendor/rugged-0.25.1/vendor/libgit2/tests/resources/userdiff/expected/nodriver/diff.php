diff --git a/files/file.php b/files/file.php
index 63250ad..967d646 100644
--- a/files/file.php
+++ b/files/file.php
@@ -12,2 +12,3 @@ class UniqueGenerator
     protected $maxRetries;
+	protected $moreStuff;
     protected $uniques = array();
@@ -17,3 +18,3 @@ class UniqueGenerator
         $this->generator = $generator;
-        $this->maxRetries = $maxRetries;
+        $this->maxRetries = $maxRetries + 1;
     }
@@ -33,10 +34,10 @@ class UniqueGenerator
     {
+        $i = 0;
         if (!isset($this->uniques[$name])) {
             $this->uniques[$name] = array();
         }
-        $i = 0;
         do {
             $res = call_user_func_array(array($this->generator, $name), $arguments);
             $i++;
-            if ($i > $this->maxRetries) {
+            if ($i >= $this->maxRetries) {
                 throw new \OverflowException(sprintf('Maximum retries of %d reached without finding a unique value', $this->maxRetries));
