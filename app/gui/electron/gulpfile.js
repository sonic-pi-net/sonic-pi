var gulp = require("gulp");
var babel = require("gulp-babel");

gulp.task("copy", function() {
  return gulp.src("src/**/*.*")
    .pipe(gulp.dest("dist"))
});

gulp.task("babelize", ["copy"], function () {
  return gulp.src("dist/**/*.js")
    .pipe(babel())
    .pipe(gulp.dest("dist"));
});

gulp.task("default", ["babelize"])
