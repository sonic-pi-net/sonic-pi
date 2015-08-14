var gulp = require("gulp");
var babel = require("gulp-babel");
var fileinclude = require("gulp-file-include");
var sourcemaps = require('gulp-sourcemaps');

gulp.task("copy-html", function() {
  return gulp.src("src/**/*.html")
    .pipe(gulp.dest("dist"))
});

gulp.task("copy-css", function() {
  return gulp.src("src/**/*.css")
    .pipe(gulp.dest("dist"))
});

gulp.task("copy-js", function() {
  return gulp.src("src/**/*.js")
    .pipe(gulp.dest("dist"))
});

gulp.task("babelize", ["copy-html", "copy-css", "copy-js"], function () {
  return gulp.src("dist/**/*.js")
    .pipe(babel())
    .pipe(sourcemaps.init())
    .pipe(sourcemaps.write('.'))
    .pipe(gulp.dest("dist"));
});

gulp.task("fileinclude", function() {
  gulp.src(["src/index.html"])
    .pipe(fileinclude({
      prefix: "@@",
      basepath: "@file"
    }))
    .pipe(gulp.dest("dist"));
});

gulp.task("default", ["babelize", "fileinclude"])
