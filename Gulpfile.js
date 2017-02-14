var gulp = require('gulp');
var sass = require('gulp.sass');
var concat = require('gulp-concat');
var rename = require('gulp-rename');

gulp.task('scss', function () {
  return gulp.src('_scss/**/*.scss')
    .pipe(sass())
    .pipe(concat())
    .pipe(rename('bundle.css'))
    .pipe(gulp.dest('css'));
});

gulp.task('js', function () {
  return gulp.src(['_js/**/*.js'])
    .pipe(concat())
    .pipe(rename('bundle.js'))
    .pipe(gulp.dest('js'));
});

gulp.task('default', ['scss', 'js']);
